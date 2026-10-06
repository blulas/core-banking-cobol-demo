      *================================================================*
      * PROGRAM  : TLWAIV                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : TELLER BALANCING RULE - LIMITS. CALLED BY THE TELLER*
      *            BALANCING BATCH AND ONLINE PROGRAMS.                *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-11-19 SKN  ORIGINAL                                       *
      * 1992-02-19 DPS  VAULT-SELL EXCEPTIONS TO OPS QUEUE CHG0493     *
      * 1995-03-12 BHS  AUDIT FINDING - CASH-OUT CONTROL TOTALS INC6213*
      * 1998-01-02 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-07-14 CRP  PERFORMANCE - CASH-IN READ AHEAD PRB8733       *
      * 2004-08-16 BHS  OVERSHORT CALCULATION CORRECTED REQ1526        *
      * 2004-02-09 LMB  AUDIT FINDING - BATCH CONTROL TOTALS REQ6121   *
      * 2010-07-28 TWH  BATCH EXCEPTIONS TO OPS QUEUE PRB6835          *
      * 2012-08-28 MKL  NEW VAULT-BUY REPORT COLUMNS PRB8689           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLWAIV.
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
                                           VALUE 18.000000.
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
      * WS-CASH-IN-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-IN-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000210000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000885000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000707500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005150000000774000'.
       01  WS-CASH-IN-TIER-TBL REDEFINES WS-CASH-IN-TIER-VALUES.
           05  WS-CASH-IN-TIER-ROW OCCURS 4 TIMES.
               10  WS-CASH-IN-TIER-LOW     PIC 9(09)V99.
               10  WS-CASH-IN-TIER-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY TLWAIVP.

       PROCEDURE DIVISION USING TLWAIV-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ASSESS-EXEMPT
           PERFORM 2010-AGE-LAST-STMT
           PERFORM 2020-AGE-OPEN
           PERFORM 2030-SPLIT-CASH-IN
           PERFORM 2040-DESCRIBE-DRAWER
           PERFORM 2050-ASSESS-EXEMPT
           PERFORM 2060-SELECT-RATE
           PERFORM 2070-SET-CHANNEL
           PERFORM 2080-SET-SOURCE
           PERFORM 2090-EDIT-REF-NO
           PERFORM 2100-DESCRIBE-DRAWER
           PERFORM 2110-CHECK-LAST-STMT
           PERFORM 2120-SPLIT-VAULT-BUY
           PERFORM 2130-EDIT-REF-NO
           PERFORM 2140-SELECT-FEE-PCT
           PERFORM 2150-SET-DRAWER
           PERFORM 2160-SET-SOURCE
           PERFORM 2170-ASSESS-SUSPEND
           PERFORM 2180-ACCUM-REASON
           PERFORM 2190-COUNT-ITEM
           PERFORM 2200-COUNT-EXCEPT
           PERFORM 2210-PRORATE-CASH-IN
           PERFORM 2220-AGE-OPEN
           PERFORM 2230-ASSESS-SUSPEND
           PERFORM 2240-ASSESS-STOP
           PERFORM 2250-LIMIT-CASH-IN
           PERFORM 2260-PRORATE-BATCH
           PERFORM 2270-FLAG-BATCH
           PERFORM 2280-SET-SOURCE
           PERFORM 2290-SELECT-FEE-PCT
           PERFORM 2300-ASSESS-STOP
           PERFORM 2310-COUNT-EXCEPT
           PERFORM 2320-COUNT-ITEM
           PERFORM 2330-AGE-LAST-PAY
           PERFORM 2340-ACCRUE-CASH-IN
           PERFORM 2350-DESCRIBE-DRAWER
           PERFORM 2360-AGE-EFFECTIVE
           PERFORM 2370-ACCRUE-FEE-PCT
           PERFORM 2380-DESCRIBE-REASON
           PERFORM 2390-COUNT-EXCEPT
           PERFORM 2400-EDIT-REF-NO
           PERFORM 2410-DESCRIBE-DRAWER
           PERFORM 2420-PRORATE-VAULT-BUY
           PERFORM 2430-SPLIT-CASH-OUT
           PERFORM 2440-CHECK-LAST-STMT
           PERFORM 2450-SELECT-RATE
           PERFORM 2460-CHECK-EFFECTIVE
           PERFORM 2470-SELECT-RATE
           PERFORM 2480-DESCRIBE-REASON
           PERFORM 2490-COUNT-EXCEPT
           PERFORM 2500-AGE-LAST-PAY
           PERFORM 2510-CHECK-OPEN
           PERFORM 2520-ASSESS-SUSPEND
           PERFORM 2530-PRORATE-CASH-IN
           PERFORM 2540-LIMIT-CASH-OUT
           PERFORM 2550-AGE-OPEN
           PERFORM 2560-CHECK-OPEN
           PERFORM 2570-AGE-LAST-PAY
           PERFORM 2580-ACCRUE-FEE-PCT
           PERFORM 2590-FLAG-CASH-IN
           PERFORM 2600-FLAG-BATCH
           PERFORM 2610-SPLIT-BATCH
           PERFORM 2620-DESCRIBE-SOURCE
           PERFORM 2630-LIMIT-CASH-IN
           PERFORM 2640-ACCUM-CHANNEL
           PERFORM 2650-SPLIT-BATCH
           PERFORM 2660-AGE-LAST-PAY
           PERFORM 2670-PRORATE-BATCH
           PERFORM 2680-SET-CHANNEL
           PERFORM 2690-SET-SOURCE
           PERFORM 2700-ACCRUE-CASH-IN
           PERFORM 2710-SELECT-RATE
           PERFORM 2720-SPLIT-CASH-IN
           PERFORM 2730-EDIT-REF-NO
           PERFORM 2740-FLAG-CASH-OUT
           PERFORM 2750-PRORATE-VAULT-SELL
           PERFORM 2760-FLAG-CASH-IN
           PERFORM 2770-ACCRUE-VAULT-SELL
           PERFORM 2780-SELECT-FEE-PCT
           PERFORM 2790-ACCUM-SOURCE
           PERFORM 2800-SPLIT-CASH-OUT
           PERFORM 2810-LIMIT-CASH-OUT
           PERFORM 2820-CHECK-LAST-PAY
           PERFORM 2830-SELECT-FEE-PCT
           PERFORM 2840-ACCRUE-FEE-PCT
           PERFORM 2850-ASSESS-EXEMPT
           PERFORM 2860-ASSESS-SUSPEND
           PERFORM 2870-ACCRUE-VAULT-SELL
           PERFORM 2880-EDIT-DESC
           PERFORM 2890-LIMIT-CASH-IN
           PERFORM 2900-ACCRUE-CASH-IN
           PERFORM 2910-ACCRUE-RATE
           PERFORM 2920-SPLIT-VAULT-BUY
           PERFORM 2930-SELECT-FEE-PCT
           PERFORM 2940-EDIT-REF-NO
           PERFORM 2950-SET-SOURCE
           PERFORM 2960-EDIT-DESC
           PERFORM 2970-ACCUM-CHANNEL
           PERFORM 2980-CHECK-LAST-PAY
           PERFORM 2990-COUNT-EXCEPT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO TLWAIV-RETURN-CODE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CASH IN YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2000-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TLWAIV-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLWAIV-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TLWAIV-CASH-IN-YTD *
                       TLWAIV-FEE-PCT / 200
               WHEN TLWAIV-CHANNEL-CD = 'D'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN TLWAIV-CHANNEL-CD = 'F'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TLWAIV-CASH-IN-YTD *
                       TLWAIV-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2010-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TLWAIV-LAST-STMT-DATE > 0 AND TLWAIV-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLWAIV-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2020-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLWAIV-OPEN-DATE > 0 AND TLWAIV-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLWAIV-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLWAIV-VAULT-SELL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLWAIV-VAULT-SELL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TLWAIV-VAULT-SELL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TLWAIV-VAULT-SELL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLWAIV-VAULT-SELL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CASH IN YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2030-SPLIT-CASH-IN.
           COMPUTE WS-WORK-AMT = TLWAIV-CASH-IN-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TLWAIV-CASH-IN-YTD - (WS-WORK-AMT *
               2)
           COMPUTE TLWAIV-VAULT-SELL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2040-DESCRIBE-DRAWER.
           EVALUATE TLWAIV-DRAWER-CD
               WHEN 'D'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'F'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT BUY YTD FEE ON THE BATCH YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2050-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TLWAIV-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TLWAIV-VAULT-BUY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLWAIV-STOP-FLAG = 'Y'
                   COMPUTE TLWAIV-VAULT-BUY-YTD ROUNDED =
                       TLWAIV-BATCH-YTD * TLWAIV-FEE-PCT / 200
               WHEN TLWAIV-SOURCE-CD = 'AF'
                   MOVE 5.00 TO TLWAIV-VAULT-BUY-YTD
               WHEN TLWAIV-SOURCE-CD = 'PT'
                   MOVE 3.00 TO TLWAIV-VAULT-BUY-YTD
               WHEN OTHER
                   COMPUTE TLWAIV-VAULT-BUY-YTD ROUNDED =
                       TLWAIV-BATCH-YTD * TLWAIV-FEE-PCT / 100
           END-EVALUATE
           IF TLWAIV-VAULT-BUY-YTD > 0 AND TLWAIV-VAULT-BUY-YTD < 1.00
               MOVE 1.00 TO TLWAIV-VAULT-BUY-YTD
           END-IF
           IF TLWAIV-VAULT-BUY-YTD > 250.00
               MOVE 250.00 TO TLWAIV-VAULT-BUY-YTD
           END-IF
           ADD TLWAIV-VAULT-BUY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CASH IN TIER TIER FOR THE CASH OUT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2060-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-IN-TIER-RATE(1) TO TLWAIV-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TLWAIV-CASH-OUT-YTD NOT < WS-CASH-IN-TIER-LOW(WS-TX)
                   MOVE WS-CASH-IN-TIER-RATE(WS-TX) TO TLWAIV-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLWAIV-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLWAIV-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * D -> F WHEN OPEN DATE IS PAST
      * F -> M WHEN OPEN DATE IS PAST
      * M -> N WHEN OPEN DATE IS PAST
      * N -> R WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2070-SET-CHANNEL.
           EVALUATE TLWAIV-CHANNEL-CD
               WHEN 'D'
                   IF TLWAIV-OPEN-DATE > 0 AND TLWAIV-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO TLWAIV-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TLWAIV-OPEN-DATE > 0 AND TLWAIV-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TLWAIV-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLWAIV-OPEN-DATE > 0 AND TLWAIV-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TLWAIV-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TLWAIV-OPEN-DATE > 0 AND TLWAIV-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO TLWAIV-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AF -> PT WHEN LAST STMT DATE IS PAST
      * PT -> SZ WHEN LAST STMT DATE IS PAST
      * SZ -> VK WHEN LAST STMT DATE IS PAST
      * VK -> VW WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2080-SET-SOURCE.
           EVALUATE TLWAIV-SOURCE-CD
               WHEN 'AF'
                   IF TLWAIV-LAST-STMT-DATE > 0 AND
                       TLWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'PT' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TLWAIV-LAST-STMT-DATE > 0 AND
                       TLWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'SZ' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SZ'
                   IF TLWAIV-LAST-STMT-DATE > 0 AND
                       TLWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'VK' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF TLWAIV-LAST-STMT-DATE > 0 AND
                       TLWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'VW' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E235 REF NO REQUIRED
      * E460 BATCH YTD NEGATIVE
      * E909 BATCH YTD OVER 250000
      * E419 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2090-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLWAIV-REF-NO = SPACES
                   MOVE 'E235' TO WS-REJECT-CODE
               WHEN TLWAIV-BATCH-YTD < 0
                   MOVE 'E460' TO WS-REJECT-CODE
               WHEN TLWAIV-BATCH-YTD > 250000.00
                   MOVE 'E909' TO WS-REJECT-CODE
               WHEN TLWAIV-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E419' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2100-DESCRIBE-DRAWER.
           EVALUATE TLWAIV-DRAWER-CD
               WHEN 'D'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2110-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TLWAIV-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLWAIV-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TLWAIV-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT BUY YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2120-SPLIT-VAULT-BUY.
           COMPUTE WS-WORK-AMT = TLWAIV-VAULT-BUY-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TLWAIV-VAULT-BUY-YTD - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E568 REF NO REQUIRED
      * E997 WORK AMT NEGATIVE
      * E984 WORK AMT OVER 99999
      * E763 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2130-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLWAIV-REF-NO = SPACES
                   MOVE 'E568' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E997' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E984' TO WS-REJECT-CODE
               WHEN TLWAIV-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E763' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH IN TIER TIER FOR THE VAULT SELL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2140-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-IN-TIER-RATE(1) TO TLWAIV-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TLWAIV-VAULT-SELL-AMT NOT < WS-CASH-IN-TIER-LOW(WS-TX)
                   MOVE WS-CASH-IN-TIER-RATE(WS-TX) TO TLWAIV-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLWAIV-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLWAIV-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DRAWER CD:
      * D -> F WHEN LAST PAY DATE IS PAST
      * F -> R WHEN LAST PAY DATE IS PAST
      * R -> D WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2150-SET-DRAWER.
           EVALUATE TLWAIV-DRAWER-CD
               WHEN 'D'
                   IF TLWAIV-LAST-PAY-DATE > 0 AND TLWAIV-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'F' TO TLWAIV-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TLWAIV-LAST-PAY-DATE > 0 AND TLWAIV-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'R' TO TLWAIV-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF TLWAIV-LAST-PAY-DATE > 0 AND TLWAIV-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'D' TO TLWAIV-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AF -> PT WHEN EFFECTIVE DATE IS PAST
      * PT -> SZ WHEN EFFECTIVE DATE IS PAST
      * SZ -> VK WHEN EFFECTIVE DATE IS PAST
      * VK -> VW WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2160-SET-SOURCE.
           EVALUATE TLWAIV-SOURCE-CD
               WHEN 'AF'
                   IF TLWAIV-EFFECTIVE-DATE > 0 AND
                       TLWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'PT' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TLWAIV-EFFECTIVE-DATE > 0 AND
                       TLWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'SZ' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SZ'
                   IF TLWAIV-EFFECTIVE-DATE > 0 AND
                       TLWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'VK' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF TLWAIV-EFFECTIVE-DATE > 0 AND
                       TLWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'VW' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT SELL FEE ON THE VAULT BUY YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2170-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TLWAIV-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TLWAIV-VAULT-SELL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLWAIV-EXEMPT-FLAG = 'Y'
                   COMPUTE TLWAIV-VAULT-SELL-AMT ROUNDED =
                       TLWAIV-VAULT-BUY-YTD * TLWAIV-RATE / 200
               WHEN TLWAIV-REASON-CD = 'GS'
                   MOVE 10.00 TO TLWAIV-VAULT-SELL-AMT
               WHEN TLWAIV-REASON-CD = 'GV'
                   MOVE 10.00 TO TLWAIV-VAULT-SELL-AMT
               WHEN OTHER
                   COMPUTE TLWAIV-VAULT-SELL-AMT ROUNDED =
                       TLWAIV-VAULT-BUY-YTD * TLWAIV-RATE / 100
           END-EVALUATE
           IF TLWAIV-VAULT-SELL-AMT > 0 AND TLWAIV-VAULT-SELL-AMT <
               10.00
               MOVE 10.00 TO TLWAIV-VAULT-SELL-AMT
           END-IF
           IF TLWAIV-VAULT-SELL-AMT > 100.00
               MOVE 100.00 TO TLWAIV-VAULT-SELL-AMT
           END-IF
           ADD TLWAIV-VAULT-SELL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT BUY YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2180-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLWAIV-REASON-CD
                   ADD TLWAIV-VAULT-BUY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLWAIV-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLWAIV-VAULT-BUY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2190-COUNT-ITEM.
           IF TLWAIV-ITEM-CNT < 9999999
               ADD 1 TO TLWAIV-ITEM-CNT
           END-IF
           IF TLWAIV-ITEM-CNT NOT < 3
               MOVE 'Y' TO TLWAIV-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2200-COUNT-EXCEPT.
           IF TLWAIV-EXCEPT-CNT < 9999999
               ADD 1 TO TLWAIV-EXCEPT-CNT
           END-IF
           IF TLWAIV-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO TLWAIV-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH IN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH IN AMT.
      *----------------------------------------------------------------*
       2210-PRORATE-CASH-IN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLWAIV-CASH-IN-AMT ROUNDED =
                   TLWAIV-CASH-IN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLWAIV-CASH-IN-AMT
           END-IF
           ADD TLWAIV-CASH-IN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2220-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLWAIV-OPEN-DATE > 0 AND TLWAIV-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLWAIV-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLWAIV-VAULT-SELL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TLWAIV-VAULT-SELL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLWAIV-VAULT-SELL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLWAIV-VAULT-SELL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLWAIV-VAULT-SELL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT SELL FEE ON THE WORK AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2230-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TLWAIV-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TLWAIV-VAULT-SELL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLWAIV-STOP-FLAG = 'Y'
                   COMPUTE TLWAIV-VAULT-SELL-AMT ROUNDED = WS-WORK-AMT *
                       TLWAIV-RATE / 200
               WHEN TLWAIV-CHANNEL-CD = 'D'
                   MOVE 20.00 TO TLWAIV-VAULT-SELL-AMT
               WHEN TLWAIV-CHANNEL-CD = 'F'
                   MOVE 7.00 TO TLWAIV-VAULT-SELL-AMT
               WHEN OTHER
                   COMPUTE TLWAIV-VAULT-SELL-AMT ROUNDED = WS-WORK-AMT *
                       TLWAIV-RATE / 100
           END-EVALUATE
           IF TLWAIV-VAULT-SELL-AMT > 0 AND TLWAIV-VAULT-SELL-AMT <
               10.00
               MOVE 10.00 TO TLWAIV-VAULT-SELL-AMT
           END-IF
           IF TLWAIV-VAULT-SELL-AMT > 50.00
               MOVE 50.00 TO TLWAIV-VAULT-SELL-AMT
           END-IF
           ADD TLWAIV-VAULT-SELL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT SELL FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2240-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TLWAIV-STOP-FLAG = 'Y'
                   MOVE ZERO TO TLWAIV-VAULT-SELL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLWAIV-EXEMPT-FLAG = 'Y'
                   COMPUTE TLWAIV-VAULT-SELL-AMT ROUNDED = WS-WORK-AMT *
                       TLWAIV-RATE / 200
               WHEN TLWAIV-REASON-CD = 'GS'
                   MOVE 7.00 TO TLWAIV-VAULT-SELL-AMT
               WHEN TLWAIV-REASON-CD = 'GV'
                   MOVE 3.00 TO TLWAIV-VAULT-SELL-AMT
               WHEN OTHER
                   COMPUTE TLWAIV-VAULT-SELL-AMT ROUNDED = WS-WORK-AMT *
                       TLWAIV-RATE / 100
           END-EVALUATE
           IF TLWAIV-VAULT-SELL-AMT > 0 AND TLWAIV-VAULT-SELL-AMT < 1.00
               MOVE 1.00 TO TLWAIV-VAULT-SELL-AMT
           END-IF
           IF TLWAIV-VAULT-SELL-AMT > 25.00
               MOVE 25.00 TO TLWAIV-VAULT-SELL-AMT
           END-IF
           ADD TLWAIV-VAULT-SELL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CASH IN AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2250-LIMIT-CASH-IN.
           IF TLWAIV-CASH-IN-AMT < 100.00
               MOVE 100.00 TO TLWAIV-CASH-IN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLWAIV-CASH-IN-AMT > 50000.00
               MOVE 50000.00 TO TLWAIV-CASH-IN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH IN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BATCH YTD.
      *----------------------------------------------------------------*
       2260-PRORATE-BATCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLWAIV-BATCH-YTD ROUNDED =
                   TLWAIV-CASH-IN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLWAIV-BATCH-YTD
           END-IF
           ADD TLWAIV-BATCH-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * BATCH YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2270-FLAG-BATCH.
           IF TLWAIV-BATCH-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLWAIV-BATCH-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AF -> PT WHEN EFFECTIVE DATE IS PAST
      * PT -> SZ WHEN EFFECTIVE DATE IS PAST
      * SZ -> VK WHEN EFFECTIVE DATE IS PAST
      * VK -> VW WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2280-SET-SOURCE.
           EVALUATE TLWAIV-SOURCE-CD
               WHEN 'AF'
                   IF TLWAIV-EFFECTIVE-DATE > 0 AND
                       TLWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'PT' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TLWAIV-EFFECTIVE-DATE > 0 AND
                       TLWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'SZ' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SZ'
                   IF TLWAIV-EFFECTIVE-DATE > 0 AND
                       TLWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'VK' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF TLWAIV-EFFECTIVE-DATE > 0 AND
                       TLWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'VW' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASH IN TIER TIER FOR THE VAULT BUY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2290-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-IN-TIER-RATE(1) TO TLWAIV-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TLWAIV-VAULT-BUY-YTD NOT < WS-CASH-IN-TIER-LOW(WS-TX)
                   MOVE WS-CASH-IN-TIER-RATE(WS-TX) TO TLWAIV-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLWAIV-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLWAIV-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BATCH YTD FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2300-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TLWAIV-STOP-FLAG = 'Y'
                   MOVE ZERO TO TLWAIV-BATCH-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLWAIV-EXEMPT-FLAG = 'Y'
                   COMPUTE TLWAIV-BATCH-YTD ROUNDED = WS-WORK-AMT *
                       TLWAIV-FEE-PCT / 200
               WHEN TLWAIV-SOURCE-CD = 'AF'
                   MOVE 7.00 TO TLWAIV-BATCH-YTD
               WHEN TLWAIV-SOURCE-CD = 'PT'
                   MOVE 5.00 TO TLWAIV-BATCH-YTD
               WHEN OTHER
                   COMPUTE TLWAIV-BATCH-YTD ROUNDED = WS-WORK-AMT *
                       TLWAIV-FEE-PCT / 100
           END-EVALUATE
           IF TLWAIV-BATCH-YTD > 0 AND TLWAIV-BATCH-YTD < 5.00
               MOVE 5.00 TO TLWAIV-BATCH-YTD
           END-IF
           IF TLWAIV-BATCH-YTD > 100.00
               MOVE 100.00 TO TLWAIV-BATCH-YTD
           END-IF
           ADD TLWAIV-BATCH-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2310-COUNT-EXCEPT.
           IF TLWAIV-EXCEPT-CNT < 9999999
               ADD 1 TO TLWAIV-EXCEPT-CNT
           END-IF
           IF TLWAIV-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TLWAIV-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2320-COUNT-ITEM.
           IF TLWAIV-ITEM-CNT < 9999999
               ADD 1 TO TLWAIV-ITEM-CNT
           END-IF
           IF TLWAIV-ITEM-CNT NOT < 6
               MOVE 'Y' TO TLWAIV-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2330-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TLWAIV-LAST-PAY-DATE > 0 AND TLWAIV-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLWAIV-CASH-IN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLWAIV-CASH-IN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TLWAIV-CASH-IN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TLWAIV-CASH-IN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLWAIV-CASH-IN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH IN YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2340-ACCRUE-CASH-IN.
           IF TLWAIV-CASH-IN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLWAIV-CASH-IN-YTD * TLWAIV-RATE / 100 / 360
               ADD WS-WORK-AMT TO TLWAIV-BATCH-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2350-DESCRIBE-DRAWER.
           EVALUATE TLWAIV-DRAWER-CD
               WHEN 'D'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'F'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'R'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2360-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLWAIV-EFFECTIVE-DATE > 0 AND TLWAIV-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLWAIV-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2370-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TLWAIV-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TLWAIV-BATCH-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2380-DESCRIBE-REASON.
           EVALUATE TLWAIV-REASON-CD
               WHEN 'GS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2390-COUNT-EXCEPT.
           IF TLWAIV-EXCEPT-CNT < 9999999
               ADD 1 TO TLWAIV-EXCEPT-CNT
           END-IF
           IF TLWAIV-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO TLWAIV-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E475 REF NO REQUIRED
      * E168 BATCH YTD NEGATIVE
      * E672 BATCH YTD OVER 50000
      * E818 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2400-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLWAIV-REF-NO = SPACES
                   MOVE 'E475' TO WS-REJECT-CODE
               WHEN TLWAIV-BATCH-YTD < 0
                   MOVE 'E168' TO WS-REJECT-CODE
               WHEN TLWAIV-BATCH-YTD > 50000.00
                   MOVE 'E672' TO WS-REJECT-CODE
               WHEN TLWAIV-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E818' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2410-DESCRIBE-DRAWER.
           EVALUATE TLWAIV-DRAWER-CD
               WHEN 'D'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'F'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VAULT BUY YTD.
      *----------------------------------------------------------------*
       2420-PRORATE-VAULT-BUY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLWAIV-VAULT-BUY-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLWAIV-VAULT-BUY-YTD
           END-IF
           ADD TLWAIV-VAULT-BUY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CASH OUT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2430-SPLIT-CASH-OUT.
           COMPUTE WS-WORK-AMT = TLWAIV-CASH-OUT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TLWAIV-CASH-OUT-YTD - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2440-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TLWAIV-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLWAIV-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TLWAIV-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH IN TIER TIER FOR THE VAULT SELL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2450-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-IN-TIER-RATE(1) TO TLWAIV-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TLWAIV-VAULT-SELL-AMT NOT < WS-CASH-IN-TIER-LOW(WS-TX)
                   MOVE WS-CASH-IN-TIER-RATE(WS-TX) TO TLWAIV-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLWAIV-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLWAIV-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2460-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLWAIV-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLWAIV-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TLWAIV-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH IN TIER TIER FOR THE CASH IN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2470-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-IN-TIER-RATE(1) TO TLWAIV-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TLWAIV-CASH-IN-AMT NOT < WS-CASH-IN-TIER-LOW(WS-TX)
                   MOVE WS-CASH-IN-TIER-RATE(WS-TX) TO TLWAIV-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLWAIV-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLWAIV-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2480-DESCRIBE-REASON.
           EVALUATE TLWAIV-REASON-CD
               WHEN 'GS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2490-COUNT-EXCEPT.
           IF TLWAIV-EXCEPT-CNT < 9999999
               ADD 1 TO TLWAIV-EXCEPT-CNT
           END-IF
           IF TLWAIV-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO TLWAIV-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2500-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TLWAIV-LAST-PAY-DATE > 0 AND TLWAIV-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2510-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLWAIV-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLWAIV-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TLWAIV-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CASH OUT YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2520-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TLWAIV-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLWAIV-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TLWAIV-CASH-OUT-YTD *
                       TLWAIV-RATE / 200
               WHEN TLWAIV-CHANNEL-CD = 'D'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN TLWAIV-CHANNEL-CD = 'F'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TLWAIV-CASH-OUT-YTD *
                       TLWAIV-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH IN YTD.
      *----------------------------------------------------------------*
       2530-PRORATE-CASH-IN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLWAIV-CASH-IN-YTD ROUNDED =
                   TLWAIV-VAULT-SELL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLWAIV-CASH-IN-YTD
           END-IF
           ADD TLWAIV-CASH-IN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CASH OUT YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2540-LIMIT-CASH-OUT.
           IF TLWAIV-CASH-OUT-YTD < 50.00
               MOVE 50.00 TO TLWAIV-CASH-OUT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLWAIV-CASH-OUT-YTD > 1000.00
               MOVE 1000.00 TO TLWAIV-CASH-OUT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2550-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLWAIV-OPEN-DATE > 0 AND TLWAIV-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLWAIV-OPEN-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2560-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLWAIV-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLWAIV-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TLWAIV-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2570-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TLWAIV-LAST-PAY-DATE > 0 AND TLWAIV-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLWAIV-CASH-OUT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TLWAIV-CASH-OUT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TLWAIV-CASH-OUT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TLWAIV-CASH-OUT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLWAIV-CASH-OUT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2580-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TLWAIV-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TLWAIV-CASH-OUT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH IN YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2590-FLAG-CASH-IN.
           IF TLWAIV-CASH-IN-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLWAIV-CASH-IN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * BATCH YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2600-FLAG-BATCH.
           IF TLWAIV-BATCH-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLWAIV-BATCH-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BATCH YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2610-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TLWAIV-BATCH-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TLWAIV-BATCH-YTD - (WS-WORK-AMT * 4)
           COMPUTE TLWAIV-VAULT-SELL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2620-DESCRIBE-SOURCE.
           EVALUATE TLWAIV-SOURCE-CD
               WHEN 'AF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH IN YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2630-LIMIT-CASH-IN.
           IF TLWAIV-CASH-IN-YTD < 25.00
               MOVE 25.00 TO TLWAIV-CASH-IN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLWAIV-CASH-IN-YTD > 50000.00
               MOVE 50000.00 TO TLWAIV-CASH-IN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH IN YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2640-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLWAIV-CHANNEL-CD
                   ADD TLWAIV-CASH-IN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLWAIV-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLWAIV-CASH-IN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BATCH YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2650-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TLWAIV-BATCH-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TLWAIV-BATCH-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2660-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TLWAIV-LAST-PAY-DATE > 0 AND TLWAIV-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLWAIV-VAULT-BUY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLWAIV-VAULT-BUY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TLWAIV-VAULT-BUY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLWAIV-VAULT-BUY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLWAIV-VAULT-BUY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BATCH YTD.
      *----------------------------------------------------------------*
       2670-PRORATE-BATCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLWAIV-BATCH-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLWAIV-BATCH-YTD
           END-IF
           ADD TLWAIV-BATCH-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * D -> F WHEN OPEN DATE IS PAST
      * F -> M WHEN OPEN DATE IS PAST
      * M -> N WHEN OPEN DATE IS PAST
      * N -> R WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2680-SET-CHANNEL.
           EVALUATE TLWAIV-CHANNEL-CD
               WHEN 'D'
                   IF TLWAIV-OPEN-DATE > 0 AND TLWAIV-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO TLWAIV-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TLWAIV-OPEN-DATE > 0 AND TLWAIV-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TLWAIV-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLWAIV-OPEN-DATE > 0 AND TLWAIV-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TLWAIV-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TLWAIV-OPEN-DATE > 0 AND TLWAIV-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO TLWAIV-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AF -> PT WHEN LAST STMT DATE IS PAST
      * PT -> SZ WHEN LAST STMT DATE IS PAST
      * SZ -> VK WHEN LAST STMT DATE IS PAST
      * VK -> VW WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2690-SET-SOURCE.
           EVALUATE TLWAIV-SOURCE-CD
               WHEN 'AF'
                   IF TLWAIV-LAST-STMT-DATE > 0 AND
                       TLWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'PT' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TLWAIV-LAST-STMT-DATE > 0 AND
                       TLWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'SZ' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SZ'
                   IF TLWAIV-LAST-STMT-DATE > 0 AND
                       TLWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'VK' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF TLWAIV-LAST-STMT-DATE > 0 AND
                       TLWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'VW' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH IN AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2700-ACCRUE-CASH-IN.
           IF TLWAIV-CASH-IN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLWAIV-CASH-IN-AMT * TLWAIV-RATE / 100 / 365
               ADD WS-WORK-AMT TO TLWAIV-CASH-IN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH IN TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2710-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-IN-TIER-RATE(1) TO TLWAIV-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-CASH-IN-TIER-LOW(WS-TX)
                   MOVE WS-CASH-IN-TIER-RATE(WS-TX) TO TLWAIV-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLWAIV-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLWAIV-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH IN YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2720-SPLIT-CASH-IN.
           COMPUTE WS-WORK-AMT = TLWAIV-CASH-IN-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TLWAIV-CASH-IN-YTD - (WS-WORK-AMT *
               12)
           COMPUTE TLWAIV-BATCH-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E895 REF NO REQUIRED
      * E155 VAULT SELL AMT NEGATIVE
      * E362 VAULT SELL AMT OVER 5000
      * E343 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2730-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLWAIV-REF-NO = SPACES
                   MOVE 'E895' TO WS-REJECT-CODE
               WHEN TLWAIV-VAULT-SELL-AMT < 0
                   MOVE 'E155' TO WS-REJECT-CODE
               WHEN TLWAIV-VAULT-SELL-AMT > 5000.00
                   MOVE 'E362' TO WS-REJECT-CODE
               WHEN TLWAIV-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E343' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2740-FLAG-CASH-OUT.
           IF TLWAIV-CASH-OUT-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLWAIV-CASH-OUT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BATCH YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VAULT SELL AMT.
      *----------------------------------------------------------------*
       2750-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLWAIV-VAULT-SELL-AMT ROUNDED =
                   TLWAIV-BATCH-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLWAIV-VAULT-SELL-AMT
           END-IF
           ADD TLWAIV-VAULT-SELL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CASH IN AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2760-FLAG-CASH-IN.
           IF TLWAIV-CASH-IN-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLWAIV-CASH-IN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2770-ACCRUE-VAULT-SELL.
           IF TLWAIV-VAULT-SELL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLWAIV-VAULT-SELL-AMT * TLWAIV-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH IN TIER TIER FOR THE CASH IN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2780-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-IN-TIER-RATE(1) TO TLWAIV-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TLWAIV-CASH-IN-AMT NOT < WS-CASH-IN-TIER-LOW(WS-TX)
                   MOVE WS-CASH-IN-TIER-RATE(WS-TX) TO TLWAIV-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLWAIV-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLWAIV-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2790-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLWAIV-SOURCE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLWAIV-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2800-SPLIT-CASH-OUT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE TLWAIV-CASH-OUT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2810-LIMIT-CASH-OUT.
           IF TLWAIV-CASH-OUT-YTD < 10.00
               MOVE 10.00 TO TLWAIV-CASH-OUT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLWAIV-CASH-OUT-YTD > 1000.00
               MOVE 1000.00 TO TLWAIV-CASH-OUT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2820-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TLWAIV-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TLWAIV-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH IN TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2830-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-IN-TIER-RATE(1) TO TLWAIV-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-CASH-IN-TIER-LOW(WS-TX)
                   MOVE WS-CASH-IN-TIER-RATE(WS-TX) TO TLWAIV-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLWAIV-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLWAIV-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2840-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TLWAIV-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TLWAIV-CASH-IN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2850-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TLWAIV-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TLWAIV-CASH-IN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLWAIV-STOP-FLAG = 'Y'
                   COMPUTE TLWAIV-CASH-IN-AMT ROUNDED = WS-WORK-AMT-2 *
                       TLWAIV-FEE-PCT / 200
               WHEN TLWAIV-SOURCE-CD = 'AF'
                   MOVE 12.00 TO TLWAIV-CASH-IN-AMT
               WHEN TLWAIV-SOURCE-CD = 'PT'
                   MOVE 12.00 TO TLWAIV-CASH-IN-AMT
               WHEN OTHER
                   COMPUTE TLWAIV-CASH-IN-AMT ROUNDED = WS-WORK-AMT-2 *
                       TLWAIV-FEE-PCT / 100
           END-EVALUATE
           IF TLWAIV-CASH-IN-AMT > 0 AND TLWAIV-CASH-IN-AMT < 1.00
               MOVE 1.00 TO TLWAIV-CASH-IN-AMT
           END-IF
           IF TLWAIV-CASH-IN-AMT > 150.00
               MOVE 150.00 TO TLWAIV-CASH-IN-AMT
           END-IF
           ADD TLWAIV-CASH-IN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CASH OUT YTD FEE ON THE BATCH YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2860-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TLWAIV-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TLWAIV-CASH-OUT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLWAIV-STOP-FLAG = 'Y'
                   COMPUTE TLWAIV-CASH-OUT-YTD ROUNDED =
                       TLWAIV-BATCH-YTD * TLWAIV-RATE / 200
               WHEN TLWAIV-DRAWER-CD = 'D'
                   MOVE 12.00 TO TLWAIV-CASH-OUT-YTD
               WHEN TLWAIV-DRAWER-CD = 'F'
                   MOVE 5.00 TO TLWAIV-CASH-OUT-YTD
               WHEN OTHER
                   COMPUTE TLWAIV-CASH-OUT-YTD ROUNDED =
                       TLWAIV-BATCH-YTD * TLWAIV-RATE / 100
           END-EVALUATE
           IF TLWAIV-CASH-OUT-YTD > 0 AND TLWAIV-CASH-OUT-YTD < 1.00
               MOVE 1.00 TO TLWAIV-CASH-OUT-YTD
           END-IF
           IF TLWAIV-CASH-OUT-YTD > 250.00
               MOVE 250.00 TO TLWAIV-CASH-OUT-YTD
           END-IF
           ADD TLWAIV-CASH-OUT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2870-ACCRUE-VAULT-SELL.
           IF TLWAIV-VAULT-SELL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLWAIV-VAULT-SELL-AMT * TLWAIV-RATE / 100 / 360
               ADD WS-WORK-AMT TO TLWAIV-VAULT-BUY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E064 DESC REQUIRED
      * E931 WORK AMT NEGATIVE
      * E100 WORK AMT OVER 10000
      * E276 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2880-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLWAIV-DESC = SPACES
                   MOVE 'E064' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E931' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E100' TO WS-REJECT-CODE
               WHEN TLWAIV-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E276' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASH IN AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2890-LIMIT-CASH-IN.
           IF TLWAIV-CASH-IN-AMT < 0.00
               MOVE 0.00 TO TLWAIV-CASH-IN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLWAIV-CASH-IN-AMT > 2500.00
               MOVE 2500.00 TO TLWAIV-CASH-IN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH IN YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2900-ACCRUE-CASH-IN.
           IF TLWAIV-CASH-IN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLWAIV-CASH-IN-YTD * TLWAIV-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TLWAIV-BATCH-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2910-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TLWAIV-RATE / 100 / 365
               ADD WS-WORK-AMT TO TLWAIV-CASH-IN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT BUY YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2920-SPLIT-VAULT-BUY.
           COMPUTE WS-WORK-AMT = TLWAIV-VAULT-BUY-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TLWAIV-VAULT-BUY-YTD - (WS-WORK-AMT *
               3)
           COMPUTE TLWAIV-CASH-OUT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH IN TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2930-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-IN-TIER-RATE(1) TO TLWAIV-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-CASH-IN-TIER-LOW(WS-TX)
                   MOVE WS-CASH-IN-TIER-RATE(WS-TX) TO TLWAIV-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLWAIV-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLWAIV-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E999 REF NO REQUIRED
      * E021 WORK AMT NEGATIVE
      * E954 WORK AMT OVER 10000
      * E213 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2940-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLWAIV-REF-NO = SPACES
                   MOVE 'E999' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E021' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E954' TO WS-REJECT-CODE
               WHEN TLWAIV-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E213' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AF -> PT WHEN EFFECTIVE DATE IS PAST
      * PT -> SZ WHEN EFFECTIVE DATE IS PAST
      * SZ -> VK WHEN EFFECTIVE DATE IS PAST
      * VK -> VW WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2950-SET-SOURCE.
           EVALUATE TLWAIV-SOURCE-CD
               WHEN 'AF'
                   IF TLWAIV-EFFECTIVE-DATE > 0 AND
                       TLWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'PT' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TLWAIV-EFFECTIVE-DATE > 0 AND
                       TLWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'SZ' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SZ'
                   IF TLWAIV-EFFECTIVE-DATE > 0 AND
                       TLWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'VK' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF TLWAIV-EFFECTIVE-DATE > 0 AND
                       TLWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'VW' TO TLWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E067 DESC REQUIRED
      * E143 CASH IN YTD NEGATIVE
      * E361 CASH IN YTD OVER 50000
      * E537 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2960-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLWAIV-DESC = SPACES
                   MOVE 'E067' TO WS-REJECT-CODE
               WHEN TLWAIV-CASH-IN-YTD < 0
                   MOVE 'E143' TO WS-REJECT-CODE
               WHEN TLWAIV-CASH-IN-YTD > 50000.00
                   MOVE 'E361' TO WS-REJECT-CODE
               WHEN TLWAIV-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E537' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2970-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLWAIV-CHANNEL-CD
                   ADD TLWAIV-CASH-OUT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLWAIV-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLWAIV-CASH-OUT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2980-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TLWAIV-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TLWAIV-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2990-COUNT-EXCEPT.
           IF TLWAIV-EXCEPT-CNT < 9999999
               ADD 1 TO TLWAIV-EXCEPT-CNT
           END-IF
           IF TLWAIV-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO TLWAIV-SUSPEND-FLAG
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO TLWAIV-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO TLWAIV-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO TLWAIV-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'TLWAIV EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
