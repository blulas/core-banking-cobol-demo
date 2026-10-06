      *================================================================*
      * PROGRAM  : SBSTAT                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : SAVINGS BOND SERVICES RULE - FEES. CALLED BY THE    *
      *            SAVINGS BOND SERVICES BATCH AND ONLINE PROGRAMS.    *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-02-11 EWS  ORIGINAL                                       *
      * 1996-05-05 GAF  FACE ROUNDING - HALF UP CHG0263                *
      * 1996-06-08 MKL  ISSUE-PRICE CALCULATION CORRECTED PRB9399      *
      * 1997-02-22 JLM  AUDIT FINDING - ISSUE-PRICE CONTROL TOTALS CHG5*
      * 1997-06-13 MKL  ISSUE-PRICE FEE SCHEDULE UPDATE PRB9296        *
      * 1999-03-07 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-06-15 RJT  PERFORMANCE - ACCRUAL READ AHEAD INC5423       *
      * 2000-09-05 CRP  NEW INTEREST REPORT COLUMNS REQ7411            *
      * 2001-04-07 TWH  PURCHASE NOTICE TEXT PER LEGAL PRB9955         *
      * 2003-04-17 EWS  REDEEM STATUS CODES ADDED PRB4298              *
      * 2004-07-11 PQR  INTEREST FEE SCHEDULE UPDATE REQ9967           *
      * 2006-10-07 LMB  REDEEM FEE SCHEDULE UPDATE CHG6328             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SBSTAT.
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
      * WS-PURCHASE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PURCHASE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000817000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000564500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000652500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008500000000078500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008750000000434000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018750000000681000'.
       01  WS-PURCHASE-TIER-TBL REDEFINES WS-PURCHASE-TIER-VALUES.
           05  WS-PURCHASE-TIER-ROW OCCURS 6 TIMES.
               10  WS-PURCHASE-TIER-LOW    PIC 9(09)V99.
               10  WS-PURCHASE-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INTEREST-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000811000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000633000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000064500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000408000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009000000000727000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009050000000677500'.
       01  WS-INTEREST-BAND-TBL REDEFINES WS-INTEREST-BAND-VALUES.
           05  WS-INTEREST-BAND-ROW OCCURS 6 TIMES.
               10  WS-INTEREST-BAND-LOW    PIC 9(09)V99.
               10  WS-INTEREST-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PURCHASE-TIERX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PURCHASE-TIERX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000627000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000623500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000709500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000800000000596000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000850000000097500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000786000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001600000000790500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006600000000255000'.
       01  WS-PURCHASE-TIERX-TBL REDEFINES WS-PURCHASE-TIERX-VALUES.
           05  WS-PURCHASE-TIERX-ROW OCCURS 8 TIMES.
               10  WS-PURCHASE-TIERX-LOW   PIC 9(09)V99.
               10  WS-PURCHASE-TIERX-RATE  PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY SBSTATP.

       PROCEDURE DIVISION USING SBSTAT-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SET-STATUS
           PERFORM 2010-COUNT-RETRY
           PERFORM 2020-ACCRUE-PENALTY-PCT
           PERFORM 2030-ACCUM-SOURCE
           PERFORM 2040-SET-STATUS
           PERFORM 2050-SET-REASON
           PERFORM 2060-CHECK-EFFECTIVE
           PERFORM 2070-PRORATE-FACE
           PERFORM 2080-ACCRUE-PENALTY-PCT
           PERFORM 2090-PRORATE-TAXABLE
           PERFORM 2100-PRORATE-REDEEM
           PERFORM 2110-SET-SOURCE
           PERFORM 2120-EDIT-NAME
           PERFORM 2130-SELECT-MARGIN
           PERFORM 2140-CHECK-NOTICE
           PERFORM 2150-ACCRUE-INTEREST
           PERFORM 2160-SET-SOURCE
           PERFORM 2170-DESCRIBE-REASON
           PERFORM 2180-LIMIT-TAXABLE
           PERFORM 2190-SELECT-PENALTY-PCT
           PERFORM 2200-ASSESS-PREFERRED
           PERFORM 2210-PRORATE-ACCRUAL
           PERFORM 2220-DESCRIBE-REASON
           PERFORM 2230-LIMIT-REDEEM
           PERFORM 2240-COUNT-RETRY
           PERFORM 2250-ACCUM-ISSUE-TYPE
           PERFORM 2260-AGE-MATURITY
           PERFORM 2270-EDIT-NAME
           PERFORM 2280-SET-ISSUE-TYPE
           PERFORM 2290-AGE-MATURITY
           PERFORM 2300-SET-ISSUE-TYPE
           PERFORM 2310-ASSESS-AUTO-RENEW
           PERFORM 2320-LIMIT-FACE
           PERFORM 2330-SELECT-MARGIN
           PERFORM 2340-LIMIT-TAXABLE
           PERFORM 2350-AGE-NEXT-DUE
           PERFORM 2360-ACCRUE-PENALTY-PCT
           PERFORM 2370-SPLIT-INTEREST
           PERFORM 2380-PRORATE-INTEREST
           PERFORM 2390-SPLIT-FACE
           PERFORM 2400-LIMIT-ITEM
           PERFORM 2410-LIMIT-TAXABLE
           PERFORM 2420-SELECT-MARGIN
           PERFORM 2430-EDIT-DESC
           PERFORM 2440-ACCRUE-REDEEM
           PERFORM 2450-SPLIT-FACE
           PERFORM 2460-SET-SOURCE
           PERFORM 2470-PRORATE-FACE
           PERFORM 2480-ASSESS-PREFERRED
           PERFORM 2490-ACCUM-STATUS
           PERFORM 2500-ASSESS-REVIEW
           PERFORM 2510-AGE-NOTICE
           PERFORM 2520-CHECK-EFFECTIVE
           PERFORM 2530-SET-SOURCE
           PERFORM 2540-SET-REASON
           PERFORM 2550-CHECK-NOTICE
           PERFORM 2560-ACCUM-REASON
           PERFORM 2570-AGE-MATURITY
           PERFORM 2580-ACCUM-REASON
           PERFORM 2590-AGE-EFFECTIVE
           PERFORM 2600-COUNT-RETRY
           PERFORM 2610-ACCRUE-MARGIN
           PERFORM 2620-CHECK-EFFECTIVE
           PERFORM 2630-EDIT-DESC
           PERFORM 2640-ACCUM-SOURCE
           PERFORM 2650-SET-ISSUE-TYPE
           PERFORM 2660-ASSESS-AUTO-RENEW
           PERFORM 2670-AGE-NOTICE
           PERFORM 2680-FLAG-FACE
           PERFORM 2690-ACCUM-STATUS
           PERFORM 2700-ASSESS-REVIEW
           PERFORM 2710-COUNT-EXCEPT
           PERFORM 2720-ACCUM-ISSUE-TYPE
           PERFORM 2730-PRORATE-REDEEM
           PERFORM 2740-COUNT-RETRY
           PERFORM 2750-SELECT-PENALTY-PCT
           PERFORM 2760-DESCRIBE-ISSUE-TYPE
           PERFORM 2770-EDIT-NAME
           PERFORM 2780-AGE-EFFECTIVE
           PERFORM 2790-AGE-NOTICE
           PERFORM 2800-DESCRIBE-REASON
           PERFORM 2810-ACCUM-STATUS
           PERFORM 2820-LIMIT-ACCRUAL
           PERFORM 2830-AGE-NOTICE
           PERFORM 2840-SELECT-MARGIN
           PERFORM 2850-EDIT-NAME
           PERFORM 2860-SET-STATUS
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO SBSTAT-RETURN-CODE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CS -> HF WHEN NEXT DUE DATE IS PAST
      * HF -> JZ WHEN NEXT DUE DATE IS PAST
      * JZ -> CS WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2000-SET-STATUS.
           EVALUATE SBSTAT-STATUS-CD
               WHEN 'CS'
                   IF SBSTAT-NEXT-DUE-DATE > 0 AND SBSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HF' TO SBSTAT-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF SBSTAT-NEXT-DUE-DATE > 0 AND SBSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JZ' TO SBSTAT-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF SBSTAT-NEXT-DUE-DATE > 0 AND SBSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'CS' TO SBSTAT-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2010-COUNT-RETRY.
           IF SBSTAT-RETRY-CNT < 9999999
               ADD 1 TO SBSTAT-RETRY-CNT
           END-IF
           IF SBSTAT-RETRY-CNT NOT < 10
               MOVE 'Y' TO SBSTAT-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2020-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * SBSTAT-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBSTAT-FACE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2030-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBSTAT-SOURCE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBSTAT-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CS -> HF WHEN NEXT DUE DATE IS PAST
      * HF -> JZ WHEN NEXT DUE DATE IS PAST
      * JZ -> CS WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2040-SET-STATUS.
           EVALUATE SBSTAT-STATUS-CD
               WHEN 'CS'
                   IF SBSTAT-NEXT-DUE-DATE > 0 AND SBSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HF' TO SBSTAT-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF SBSTAT-NEXT-DUE-DATE > 0 AND SBSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JZ' TO SBSTAT-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF SBSTAT-NEXT-DUE-DATE > 0 AND SBSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'CS' TO SBSTAT-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DG -> UK WHEN EFFECTIVE DATE IS PAST
      * UK -> VH WHEN EFFECTIVE DATE IS PAST
      * VH -> XD WHEN EFFECTIVE DATE IS PAST
      * XD -> DG WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2050-SET-REASON.
           EVALUATE SBSTAT-REASON-CD
               WHEN 'DG'
                   IF SBSTAT-EFFECTIVE-DATE > 0 AND
                       SBSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'UK' TO SBSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF SBSTAT-EFFECTIVE-DATE > 0 AND
                       SBSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'VH' TO SBSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VH'
                   IF SBSTAT-EFFECTIVE-DATE > 0 AND
                       SBSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'XD' TO SBSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF SBSTAT-EFFECTIVE-DATE > 0 AND
                       SBSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'DG' TO SBSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2060-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF SBSTAT-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBSTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO SBSTAT-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FACE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2070-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   SBSTAT-FACE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2080-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * SBSTAT-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBSTAT-FACE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TAXABLE YTD.
      *----------------------------------------------------------------*
       2090-PRORATE-TAXABLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBSTAT-TAXABLE-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBSTAT-TAXABLE-YTD
           END-IF
           ADD SBSTAT-TAXABLE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE REDEEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2100-PRORATE-REDEEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   SBSTAT-REDEEM-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * E -> J WHEN NOTICE DATE IS PAST
      * J -> L WHEN NOTICE DATE IS PAST
      * L -> U WHEN NOTICE DATE IS PAST
      * U -> E WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2110-SET-SOURCE.
           EVALUATE SBSTAT-SOURCE-CD
               WHEN 'E'
                   IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO SBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO SBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO SBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E925 NAME REQUIRED
      * E215 WORK AMT 2 NEGATIVE
      * E550 WORK AMT 2 OVER 25000
      * E972 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2120-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBSTAT-NAME = SPACES
                   MOVE 'E925' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E215' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E550' TO WS-REJECT-CODE
               WHEN SBSTAT-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E972' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST BAND TIER FOR THE ACCRUAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2130-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-BAND-RATE(1) TO SBSTAT-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF SBSTAT-ACCRUAL-AMT NOT < WS-INTEREST-BAND-LOW(WS-TX)
                   MOVE WS-INTEREST-BAND-RATE(WS-TX) TO SBSTAT-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBSTAT-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBSTAT-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2140-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBSTAT-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBSTAT-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO SBSTAT-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2150-ACCRUE-INTEREST.
           IF SBSTAT-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBSTAT-INTEREST-YTD * SBSTAT-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBSTAT-FACE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * E -> J WHEN NEXT DUE DATE IS PAST
      * J -> L WHEN NEXT DUE DATE IS PAST
      * L -> U WHEN NEXT DUE DATE IS PAST
      * U -> E WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2160-SET-SOURCE.
           EVALUATE SBSTAT-SOURCE-CD
               WHEN 'E'
                   IF SBSTAT-NEXT-DUE-DATE > 0 AND SBSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'J' TO SBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF SBSTAT-NEXT-DUE-DATE > 0 AND SBSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'L' TO SBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SBSTAT-NEXT-DUE-DATE > 0 AND SBSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'U' TO SBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF SBSTAT-NEXT-DUE-DATE > 0 AND SBSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'E' TO SBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2170-DESCRIBE-REASON.
           EVALUATE SBSTAT-REASON-CD
               WHEN 'DG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TAXABLE YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2180-LIMIT-TAXABLE.
           IF SBSTAT-TAXABLE-YTD < 100.00
               MOVE 100.00 TO SBSTAT-TAXABLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBSTAT-TAXABLE-YTD > 50000.00
               MOVE 50000.00 TO SBSTAT-TAXABLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE TIERX TIER FOR THE ACCRUAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2190-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-TIERX-RATE(1) TO SBSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SBSTAT-ACCRUAL-AMT NOT < WS-PURCHASE-TIERX-LOW(WS-TX)
                   MOVE WS-PURCHASE-TIERX-RATE(WS-TX) TO
                       SBSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE FACE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2200-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SBSTAT-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBSTAT-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SBSTAT-FACE-AMT *
                       SBSTAT-PENALTY-PCT / 200
               WHEN SBSTAT-ISSUE-TYPE-CD = 'CW'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN SBSTAT-ISSUE-TYPE-CD = 'DY'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SBSTAT-FACE-AMT *
                       SBSTAT-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ACCRUAL AMT.
      *----------------------------------------------------------------*
       2210-PRORATE-ACCRUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBSTAT-ACCRUAL-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBSTAT-ACCRUAL-AMT
           END-IF
           ADD SBSTAT-ACCRUAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2220-DESCRIBE-REASON.
           EVALUATE SBSTAT-REASON-CD
               WHEN 'DG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REDEEM AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2230-LIMIT-REDEEM.
           IF SBSTAT-REDEEM-AMT < 10.00
               MOVE 10.00 TO SBSTAT-REDEEM-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBSTAT-REDEEM-AMT > 5000.00
               MOVE 5000.00 TO SBSTAT-REDEEM-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2240-COUNT-RETRY.
           IF SBSTAT-RETRY-CNT < 9999999
               ADD 1 TO SBSTAT-RETRY-CNT
           END-IF
           IF SBSTAT-RETRY-CNT NOT < 3
               MOVE 'Y' TO SBSTAT-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAXABLE YTD BY ISSUE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2250-ACCUM-ISSUE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBSTAT-ISSUE-TYPE-CD
                   ADD SBSTAT-TAXABLE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBSTAT-ISSUE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBSTAT-TAXABLE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2260-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SBSTAT-MATURITY-DATE > 0 AND SBSTAT-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBSTAT-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E841 NAME REQUIRED
      * E652 WORK AMT NEGATIVE
      * E843 WORK AMT OVER 250000
      * E026 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2270-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBSTAT-NAME = SPACES
                   MOVE 'E841' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E652' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E843' TO WS-REJECT-CODE
               WHEN SBSTAT-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E026' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ISSUE TYPE CD:
      * CW -> DY WHEN EFFECTIVE DATE IS PAST
      * DY -> JA WHEN EFFECTIVE DATE IS PAST
      * JA -> KT WHEN EFFECTIVE DATE IS PAST
      * KT -> MF WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2280-SET-ISSUE-TYPE.
           EVALUATE SBSTAT-ISSUE-TYPE-CD
               WHEN 'CW'
                   IF SBSTAT-EFFECTIVE-DATE > 0 AND
                       SBSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'DY' TO SBSTAT-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DY'
                   IF SBSTAT-EFFECTIVE-DATE > 0 AND
                       SBSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'JA' TO SBSTAT-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF SBSTAT-EFFECTIVE-DATE > 0 AND
                       SBSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'KT' TO SBSTAT-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KT'
                   IF SBSTAT-EFFECTIVE-DATE > 0 AND
                       SBSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'MF' TO SBSTAT-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       2290-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SBSTAT-MATURITY-DATE > 0 AND SBSTAT-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBSTAT-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBSTAT-TAXABLE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBSTAT-TAXABLE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD SBSTAT-TAXABLE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD SBSTAT-TAXABLE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBSTAT-TAXABLE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ISSUE TYPE CD:
      * CW -> DY WHEN EFFECTIVE DATE IS PAST
      * DY -> JA WHEN EFFECTIVE DATE IS PAST
      * JA -> KT WHEN EFFECTIVE DATE IS PAST
      * KT -> MF WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2300-SET-ISSUE-TYPE.
           EVALUATE SBSTAT-ISSUE-TYPE-CD
               WHEN 'CW'
                   IF SBSTAT-EFFECTIVE-DATE > 0 AND
                       SBSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'DY' TO SBSTAT-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DY'
                   IF SBSTAT-EFFECTIVE-DATE > 0 AND
                       SBSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'JA' TO SBSTAT-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF SBSTAT-EFFECTIVE-DATE > 0 AND
                       SBSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'KT' TO SBSTAT-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KT'
                   IF SBSTAT-EFFECTIVE-DATE > 0 AND
                       SBSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'MF' TO SBSTAT-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FACE YTD FEE ON THE TAXABLE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2310-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SBSTAT-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SBSTAT-FACE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBSTAT-PREFERRED-FLAG = 'Y'
                   COMPUTE SBSTAT-FACE-YTD ROUNDED = SBSTAT-TAXABLE-YTD
                       * SBSTAT-PENALTY-PCT / 200
               WHEN SBSTAT-ISSUE-TYPE-CD = 'CW'
                   MOVE 7.00 TO SBSTAT-FACE-YTD
               WHEN SBSTAT-ISSUE-TYPE-CD = 'DY'
                   MOVE 10.00 TO SBSTAT-FACE-YTD
               WHEN OTHER
                   COMPUTE SBSTAT-FACE-YTD ROUNDED = SBSTAT-TAXABLE-YTD
                       * SBSTAT-PENALTY-PCT / 100
           END-EVALUATE
           IF SBSTAT-FACE-YTD > 0 AND SBSTAT-FACE-YTD < 10.00
               MOVE 10.00 TO SBSTAT-FACE-YTD
           END-IF
           IF SBSTAT-FACE-YTD > 50.00
               MOVE 50.00 TO SBSTAT-FACE-YTD
           END-IF
           ADD SBSTAT-FACE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FACE AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2320-LIMIT-FACE.
           IF SBSTAT-FACE-AMT < 100.00
               MOVE 100.00 TO SBSTAT-FACE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBSTAT-FACE-AMT > 2500.00
               MOVE 2500.00 TO SBSTAT-FACE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE TIERX TIER FOR THE TAXABLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2330-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-TIERX-RATE(1) TO SBSTAT-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SBSTAT-TAXABLE-YTD NOT < WS-PURCHASE-TIERX-LOW(WS-TX)
                   MOVE WS-PURCHASE-TIERX-RATE(WS-TX) TO SBSTAT-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBSTAT-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBSTAT-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAXABLE YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2340-LIMIT-TAXABLE.
           IF SBSTAT-TAXABLE-YTD < 50.00
               MOVE 50.00 TO SBSTAT-TAXABLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBSTAT-TAXABLE-YTD > 2500.00
               MOVE 2500.00 TO SBSTAT-TAXABLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2350-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SBSTAT-NEXT-DUE-DATE > 0 AND SBSTAT-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBSTAT-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBSTAT-TAXABLE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBSTAT-TAXABLE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SBSTAT-TAXABLE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBSTAT-TAXABLE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBSTAT-TAXABLE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2360-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * SBSTAT-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBSTAT-FACE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2370-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = SBSTAT-INTEREST-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SBSTAT-INTEREST-YTD - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2380-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   SBSTAT-INTEREST-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT FACE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2390-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = SBSTAT-FACE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = SBSTAT-FACE-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2400-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAXABLE YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2410-LIMIT-TAXABLE.
           IF SBSTAT-TAXABLE-YTD < 50.00
               MOVE 50.00 TO SBSTAT-TAXABLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBSTAT-TAXABLE-YTD > 1000.00
               MOVE 1000.00 TO SBSTAT-TAXABLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE TIERX TIER FOR THE FACE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2420-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-TIERX-RATE(1) TO SBSTAT-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SBSTAT-FACE-YTD NOT < WS-PURCHASE-TIERX-LOW(WS-TX)
                   MOVE WS-PURCHASE-TIERX-RATE(WS-TX) TO SBSTAT-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBSTAT-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBSTAT-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E629 DESC REQUIRED
      * E844 TAXABLE YTD NEGATIVE
      * E373 TAXABLE YTD OVER 1000000
      * E126 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2430-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBSTAT-DESC = SPACES
                   MOVE 'E629' TO WS-REJECT-CODE
               WHEN SBSTAT-TAXABLE-YTD < 0
                   MOVE 'E844' TO WS-REJECT-CODE
               WHEN SBSTAT-TAXABLE-YTD > 1000000.00
                   MOVE 'E373' TO WS-REJECT-CODE
               WHEN SBSTAT-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E126' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REDEEM AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2440-ACCRUE-REDEEM.
           IF SBSTAT-REDEEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBSTAT-REDEEM-AMT * SBSTAT-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO SBSTAT-FACE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FACE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2450-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = SBSTAT-FACE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SBSTAT-FACE-AMT - (WS-WORK-AMT * 2)
           COMPUTE SBSTAT-FACE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * E -> J WHEN NOTICE DATE IS PAST
      * J -> L WHEN NOTICE DATE IS PAST
      * L -> U WHEN NOTICE DATE IS PAST
      * U -> E WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2460-SET-SOURCE.
           EVALUATE SBSTAT-SOURCE-CD
               WHEN 'E'
                   IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO SBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO SBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO SBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REDEEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FACE YTD.
      *----------------------------------------------------------------*
       2470-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBSTAT-FACE-YTD ROUNDED =
                   SBSTAT-REDEEM-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBSTAT-FACE-YTD
           END-IF
           ADD SBSTAT-FACE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE INTEREST YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2480-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SBSTAT-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBSTAT-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = SBSTAT-INTEREST-YTD *
                       SBSTAT-PENALTY-PCT / 200
               WHEN SBSTAT-REASON-CD = 'DG'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN SBSTAT-REASON-CD = 'UK'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = SBSTAT-INTEREST-YTD *
                       SBSTAT-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE TAXABLE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2490-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBSTAT-STATUS-CD
                   ADD SBSTAT-TAXABLE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBSTAT-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBSTAT-TAXABLE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUAL FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2500-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SBSTAT-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SBSTAT-ACCRUAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBSTAT-PREFERRED-FLAG = 'Y'
                   COMPUTE SBSTAT-ACCRUAL-AMT ROUNDED = WS-WORK-AMT-2 *
                       SBSTAT-MARGIN / 200
               WHEN SBSTAT-REASON-CD = 'DG'
                   MOVE 10.00 TO SBSTAT-ACCRUAL-AMT
               WHEN SBSTAT-REASON-CD = 'UK'
                   MOVE 5.00 TO SBSTAT-ACCRUAL-AMT
               WHEN OTHER
                   COMPUTE SBSTAT-ACCRUAL-AMT ROUNDED = WS-WORK-AMT-2 *
                       SBSTAT-MARGIN / 100
           END-EVALUATE
           IF SBSTAT-ACCRUAL-AMT > 0 AND SBSTAT-ACCRUAL-AMT < 10.00
               MOVE 10.00 TO SBSTAT-ACCRUAL-AMT
           END-IF
           IF SBSTAT-ACCRUAL-AMT > 35.00
               MOVE 35.00 TO SBSTAT-ACCRUAL-AMT
           END-IF
           ADD SBSTAT-ACCRUAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2510-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBSTAT-NOTICE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2520-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF SBSTAT-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBSTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO SBSTAT-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * E -> J WHEN NOTICE DATE IS PAST
      * J -> L WHEN NOTICE DATE IS PAST
      * L -> U WHEN NOTICE DATE IS PAST
      * U -> E WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2530-SET-SOURCE.
           EVALUATE SBSTAT-SOURCE-CD
               WHEN 'E'
                   IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO SBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO SBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO SBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DG -> UK WHEN NEXT DUE DATE IS PAST
      * UK -> VH WHEN NEXT DUE DATE IS PAST
      * VH -> XD WHEN NEXT DUE DATE IS PAST
      * XD -> DG WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2540-SET-REASON.
           EVALUATE SBSTAT-REASON-CD
               WHEN 'DG'
                   IF SBSTAT-NEXT-DUE-DATE > 0 AND SBSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UK' TO SBSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF SBSTAT-NEXT-DUE-DATE > 0 AND SBSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VH' TO SBSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VH'
                   IF SBSTAT-NEXT-DUE-DATE > 0 AND SBSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XD' TO SBSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF SBSTAT-NEXT-DUE-DATE > 0 AND SBSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DG' TO SBSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2550-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBSTAT-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBSTAT-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO SBSTAT-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2560-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBSTAT-REASON-CD
                   ADD SBSTAT-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBSTAT-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBSTAT-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2570-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SBSTAT-MATURITY-DATE > 0 AND SBSTAT-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBSTAT-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBSTAT-FACE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SBSTAT-FACE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SBSTAT-FACE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SBSTAT-FACE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBSTAT-FACE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2580-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBSTAT-REASON-CD
                   ADD SBSTAT-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBSTAT-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBSTAT-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2590-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF SBSTAT-EFFECTIVE-DATE > 0 AND SBSTAT-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBSTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBSTAT-FACE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD SBSTAT-FACE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SBSTAT-FACE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBSTAT-FACE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBSTAT-FACE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2600-COUNT-RETRY.
           IF SBSTAT-RETRY-CNT < 9999999
               ADD 1 TO SBSTAT-RETRY-CNT
           END-IF
           IF SBSTAT-RETRY-CNT NOT < 6
               MOVE 'Y' TO SBSTAT-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2610-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * SBSTAT-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO SBSTAT-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2620-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF SBSTAT-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBSTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO SBSTAT-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E469 DESC REQUIRED
      * E212 REDEEM AMT NEGATIVE
      * E296 REDEEM AMT OVER 1000000
      * E586 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2630-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBSTAT-DESC = SPACES
                   MOVE 'E469' TO WS-REJECT-CODE
               WHEN SBSTAT-REDEEM-AMT < 0
                   MOVE 'E212' TO WS-REJECT-CODE
               WHEN SBSTAT-REDEEM-AMT > 1000000.00
                   MOVE 'E296' TO WS-REJECT-CODE
               WHEN SBSTAT-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E586' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2640-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBSTAT-SOURCE-CD
                   ADD SBSTAT-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBSTAT-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBSTAT-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ISSUE TYPE CD:
      * CW -> DY WHEN NOTICE DATE IS PAST
      * DY -> JA WHEN NOTICE DATE IS PAST
      * JA -> KT WHEN NOTICE DATE IS PAST
      * KT -> MF WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2650-SET-ISSUE-TYPE.
           EVALUATE SBSTAT-ISSUE-TYPE-CD
               WHEN 'CW'
                   IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DY' TO SBSTAT-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DY'
                   IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JA' TO SBSTAT-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KT' TO SBSTAT-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KT'
                   IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MF' TO SBSTAT-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FACE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2660-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SBSTAT-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SBSTAT-FACE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBSTAT-REVIEW-FLAG = 'Y'
                   COMPUTE SBSTAT-FACE-YTD ROUNDED = WS-WORK-AMT *
                       SBSTAT-MARGIN / 200
               WHEN SBSTAT-STATUS-CD = 'CS'
                   MOVE 20.00 TO SBSTAT-FACE-YTD
               WHEN SBSTAT-STATUS-CD = 'HF'
                   MOVE 5.00 TO SBSTAT-FACE-YTD
               WHEN OTHER
                   COMPUTE SBSTAT-FACE-YTD ROUNDED = WS-WORK-AMT *
                       SBSTAT-MARGIN / 100
           END-EVALUATE
           IF SBSTAT-FACE-YTD > 0 AND SBSTAT-FACE-YTD < 1.00
               MOVE 1.00 TO SBSTAT-FACE-YTD
           END-IF
           IF SBSTAT-FACE-YTD > 35.00
               MOVE 35.00 TO SBSTAT-FACE-YTD
           END-IF
           ADD SBSTAT-FACE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2670-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBSTAT-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBSTAT-ACCRUAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBSTAT-ACCRUAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD SBSTAT-ACCRUAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBSTAT-ACCRUAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBSTAT-ACCRUAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FACE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2680-FLAG-FACE.
           IF SBSTAT-FACE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBSTAT-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2690-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBSTAT-STATUS-CD
                   ADD SBSTAT-FACE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBSTAT-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBSTAT-FACE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAXABLE YTD FEE ON THE REDEEM AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2700-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SBSTAT-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SBSTAT-TAXABLE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBSTAT-PREFERRED-FLAG = 'Y'
                   COMPUTE SBSTAT-TAXABLE-YTD ROUNDED =
                       SBSTAT-REDEEM-AMT * SBSTAT-MARGIN / 200
               WHEN SBSTAT-REASON-CD = 'DG'
                   MOVE 12.00 TO SBSTAT-TAXABLE-YTD
               WHEN SBSTAT-REASON-CD = 'UK'
                   MOVE 12.00 TO SBSTAT-TAXABLE-YTD
               WHEN OTHER
                   COMPUTE SBSTAT-TAXABLE-YTD ROUNDED =
                       SBSTAT-REDEEM-AMT * SBSTAT-MARGIN / 100
           END-EVALUATE
           IF SBSTAT-TAXABLE-YTD > 0 AND SBSTAT-TAXABLE-YTD < 2.00
               MOVE 2.00 TO SBSTAT-TAXABLE-YTD
           END-IF
           IF SBSTAT-TAXABLE-YTD > 75.00
               MOVE 75.00 TO SBSTAT-TAXABLE-YTD
           END-IF
           ADD SBSTAT-TAXABLE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2710-COUNT-EXCEPT.
           IF SBSTAT-EXCEPT-CNT < 9999999
               ADD 1 TO SBSTAT-EXCEPT-CNT
           END-IF
           IF SBSTAT-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO SBSTAT-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REDEEM AMT BY ISSUE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2720-ACCUM-ISSUE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBSTAT-ISSUE-TYPE-CD
                   ADD SBSTAT-REDEEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBSTAT-ISSUE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBSTAT-REDEEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REDEEM AMT.
      *----------------------------------------------------------------*
       2730-PRORATE-REDEEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBSTAT-REDEEM-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBSTAT-REDEEM-AMT
           END-IF
           ADD SBSTAT-REDEEM-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2740-COUNT-RETRY.
           IF SBSTAT-RETRY-CNT < 9999999
               ADD 1 TO SBSTAT-RETRY-CNT
           END-IF
           IF SBSTAT-RETRY-CNT NOT < 6
               MOVE 'Y' TO SBSTAT-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE TIER TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2750-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-TIER-RATE(1) TO SBSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF SBSTAT-INTEREST-YTD NOT < WS-PURCHASE-TIER-LOW(WS-TX)
                   MOVE WS-PURCHASE-TIER-RATE(WS-TX) TO
                       SBSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2760-DESCRIBE-ISSUE-TYPE.
           EVALUATE SBSTAT-ISSUE-TYPE-CD
               WHEN 'CW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'DY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E982 NAME REQUIRED
      * E606 INTEREST YTD NEGATIVE
      * E892 INTEREST YTD OVER 250000
      * E753 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2770-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBSTAT-NAME = SPACES
                   MOVE 'E982' TO WS-REJECT-CODE
               WHEN SBSTAT-INTEREST-YTD < 0
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN SBSTAT-INTEREST-YTD > 250000.00
                   MOVE 'E892' TO WS-REJECT-CODE
               WHEN SBSTAT-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E753' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2780-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF SBSTAT-EFFECTIVE-DATE > 0 AND SBSTAT-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBSTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBSTAT-TAXABLE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SBSTAT-TAXABLE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SBSTAT-TAXABLE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBSTAT-TAXABLE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBSTAT-TAXABLE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2790-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBSTAT-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBSTAT-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SBSTAT-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SBSTAT-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBSTAT-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBSTAT-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2800-DESCRIBE-REASON.
           EVALUATE SBSTAT-REASON-CD
               WHEN 'DG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2810-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBSTAT-STATUS-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBSTAT-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUAL AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2820-LIMIT-ACCRUAL.
           IF SBSTAT-ACCRUAL-AMT < 10.00
               MOVE 10.00 TO SBSTAT-ACCRUAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBSTAT-ACCRUAL-AMT > 500.00
               MOVE 500.00 TO SBSTAT-ACCRUAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2830-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBSTAT-NOTICE-DATE > 0 AND SBSTAT-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBSTAT-NOTICE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE TIERX TIER FOR THE TAXABLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2840-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-TIERX-RATE(1) TO SBSTAT-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SBSTAT-TAXABLE-YTD NOT < WS-PURCHASE-TIERX-LOW(WS-TX)
                   MOVE WS-PURCHASE-TIERX-RATE(WS-TX) TO SBSTAT-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBSTAT-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBSTAT-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E387 NAME REQUIRED
      * E083 WORK AMT 2 NEGATIVE
      * E237 WORK AMT 2 OVER 99999
      * E746 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2850-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBSTAT-NAME = SPACES
                   MOVE 'E387' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E083' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E237' TO WS-REJECT-CODE
               WHEN SBSTAT-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E746' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CS -> HF WHEN MATURITY DATE IS PAST
      * HF -> JZ WHEN MATURITY DATE IS PAST
      * JZ -> CS WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2860-SET-STATUS.
           EVALUATE SBSTAT-STATUS-CD
               WHEN 'CS'
                   IF SBSTAT-MATURITY-DATE > 0 AND SBSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HF' TO SBSTAT-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF SBSTAT-MATURITY-DATE > 0 AND SBSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JZ' TO SBSTAT-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF SBSTAT-MATURITY-DATE > 0 AND SBSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'CS' TO SBSTAT-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO SBSTAT-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO SBSTAT-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO SBSTAT-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'SBSTAT EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
