      *================================================================*
      * PROGRAM  : TLPEN                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : TELLER BALANCING RULE - LIMITS. CALLED BY THE TELLER*
      *            BALANCING BATCH AND ONLINE PROGRAMS.                *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-03-15 LMB  ORIGINAL                                       *
      * 1991-11-12 GAF  AUDIT FINDING - VAULT-SELL CONTROL TOTALS INC84*
      * 1992-11-09 PQR  CASH-OUT CUTOFF MOVED CHG3443                  *
      * 1993-12-28 GAF  PERFORMANCE - VAULT-SELL READ AHEAD REQ9349    *
      * 1995-06-16 TWH  CASH-OUT LIMIT REVISED CHG7442                 *
      * 2000-04-18 BHS  OVERSHORT CALCULATION CORRECTED INC6354        *
      * 2001-10-18 CRP  VAULT-BUY FEE SCHEDULE UPDATE INC0385          *
      * 2001-12-25 LMB  VAULT-BUY RERUN PROTECTION PRB9926             *
      * 2004-03-22 JLM  OVERSHORT FEE SCHEDULE UPDATE PRB3587          *
      * 2005-06-25 PQR  PERFORMANCE - VAULT-BUY READ AHEAD PRB3854     *
      * 2007-05-06 MKL  OVERSHORT CALCULATION CORRECTED REQ8401        *
      * 2008-10-22 PQR  NEW VAULT-SELL REPORT COLUMNS CHG6256          *
      * 2012-05-07 SKN  VAULT-SELL RERUN PROTECTION REQ2589            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLPEN.
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
                                           VALUE 21.000000.
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
      * WS-OVERSHOR-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-OVERSHOR-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000096000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000504000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000068500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000898000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017600000000522000'.
       01  WS-OVERSHOR-BAND-TBL REDEFINES WS-OVERSHOR-BAND-VALUES.
           05  WS-OVERSHOR-BAND-ROW OCCURS 5 TIMES.
               10  WS-OVERSHOR-BAND-LOW    PIC 9(09)V99.
               10  WS-OVERSHOR-BAND-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY TLPENP.

       PROCEDURE DIVISION USING TLPEN-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SELECT-RATE
           PERFORM 2010-FLAG-VAULT-BUY
           PERFORM 2020-EDIT-DESC
           PERFORM 2030-ACCUM-REASON
           PERFORM 2040-EDIT-REF-NO
           PERFORM 2050-PRORATE-CASH-IN
           PERFORM 2060-SET-SOURCE
           PERFORM 2070-SPLIT-VAULT-BUY
           PERFORM 2080-COUNT-ITEM
           PERFORM 2090-ACCUM-CHANNEL
           PERFORM 2100-PRORATE-BATCH
           PERFORM 2110-COUNT-ITEM
           PERFORM 2120-DESCRIBE-CHANNEL
           PERFORM 2130-PRORATE-CASH-OUT
           PERFORM 2140-CHECK-NOTICE
           PERFORM 2150-AGE-EFFECTIVE
           PERFORM 2160-SELECT-RATE
           PERFORM 2170-DESCRIBE-CHANNEL
           PERFORM 2180-SPLIT-OVERSHORT
           PERFORM 2190-SELECT-FEE-PCT
           PERFORM 2200-ACCUM-CHANNEL
           PERFORM 2210-COUNT-RETRY
           PERFORM 2220-EDIT-DESC
           PERFORM 2230-CHECK-OPEN
           PERFORM 2240-SELECT-FEE-PCT
           PERFORM 2250-AGE-NEXT-REVIEW
           PERFORM 2260-PRORATE-OVERSHORT
           PERFORM 2270-LIMIT-ITEM
           PERFORM 2280-ACCUM-DRAWER
           PERFORM 2290-FLAG-CASH-OUT
           PERFORM 2300-SPLIT-CASH-OUT
           PERFORM 2310-COUNT-RETRY
           PERFORM 2320-CHECK-OPEN
           PERFORM 2330-SPLIT-BATCH
           PERFORM 2340-PRORATE-CASH-OUT
           PERFORM 2350-FLAG-OVERSHORT
           PERFORM 2360-DESCRIBE-CHANNEL
           PERFORM 2370-ACCUM-DRAWER
           PERFORM 2380-DESCRIBE-CHANNEL
           PERFORM 2390-LIMIT-CASH-OUT
           PERFORM 2400-AGE-NEXT-REVIEW
           PERFORM 2410-LIMIT-CASH-OUT
           PERFORM 2420-FLAG-CASH-IN
           PERFORM 2430-ACCUM-REASON
           PERFORM 2440-LIMIT-BATCH
           PERFORM 2450-ASSESS-REVIEW
           PERFORM 2460-ASSESS-HOLD
           PERFORM 2470-ACCRUE-CASH-OUT
           PERFORM 2480-DESCRIBE-CHANNEL
           PERFORM 2490-ACCUM-REASON
           PERFORM 2500-CHECK-NEXT-REVIEW
           PERFORM 2510-SELECT-RATE
           PERFORM 2520-CHECK-NOTICE
           PERFORM 2530-ACCRUE-FEE-PCT
           PERFORM 2540-ACCUM-REASON
           PERFORM 2550-ACCUM-DRAWER
           PERFORM 2560-AGE-NEXT-REVIEW
           PERFORM 2570-SPLIT-CASH-IN
           PERFORM 2580-CHECK-OPEN
           PERFORM 2590-AGE-NEXT-REVIEW
           PERFORM 2600-SET-SOURCE
           PERFORM 2610-SPLIT-CASH-OUT
           PERFORM 2620-ACCRUE-BATCH
           PERFORM 2630-AGE-EFFECTIVE
           PERFORM 2640-ACCRUE-OVERSHORT
           PERFORM 2650-ASSESS-REVIEW
           PERFORM 2660-AGE-NOTICE
           PERFORM 2670-PRORATE-OVERSHORT
           PERFORM 2680-DESCRIBE-DRAWER
           PERFORM 2690-DESCRIBE-CHANNEL
           PERFORM 2700-PRORATE-CASH-OUT
           PERFORM 2710-ACCUM-REASON
           PERFORM 2720-SET-DRAWER
           PERFORM 2730-ACCUM-CHANNEL
           PERFORM 2740-ACCUM-CHANNEL
           PERFORM 2750-CHECK-EFFECTIVE
           PERFORM 2760-SPLIT-CASH-OUT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO TLPEN-RETURN-CODE.

      *----------------------------------------------------------------*
      * SELECT THE OVERSHOR BAND TIER FOR THE CASH IN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2000-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-OVERSHOR-BAND-RATE(1) TO TLPEN-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TLPEN-CASH-IN-AMT NOT < WS-OVERSHOR-BAND-LOW(WS-TX)
                   MOVE WS-OVERSHOR-BAND-RATE(WS-TX) TO TLPEN-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLPEN-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLPEN-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VAULT BUY AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2010-FLAG-VAULT-BUY.
           IF TLPEN-VAULT-BUY-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLPEN-VAULT-BUY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E688 DESC REQUIRED
      * E661 OVERSHORT AMT NEGATIVE
      * E349 OVERSHORT AMT OVER 250000
      * E169 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2020-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLPEN-DESC = SPACES
                   MOVE 'E688' TO WS-REJECT-CODE
               WHEN TLPEN-OVERSHORT-AMT < 0
                   MOVE 'E661' TO WS-REJECT-CODE
               WHEN TLPEN-OVERSHORT-AMT > 250000.00
                   MOVE 'E349' TO WS-REJECT-CODE
               WHEN TLPEN-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E169' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2030-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLPEN-REASON-CD
                   ADD TLPEN-CASH-OUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLPEN-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLPEN-CASH-OUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E396 REF NO REQUIRED
      * E098 CASH IN AMT NEGATIVE
      * E869 CASH IN AMT OVER 99999
      * E130 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2040-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLPEN-REF-NO = SPACES
                   MOVE 'E396' TO WS-REJECT-CODE
               WHEN TLPEN-CASH-IN-AMT < 0
                   MOVE 'E098' TO WS-REJECT-CODE
               WHEN TLPEN-CASH-IN-AMT > 99999.00
                   MOVE 'E869' TO WS-REJECT-CODE
               WHEN TLPEN-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E130' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH IN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2050-PRORATE-CASH-IN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLPEN-CASH-IN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AF -> PT WHEN NEXT REVIEW DATE IS PAST
      * PT -> SZ WHEN NEXT REVIEW DATE IS PAST
      * SZ -> VK WHEN NEXT REVIEW DATE IS PAST
      * VK -> VW WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2060-SET-SOURCE.
           EVALUATE TLPEN-SOURCE-CD
               WHEN 'AF'
                   IF TLPEN-NEXT-REVIEW-DATE > 0 AND
                       TLPEN-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PT' TO TLPEN-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TLPEN-NEXT-REVIEW-DATE > 0 AND
                       TLPEN-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'SZ' TO TLPEN-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SZ'
                   IF TLPEN-NEXT-REVIEW-DATE > 0 AND
                       TLPEN-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VK' TO TLPEN-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF TLPEN-NEXT-REVIEW-DATE > 0 AND
                       TLPEN-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VW' TO TLPEN-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VAULT BUY AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2070-SPLIT-VAULT-BUY.
           COMPUTE WS-WORK-AMT = TLPEN-VAULT-BUY-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TLPEN-VAULT-BUY-AMT - (WS-WORK-AMT *
               4)
           COMPUTE TLPEN-OVERSHORT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2080-COUNT-ITEM.
           IF TLPEN-ITEM-CNT < 9999999
               ADD 1 TO TLPEN-ITEM-CNT
           END-IF
           IF TLPEN-ITEM-CNT NOT < 3
               MOVE 'Y' TO TLPEN-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2090-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLPEN-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLPEN-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH OUT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BATCH AMT.
      *----------------------------------------------------------------*
       2100-PRORATE-BATCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLPEN-BATCH-AMT ROUNDED =
                   TLPEN-CASH-OUT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLPEN-BATCH-AMT
           END-IF
           ADD TLPEN-BATCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2110-COUNT-ITEM.
           IF TLPEN-ITEM-CNT < 9999999
               ADD 1 TO TLPEN-ITEM-CNT
           END-IF
           IF TLPEN-ITEM-CNT NOT < 5
               MOVE 'Y' TO TLPEN-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2120-DESCRIBE-CHANNEL.
           EVALUATE TLPEN-CHANNEL-CD
               WHEN 'D'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'F'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'N'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH OUT YTD.
      *----------------------------------------------------------------*
       2130-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLPEN-CASH-OUT-YTD ROUNDED =
                   TLPEN-OVERSHORT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLPEN-CASH-OUT-YTD
           END-IF
           ADD TLPEN-CASH-OUT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2140-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TLPEN-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLPEN-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TLPEN-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2150-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLPEN-EFFECTIVE-DATE > 0 AND TLPEN-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLPEN-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE OVERSHOR BAND TIER FOR THE VAULT BUY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2160-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-OVERSHOR-BAND-RATE(1) TO TLPEN-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TLPEN-VAULT-BUY-AMT NOT < WS-OVERSHOR-BAND-LOW(WS-TX)
                   MOVE WS-OVERSHOR-BAND-RATE(WS-TX) TO TLPEN-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLPEN-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLPEN-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2170-DESCRIBE-CHANNEL.
           EVALUATE TLPEN-CHANNEL-CD
               WHEN 'D'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'M'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'N'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2180-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE TLPEN-OVERSHORT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OVERSHOR BAND TIER FOR THE CASH OUT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2190-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-OVERSHOR-BAND-RATE(1) TO TLPEN-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TLPEN-CASH-OUT-YTD NOT < WS-OVERSHOR-BAND-LOW(WS-TX)
                   MOVE WS-OVERSHOR-BAND-RATE(WS-TX) TO TLPEN-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLPEN-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLPEN-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2200-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLPEN-CHANNEL-CD
                   ADD TLPEN-CASH-OUT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLPEN-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLPEN-CASH-OUT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2210-COUNT-RETRY.
           IF TLPEN-RETRY-CNT < 9999999
               ADD 1 TO TLPEN-RETRY-CNT
           END-IF
           IF TLPEN-RETRY-CNT NOT < 5
               MOVE 'Y' TO TLPEN-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E362 DESC REQUIRED
      * E283 BATCH AMT NEGATIVE
      * E395 BATCH AMT OVER 25000
      * E702 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2220-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLPEN-DESC = SPACES
                   MOVE 'E362' TO WS-REJECT-CODE
               WHEN TLPEN-BATCH-AMT < 0
                   MOVE 'E283' TO WS-REJECT-CODE
               WHEN TLPEN-BATCH-AMT > 25000.00
                   MOVE 'E395' TO WS-REJECT-CODE
               WHEN TLPEN-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E702' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2230-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLPEN-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLPEN-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TLPEN-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OVERSHOR BAND TIER FOR THE CASH OUT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2240-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-OVERSHOR-BAND-RATE(1) TO TLPEN-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TLPEN-CASH-OUT-YTD NOT < WS-OVERSHOR-BAND-LOW(WS-TX)
                   MOVE WS-OVERSHOR-BAND-RATE(WS-TX) TO TLPEN-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLPEN-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLPEN-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2250-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TLPEN-NEXT-REVIEW-DATE > 0 AND TLPEN-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLPEN-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CASH OUT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OVERSHORT AMT.
      *----------------------------------------------------------------*
       2260-PRORATE-OVERSHORT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLPEN-OVERSHORT-AMT ROUNDED =
                   TLPEN-CASH-OUT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLPEN-OVERSHORT-AMT
           END-IF
           ADD TLPEN-OVERSHORT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2270-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT AMT BY DRAWER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2280-ACCUM-DRAWER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLPEN-DRAWER-CD
                   ADD TLPEN-CASH-OUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLPEN-DRAWER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLPEN-CASH-OUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2290-FLAG-CASH-OUT.
           IF TLPEN-CASH-OUT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLPEN-CASH-OUT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2300-SPLIT-CASH-OUT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE TLPEN-CASH-OUT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2310-COUNT-RETRY.
           IF TLPEN-RETRY-CNT < 9999999
               ADD 1 TO TLPEN-RETRY-CNT
           END-IF
           IF TLPEN-RETRY-CNT NOT < 5
               MOVE 'Y' TO TLPEN-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2320-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLPEN-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLPEN-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TLPEN-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BATCH AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2330-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TLPEN-BATCH-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TLPEN-BATCH-AMT - (WS-WORK-AMT * 4)
           COMPUTE TLPEN-CASH-OUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH IN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH OUT AMT.
      *----------------------------------------------------------------*
       2340-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLPEN-CASH-OUT-AMT ROUNDED =
                   TLPEN-CASH-IN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLPEN-CASH-OUT-AMT
           END-IF
           ADD TLPEN-CASH-OUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * OVERSHORT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2350-FLAG-OVERSHORT.
           IF TLPEN-OVERSHORT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLPEN-OVERSHORT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2360-DESCRIBE-CHANNEL.
           EVALUATE TLPEN-CHANNEL-CD
               WHEN 'D'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'N'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'R'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'T'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT BUY AMT BY DRAWER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2370-ACCUM-DRAWER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLPEN-DRAWER-CD
                   ADD TLPEN-VAULT-BUY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLPEN-DRAWER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLPEN-VAULT-BUY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2380-DESCRIBE-CHANNEL.
           EVALUATE TLPEN-CHANNEL-CD
               WHEN 'D'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'R'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH OUT YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2390-LIMIT-CASH-OUT.
           IF TLPEN-CASH-OUT-YTD < 10.00
               MOVE 10.00 TO TLPEN-CASH-OUT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLPEN-CASH-OUT-YTD > 2500.00
               MOVE 2500.00 TO TLPEN-CASH-OUT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2400-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TLPEN-NEXT-REVIEW-DATE > 0 AND TLPEN-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLPEN-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLPEN-CASH-OUT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TLPEN-CASH-OUT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TLPEN-CASH-OUT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLPEN-CASH-OUT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLPEN-CASH-OUT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH OUT YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2410-LIMIT-CASH-OUT.
           IF TLPEN-CASH-OUT-YTD < 0.00
               MOVE 0.00 TO TLPEN-CASH-OUT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLPEN-CASH-OUT-YTD > 2500.00
               MOVE 2500.00 TO TLPEN-CASH-OUT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH IN AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2420-FLAG-CASH-IN.
           IF TLPEN-CASH-IN-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLPEN-CASH-IN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT BUY AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2430-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLPEN-REASON-CD
                   ADD TLPEN-VAULT-BUY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLPEN-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLPEN-VAULT-BUY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * BATCH AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2440-LIMIT-BATCH.
           IF TLPEN-BATCH-AMT < 0.00
               MOVE 0.00 TO TLPEN-BATCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLPEN-BATCH-AMT > 500.00
               MOVE 500.00 TO TLPEN-BATCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT FEE ON THE CASH IN AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2450-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TLPEN-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TLPEN-OVERSHORT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLPEN-SUSPEND-FLAG = 'Y'
                   COMPUTE TLPEN-OVERSHORT-AMT ROUNDED =
                       TLPEN-CASH-IN-AMT * TLPEN-RATE / 200
               WHEN TLPEN-CHANNEL-CD = 'D'
                   MOVE 3.00 TO TLPEN-OVERSHORT-AMT
               WHEN TLPEN-CHANNEL-CD = 'F'
                   MOVE 12.00 TO TLPEN-OVERSHORT-AMT
               WHEN OTHER
                   COMPUTE TLPEN-OVERSHORT-AMT ROUNDED =
                       TLPEN-CASH-IN-AMT * TLPEN-RATE / 100
           END-EVALUATE
           IF TLPEN-OVERSHORT-AMT > 0 AND TLPEN-OVERSHORT-AMT < 5.00
               MOVE 5.00 TO TLPEN-OVERSHORT-AMT
           END-IF
           IF TLPEN-OVERSHORT-AMT > 25.00
               MOVE 25.00 TO TLPEN-OVERSHORT-AMT
           END-IF
           ADD TLPEN-OVERSHORT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CASH OUT FEE ON THE CASH OUT YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2460-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN TLPEN-HOLD-FLAG = 'Y'
                   MOVE ZERO TO TLPEN-CASH-OUT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLPEN-SUSPEND-FLAG = 'Y'
                   COMPUTE TLPEN-CASH-OUT-AMT ROUNDED =
                       TLPEN-CASH-OUT-YTD * TLPEN-FEE-PCT / 200
               WHEN TLPEN-SOURCE-CD = 'AF'
                   MOVE 3.00 TO TLPEN-CASH-OUT-AMT
               WHEN TLPEN-SOURCE-CD = 'PT'
                   MOVE 10.00 TO TLPEN-CASH-OUT-AMT
               WHEN OTHER
                   COMPUTE TLPEN-CASH-OUT-AMT ROUNDED =
                       TLPEN-CASH-OUT-YTD * TLPEN-FEE-PCT / 100
           END-EVALUATE
           IF TLPEN-CASH-OUT-AMT > 0 AND TLPEN-CASH-OUT-AMT < 10.00
               MOVE 10.00 TO TLPEN-CASH-OUT-AMT
           END-IF
           IF TLPEN-CASH-OUT-AMT > 100.00
               MOVE 100.00 TO TLPEN-CASH-OUT-AMT
           END-IF
           ADD TLPEN-CASH-OUT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH OUT YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2470-ACCRUE-CASH-OUT.
           IF TLPEN-CASH-OUT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLPEN-CASH-OUT-YTD * TLPEN-RATE / 100 / 360
               ADD WS-WORK-AMT TO TLPEN-CASH-OUT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2480-DESCRIBE-CHANNEL.
           EVALUATE TLPEN-CHANNEL-CD
               WHEN 'D'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'M'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'R'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2490-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLPEN-REASON-CD
                   ADD TLPEN-CASH-OUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLPEN-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLPEN-CASH-OUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2500-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TLPEN-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLPEN-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TLPEN-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OVERSHOR BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2510-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-OVERSHOR-BAND-RATE(1) TO TLPEN-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT NOT < WS-OVERSHOR-BAND-LOW(WS-TX)
                   MOVE WS-OVERSHOR-BAND-RATE(WS-TX) TO TLPEN-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLPEN-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLPEN-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2520-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TLPEN-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLPEN-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TLPEN-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2530-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TLPEN-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TLPEN-OVERSHORT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BATCH AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2540-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLPEN-REASON-CD
                   ADD TLPEN-BATCH-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLPEN-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLPEN-BATCH-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY DRAWER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2550-ACCUM-DRAWER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLPEN-DRAWER-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLPEN-DRAWER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2560-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TLPEN-NEXT-REVIEW-DATE > 0 AND TLPEN-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLPEN-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLPEN-CASH-OUT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLPEN-CASH-OUT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TLPEN-CASH-OUT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLPEN-CASH-OUT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLPEN-CASH-OUT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CASH IN AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2570-SPLIT-CASH-IN.
           COMPUTE WS-WORK-AMT = TLPEN-CASH-IN-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TLPEN-CASH-IN-AMT - (WS-WORK-AMT * 3)
           COMPUTE TLPEN-VAULT-BUY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2580-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLPEN-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLPEN-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TLPEN-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2590-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TLPEN-NEXT-REVIEW-DATE > 0 AND TLPEN-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLPEN-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLPEN-VAULT-BUY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLPEN-VAULT-BUY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TLPEN-VAULT-BUY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLPEN-VAULT-BUY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLPEN-VAULT-BUY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AF -> PT WHEN OPEN DATE IS PAST
      * PT -> SZ WHEN OPEN DATE IS PAST
      * SZ -> VK WHEN OPEN DATE IS PAST
      * VK -> VW WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2600-SET-SOURCE.
           EVALUATE TLPEN-SOURCE-CD
               WHEN 'AF'
                   IF TLPEN-OPEN-DATE > 0 AND TLPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TLPEN-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TLPEN-OPEN-DATE > 0 AND TLPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SZ' TO TLPEN-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SZ'
                   IF TLPEN-OPEN-DATE > 0 AND TLPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VK' TO TLPEN-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF TLPEN-OPEN-DATE > 0 AND TLPEN-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VW' TO TLPEN-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CASH OUT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2610-SPLIT-CASH-OUT.
           COMPUTE WS-WORK-AMT = TLPEN-CASH-OUT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TLPEN-CASH-OUT-YTD - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BATCH AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2620-ACCRUE-BATCH.
           IF TLPEN-BATCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLPEN-BATCH-AMT * TLPEN-RATE / 100 / 360
               ADD WS-WORK-AMT TO TLPEN-VAULT-BUY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2630-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLPEN-EFFECTIVE-DATE > 0 AND TLPEN-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLPEN-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLPEN-BATCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLPEN-BATCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TLPEN-BATCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TLPEN-BATCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLPEN-BATCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OVERSHORT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2640-ACCRUE-OVERSHORT.
           IF TLPEN-OVERSHORT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLPEN-OVERSHORT-AMT * TLPEN-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE BATCH AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2650-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TLPEN-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLPEN-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TLPEN-BATCH-AMT *
                       TLPEN-FEE-PCT / 200
               WHEN TLPEN-REASON-CD = 'GS'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN TLPEN-REASON-CD = 'GV'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TLPEN-BATCH-AMT *
                       TLPEN-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2660-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TLPEN-NOTICE-DATE > 0 AND TLPEN-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLPEN-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLPEN-CASH-IN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TLPEN-CASH-IN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TLPEN-CASH-IN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLPEN-CASH-IN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLPEN-CASH-IN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OVERSHORT AMT.
      *----------------------------------------------------------------*
       2670-PRORATE-OVERSHORT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLPEN-OVERSHORT-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLPEN-OVERSHORT-AMT
           END-IF
           ADD TLPEN-OVERSHORT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2680-DESCRIBE-DRAWER.
           EVALUATE TLPEN-DRAWER-CD
               WHEN 'D'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'R'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2690-DESCRIBE-CHANNEL.
           EVALUATE TLPEN-CHANNEL-CD
               WHEN 'D'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'F'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'M'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'N'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH OUT YTD.
      *----------------------------------------------------------------*
       2700-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLPEN-CASH-OUT-YTD ROUNDED =
                   TLPEN-OVERSHORT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLPEN-CASH-OUT-YTD
           END-IF
           ADD TLPEN-CASH-OUT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2710-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLPEN-REASON-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLPEN-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DRAWER CD:
      * D -> F WHEN NOTICE DATE IS PAST
      * F -> R WHEN NOTICE DATE IS PAST
      * R -> D WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2720-SET-DRAWER.
           EVALUATE TLPEN-DRAWER-CD
               WHEN 'D'
                   IF TLPEN-NOTICE-DATE > 0 AND TLPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO TLPEN-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TLPEN-NOTICE-DATE > 0 AND TLPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO TLPEN-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF TLPEN-NOTICE-DATE > 0 AND TLPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TLPEN-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2730-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLPEN-CHANNEL-CD
                   ADD TLPEN-OVERSHORT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLPEN-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLPEN-OVERSHORT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BATCH AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2740-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLPEN-CHANNEL-CD
                   ADD TLPEN-BATCH-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLPEN-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLPEN-BATCH-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2750-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLPEN-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLPEN-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TLPEN-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH OUT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2760-SPLIT-CASH-OUT.
           COMPUTE WS-WORK-AMT = TLPEN-CASH-OUT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TLPEN-CASH-OUT-YTD - (WS-WORK-AMT *
               2)
           COMPUTE TLPEN-BATCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO TLPEN-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO TLPEN-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO TLPEN-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'TLPEN EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
