      *================================================================*
      * PROGRAM  : BUTIER                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : BUMP-UP CERTIFICATES RULE - LIMITS. CALLED BY THE   *
      *            BUMP-UP CERTIFICATES BATCH AND ONLINE PROGRAMS.     *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-12-22 TWH  ORIGINAL                                       *
      * 1995-07-06 EWS  AUDIT FINDING - PRINCIPAL CONTROL TOTALS CHG172*
      * 2001-02-02 CRP  PRINCIPAL CUTOFF MOVED INC8262                 *
      * 2003-08-28 LMB  AUDIT FINDING - ADDON CONTROL TOTALS INC0933   *
      * 2005-07-18 PQR  BUMP CALCULATION CORRECTED INC9086             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUTIER.
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
                                           VALUE 25.000000.
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
      * WS-PRINCIPA-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINCIPA-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000531500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000410000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000181000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015050000000801000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016050000000494000'.
       01  WS-PRINCIPA-TIER-TBL REDEFINES WS-PRINCIPA-TIER-VALUES.
           05  WS-PRINCIPA-TIER-ROW OCCURS 5 TIMES.
               10  WS-PRINCIPA-TIER-LOW    PIC 9(09)V99.
               10  WS-PRINCIPA-TIER-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY BUTIERP.

       PROCEDURE DIVISION USING BUTIER-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ASSESS-EXEMPT
           PERFORM 2010-ACCUM-CHANNEL
           PERFORM 2020-SET-SOURCE
           PERFORM 2030-CHECK-MATURITY
           PERFORM 2040-SPLIT-PRINCIPAL
           PERFORM 2050-PRORATE-BUMP
           PERFORM 2060-ASSESS-EXEMPT
           PERFORM 2070-PRORATE-PENALTY
           PERFORM 2080-SELECT-MARGIN
           PERFORM 2090-ACCUM-CHANNEL
           PERFORM 2100-SPLIT-PRINCIPAL
           PERFORM 2110-SET-REGION
           PERFORM 2120-ACCUM-CYCLE
           PERFORM 2130-SET-SOURCE
           PERFORM 2140-FLAG-ADDON
           PERFORM 2150-PRORATE-PRINCIPAL
           PERFORM 2160-ACCUM-SOURCE
           PERFORM 2170-DESCRIBE-CHANNEL
           PERFORM 2180-ASSESS-OVERRIDE
           PERFORM 2190-SPLIT-PRINCIPAL
           PERFORM 2200-ACCUM-CHANNEL
           PERFORM 2210-EDIT-CUST-ID
           PERFORM 2220-ACCRUE-BUMP
           PERFORM 2230-SET-SOURCE
           PERFORM 2240-COUNT-RETRY
           PERFORM 2250-ACCUM-CYCLE
           PERFORM 2260-SET-SOURCE
           PERFORM 2270-DESCRIBE-CYCLE
           PERFORM 2280-LIMIT-BUMP
           PERFORM 2290-COUNT-RETRY
           PERFORM 2300-FLAG-INTEREST
           PERFORM 2310-SET-SOURCE
           PERFORM 2320-ACCRUE-INTEREST
           PERFORM 2330-ACCRUE-RATE
           PERFORM 2340-ACCUM-CHANNEL
           PERFORM 2350-SELECT-RATE
           PERFORM 2360-LIMIT-BUMP
           PERFORM 2370-LIMIT-PRINCIPAL
           PERFORM 2380-FLAG-ITEM
           PERFORM 2390-SPLIT-PRINCIPAL
           PERFORM 2400-SELECT-RATE
           PERFORM 2410-PRORATE-BUMP
           PERFORM 2420-AGE-MATURITY
           PERFORM 2430-CHECK-EFFECTIVE
           PERFORM 2440-CHECK-CLOSE
           PERFORM 2450-SET-REGION
           PERFORM 2460-ASSESS-OVERRIDE
           PERFORM 2470-AGE-CLOSE
           PERFORM 2480-LIMIT-PENALTY
           PERFORM 2490-LIMIT-PRINCIPAL
           PERFORM 2500-PRORATE-INTEREST
           PERFORM 2510-ACCRUE-PRINCIPAL
           PERFORM 2520-SPLIT-PRINCIPAL
           PERFORM 2530-COUNT-RETRY
           PERFORM 2540-LIMIT-ITEM
           PERFORM 2550-SPLIT-BUMP
           PERFORM 2560-EDIT-CUST-ID
           PERFORM 2570-ACCUM-CHANNEL
           PERFORM 2580-EDIT-REF-NO
           PERFORM 2590-ACCRUE-PRINCIPAL
           PERFORM 2600-FLAG-ADDON
           PERFORM 2610-COUNT-EXCEPT
           PERFORM 2620-CHECK-CLOSE
           PERFORM 2630-LIMIT-ADDON
           PERFORM 2640-ACCRUE-ADDON
           PERFORM 2650-SPLIT-BUMP
           PERFORM 2660-ACCUM-SOURCE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO BUTIER-RETURN-CODE.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY YTD FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2000-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BUTIER-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BUTIER-PENALTY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUTIER-OVERRIDE-FLAG = 'Y'
                   COMPUTE BUTIER-PENALTY-YTD ROUNDED = WS-WORK-AMT *
                       BUTIER-MARGIN / 200
               WHEN BUTIER-CHANNEL-CD = 'EK'
                   MOVE 3.00 TO BUTIER-PENALTY-YTD
               WHEN BUTIER-CHANNEL-CD = 'JE'
                   MOVE 3.00 TO BUTIER-PENALTY-YTD
               WHEN OTHER
                   COMPUTE BUTIER-PENALTY-YTD ROUNDED = WS-WORK-AMT *
                       BUTIER-MARGIN / 100
           END-EVALUATE
           IF BUTIER-PENALTY-YTD > 0 AND BUTIER-PENALTY-YTD < 5.00
               MOVE 5.00 TO BUTIER-PENALTY-YTD
           END-IF
           IF BUTIER-PENALTY-YTD > 250.00
               MOVE 250.00 TO BUTIER-PENALTY-YTD
           END-IF
           ADD BUTIER-PENALTY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2010-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUTIER-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUTIER-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN EFFECTIVE DATE IS PAST
      * BF -> HF WHEN EFFECTIVE DATE IS PAST
      * HF -> HR WHEN EFFECTIVE DATE IS PAST
      * HR -> UF WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2020-SET-SOURCE.
           EVALUATE BUTIER-SOURCE-CD
               WHEN 'AG'
                   IF BUTIER-EFFECTIVE-DATE > 0 AND
                       BUTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'BF' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BUTIER-EFFECTIVE-DATE > 0 AND
                       BUTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'HF' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BUTIER-EFFECTIVE-DATE > 0 AND
                       BUTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'HR' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BUTIER-EFFECTIVE-DATE > 0 AND
                       BUTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'UF' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2030-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BUTIER-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUTIER-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BUTIER-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2040-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = BUTIER-PRINCIPAL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BUTIER-PRINCIPAL-AMT - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ADDON AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BUMP AMT.
      *----------------------------------------------------------------*
       2050-PRORATE-BUMP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BUTIER-BUMP-AMT ROUNDED =
                   BUTIER-ADDON-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BUTIER-BUMP-AMT
           END-IF
           ADD BUTIER-BUMP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2060-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BUTIER-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BUTIER-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUTIER-REVIEW-FLAG = 'Y'
                   COMPUTE BUTIER-INTEREST-AMT ROUNDED =
                       BUTIER-PRINCIPAL-AMT * BUTIER-RATE / 200
               WHEN BUTIER-REGION-CD = 'BGJ'
                   MOVE 10.00 TO BUTIER-INTEREST-AMT
               WHEN BUTIER-REGION-CD = 'LFX'
                   MOVE 15.00 TO BUTIER-INTEREST-AMT
               WHEN OTHER
                   COMPUTE BUTIER-INTEREST-AMT ROUNDED =
                       BUTIER-PRINCIPAL-AMT * BUTIER-RATE / 100
           END-EVALUATE
           IF BUTIER-INTEREST-AMT > 0 AND BUTIER-INTEREST-AMT < 10.00
               MOVE 10.00 TO BUTIER-INTEREST-AMT
           END-IF
           IF BUTIER-INTEREST-AMT > 75.00
               MOVE 75.00 TO BUTIER-INTEREST-AMT
           END-IF
           ADD BUTIER-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PENALTY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2070-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   BUTIER-PENALTY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA TIER TIER FOR THE BUMP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2080-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-TIER-RATE(1) TO BUTIER-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF BUTIER-BUMP-AMT NOT < WS-PRINCIPA-TIER-LOW(WS-TX)
                   MOVE WS-PRINCIPA-TIER-RATE(WS-TX) TO BUTIER-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUTIER-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUTIER-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2090-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUTIER-CHANNEL-CD
                   ADD BUTIER-PENALTY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUTIER-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUTIER-PENALTY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2100-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = BUTIER-PRINCIPAL-AMT / 12
           COMPUTE WS-WORK-AMT-2 = BUTIER-PRINCIPAL-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BGJ -> LFX WHEN MATURITY DATE IS PAST
      * LFX -> MLA WHEN MATURITY DATE IS PAST
      * MLA -> ULA WHEN MATURITY DATE IS PAST
      * ULA -> BGJ WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2110-SET-REGION.
           EVALUATE BUTIER-REGION-CD
               WHEN 'BGJ'
                   IF BUTIER-MATURITY-DATE > 0 AND BUTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LFX' TO BUTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LFX'
                   IF BUTIER-MATURITY-DATE > 0 AND BUTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MLA' TO BUTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MLA'
                   IF BUTIER-MATURITY-DATE > 0 AND BUTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ULA' TO BUTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULA'
                   IF BUTIER-MATURITY-DATE > 0 AND BUTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BGJ' TO BUTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2120-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUTIER-CYCLE-CD
                   ADD BUTIER-PENALTY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUTIER-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUTIER-PENALTY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN MATURITY DATE IS PAST
      * BF -> HF WHEN MATURITY DATE IS PAST
      * HF -> HR WHEN MATURITY DATE IS PAST
      * HR -> UF WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2130-SET-SOURCE.
           EVALUATE BUTIER-SOURCE-CD
               WHEN 'AG'
                   IF BUTIER-MATURITY-DATE > 0 AND BUTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BF' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BUTIER-MATURITY-DATE > 0 AND BUTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HF' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BUTIER-MATURITY-DATE > 0 AND BUTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HR' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BUTIER-MATURITY-DATE > 0 AND BUTIER-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UF' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ADDON AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2140-FLAG-ADDON.
           IF BUTIER-ADDON-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BUTIER-ADDON-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINCIPAL YTD.
      *----------------------------------------------------------------*
       2150-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BUTIER-PRINCIPAL-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BUTIER-PRINCIPAL-YTD
           END-IF
           ADD BUTIER-PRINCIPAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2160-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUTIER-SOURCE-CD
                   ADD BUTIER-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUTIER-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUTIER-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2170-DESCRIBE-CHANNEL.
           EVALUATE BUTIER-CHANNEL-CD
               WHEN 'EK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2180-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BUTIER-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BUTIER-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUTIER-EXEMPT-FLAG = 'Y'
                   COMPUTE BUTIER-PRINCIPAL-AMT ROUNDED = WS-WORK-AMT *
                       BUTIER-RATE / 200
               WHEN BUTIER-REGION-CD = 'BGJ'
                   MOVE 10.00 TO BUTIER-PRINCIPAL-AMT
               WHEN BUTIER-REGION-CD = 'LFX'
                   MOVE 15.00 TO BUTIER-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE BUTIER-PRINCIPAL-AMT ROUNDED = WS-WORK-AMT *
                       BUTIER-RATE / 100
           END-EVALUATE
           IF BUTIER-PRINCIPAL-AMT > 0 AND BUTIER-PRINCIPAL-AMT < 2.00
               MOVE 2.00 TO BUTIER-PRINCIPAL-AMT
           END-IF
           IF BUTIER-PRINCIPAL-AMT > 100.00
               MOVE 100.00 TO BUTIER-PRINCIPAL-AMT
           END-IF
           ADD BUTIER-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2190-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = BUTIER-PRINCIPAL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BUTIER-PRINCIPAL-AMT - (WS-WORK-AMT *
               3)
           COMPUTE BUTIER-ADDON-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2200-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUTIER-CHANNEL-CD
                   ADD BUTIER-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUTIER-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUTIER-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E645 CUST ID REQUIRED
      * E099 BUMP AMT NEGATIVE
      * E632 BUMP AMT OVER 1000000
      * E554 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2210-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BUTIER-CUST-ID = SPACES
                   MOVE 'E645' TO WS-REJECT-CODE
               WHEN BUTIER-BUMP-AMT < 0
                   MOVE 'E099' TO WS-REJECT-CODE
               WHEN BUTIER-BUMP-AMT > 1000000.00
                   MOVE 'E632' TO WS-REJECT-CODE
               WHEN BUTIER-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E554' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUMP AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2220-ACCRUE-BUMP.
           IF BUTIER-BUMP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BUTIER-BUMP-AMT * BUTIER-RATE / 100 / 365
               ADD WS-WORK-AMT TO BUTIER-PRINCIPAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN EFFECTIVE DATE IS PAST
      * BF -> HF WHEN EFFECTIVE DATE IS PAST
      * HF -> HR WHEN EFFECTIVE DATE IS PAST
      * HR -> UF WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2230-SET-SOURCE.
           EVALUATE BUTIER-SOURCE-CD
               WHEN 'AG'
                   IF BUTIER-EFFECTIVE-DATE > 0 AND
                       BUTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'BF' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BUTIER-EFFECTIVE-DATE > 0 AND
                       BUTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'HF' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BUTIER-EFFECTIVE-DATE > 0 AND
                       BUTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'HR' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BUTIER-EFFECTIVE-DATE > 0 AND
                       BUTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'UF' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2240-COUNT-RETRY.
           IF BUTIER-RETRY-CNT < 9999999
               ADD 1 TO BUTIER-RETRY-CNT
           END-IF
           IF BUTIER-RETRY-CNT NOT < 5
               MOVE 'Y' TO BUTIER-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2250-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUTIER-CYCLE-CD
                   ADD BUTIER-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUTIER-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUTIER-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN CLOSE DATE IS PAST
      * BF -> HF WHEN CLOSE DATE IS PAST
      * HF -> HR WHEN CLOSE DATE IS PAST
      * HR -> UF WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2260-SET-SOURCE.
           EVALUATE BUTIER-SOURCE-CD
               WHEN 'AG'
                   IF BUTIER-CLOSE-DATE > 0 AND BUTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BF' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BUTIER-CLOSE-DATE > 0 AND BUTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HF' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BUTIER-CLOSE-DATE > 0 AND BUTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BUTIER-CLOSE-DATE > 0 AND BUTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UF' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2270-DESCRIBE-CYCLE.
           EVALUATE BUTIER-CYCLE-CD
               WHEN 'A'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'S'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'T'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Y'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BUMP AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2280-LIMIT-BUMP.
           IF BUTIER-BUMP-AMT < 25.00
               MOVE 25.00 TO BUTIER-BUMP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BUTIER-BUMP-AMT > 500.00
               MOVE 500.00 TO BUTIER-BUMP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2290-COUNT-RETRY.
           IF BUTIER-RETRY-CNT < 9999999
               ADD 1 TO BUTIER-RETRY-CNT
           END-IF
           IF BUTIER-RETRY-CNT NOT < 10
               MOVE 'Y' TO BUTIER-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2300-FLAG-INTEREST.
           IF BUTIER-INTEREST-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BUTIER-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN CLOSE DATE IS PAST
      * BF -> HF WHEN CLOSE DATE IS PAST
      * HF -> HR WHEN CLOSE DATE IS PAST
      * HR -> UF WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2310-SET-SOURCE.
           EVALUATE BUTIER-SOURCE-CD
               WHEN 'AG'
                   IF BUTIER-CLOSE-DATE > 0 AND BUTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BF' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BUTIER-CLOSE-DATE > 0 AND BUTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HF' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BUTIER-CLOSE-DATE > 0 AND BUTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BUTIER-CLOSE-DATE > 0 AND BUTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UF' TO BUTIER-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2320-ACCRUE-INTEREST.
           IF BUTIER-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BUTIER-INTEREST-AMT * BUTIER-RATE / 100 / 365
               ADD WS-WORK-AMT TO BUTIER-BUMP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2330-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * BUTIER-RATE / 100 / 360
               ADD WS-WORK-AMT TO BUTIER-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2340-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUTIER-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUTIER-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2350-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-TIER-RATE(1) TO BUTIER-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT NOT < WS-PRINCIPA-TIER-LOW(WS-TX)
                   MOVE WS-PRINCIPA-TIER-RATE(WS-TX) TO BUTIER-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUTIER-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUTIER-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BUMP AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2360-LIMIT-BUMP.
           IF BUTIER-BUMP-AMT < 100.00
               MOVE 100.00 TO BUTIER-BUMP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BUTIER-BUMP-AMT > 2500.00
               MOVE 2500.00 TO BUTIER-BUMP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2370-LIMIT-PRINCIPAL.
           IF BUTIER-PRINCIPAL-AMT < 100.00
               MOVE 100.00 TO BUTIER-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BUTIER-PRINCIPAL-AMT > 500.00
               MOVE 500.00 TO BUTIER-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2380-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2390-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE BUTIER-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA TIER TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2400-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-TIER-RATE(1) TO BUTIER-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF BUTIER-INTEREST-AMT NOT < WS-PRINCIPA-TIER-LOW(WS-TX)
                   MOVE WS-PRINCIPA-TIER-RATE(WS-TX) TO BUTIER-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUTIER-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUTIER-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BUMP AMT.
      *----------------------------------------------------------------*
       2410-PRORATE-BUMP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BUTIER-BUMP-AMT ROUNDED =
                   BUTIER-PRINCIPAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BUTIER-BUMP-AMT
           END-IF
           ADD BUTIER-BUMP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2420-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BUTIER-MATURITY-DATE > 0 AND BUTIER-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUTIER-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BUTIER-ADDON-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BUTIER-ADDON-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BUTIER-ADDON-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BUTIER-ADDON-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BUTIER-ADDON-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2430-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BUTIER-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUTIER-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BUTIER-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2440-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BUTIER-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUTIER-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO BUTIER-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BGJ -> LFX WHEN EFFECTIVE DATE IS PAST
      * LFX -> MLA WHEN EFFECTIVE DATE IS PAST
      * MLA -> ULA WHEN EFFECTIVE DATE IS PAST
      * ULA -> BGJ WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2450-SET-REGION.
           EVALUATE BUTIER-REGION-CD
               WHEN 'BGJ'
                   IF BUTIER-EFFECTIVE-DATE > 0 AND
                       BUTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'LFX' TO BUTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LFX'
                   IF BUTIER-EFFECTIVE-DATE > 0 AND
                       BUTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'MLA' TO BUTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MLA'
                   IF BUTIER-EFFECTIVE-DATE > 0 AND
                       BUTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'ULA' TO BUTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULA'
                   IF BUTIER-EFFECTIVE-DATE > 0 AND
                       BUTIER-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'BGJ' TO BUTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE ADDON AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2460-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BUTIER-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BUTIER-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUTIER-EXEMPT-FLAG = 'Y'
                   COMPUTE BUTIER-PRINCIPAL-AMT ROUNDED =
                       BUTIER-ADDON-AMT * BUTIER-MARGIN / 200
               WHEN BUTIER-CYCLE-CD = 'A'
                   MOVE 7.00 TO BUTIER-PRINCIPAL-AMT
               WHEN BUTIER-CYCLE-CD = 'S'
                   MOVE 10.00 TO BUTIER-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE BUTIER-PRINCIPAL-AMT ROUNDED =
                       BUTIER-ADDON-AMT * BUTIER-MARGIN / 100
           END-EVALUATE
           IF BUTIER-PRINCIPAL-AMT > 0 AND BUTIER-PRINCIPAL-AMT < 10.00
               MOVE 10.00 TO BUTIER-PRINCIPAL-AMT
           END-IF
           IF BUTIER-PRINCIPAL-AMT > 25.00
               MOVE 25.00 TO BUTIER-PRINCIPAL-AMT
           END-IF
           ADD BUTIER-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2470-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BUTIER-CLOSE-DATE > 0 AND BUTIER-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUTIER-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BUTIER-BUMP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BUTIER-BUMP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BUTIER-BUMP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BUTIER-BUMP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BUTIER-BUMP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PENALTY YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2480-LIMIT-PENALTY.
           IF BUTIER-PENALTY-YTD < 10.00
               MOVE 10.00 TO BUTIER-PENALTY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BUTIER-PENALTY-YTD > 5000.00
               MOVE 5000.00 TO BUTIER-PENALTY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2490-LIMIT-PRINCIPAL.
           IF BUTIER-PRINCIPAL-YTD < 50.00
               MOVE 50.00 TO BUTIER-PRINCIPAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BUTIER-PRINCIPAL-YTD > 5000.00
               MOVE 5000.00 TO BUTIER-PRINCIPAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2500-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BUTIER-INTEREST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2510-ACCRUE-PRINCIPAL.
           IF BUTIER-PRINCIPAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BUTIER-PRINCIPAL-YTD * BUTIER-RATE / 100 / 360
               ADD WS-WORK-AMT TO BUTIER-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2520-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = BUTIER-PRINCIPAL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BUTIER-PRINCIPAL-AMT - (WS-WORK-AMT *
               3)
           COMPUTE BUTIER-BUMP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2530-COUNT-RETRY.
           IF BUTIER-RETRY-CNT < 9999999
               ADD 1 TO BUTIER-RETRY-CNT
           END-IF
           IF BUTIER-RETRY-CNT NOT < 12
               MOVE 'Y' TO BUTIER-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2540-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUMP AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2550-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = BUTIER-BUMP-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BUTIER-BUMP-AMT - (WS-WORK-AMT * 4)
           COMPUTE BUTIER-ADDON-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E337 CUST ID REQUIRED
      * E241 BUMP AMT NEGATIVE
      * E469 BUMP AMT OVER 10000
      * E290 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2560-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BUTIER-CUST-ID = SPACES
                   MOVE 'E337' TO WS-REJECT-CODE
               WHEN BUTIER-BUMP-AMT < 0
                   MOVE 'E241' TO WS-REJECT-CODE
               WHEN BUTIER-BUMP-AMT > 10000.00
                   MOVE 'E469' TO WS-REJECT-CODE
               WHEN BUTIER-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E290' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2570-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUTIER-CHANNEL-CD
                   ADD BUTIER-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUTIER-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUTIER-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E201 REF NO REQUIRED
      * E937 BUMP AMT NEGATIVE
      * E210 BUMP AMT OVER 50000
      * E135 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2580-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BUTIER-REF-NO = SPACES
                   MOVE 'E201' TO WS-REJECT-CODE
               WHEN BUTIER-BUMP-AMT < 0
                   MOVE 'E937' TO WS-REJECT-CODE
               WHEN BUTIER-BUMP-AMT > 50000.00
                   MOVE 'E210' TO WS-REJECT-CODE
               WHEN BUTIER-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E135' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2590-ACCRUE-PRINCIPAL.
           IF BUTIER-PRINCIPAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BUTIER-PRINCIPAL-YTD * BUTIER-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ADDON AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2600-FLAG-ADDON.
           IF BUTIER-ADDON-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BUTIER-ADDON-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2610-COUNT-EXCEPT.
           IF BUTIER-EXCEPT-CNT < 9999999
               ADD 1 TO BUTIER-EXCEPT-CNT
           END-IF
           IF BUTIER-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO BUTIER-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2620-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BUTIER-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUTIER-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO BUTIER-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ADDON AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2630-LIMIT-ADDON.
           IF BUTIER-ADDON-AMT < 50.00
               MOVE 50.00 TO BUTIER-ADDON-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BUTIER-ADDON-AMT > 50000.00
               MOVE 50000.00 TO BUTIER-ADDON-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADDON AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2640-ACCRUE-ADDON.
           IF BUTIER-ADDON-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BUTIER-ADDON-AMT * BUTIER-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2650-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE BUTIER-BUMP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2660-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUTIER-SOURCE-CD
                   ADD BUTIER-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUTIER-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUTIER-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO BUTIER-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO BUTIER-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO BUTIER-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'BUTIER EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
