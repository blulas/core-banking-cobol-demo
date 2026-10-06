      *================================================================*
      * PROGRAM  : BUVAL                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : BUMP-UP CERTIFICATES RULE - LIMITS. CALLED BY THE   *
      *            BUMP-UP CERTIFICATES BATCH AND ONLINE PROGRAMS.     *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-11-02 JLM  ORIGINAL                                       *
      * 1995-09-23 LMB  AUDIT FINDING - INTEREST CONTROL TOTALS CHG2954*
      * 1995-07-20 JLM  PERFORMANCE - BUMP READ AHEAD PRB2762          *
      * 1995-03-01 RJT  INTEREST TABLE EXPANDED INC5416                *
      * 1996-01-02 TWH  INTEREST EXCEPTIONS TO OPS QUEUE CHG1149       *
      * 1997-09-01 GAF  PERFORMANCE - ADDON READ AHEAD PRB2067         *
      * 1998-12-22 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-03-02 LMB  ADDON RERUN PROTECTION CHG5203                 *
      * 2007-12-27 RJT  INTEREST CUTOFF MOVED CHG0636                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUVAL.
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
                                           VALUE 11.000000.
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
      * WS-PENALTY-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PENALTY-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000620500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000366500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000705500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000559500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004000000000867500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000411500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016500000000032000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021500000000514500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026500000000282000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027000000000129000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027500000000533000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030000000000474500'.
       01  WS-PENALTY-GRID-TBL REDEFINES WS-PENALTY-GRID-VALUES.
           05  WS-PENALTY-GRID-ROW OCCURS 12 TIMES.
               10  WS-PENALTY-GRID-LOW     PIC 9(09)V99.
               10  WS-PENALTY-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PENALTY-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PENALTY-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000370000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000127500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000220000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000290000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006350000000203000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016350000000806000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016850000000429500'.
       01  WS-PENALTY-TIER-TBL REDEFINES WS-PENALTY-TIER-VALUES.
           05  WS-PENALTY-TIER-ROW OCCURS 7 TIMES.
               10  WS-PENALTY-TIER-LOW     PIC 9(09)V99.
               10  WS-PENALTY-TIER-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY BUVALP.

       PROCEDURE DIVISION USING BUVAL-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-AGE-EFFECTIVE
           PERFORM 2010-SPLIT-ADDON
           PERFORM 2020-SET-SOURCE
           PERFORM 2030-FLAG-PRINCIPAL
           PERFORM 2040-EDIT-DESC
           PERFORM 2050-EDIT-DESC
           PERFORM 2060-SET-CHANNEL
           PERFORM 2070-DESCRIBE-STEP-PLAN
           PERFORM 2080-DESCRIBE-REGION
           PERFORM 2090-LIMIT-ITEM
           PERFORM 2100-LIMIT-ADDON
           PERFORM 2110-ACCRUE-MARGIN
           PERFORM 2120-FLAG-ADDON
           PERFORM 2130-SET-STEP-PLAN
           PERFORM 2140-SET-SOURCE
           PERFORM 2150-SPLIT-INTEREST
           PERFORM 2160-SELECT-RATE
           PERFORM 2170-PRORATE-ITEM
           PERFORM 2180-SET-SOURCE
           PERFORM 2190-AGE-LAST-STMT
           PERFORM 2200-SELECT-MARGIN
           PERFORM 2210-ACCUM-CHANNEL
           PERFORM 2220-CHECK-EFFECTIVE
           PERFORM 2230-ACCRUE-ADDON
           PERFORM 2240-LIMIT-ITEM
           PERFORM 2250-ACCUM-CHANNEL
           PERFORM 2260-SELECT-RATE
           PERFORM 2270-CHECK-LAST-STMT
           PERFORM 2280-ACCUM-CHANNEL
           PERFORM 2290-DESCRIBE-STEP-PLAN
           PERFORM 2300-CHECK-LAST-PAY
           PERFORM 2310-SPLIT-ADDON
           PERFORM 2320-ACCUM-CHANNEL
           PERFORM 2330-COUNT-CYCLE
           PERFORM 2340-AGE-EFFECTIVE
           PERFORM 2350-SPLIT-PRINCIPAL
           PERFORM 2360-SELECT-MARGIN
           PERFORM 2370-FLAG-ADDON
           PERFORM 2380-ASSESS-STOP
           PERFORM 2390-AGE-LAST-PAY
           PERFORM 2400-FLAG-PENALTY
           PERFORM 2410-EDIT-DESC
           PERFORM 2420-PRORATE-INTEREST
           PERFORM 2430-PRORATE-PENALTY
           PERFORM 2440-ASSESS-EXEMPT
           PERFORM 2450-ASSESS-OVERRIDE
           PERFORM 2460-ACCRUE-PRINCIPAL
           PERFORM 2470-SET-STEP-PLAN
           PERFORM 2480-SELECT-MARGIN
           PERFORM 2490-CHECK-EFFECTIVE
           PERFORM 2500-ACCRUE-INTEREST
           PERFORM 2510-SPLIT-PRINCIPAL
           PERFORM 2520-CHECK-LAST-STMT
           PERFORM 2530-SPLIT-PENALTY
           PERFORM 2540-ACCUM-REGION
           PERFORM 2550-CHECK-LAST-STMT
           PERFORM 2560-SPLIT-PRINCIPAL
           PERFORM 2570-COUNT-ITEM
           PERFORM 2580-ACCUM-STEP-PLAN
           PERFORM 2590-SELECT-MARGIN
           PERFORM 2600-CHECK-EFFECTIVE
           PERFORM 2610-EDIT-NAME
           PERFORM 2620-SELECT-RATE
           PERFORM 2630-EDIT-DESC
           PERFORM 2640-SELECT-MARGIN
           PERFORM 2650-ACCRUE-RATE
           PERFORM 2660-PRORATE-INTEREST
           PERFORM 2670-LIMIT-PENALTY
           PERFORM 2680-ASSESS-OVERRIDE
           PERFORM 2690-ACCUM-REGION
           PERFORM 2700-SELECT-MARGIN
           PERFORM 2710-EDIT-DESC
           PERFORM 2720-COUNT-ITEM
           PERFORM 2730-SELECT-MARGIN
           PERFORM 2740-CHECK-EFFECTIVE
           PERFORM 2750-ASSESS-OVERRIDE
           PERFORM 2760-ACCUM-STEP-PLAN
           PERFORM 2770-ACCUM-CHANNEL
           PERFORM 2780-COUNT-CYCLE
           PERFORM 2790-ASSESS-STOP
           PERFORM 2800-AGE-EFFECTIVE
           PERFORM 2810-EDIT-DESC
           PERFORM 2820-PRORATE-PRINCIPAL
           PERFORM 2830-LIMIT-PRINCIPAL
           PERFORM 2840-COUNT-ITEM
           PERFORM 2850-PRORATE-PENALTY
           PERFORM 2860-ASSESS-OVERRIDE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO BUVAL-RETURN-CODE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2000-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BUVAL-EFFECTIVE-DATE > 0 AND BUVAL-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUVAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BUVAL-BUMP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BUVAL-BUMP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BUVAL-BUMP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BUVAL-BUMP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BUVAL-BUMP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ADDON YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2010-SPLIT-ADDON.
           COMPUTE WS-WORK-AMT = BUVAL-ADDON-YTD / 4
           COMPUTE WS-WORK-AMT-2 = BUVAL-ADDON-YTD - (WS-WORK-AMT * 4)
           COMPUTE BUVAL-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN LAST PAY DATE IS PAST
      * BF -> HF WHEN LAST PAY DATE IS PAST
      * HF -> HR WHEN LAST PAY DATE IS PAST
      * HR -> UF WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2020-SET-SOURCE.
           EVALUATE BUVAL-SOURCE-CD
               WHEN 'AG'
                   IF BUVAL-LAST-PAY-DATE > 0 AND BUVAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BF' TO BUVAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BUVAL-LAST-PAY-DATE > 0 AND BUVAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HF' TO BUVAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BUVAL-LAST-PAY-DATE > 0 AND BUVAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO BUVAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BUVAL-LAST-PAY-DATE > 0 AND BUVAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UF' TO BUVAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2030-FLAG-PRINCIPAL.
           IF BUVAL-PRINCIPAL-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BUVAL-PRINCIPAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E423 DESC REQUIRED
      * E798 WORK AMT NEGATIVE
      * E232 WORK AMT OVER 50000
      * E662 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2040-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BUVAL-DESC = SPACES
                   MOVE 'E423' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E798' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E232' TO WS-REJECT-CODE
               WHEN BUVAL-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E662' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E485 DESC REQUIRED
      * E604 BUMP AMT NEGATIVE
      * E909 BUMP AMT OVER 10000
      * E581 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2050-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BUVAL-DESC = SPACES
                   MOVE 'E485' TO WS-REJECT-CODE
               WHEN BUVAL-BUMP-AMT < 0
                   MOVE 'E604' TO WS-REJECT-CODE
               WHEN BUVAL-BUMP-AMT > 10000.00
                   MOVE 'E909' TO WS-REJECT-CODE
               WHEN BUVAL-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E581' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * EK -> JE WHEN EFFECTIVE DATE IS PAST
      * JE -> PN WHEN EFFECTIVE DATE IS PAST
      * PN -> WV WHEN EFFECTIVE DATE IS PAST
      * WV -> EK WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2060-SET-CHANNEL.
           EVALUATE BUVAL-CHANNEL-CD
               WHEN 'EK'
                   IF BUVAL-EFFECTIVE-DATE > 0 AND BUVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JE' TO BUVAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF BUVAL-EFFECTIVE-DATE > 0 AND BUVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PN' TO BUVAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PN'
                   IF BUVAL-EFFECTIVE-DATE > 0 AND BUVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WV' TO BUVAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF BUVAL-EFFECTIVE-DATE > 0 AND BUVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EK' TO BUVAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STEP PLAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2070-DESCRIBE-STEP-PLAN.
           EVALUATE BUVAL-STEP-PLAN-CD
               WHEN 'AYX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DGU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GWJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RJL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UPM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VCX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YPY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2080-DESCRIBE-REGION.
           EVALUATE BUVAL-REGION-CD
               WHEN 'BGJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LFX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MLA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ULA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2090-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ADDON YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2100-LIMIT-ADDON.
           IF BUVAL-ADDON-YTD < 25.00
               MOVE 25.00 TO BUVAL-ADDON-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BUVAL-ADDON-YTD > 1000.00
               MOVE 1000.00 TO BUVAL-ADDON-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2110-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * BUVAL-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BUVAL-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ADDON YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2120-FLAG-ADDON.
           IF BUVAL-ADDON-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BUVAL-ADDON-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STEP PLAN CD:
      * AYX -> DGU WHEN LAST STMT DATE IS PAST
      * DGU -> GWJ WHEN LAST STMT DATE IS PAST
      * GWJ -> RJL WHEN LAST STMT DATE IS PAST
      * RJL -> UPM WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2130-SET-STEP-PLAN.
           EVALUATE BUVAL-STEP-PLAN-CD
               WHEN 'AYX'
                   IF BUVAL-LAST-STMT-DATE > 0 AND BUVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DGU' TO BUVAL-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DGU'
                   IF BUVAL-LAST-STMT-DATE > 0 AND BUVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GWJ' TO BUVAL-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GWJ'
                   IF BUVAL-LAST-STMT-DATE > 0 AND BUVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RJL' TO BUVAL-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJL'
                   IF BUVAL-LAST-STMT-DATE > 0 AND BUVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UPM' TO BUVAL-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN LAST MAINT DATE IS PAST
      * BF -> HF WHEN LAST MAINT DATE IS PAST
      * HF -> HR WHEN LAST MAINT DATE IS PAST
      * HR -> UF WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2140-SET-SOURCE.
           EVALUATE BUVAL-SOURCE-CD
               WHEN 'AG'
                   IF BUVAL-LAST-MAINT-DATE > 0 AND
                       BUVAL-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'BF' TO BUVAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BUVAL-LAST-MAINT-DATE > 0 AND
                       BUVAL-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'HF' TO BUVAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BUVAL-LAST-MAINT-DATE > 0 AND
                       BUVAL-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'HR' TO BUVAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BUVAL-LAST-MAINT-DATE > 0 AND
                       BUVAL-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'UF' TO BUVAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2150-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BUVAL-INTEREST-YTD / 3
           COMPUTE WS-WORK-AMT-2 = BUVAL-INTEREST-YTD - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE ADDON YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2160-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO BUVAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BUVAL-ADDON-YTD NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO BUVAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUVAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUVAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2170-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN EFFECTIVE DATE IS PAST
      * BF -> HF WHEN EFFECTIVE DATE IS PAST
      * HF -> HR WHEN EFFECTIVE DATE IS PAST
      * HR -> UF WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2180-SET-SOURCE.
           EVALUATE BUVAL-SOURCE-CD
               WHEN 'AG'
                   IF BUVAL-EFFECTIVE-DATE > 0 AND BUVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BF' TO BUVAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BUVAL-EFFECTIVE-DATE > 0 AND BUVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HF' TO BUVAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BUVAL-EFFECTIVE-DATE > 0 AND BUVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HR' TO BUVAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BUVAL-EFFECTIVE-DATE > 0 AND BUVAL-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UF' TO BUVAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2190-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BUVAL-LAST-STMT-DATE > 0 AND BUVAL-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUVAL-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2200-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO BUVAL-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BUVAL-INTEREST-YTD NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO BUVAL-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUVAL-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUVAL-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2210-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUVAL-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUVAL-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2220-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BUVAL-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUVAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO BUVAL-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADDON YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2230-ACCRUE-ADDON.
           IF BUVAL-ADDON-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BUVAL-ADDON-YTD * BUVAL-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BUVAL-PRINCIPAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2240-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2250-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUVAL-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUVAL-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE PENALTY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2260-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO BUVAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BUVAL-PENALTY-AMT NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO BUVAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUVAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUVAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2270-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BUVAL-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUVAL-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO BUVAL-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2280-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUVAL-CHANNEL-CD
                   ADD BUVAL-ADDON-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUVAL-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUVAL-ADDON-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STEP PLAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2290-DESCRIBE-STEP-PLAN.
           EVALUATE BUVAL-STEP-PLAN-CD
               WHEN 'AYX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DGU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GWJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RJL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UPM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VCX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YPY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2300-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF BUVAL-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUVAL-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO BUVAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADDON YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2310-SPLIT-ADDON.
           COMPUTE WS-WORK-AMT = BUVAL-ADDON-YTD / 3
           COMPUTE WS-WORK-AMT-2 = BUVAL-ADDON-YTD - (WS-WORK-AMT * 3)
           COMPUTE BUVAL-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2320-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUVAL-CHANNEL-CD
                   ADD BUVAL-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUVAL-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUVAL-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2330-COUNT-CYCLE.
           IF BUVAL-CYCLE-CNT < 9999999
               ADD 1 TO BUVAL-CYCLE-CNT
           END-IF
           IF BUVAL-CYCLE-CNT NOT < 5
               MOVE 'Y' TO BUVAL-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2340-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BUVAL-EFFECTIVE-DATE > 0 AND BUVAL-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUVAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BUVAL-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BUVAL-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD BUVAL-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BUVAL-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BUVAL-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2350-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = BUVAL-PRINCIPAL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BUVAL-PRINCIPAL-AMT - (WS-WORK-AMT *
               2)
           COMPUTE BUVAL-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY GRID TIER FOR THE ADDON YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2360-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-GRID-RATE(1) TO BUVAL-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BUVAL-ADDON-YTD NOT < WS-PENALTY-GRID-LOW(WS-TX)
                   MOVE WS-PENALTY-GRID-RATE(WS-TX) TO BUVAL-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUVAL-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUVAL-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ADDON YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2370-FLAG-ADDON.
           IF BUVAL-ADDON-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BUVAL-ADDON-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE INTEREST YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2380-ASSESS-STOP.
           EVALUATE TRUE
               WHEN BUVAL-STOP-FLAG = 'Y'
                   MOVE ZERO TO BUVAL-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUVAL-OVERRIDE-FLAG = 'Y'
                   COMPUTE BUVAL-PRINCIPAL-YTD ROUNDED =
                       BUVAL-INTEREST-YTD * BUVAL-RATE / 200
               WHEN BUVAL-REGION-CD = 'BGJ'
                   MOVE 20.00 TO BUVAL-PRINCIPAL-YTD
               WHEN BUVAL-REGION-CD = 'LFX'
                   MOVE 5.00 TO BUVAL-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE BUVAL-PRINCIPAL-YTD ROUNDED =
                       BUVAL-INTEREST-YTD * BUVAL-RATE / 100
           END-EVALUATE
           IF BUVAL-PRINCIPAL-YTD > 0 AND BUVAL-PRINCIPAL-YTD < 5.00
               MOVE 5.00 TO BUVAL-PRINCIPAL-YTD
           END-IF
           IF BUVAL-PRINCIPAL-YTD > 100.00
               MOVE 100.00 TO BUVAL-PRINCIPAL-YTD
           END-IF
           ADD BUVAL-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2390-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF BUVAL-LAST-PAY-DATE > 0 AND BUVAL-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUVAL-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BUVAL-PRINCIPAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BUVAL-PRINCIPAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BUVAL-PRINCIPAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BUVAL-PRINCIPAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BUVAL-PRINCIPAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PENALTY AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2400-FLAG-PENALTY.
           IF BUVAL-PENALTY-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BUVAL-PENALTY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E740 DESC REQUIRED
      * E334 INTEREST YTD NEGATIVE
      * E008 INTEREST YTD OVER 50000
      * E707 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2410-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BUVAL-DESC = SPACES
                   MOVE 'E740' TO WS-REJECT-CODE
               WHEN BUVAL-INTEREST-YTD < 0
                   MOVE 'E334' TO WS-REJECT-CODE
               WHEN BUVAL-INTEREST-YTD > 50000.00
                   MOVE 'E008' TO WS-REJECT-CODE
               WHEN BUVAL-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E707' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       2420-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BUVAL-INTEREST-YTD ROUNDED =
                   BUVAL-PRINCIPAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BUVAL-INTEREST-YTD
           END-IF
           ADD BUVAL-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PENALTY AMT.
      *----------------------------------------------------------------*
       2430-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BUVAL-PENALTY-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BUVAL-PENALTY-AMT
           END-IF
           ADD BUVAL-PENALTY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE BUMP AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2440-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BUVAL-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BUVAL-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUVAL-STOP-FLAG = 'Y'
                   COMPUTE BUVAL-INTEREST-YTD ROUNDED = BUVAL-BUMP-AMT *
                       BUVAL-RATE / 200
               WHEN BUVAL-CHANNEL-CD = 'EK'
                   MOVE 15.00 TO BUVAL-INTEREST-YTD
               WHEN BUVAL-CHANNEL-CD = 'JE'
                   MOVE 15.00 TO BUVAL-INTEREST-YTD
               WHEN OTHER
                   COMPUTE BUVAL-INTEREST-YTD ROUNDED = BUVAL-BUMP-AMT *
                       BUVAL-RATE / 100
           END-EVALUATE
           IF BUVAL-INTEREST-YTD > 0 AND BUVAL-INTEREST-YTD < 1.00
               MOVE 1.00 TO BUVAL-INTEREST-YTD
           END-IF
           IF BUVAL-INTEREST-YTD > 250.00
               MOVE 250.00 TO BUVAL-INTEREST-YTD
           END-IF
           ADD BUVAL-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE PENALTY AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2450-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BUVAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BUVAL-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUVAL-EXEMPT-FLAG = 'Y'
                   COMPUTE BUVAL-PRINCIPAL-YTD ROUNDED =
                       BUVAL-PENALTY-AMT * BUVAL-MARGIN / 200
               WHEN BUVAL-CHANNEL-CD = 'EK'
                   MOVE 3.00 TO BUVAL-PRINCIPAL-YTD
               WHEN BUVAL-CHANNEL-CD = 'JE'
                   MOVE 12.00 TO BUVAL-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE BUVAL-PRINCIPAL-YTD ROUNDED =
                       BUVAL-PENALTY-AMT * BUVAL-MARGIN / 100
           END-EVALUATE
           IF BUVAL-PRINCIPAL-YTD > 0 AND BUVAL-PRINCIPAL-YTD < 5.00
               MOVE 5.00 TO BUVAL-PRINCIPAL-YTD
           END-IF
           IF BUVAL-PRINCIPAL-YTD > 25.00
               MOVE 25.00 TO BUVAL-PRINCIPAL-YTD
           END-IF
           ADD BUVAL-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2460-ACCRUE-PRINCIPAL.
           IF BUVAL-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BUVAL-PRINCIPAL-AMT * BUVAL-RATE / 100 / 365
               ADD WS-WORK-AMT TO BUVAL-ADDON-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STEP PLAN CD:
      * AYX -> DGU WHEN LAST STMT DATE IS PAST
      * DGU -> GWJ WHEN LAST STMT DATE IS PAST
      * GWJ -> RJL WHEN LAST STMT DATE IS PAST
      * RJL -> UPM WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2470-SET-STEP-PLAN.
           EVALUATE BUVAL-STEP-PLAN-CD
               WHEN 'AYX'
                   IF BUVAL-LAST-STMT-DATE > 0 AND BUVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DGU' TO BUVAL-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DGU'
                   IF BUVAL-LAST-STMT-DATE > 0 AND BUVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GWJ' TO BUVAL-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GWJ'
                   IF BUVAL-LAST-STMT-DATE > 0 AND BUVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RJL' TO BUVAL-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJL'
                   IF BUVAL-LAST-STMT-DATE > 0 AND BUVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UPM' TO BUVAL-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE BUMP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2480-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO BUVAL-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BUVAL-BUMP-AMT NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO BUVAL-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUVAL-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUVAL-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2490-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BUVAL-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUVAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BUVAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2500-ACCRUE-INTEREST.
           IF BUVAL-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BUVAL-INTEREST-YTD * BUVAL-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BUVAL-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2510-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = BUVAL-PRINCIPAL-YTD / 4
           COMPUTE WS-WORK-AMT-2 = BUVAL-PRINCIPAL-YTD - (WS-WORK-AMT *
               4)
           COMPUTE BUVAL-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2520-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BUVAL-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUVAL-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO BUVAL-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2530-SPLIT-PENALTY.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE BUVAL-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2540-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUVAL-REGION-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUVAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2550-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BUVAL-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUVAL-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO BUVAL-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2560-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE BUVAL-PRINCIPAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2570-COUNT-ITEM.
           IF BUVAL-ITEM-CNT < 9999999
               ADD 1 TO BUVAL-ITEM-CNT
           END-IF
           IF BUVAL-ITEM-CNT NOT < 10
               MOVE 'Y' TO BUVAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY STEP PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2580-ACCUM-STEP-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUVAL-STEP-PLAN-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUVAL-STEP-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2590-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-GRID-RATE(1) TO BUVAL-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-PENALTY-GRID-LOW(WS-TX)
                   MOVE WS-PENALTY-GRID-RATE(WS-TX) TO BUVAL-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUVAL-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUVAL-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2600-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BUVAL-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUVAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BUVAL-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E137 NAME REQUIRED
      * E935 PRINCIPAL YTD NEGATIVE
      * E081 PRINCIPAL YTD OVER 5000
      * E303 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2610-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BUVAL-NAME = SPACES
                   MOVE 'E137' TO WS-REJECT-CODE
               WHEN BUVAL-PRINCIPAL-YTD < 0
                   MOVE 'E935' TO WS-REJECT-CODE
               WHEN BUVAL-PRINCIPAL-YTD > 5000.00
                   MOVE 'E081' TO WS-REJECT-CODE
               WHEN BUVAL-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E303' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE ADDON YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2620-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO BUVAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BUVAL-ADDON-YTD NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO BUVAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUVAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUVAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E801 DESC REQUIRED
      * E352 PENALTY AMT NEGATIVE
      * E603 PENALTY AMT OVER 25000
      * E266 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2630-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BUVAL-DESC = SPACES
                   MOVE 'E801' TO WS-REJECT-CODE
               WHEN BUVAL-PENALTY-AMT < 0
                   MOVE 'E352' TO WS-REJECT-CODE
               WHEN BUVAL-PENALTY-AMT > 25000.00
                   MOVE 'E603' TO WS-REJECT-CODE
               WHEN BUVAL-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E266' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY GRID TIER FOR THE BUMP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2640-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-GRID-RATE(1) TO BUVAL-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BUVAL-BUMP-AMT NOT < WS-PENALTY-GRID-LOW(WS-TX)
                   MOVE WS-PENALTY-GRID-RATE(WS-TX) TO BUVAL-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUVAL-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUVAL-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2650-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * BUVAL-RATE / 100 / 360
               ADD WS-WORK-AMT TO BUVAL-PRINCIPAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       2660-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BUVAL-INTEREST-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BUVAL-INTEREST-YTD
           END-IF
           ADD BUVAL-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PENALTY AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2670-LIMIT-PENALTY.
           IF BUVAL-PENALTY-AMT < 50.00
               MOVE 50.00 TO BUVAL-PENALTY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BUVAL-PENALTY-AMT > 5000.00
               MOVE 5000.00 TO BUVAL-PENALTY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2680-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BUVAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUVAL-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BUVAL-PRINCIPAL-AMT *
                       BUVAL-RATE / 200
               WHEN BUVAL-STEP-PLAN-CD = 'AYX'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN BUVAL-STEP-PLAN-CD = 'DGU'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BUVAL-PRINCIPAL-AMT *
                       BUVAL-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2690-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUVAL-REGION-CD
                   ADD BUVAL-PENALTY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUVAL-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUVAL-PENALTY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2700-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO BUVAL-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BUVAL-INTEREST-YTD NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO BUVAL-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUVAL-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUVAL-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E679 DESC REQUIRED
      * E993 WORK AMT 2 NEGATIVE
      * E226 WORK AMT 2 OVER 1000000
      * E594 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2710-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BUVAL-DESC = SPACES
                   MOVE 'E679' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E993' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E226' TO WS-REJECT-CODE
               WHEN BUVAL-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E594' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2720-COUNT-ITEM.
           IF BUVAL-ITEM-CNT < 9999999
               ADD 1 TO BUVAL-ITEM-CNT
           END-IF
           IF BUVAL-ITEM-CNT NOT < 6
               MOVE 'Y' TO BUVAL-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY GRID TIER FOR THE BUMP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2730-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-GRID-RATE(1) TO BUVAL-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BUVAL-BUMP-AMT NOT < WS-PENALTY-GRID-LOW(WS-TX)
                   MOVE WS-PENALTY-GRID-RATE(WS-TX) TO BUVAL-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUVAL-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUVAL-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2740-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BUVAL-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUVAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO BUVAL-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BUMP FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2750-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BUVAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BUVAL-BUMP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUVAL-STOP-FLAG = 'Y'
                   COMPUTE BUVAL-BUMP-AMT ROUNDED = WS-WORK-AMT-2 *
                       BUVAL-MARGIN / 200
               WHEN BUVAL-SOURCE-CD = 'AG'
                   MOVE 5.00 TO BUVAL-BUMP-AMT
               WHEN BUVAL-SOURCE-CD = 'BF'
                   MOVE 12.00 TO BUVAL-BUMP-AMT
               WHEN OTHER
                   COMPUTE BUVAL-BUMP-AMT ROUNDED = WS-WORK-AMT-2 *
                       BUVAL-MARGIN / 100
           END-EVALUATE
           IF BUVAL-BUMP-AMT > 0 AND BUVAL-BUMP-AMT < 10.00
               MOVE 10.00 TO BUVAL-BUMP-AMT
           END-IF
           IF BUVAL-BUMP-AMT > 35.00
               MOVE 35.00 TO BUVAL-BUMP-AMT
           END-IF
           ADD BUVAL-BUMP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY AMT BY STEP PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2760-ACCUM-STEP-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUVAL-STEP-PLAN-CD
                   ADD BUVAL-PENALTY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUVAL-STEP-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUVAL-PENALTY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2770-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUVAL-CHANNEL-CD
                   ADD BUVAL-ADDON-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUVAL-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUVAL-ADDON-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2780-COUNT-CYCLE.
           IF BUVAL-CYCLE-CNT < 9999999
               ADD 1 TO BUVAL-CYCLE-CNT
           END-IF
           IF BUVAL-CYCLE-CNT NOT < 6
               MOVE 'Y' TO BUVAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BUMP FEE ON THE ADDON YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2790-ASSESS-STOP.
           EVALUATE TRUE
               WHEN BUVAL-STOP-FLAG = 'Y'
                   MOVE ZERO TO BUVAL-BUMP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUVAL-EXEMPT-FLAG = 'Y'
                   COMPUTE BUVAL-BUMP-AMT ROUNDED = BUVAL-ADDON-YTD *
                       BUVAL-RATE / 200
               WHEN BUVAL-REGION-CD = 'BGJ'
                   MOVE 10.00 TO BUVAL-BUMP-AMT
               WHEN BUVAL-REGION-CD = 'LFX'
                   MOVE 3.00 TO BUVAL-BUMP-AMT
               WHEN OTHER
                   COMPUTE BUVAL-BUMP-AMT ROUNDED = BUVAL-ADDON-YTD *
                       BUVAL-RATE / 100
           END-EVALUATE
           IF BUVAL-BUMP-AMT > 0 AND BUVAL-BUMP-AMT < 10.00
               MOVE 10.00 TO BUVAL-BUMP-AMT
           END-IF
           IF BUVAL-BUMP-AMT > 50.00
               MOVE 50.00 TO BUVAL-BUMP-AMT
           END-IF
           ADD BUVAL-BUMP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2800-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BUVAL-EFFECTIVE-DATE > 0 AND BUVAL-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUVAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BUVAL-PENALTY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BUVAL-PENALTY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BUVAL-PENALTY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BUVAL-PENALTY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BUVAL-PENALTY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E374 DESC REQUIRED
      * E481 PRINCIPAL AMT NEGATIVE
      * E152 PRINCIPAL AMT OVER 1000000
      * E967 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2810-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BUVAL-DESC = SPACES
                   MOVE 'E374' TO WS-REJECT-CODE
               WHEN BUVAL-PRINCIPAL-AMT < 0
                   MOVE 'E481' TO WS-REJECT-CODE
               WHEN BUVAL-PRINCIPAL-AMT > 1000000.00
                   MOVE 'E152' TO WS-REJECT-CODE
               WHEN BUVAL-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E967' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINCIPAL YTD.
      *----------------------------------------------------------------*
       2820-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BUVAL-PRINCIPAL-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BUVAL-PRINCIPAL-YTD
           END-IF
           ADD BUVAL-PRINCIPAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2830-LIMIT-PRINCIPAL.
           IF BUVAL-PRINCIPAL-YTD < 10.00
               MOVE 10.00 TO BUVAL-PRINCIPAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BUVAL-PRINCIPAL-YTD > 5000.00
               MOVE 5000.00 TO BUVAL-PRINCIPAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2840-COUNT-ITEM.
           IF BUVAL-ITEM-CNT < 9999999
               ADD 1 TO BUVAL-ITEM-CNT
           END-IF
           IF BUVAL-ITEM-CNT NOT < 12
               MOVE 'Y' TO BUVAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PENALTY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2850-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   BUVAL-PENALTY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE PENALTY AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2860-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BUVAL-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BUVAL-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUVAL-STOP-FLAG = 'Y'
                   COMPUTE BUVAL-INTEREST-YTD ROUNDED =
                       BUVAL-PENALTY-AMT * BUVAL-MARGIN / 200
               WHEN BUVAL-STEP-PLAN-CD = 'AYX'
                   MOVE 15.00 TO BUVAL-INTEREST-YTD
               WHEN BUVAL-STEP-PLAN-CD = 'DGU'
                   MOVE 10.00 TO BUVAL-INTEREST-YTD
               WHEN OTHER
                   COMPUTE BUVAL-INTEREST-YTD ROUNDED =
                       BUVAL-PENALTY-AMT * BUVAL-MARGIN / 100
           END-EVALUATE
           IF BUVAL-INTEREST-YTD > 0 AND BUVAL-INTEREST-YTD < 1.00
               MOVE 1.00 TO BUVAL-INTEREST-YTD
           END-IF
           IF BUVAL-INTEREST-YTD > 100.00
               MOVE 100.00 TO BUVAL-INTEREST-YTD
           END-IF
           ADD BUVAL-INTEREST-YTD TO WS-TOTAL-FEES.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO BUVAL-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO BUVAL-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO BUVAL-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'BUVAL EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
