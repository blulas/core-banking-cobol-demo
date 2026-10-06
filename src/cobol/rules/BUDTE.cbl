      *================================================================*
      * PROGRAM  : BUDTE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : BUMP-UP CERTIFICATES RULE - LIMITS. CALLED BY THE   *
      *            BUMP-UP CERTIFICATES BATCH AND ONLINE PROGRAMS.     *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-03-11 SKN  ORIGINAL                                       *
      * 1995-02-27 JLM  PENALTY RERUN PROTECTION INC7956               *
      * 1996-05-16 SKN  BUMP STATUS CODES ADDED PRB1873                *
      * 1997-07-10 MKL  BUMP STATUS CODES ADDED CHG1814                *
      * 2002-03-02 TWH  BUMP CALCULATION CORRECTED REQ9029             *
      * 2003-04-23 MKL  PRINCIPAL RERUN PROTECTION CHG2664             *
      * 2003-04-05 JLM  PRINCIPAL FEE SCHEDULE UPDATE INC8337          *
      * 2004-10-27 PQR  PERFORMANCE - ADDON READ AHEAD REQ2472         *
      * 2005-02-10 RJT  AUDIT FINDING - INTEREST CONTROL TOTALS REQ8195*
      * 2005-12-03 EWS  BUMP FEE SCHEDULE UPDATE PRB7057               *
      * 2007-02-07 LMB  BUMP NOTICE TEXT PER LEGAL PRB8250             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUDTE.
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
                                           VALUE 14.000000.
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
      * WS-INTEREST-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000232000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000182500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000434000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000720000'.
       01  WS-INTEREST-SCHED-TBL REDEFINES WS-INTEREST-SCHED-VALUES.
           05  WS-INTEREST-SCHED-ROW OCCURS 4 TIMES.
               10  WS-INTEREST-SCHED-LOW   PIC 9(09)V99.
               10  WS-INTEREST-SCHED-RATE  PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY BUDTEP.

       PROCEDURE DIVISION USING BUDTE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-AGE-LAST-MAINT
           PERFORM 2010-SELECT-PENALTY-PCT
           PERFORM 2020-SET-ELECTION
           PERFORM 2030-ACCUM-SOURCE
           PERFORM 2040-FLAG-PENALTY
           PERFORM 2050-SET-REGION
           PERFORM 2060-ASSESS-REVIEW
           PERFORM 2070-ACCRUE-BUMP
           PERFORM 2080-SPLIT-ADDON
           PERFORM 2090-SET-CYCLE
           PERFORM 2100-EDIT-NAME
           PERFORM 2110-ACCRUE-BUMP
           PERFORM 2120-EDIT-NAME
           PERFORM 2130-SET-CYCLE
           PERFORM 2140-AGE-LAST-MAINT
           PERFORM 2150-LIMIT-INTEREST
           PERFORM 2160-DESCRIBE-ELECTION
           PERFORM 2170-SET-ELECTION
           PERFORM 2180-ACCUM-CYCLE
           PERFORM 2190-SET-ELECTION
           PERFORM 2200-SELECT-PENALTY-PCT
           PERFORM 2210-FLAG-ITEM
           PERFORM 2220-PRORATE-PENALTY
           PERFORM 2230-COUNT-EXCEPT
           PERFORM 2240-ACCUM-CYCLE
           PERFORM 2250-SET-SOURCE
           PERFORM 2260-ASSESS-SUSPEND
           PERFORM 2270-CHECK-EXPIRY
           PERFORM 2280-CHECK-EXPIRY
           PERFORM 2290-CHECK-LAST-PAY
           PERFORM 2300-DESCRIBE-ELECTION
           PERFORM 2310-FLAG-ADDON
           PERFORM 2320-CHECK-LAST-PAY
           PERFORM 2330-CHECK-LAST-PAY
           PERFORM 2340-CHECK-EXPIRY
           PERFORM 2350-FLAG-ITEM
           PERFORM 2360-FLAG-BUMP
           PERFORM 2370-COUNT-CYCLE
           PERFORM 2380-SPLIT-BUMP
           PERFORM 2390-PRORATE-PENALTY
           PERFORM 2400-COUNT-CYCLE
           PERFORM 2410-DESCRIBE-CYCLE
           PERFORM 2420-ACCRUE-PENALTY
           PERFORM 2430-COUNT-CYCLE
           PERFORM 2440-EDIT-NAME
           PERFORM 2450-COUNT-EXCEPT
           PERFORM 2460-SELECT-MARGIN
           PERFORM 2470-CHECK-LAST-STMT
           PERFORM 2480-ACCUM-ELECTION
           PERFORM 2490-SET-ELECTION
           PERFORM 2500-COUNT-EXCEPT
           PERFORM 2510-ACCRUE-ADDON
           PERFORM 2520-SET-CYCLE
           PERFORM 2530-ASSESS-REVIEW
           PERFORM 2540-ACCUM-CYCLE
           PERFORM 2550-ACCUM-ELECTION
           PERFORM 2560-SPLIT-BUMP
           PERFORM 2570-DESCRIBE-REGION
           PERFORM 2580-ACCUM-SOURCE
           PERFORM 2590-SET-ELECTION
           PERFORM 2600-COUNT-EXCEPT
           PERFORM 2610-ACCUM-REGION
           PERFORM 2620-SET-SOURCE
           PERFORM 2630-CHECK-LAST-STMT
           PERFORM 2640-SET-SOURCE
           PERFORM 2650-SPLIT-PRINCIPAL
           PERFORM 2660-ACCUM-SOURCE
           PERFORM 2670-LIMIT-ADDON
           PERFORM 2680-SET-ELECTION
           PERFORM 2690-ACCRUE-MARGIN
           PERFORM 2700-LIMIT-PRINCIPAL
           PERFORM 2710-SELECT-MARGIN
           PERFORM 2720-AGE-LAST-PAY
           PERFORM 2730-DESCRIBE-SOURCE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO BUDTE-RETURN-CODE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2000-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF BUDTE-LAST-MAINT-DATE > 0 AND BUDTE-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUDTE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BUDTE-PENALTY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BUDTE-PENALTY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BUDTE-PENALTY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BUDTE-PENALTY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BUDTE-PENALTY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE PRINCIPAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2010-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO BUDTE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF BUDTE-PRINCIPAL-YTD NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO
                       BUDTE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUDTE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUDTE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN LAST STMT DATE IS PAST
      * M -> N WHEN LAST STMT DATE IS PAST
      * N -> R WHEN LAST STMT DATE IS PAST
      * R -> V WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2020-SET-ELECTION.
           EVALUATE BUDTE-ELECTION-CD
               WHEN 'L'
                   IF BUDTE-LAST-STMT-DATE > 0 AND BUDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'M' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BUDTE-LAST-STMT-DATE > 0 AND BUDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'N' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BUDTE-LAST-STMT-DATE > 0 AND BUDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'R' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BUDTE-LAST-STMT-DATE > 0 AND BUDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'V' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2030-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUDTE-SOURCE-CD
                   ADD BUDTE-ADDON-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUDTE-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUDTE-ADDON-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2040-FLAG-PENALTY.
           IF BUDTE-PENALTY-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BUDTE-PENALTY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BGJ -> LFX WHEN LAST MAINT DATE IS PAST
      * LFX -> MLA WHEN LAST MAINT DATE IS PAST
      * MLA -> ULA WHEN LAST MAINT DATE IS PAST
      * ULA -> BGJ WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2050-SET-REGION.
           EVALUATE BUDTE-REGION-CD
               WHEN 'BGJ'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'LFX' TO BUDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LFX'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'MLA' TO BUDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MLA'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'ULA' TO BUDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULA'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'BGJ' TO BUDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE PRINCIPAL YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2060-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN BUDTE-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO BUDTE-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUDTE-OVERRIDE-FLAG = 'Y'
                   COMPUTE BUDTE-INTEREST-YTD ROUNDED =
                       BUDTE-PRINCIPAL-YTD * BUDTE-MARGIN / 200
               WHEN BUDTE-SOURCE-CD = 'AG'
                   MOVE 10.00 TO BUDTE-INTEREST-YTD
               WHEN BUDTE-SOURCE-CD = 'BF'
                   MOVE 10.00 TO BUDTE-INTEREST-YTD
               WHEN OTHER
                   COMPUTE BUDTE-INTEREST-YTD ROUNDED =
                       BUDTE-PRINCIPAL-YTD * BUDTE-MARGIN / 100
           END-EVALUATE
           IF BUDTE-INTEREST-YTD > 0 AND BUDTE-INTEREST-YTD < 2.00
               MOVE 2.00 TO BUDTE-INTEREST-YTD
           END-IF
           IF BUDTE-INTEREST-YTD > 150.00
               MOVE 150.00 TO BUDTE-INTEREST-YTD
           END-IF
           ADD BUDTE-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUMP AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2070-ACCRUE-BUMP.
           IF BUDTE-BUMP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BUDTE-BUMP-AMT * BUDTE-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BUDTE-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADDON AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2080-SPLIT-ADDON.
           COMPUTE WS-WORK-AMT = BUDTE-ADDON-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BUDTE-ADDON-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> S WHEN EXPIRY DATE IS PAST
      * S -> T WHEN EXPIRY DATE IS PAST
      * T -> U WHEN EXPIRY DATE IS PAST
      * U -> X WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2090-SET-CYCLE.
           EVALUATE BUDTE-CYCLE-CD
               WHEN 'A'
                   IF BUDTE-EXPIRY-DATE > 0 AND BUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO BUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF BUDTE-EXPIRY-DATE > 0 AND BUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO BUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF BUDTE-EXPIRY-DATE > 0 AND BUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO BUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF BUDTE-EXPIRY-DATE > 0 AND BUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO BUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E116 NAME REQUIRED
      * E641 WORK AMT NEGATIVE
      * E942 WORK AMT OVER 1000000
      * E203 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2100-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BUDTE-NAME = SPACES
                   MOVE 'E116' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E641' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E942' TO WS-REJECT-CODE
               WHEN BUDTE-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E203' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUMP YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2110-ACCRUE-BUMP.
           IF BUDTE-BUMP-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BUDTE-BUMP-YTD * BUDTE-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E905 NAME REQUIRED
      * E371 BUMP YTD NEGATIVE
      * E896 BUMP YTD OVER 99999
      * E733 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2120-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BUDTE-NAME = SPACES
                   MOVE 'E905' TO WS-REJECT-CODE
               WHEN BUDTE-BUMP-YTD < 0
                   MOVE 'E371' TO WS-REJECT-CODE
               WHEN BUDTE-BUMP-YTD > 99999.00
                   MOVE 'E896' TO WS-REJECT-CODE
               WHEN BUDTE-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E733' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> S WHEN LAST STMT DATE IS PAST
      * S -> T WHEN LAST STMT DATE IS PAST
      * T -> U WHEN LAST STMT DATE IS PAST
      * U -> X WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2130-SET-CYCLE.
           EVALUATE BUDTE-CYCLE-CD
               WHEN 'A'
                   IF BUDTE-LAST-STMT-DATE > 0 AND BUDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'S' TO BUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF BUDTE-LAST-STMT-DATE > 0 AND BUDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'T' TO BUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF BUDTE-LAST-STMT-DATE > 0 AND BUDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'U' TO BUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF BUDTE-LAST-STMT-DATE > 0 AND BUDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'X' TO BUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2140-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF BUDTE-LAST-MAINT-DATE > 0 AND BUDTE-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUDTE-LAST-MAINT-DATE TO DTU-DATE-1
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
      * INTEREST YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2150-LIMIT-INTEREST.
           IF BUDTE-INTEREST-YTD < 100.00
               MOVE 100.00 TO BUDTE-INTEREST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BUDTE-INTEREST-YTD > 50000.00
               MOVE 50000.00 TO BUDTE-INTEREST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ELECTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2160-DESCRIBE-ELECTION.
           EVALUATE BUDTE-ELECTION-CD
               WHEN 'L'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'N'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'V'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN LAST MAINT DATE IS PAST
      * M -> N WHEN LAST MAINT DATE IS PAST
      * N -> R WHEN LAST MAINT DATE IS PAST
      * R -> V WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2170-SET-ELECTION.
           EVALUATE BUDTE-ELECTION-CD
               WHEN 'L'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'M' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'N' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'R' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'V' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2180-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUDTE-CYCLE-CD
                   ADD BUDTE-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUDTE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUDTE-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN LAST MAINT DATE IS PAST
      * M -> N WHEN LAST MAINT DATE IS PAST
      * N -> R WHEN LAST MAINT DATE IS PAST
      * R -> V WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2190-SET-ELECTION.
           EVALUATE BUDTE-ELECTION-CD
               WHEN 'L'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'M' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'N' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'R' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'V' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2200-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO BUDTE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO
                       BUDTE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUDTE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUDTE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2210-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUMP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PENALTY YTD.
      *----------------------------------------------------------------*
       2220-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BUDTE-PENALTY-YTD ROUNDED =
                   BUDTE-BUMP-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BUDTE-PENALTY-YTD
           END-IF
           ADD BUDTE-PENALTY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2230-COUNT-EXCEPT.
           IF BUDTE-EXCEPT-CNT < 9999999
               ADD 1 TO BUDTE-EXCEPT-CNT
           END-IF
           IF BUDTE-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO BUDTE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2240-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUDTE-CYCLE-CD
                   ADD BUDTE-PENALTY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUDTE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUDTE-PENALTY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN EXPIRY DATE IS PAST
      * BF -> HF WHEN EXPIRY DATE IS PAST
      * HF -> HR WHEN EXPIRY DATE IS PAST
      * HR -> UF WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2250-SET-SOURCE.
           EVALUATE BUDTE-SOURCE-CD
               WHEN 'AG'
                   IF BUDTE-EXPIRY-DATE > 0 AND BUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BF' TO BUDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BUDTE-EXPIRY-DATE > 0 AND BUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HF' TO BUDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BUDTE-EXPIRY-DATE > 0 AND BUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO BUDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BUDTE-EXPIRY-DATE > 0 AND BUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UF' TO BUDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE ADDON AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2260-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BUDTE-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BUDTE-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUDTE-REVIEW-FLAG = 'Y'
                   COMPUTE BUDTE-PRINCIPAL-YTD ROUNDED = BUDTE-ADDON-AMT
                       * BUDTE-MARGIN / 200
               WHEN BUDTE-REGION-CD = 'BGJ'
                   MOVE 5.00 TO BUDTE-PRINCIPAL-YTD
               WHEN BUDTE-REGION-CD = 'LFX'
                   MOVE 10.00 TO BUDTE-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE BUDTE-PRINCIPAL-YTD ROUNDED = BUDTE-ADDON-AMT
                       * BUDTE-MARGIN / 100
           END-EVALUATE
           IF BUDTE-PRINCIPAL-YTD > 0 AND BUDTE-PRINCIPAL-YTD < 10.00
               MOVE 10.00 TO BUDTE-PRINCIPAL-YTD
           END-IF
           IF BUDTE-PRINCIPAL-YTD > 100.00
               MOVE 100.00 TO BUDTE-PRINCIPAL-YTD
           END-IF
           ADD BUDTE-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2270-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BUDTE-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUDTE-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO BUDTE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2280-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BUDTE-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUDTE-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BUDTE-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2290-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF BUDTE-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUDTE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BUDTE-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ELECTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2300-DESCRIBE-ELECTION.
           EVALUATE BUDTE-ELECTION-CD
               WHEN 'L'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ADDON AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2310-FLAG-ADDON.
           IF BUDTE-ADDON-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BUDTE-ADDON-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2320-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF BUDTE-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUDTE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BUDTE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2330-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF BUDTE-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUDTE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO BUDTE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2340-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BUDTE-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUDTE-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO BUDTE-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2350-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * BUMP YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2360-FLAG-BUMP.
           IF BUDTE-BUMP-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BUDTE-BUMP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2370-COUNT-CYCLE.
           IF BUDTE-CYCLE-CNT < 9999999
               ADD 1 TO BUDTE-CYCLE-CNT
           END-IF
           IF BUDTE-CYCLE-CNT NOT < 12
               MOVE 'Y' TO BUDTE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2380-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE BUDTE-BUMP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ADDON AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PENALTY YTD.
      *----------------------------------------------------------------*
       2390-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BUDTE-PENALTY-YTD ROUNDED =
                   BUDTE-ADDON-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BUDTE-PENALTY-YTD
           END-IF
           ADD BUDTE-PENALTY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2400-COUNT-CYCLE.
           IF BUDTE-CYCLE-CNT < 9999999
               ADD 1 TO BUDTE-CYCLE-CNT
           END-IF
           IF BUDTE-CYCLE-CNT NOT < 3
               MOVE 'Y' TO BUDTE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2410-DESCRIBE-CYCLE.
           EVALUATE BUDTE-CYCLE-CD
               WHEN 'A'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'U'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'X'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'Y'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PENALTY YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2420-ACCRUE-PENALTY.
           IF BUDTE-PENALTY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BUDTE-PENALTY-YTD * BUDTE-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BUDTE-BUMP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2430-COUNT-CYCLE.
           IF BUDTE-CYCLE-CNT < 9999999
               ADD 1 TO BUDTE-CYCLE-CNT
           END-IF
           IF BUDTE-CYCLE-CNT NOT < 5
               MOVE 'Y' TO BUDTE-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E916 NAME REQUIRED
      * E869 BUMP AMT NEGATIVE
      * E863 BUMP AMT OVER 25000
      * E654 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2440-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BUDTE-NAME = SPACES
                   MOVE 'E916' TO WS-REJECT-CODE
               WHEN BUDTE-BUMP-AMT < 0
                   MOVE 'E869' TO WS-REJECT-CODE
               WHEN BUDTE-BUMP-AMT > 25000.00
                   MOVE 'E863' TO WS-REJECT-CODE
               WHEN BUDTE-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E654' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2450-COUNT-EXCEPT.
           IF BUDTE-EXCEPT-CNT < 9999999
               ADD 1 TO BUDTE-EXCEPT-CNT
           END-IF
           IF BUDTE-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO BUDTE-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE PENALTY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2460-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO BUDTE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF BUDTE-PENALTY-YTD NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO BUDTE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUDTE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUDTE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2470-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BUDTE-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUDTE-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO BUDTE-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY YTD BY ELECTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2480-ACCUM-ELECTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUDTE-ELECTION-CD
                   ADD BUDTE-PENALTY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUDTE-ELECTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUDTE-PENALTY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN LAST MAINT DATE IS PAST
      * M -> N WHEN LAST MAINT DATE IS PAST
      * N -> R WHEN LAST MAINT DATE IS PAST
      * R -> V WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2490-SET-ELECTION.
           EVALUATE BUDTE-ELECTION-CD
               WHEN 'L'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'M' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'N' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'R' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'V' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2500-COUNT-EXCEPT.
           IF BUDTE-EXCEPT-CNT < 9999999
               ADD 1 TO BUDTE-EXCEPT-CNT
           END-IF
           IF BUDTE-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO BUDTE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADDON AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2510-ACCRUE-ADDON.
           IF BUDTE-ADDON-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BUDTE-ADDON-AMT * BUDTE-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BUDTE-BUMP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> S WHEN LAST STMT DATE IS PAST
      * S -> T WHEN LAST STMT DATE IS PAST
      * T -> U WHEN LAST STMT DATE IS PAST
      * U -> X WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2520-SET-CYCLE.
           EVALUATE BUDTE-CYCLE-CD
               WHEN 'A'
                   IF BUDTE-LAST-STMT-DATE > 0 AND BUDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'S' TO BUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF BUDTE-LAST-STMT-DATE > 0 AND BUDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'T' TO BUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF BUDTE-LAST-STMT-DATE > 0 AND BUDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'U' TO BUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF BUDTE-LAST-STMT-DATE > 0 AND BUDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'X' TO BUDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2530-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN BUDTE-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO BUDTE-ADDON-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BUDTE-OVERRIDE-FLAG = 'Y'
                   COMPUTE BUDTE-ADDON-AMT ROUNDED = WS-WORK-AMT-2 *
                       BUDTE-MARGIN / 200
               WHEN BUDTE-SOURCE-CD = 'AG'
                   MOVE 5.00 TO BUDTE-ADDON-AMT
               WHEN BUDTE-SOURCE-CD = 'BF'
                   MOVE 15.00 TO BUDTE-ADDON-AMT
               WHEN OTHER
                   COMPUTE BUDTE-ADDON-AMT ROUNDED = WS-WORK-AMT-2 *
                       BUDTE-MARGIN / 100
           END-EVALUATE
           IF BUDTE-ADDON-AMT > 0 AND BUDTE-ADDON-AMT < 5.00
               MOVE 5.00 TO BUDTE-ADDON-AMT
           END-IF
           IF BUDTE-ADDON-AMT > 150.00
               MOVE 150.00 TO BUDTE-ADDON-AMT
           END-IF
           ADD BUDTE-ADDON-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE BUMP AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2540-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUDTE-CYCLE-CD
                   ADD BUDTE-BUMP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUDTE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUDTE-BUMP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUMP YTD BY ELECTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2550-ACCUM-ELECTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUDTE-ELECTION-CD
                   ADD BUDTE-BUMP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUDTE-ELECTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUDTE-BUMP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2560-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE BUDTE-BUMP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2570-DESCRIBE-REGION.
           EVALUATE BUDTE-REGION-CD
               WHEN 'BGJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LFX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MLA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ULA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2580-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUDTE-SOURCE-CD
                   ADD BUDTE-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUDTE-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUDTE-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN EXPIRY DATE IS PAST
      * M -> N WHEN EXPIRY DATE IS PAST
      * N -> R WHEN EXPIRY DATE IS PAST
      * R -> V WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2590-SET-ELECTION.
           EVALUATE BUDTE-ELECTION-CD
               WHEN 'L'
                   IF BUDTE-EXPIRY-DATE > 0 AND BUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BUDTE-EXPIRY-DATE > 0 AND BUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BUDTE-EXPIRY-DATE > 0 AND BUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BUDTE-EXPIRY-DATE > 0 AND BUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2600-COUNT-EXCEPT.
           IF BUDTE-EXCEPT-CNT < 9999999
               ADD 1 TO BUDTE-EXCEPT-CNT
           END-IF
           IF BUDTE-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO BUDTE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2610-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUDTE-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUDTE-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN LAST MAINT DATE IS PAST
      * BF -> HF WHEN LAST MAINT DATE IS PAST
      * HF -> HR WHEN LAST MAINT DATE IS PAST
      * HR -> UF WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2620-SET-SOURCE.
           EVALUATE BUDTE-SOURCE-CD
               WHEN 'AG'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'BF' TO BUDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'HF' TO BUDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'HR' TO BUDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BUDTE-LAST-MAINT-DATE > 0 AND
                       BUDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'UF' TO BUDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2630-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BUDTE-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUDTE-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BUDTE-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN LAST STMT DATE IS PAST
      * BF -> HF WHEN LAST STMT DATE IS PAST
      * HF -> HR WHEN LAST STMT DATE IS PAST
      * HR -> UF WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2640-SET-SOURCE.
           EVALUATE BUDTE-SOURCE-CD
               WHEN 'AG'
                   IF BUDTE-LAST-STMT-DATE > 0 AND BUDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BF' TO BUDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BUDTE-LAST-STMT-DATE > 0 AND BUDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HF' TO BUDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BUDTE-LAST-STMT-DATE > 0 AND BUDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HR' TO BUDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BUDTE-LAST-STMT-DATE > 0 AND BUDTE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UF' TO BUDTE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2650-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = BUDTE-PRINCIPAL-YTD / 4
           COMPUTE WS-WORK-AMT-2 = BUDTE-PRINCIPAL-YTD - (WS-WORK-AMT *
               4)
           COMPUTE BUDTE-PENALTY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUMP AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2660-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BUDTE-SOURCE-CD
                   ADD BUDTE-BUMP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BUDTE-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BUDTE-BUMP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ADDON AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2670-LIMIT-ADDON.
           IF BUDTE-ADDON-AMT < 10.00
               MOVE 10.00 TO BUDTE-ADDON-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BUDTE-ADDON-AMT > 5000.00
               MOVE 5000.00 TO BUDTE-ADDON-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN EXPIRY DATE IS PAST
      * M -> N WHEN EXPIRY DATE IS PAST
      * N -> R WHEN EXPIRY DATE IS PAST
      * R -> V WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2680-SET-ELECTION.
           EVALUATE BUDTE-ELECTION-CD
               WHEN 'L'
                   IF BUDTE-EXPIRY-DATE > 0 AND BUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BUDTE-EXPIRY-DATE > 0 AND BUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BUDTE-EXPIRY-DATE > 0 AND BUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BUDTE-EXPIRY-DATE > 0 AND BUDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO BUDTE-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2690-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * BUDTE-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BUDTE-PENALTY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2700-LIMIT-PRINCIPAL.
           IF BUDTE-PRINCIPAL-YTD < 50.00
               MOVE 50.00 TO BUDTE-PRINCIPAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BUDTE-PRINCIPAL-YTD > 1000.00
               MOVE 1000.00 TO BUDTE-PRINCIPAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE ADDON AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2710-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO BUDTE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF BUDTE-ADDON-AMT NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO BUDTE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BUDTE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BUDTE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2720-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF BUDTE-LAST-PAY-DATE > 0 AND BUDTE-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BUDTE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BUDTE-BUMP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BUDTE-BUMP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BUDTE-BUMP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD BUDTE-BUMP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BUDTE-BUMP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2730-DESCRIBE-SOURCE.
           EVALUATE BUDTE-SOURCE-CD
               WHEN 'AG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'YD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO BUDTE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO BUDTE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO BUDTE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'BUDTE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
