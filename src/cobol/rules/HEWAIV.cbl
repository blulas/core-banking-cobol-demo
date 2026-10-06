      *================================================================*
      * PROGRAM  : HEWAIV                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : HOME EQUITY LINES RULE - LIMITS. CALLED BY THE HOME *
      *            EQUITY LINES BATCH AND ONLINE PROGRAMS.             *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-04-02 SKN  ORIGINAL                                       *
      * 1998-06-19 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-09-01 GAF  ANNUAL-FEE NOTICE TEXT PER LEGAL PRB5468       *
      * 2001-08-21 CRP  NEW BALANCE REPORT COLUMNS INC1332             *
      * 2001-03-19 RJT  ANNUAL-FEE NOTICE TEXT PER LEGAL INC2149       *
      * 2005-01-16 RJT  INTEREST NOTICE TEXT PER LEGAL REQ7848         *
      * 2005-10-15 CRP  INTEREST STATUS CODES ADDED REQ4669            *
      * 2006-02-17 DPS  MIN-PAY NOTICE TEXT PER LEGAL CHG5130          *
      * 2007-11-16 EWS  LIMIT STATUS CODES ADDED INC7846               *
      * 2008-02-27 BHS  MIN-PAY ROUNDING - HALF UP PRB4381             *
      * 2008-08-24 CRP  DRAW LIMIT REVISED INC1631                     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HEWAIV.
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
                                           VALUE 23.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 30.
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
      * WS-ANNUAL-F-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ANNUAL-F-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000796500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000563000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000516000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000380000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008100000000745000'.
       01  WS-ANNUAL-F-TIER-TBL REDEFINES WS-ANNUAL-F-TIER-VALUES.
           05  WS-ANNUAL-F-TIER-ROW OCCURS 5 TIMES.
               10  WS-ANNUAL-F-TIER-LOW    PIC 9(09)V99.
               10  WS-ANNUAL-F-TIER-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY HEWAIVP.

       PROCEDURE DIVISION USING HEWAIV-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCUM-DRAW-PERIOD
           PERFORM 2010-SELECT-MARGIN
           PERFORM 2020-FLAG-ANNUAL-FEE
           PERFORM 2030-AGE-LAST-MAINT
           PERFORM 2040-SELECT-PENALTY-PCT
           PERFORM 2050-ACCUM-REASON
           PERFORM 2060-LIMIT-LIMIT
           PERFORM 2070-ASSESS-PREFERRED
           PERFORM 2080-DESCRIBE-DRAW-PERIOD
           PERFORM 2090-EDIT-DESC
           PERFORM 2100-LIMIT-INTEREST
           PERFORM 2110-ASSESS-HOLD
           PERFORM 2120-EDIT-CUST-ID
           PERFORM 2130-ASSESS-PREFERRED
           PERFORM 2140-LIMIT-INTEREST
           PERFORM 2150-CHECK-MATURITY
           PERFORM 2160-COUNT-NOTICE
           PERFORM 2170-ASSESS-PREFERRED
           PERFORM 2180-ACCRUE-ANNUAL-FEE
           PERFORM 2190-ASSESS-SUSPEND
           PERFORM 2200-CHECK-MATURITY
           PERFORM 2210-FLAG-DRAW
           PERFORM 2220-FLAG-ANNUAL-FEE
           PERFORM 2230-ACCRUE-ANNUAL-FEE
           PERFORM 2240-FLAG-ANNUAL-FEE
           PERFORM 2250-ACCRUE-ANNUAL-FEE
           PERFORM 2260-COUNT-EXCEPT
           PERFORM 2270-ASSESS-SUSPEND
           PERFORM 2280-SELECT-PENALTY-PCT
           PERFORM 2290-ACCRUE-BALANCE
           PERFORM 2300-CHECK-LAST-PAY
           PERFORM 2310-LIMIT-LIMIT
           PERFORM 2320-AGE-EFFECTIVE
           PERFORM 2330-ACCUM-DRAW-PERIOD
           PERFORM 2340-SELECT-PENALTY-PCT
           PERFORM 2350-SPLIT-ITEM
           PERFORM 2360-ACCUM-DRAW-PERIOD
           PERFORM 2370-LIMIT-LIMIT
           PERFORM 2380-FLAG-LIMIT
           PERFORM 2390-ACCRUE-LIMIT
           PERFORM 2400-PRORATE-ANNUAL-FEE
           PERFORM 2410-SET-REASON
           PERFORM 2420-LIMIT-LIMIT
           PERFORM 2430-LIMIT-LIMIT
           PERFORM 2440-FLAG-ITEM
           PERFORM 2450-CHECK-MATURITY
           PERFORM 2460-FLAG-INTEREST
           PERFORM 2470-ASSESS-SUSPEND
           PERFORM 2480-SELECT-PENALTY-PCT
           PERFORM 2490-ACCUM-DRAW-PERIOD
           PERFORM 2500-CHECK-EFFECTIVE
           PERFORM 2510-CHECK-LAST-PAY
           PERFORM 2520-PRORATE-LIMIT
           PERFORM 2530-SELECT-PENALTY-PCT
           PERFORM 2540-AGE-LAST-MAINT
           PERFORM 2550-CHECK-LAST-PAY
           PERFORM 2560-SELECT-PENALTY-PCT
           PERFORM 2570-CHECK-LAST-PAY
           PERFORM 2580-ASSESS-PREFERRED
           PERFORM 2590-LIMIT-DRAW
           PERFORM 2600-ACCUM-REASON
           PERFORM 2610-SELECT-MARGIN
           PERFORM 2620-SELECT-MARGIN
           PERFORM 2630-ACCUM-REGION
           PERFORM 2640-FLAG-DRAW
           PERFORM 2650-CHECK-LAST-PAY
           PERFORM 2660-SET-REASON
           PERFORM 2670-EDIT-DESC
           PERFORM 2680-FLAG-ITEM
           PERFORM 2690-DESCRIBE-DRAW-PERIOD
           PERFORM 2700-FLAG-ITEM
           PERFORM 2710-ACCUM-LIEN-POS
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO HEWAIV-RETURN-CODE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY DRAW PERIOD CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2000-ACCUM-DRAW-PERIOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HEWAIV-DRAW-PERIOD-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HEWAIV-DRAW-PERIOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ANNUAL F TIER TIER FOR THE LIMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2010-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ANNUAL-F-TIER-RATE(1) TO HEWAIV-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF HEWAIV-LIMIT-AMT NOT < WS-ANNUAL-F-TIER-LOW(WS-TX)
                   MOVE WS-ANNUAL-F-TIER-RATE(WS-TX) TO HEWAIV-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HEWAIV-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HEWAIV-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2020-FLAG-ANNUAL-FEE.
           IF HEWAIV-ANNUAL-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HEWAIV-ANNUAL-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2030-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF HEWAIV-LAST-MAINT-DATE > 0 AND HEWAIV-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEWAIV-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HEWAIV-BALANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HEWAIV-BALANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD HEWAIV-BALANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD HEWAIV-BALANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HEWAIV-BALANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ANNUAL F TIER TIER FOR THE LIMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2040-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ANNUAL-F-TIER-RATE(1) TO HEWAIV-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF HEWAIV-LIMIT-YTD NOT < WS-ANNUAL-F-TIER-LOW(WS-TX)
                   MOVE WS-ANNUAL-F-TIER-RATE(WS-TX) TO
                       HEWAIV-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HEWAIV-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HEWAIV-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2050-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HEWAIV-REASON-CD
                   ADD HEWAIV-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HEWAIV-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HEWAIV-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2060-LIMIT-LIMIT.
           IF HEWAIV-LIMIT-AMT < 25.00
               MOVE 25.00 TO HEWAIV-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HEWAIV-LIMIT-AMT > 1000.00
               MOVE 1000.00 TO HEWAIV-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ANNUAL FEE FEE ON THE BALANCE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2070-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HEWAIV-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HEWAIV-ANNUAL-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HEWAIV-HOLD-FLAG = 'Y'
                   COMPUTE HEWAIV-ANNUAL-FEE-AMT ROUNDED =
                       HEWAIV-BALANCE-AMT * HEWAIV-MARGIN / 200
               WHEN HEWAIV-REASON-CD = 'KST'
                   MOVE 7.00 TO HEWAIV-ANNUAL-FEE-AMT
               WHEN HEWAIV-REASON-CD = 'NWS'
                   MOVE 3.00 TO HEWAIV-ANNUAL-FEE-AMT
               WHEN OTHER
                   COMPUTE HEWAIV-ANNUAL-FEE-AMT ROUNDED =
                       HEWAIV-BALANCE-AMT * HEWAIV-MARGIN / 100
           END-EVALUATE
           IF HEWAIV-ANNUAL-FEE-AMT > 0 AND HEWAIV-ANNUAL-FEE-AMT < 2.00
               MOVE 2.00 TO HEWAIV-ANNUAL-FEE-AMT
           END-IF
           IF HEWAIV-ANNUAL-FEE-AMT > 25.00
               MOVE 25.00 TO HEWAIV-ANNUAL-FEE-AMT
           END-IF
           ADD HEWAIV-ANNUAL-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2080-DESCRIBE-DRAW-PERIOD.
           EVALUATE HEWAIV-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E988 DESC REQUIRED
      * E977 BALANCE AMT NEGATIVE
      * E610 BALANCE AMT OVER 10000
      * E004 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2090-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HEWAIV-DESC = SPACES
                   MOVE 'E988' TO WS-REJECT-CODE
               WHEN HEWAIV-BALANCE-AMT < 0
                   MOVE 'E977' TO WS-REJECT-CODE
               WHEN HEWAIV-BALANCE-AMT > 10000.00
                   MOVE 'E610' TO WS-REJECT-CODE
               WHEN HEWAIV-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E004' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2100-LIMIT-INTEREST.
           IF HEWAIV-INTEREST-AMT < 10.00
               MOVE 10.00 TO HEWAIV-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HEWAIV-INTEREST-AMT > 500.00
               MOVE 500.00 TO HEWAIV-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ANNUAL FEE FEE ON THE WORK AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2110-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN HEWAIV-HOLD-FLAG = 'Y'
                   MOVE ZERO TO HEWAIV-ANNUAL-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HEWAIV-SUSPEND-FLAG = 'Y'
                   COMPUTE HEWAIV-ANNUAL-FEE-AMT ROUNDED = WS-WORK-AMT *
                       HEWAIV-PENALTY-PCT / 200
               WHEN HEWAIV-REASON-CD = 'KST'
                   MOVE 5.00 TO HEWAIV-ANNUAL-FEE-AMT
               WHEN HEWAIV-REASON-CD = 'NWS'
                   MOVE 20.00 TO HEWAIV-ANNUAL-FEE-AMT
               WHEN OTHER
                   COMPUTE HEWAIV-ANNUAL-FEE-AMT ROUNDED = WS-WORK-AMT *
                       HEWAIV-PENALTY-PCT / 100
           END-EVALUATE
           IF HEWAIV-ANNUAL-FEE-AMT > 0 AND HEWAIV-ANNUAL-FEE-AMT < 1.00
               MOVE 1.00 TO HEWAIV-ANNUAL-FEE-AMT
           END-IF
           IF HEWAIV-ANNUAL-FEE-AMT > 35.00
               MOVE 35.00 TO HEWAIV-ANNUAL-FEE-AMT
           END-IF
           ADD HEWAIV-ANNUAL-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E407 CUST ID REQUIRED
      * E520 WORK AMT NEGATIVE
      * E278 WORK AMT OVER 250000
      * E259 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2120-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HEWAIV-CUST-ID = SPACES
                   MOVE 'E407' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E520' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E278' TO WS-REJECT-CODE
               WHEN HEWAIV-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E259' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE FEE ON THE DRAW AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2130-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HEWAIV-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HEWAIV-BALANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HEWAIV-HOLD-FLAG = 'Y'
                   COMPUTE HEWAIV-BALANCE-AMT ROUNDED = HEWAIV-DRAW-AMT
                       * HEWAIV-PENALTY-PCT / 200
               WHEN HEWAIV-DRAW-PERIOD-CD = 'CCV'
                   MOVE 12.00 TO HEWAIV-BALANCE-AMT
               WHEN HEWAIV-DRAW-PERIOD-CD = 'JSU'
                   MOVE 12.00 TO HEWAIV-BALANCE-AMT
               WHEN OTHER
                   COMPUTE HEWAIV-BALANCE-AMT ROUNDED = HEWAIV-DRAW-AMT
                       * HEWAIV-PENALTY-PCT / 100
           END-EVALUATE
           IF HEWAIV-BALANCE-AMT > 0 AND HEWAIV-BALANCE-AMT < 5.00
               MOVE 5.00 TO HEWAIV-BALANCE-AMT
           END-IF
           IF HEWAIV-BALANCE-AMT > 250.00
               MOVE 250.00 TO HEWAIV-BALANCE-AMT
           END-IF
           ADD HEWAIV-BALANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2140-LIMIT-INTEREST.
           IF HEWAIV-INTEREST-AMT < 25.00
               MOVE 25.00 TO HEWAIV-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HEWAIV-INTEREST-AMT > 2500.00
               MOVE 2500.00 TO HEWAIV-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2150-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF HEWAIV-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEWAIV-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO HEWAIV-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2160-COUNT-NOTICE.
           IF HEWAIV-NOTICE-CNT < 9999999
               ADD 1 TO HEWAIV-NOTICE-CNT
           END-IF
           IF HEWAIV-NOTICE-CNT NOT < 6
               MOVE 'Y' TO HEWAIV-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LIMIT FEE ON THE LIMIT YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2170-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HEWAIV-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HEWAIV-LIMIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HEWAIV-SUSPEND-FLAG = 'Y'
                   COMPUTE HEWAIV-LIMIT-AMT ROUNDED = HEWAIV-LIMIT-YTD *
                       HEWAIV-PENALTY-PCT / 200
               WHEN HEWAIV-DRAW-PERIOD-CD = 'CCV'
                   MOVE 10.00 TO HEWAIV-LIMIT-AMT
               WHEN HEWAIV-DRAW-PERIOD-CD = 'JSU'
                   MOVE 15.00 TO HEWAIV-LIMIT-AMT
               WHEN OTHER
                   COMPUTE HEWAIV-LIMIT-AMT ROUNDED = HEWAIV-LIMIT-YTD *
                       HEWAIV-PENALTY-PCT / 100
           END-EVALUATE
           IF HEWAIV-LIMIT-AMT > 0 AND HEWAIV-LIMIT-AMT < 5.00
               MOVE 5.00 TO HEWAIV-LIMIT-AMT
           END-IF
           IF HEWAIV-LIMIT-AMT > 75.00
               MOVE 75.00 TO HEWAIV-LIMIT-AMT
           END-IF
           ADD HEWAIV-LIMIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2180-ACCRUE-ANNUAL-FEE.
           IF HEWAIV-ANNUAL-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HEWAIV-ANNUAL-FEE-AMT * HEWAIV-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO HEWAIV-LIMIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE ANNUAL FEE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2190-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN HEWAIV-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO HEWAIV-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HEWAIV-PREFERRED-FLAG = 'Y'
                   COMPUTE HEWAIV-INTEREST-AMT ROUNDED =
                       HEWAIV-ANNUAL-FEE-AMT * HEWAIV-PENALTY-PCT / 200
               WHEN HEWAIV-DRAW-PERIOD-CD = 'CCV'
                   MOVE 20.00 TO HEWAIV-INTEREST-AMT
               WHEN HEWAIV-DRAW-PERIOD-CD = 'JSU'
                   MOVE 3.00 TO HEWAIV-INTEREST-AMT
               WHEN OTHER
                   COMPUTE HEWAIV-INTEREST-AMT ROUNDED =
                       HEWAIV-ANNUAL-FEE-AMT * HEWAIV-PENALTY-PCT / 100
           END-EVALUATE
           IF HEWAIV-INTEREST-AMT > 0 AND HEWAIV-INTEREST-AMT < 1.00
               MOVE 1.00 TO HEWAIV-INTEREST-AMT
           END-IF
           IF HEWAIV-INTEREST-AMT > 250.00
               MOVE 250.00 TO HEWAIV-INTEREST-AMT
           END-IF
           ADD HEWAIV-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2200-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF HEWAIV-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEWAIV-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO HEWAIV-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DRAW AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2210-FLAG-DRAW.
           IF HEWAIV-DRAW-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HEWAIV-DRAW-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2220-FLAG-ANNUAL-FEE.
           IF HEWAIV-ANNUAL-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HEWAIV-ANNUAL-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2230-ACCRUE-ANNUAL-FEE.
           IF HEWAIV-ANNUAL-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HEWAIV-ANNUAL-FEE-AMT * HEWAIV-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO HEWAIV-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2240-FLAG-ANNUAL-FEE.
           IF HEWAIV-ANNUAL-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HEWAIV-ANNUAL-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2250-ACCRUE-ANNUAL-FEE.
           IF HEWAIV-ANNUAL-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HEWAIV-ANNUAL-FEE-AMT * HEWAIV-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO HEWAIV-DRAW-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2260-COUNT-EXCEPT.
           IF HEWAIV-EXCEPT-CNT < 9999999
               ADD 1 TO HEWAIV-EXCEPT-CNT
           END-IF
           IF HEWAIV-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO HEWAIV-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DRAW FEE ON THE LIMIT YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2270-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN HEWAIV-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO HEWAIV-DRAW-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HEWAIV-HOLD-FLAG = 'Y'
                   COMPUTE HEWAIV-DRAW-AMT ROUNDED = HEWAIV-LIMIT-YTD *
                       HEWAIV-PENALTY-PCT / 200
               WHEN HEWAIV-REASON-CD = 'KST'
                   MOVE 5.00 TO HEWAIV-DRAW-AMT
               WHEN HEWAIV-REASON-CD = 'NWS'
                   MOVE 20.00 TO HEWAIV-DRAW-AMT
               WHEN OTHER
                   COMPUTE HEWAIV-DRAW-AMT ROUNDED = HEWAIV-LIMIT-YTD *
                       HEWAIV-PENALTY-PCT / 100
           END-EVALUATE
           IF HEWAIV-DRAW-AMT > 0 AND HEWAIV-DRAW-AMT < 5.00
               MOVE 5.00 TO HEWAIV-DRAW-AMT
           END-IF
           IF HEWAIV-DRAW-AMT > 25.00
               MOVE 25.00 TO HEWAIV-DRAW-AMT
           END-IF
           ADD HEWAIV-DRAW-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE ANNUAL F TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2280-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ANNUAL-F-TIER-RATE(1) TO HEWAIV-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-ANNUAL-F-TIER-LOW(WS-TX)
                   MOVE WS-ANNUAL-F-TIER-RATE(WS-TX) TO
                       HEWAIV-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HEWAIV-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HEWAIV-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BALANCE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2290-ACCRUE-BALANCE.
           IF HEWAIV-BALANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HEWAIV-BALANCE-AMT * HEWAIV-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO HEWAIV-DRAW-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2300-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF HEWAIV-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO HEWAIV-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2310-LIMIT-LIMIT.
           IF HEWAIV-LIMIT-AMT < 50.00
               MOVE 50.00 TO HEWAIV-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HEWAIV-LIMIT-AMT > 5000.00
               MOVE 5000.00 TO HEWAIV-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2320-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HEWAIV-EFFECTIVE-DATE > 0 AND HEWAIV-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEWAIV-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HEWAIV-LIMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD HEWAIV-LIMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD HEWAIV-LIMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HEWAIV-LIMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HEWAIV-LIMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY DRAW PERIOD CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2330-ACCUM-DRAW-PERIOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HEWAIV-DRAW-PERIOD-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HEWAIV-DRAW-PERIOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ANNUAL F TIER TIER FOR THE BALANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2340-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ANNUAL-F-TIER-RATE(1) TO HEWAIV-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF HEWAIV-BALANCE-AMT NOT < WS-ANNUAL-F-TIER-LOW(WS-TX)
                   MOVE WS-ANNUAL-F-TIER-RATE(WS-TX) TO
                       HEWAIV-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HEWAIV-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HEWAIV-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2350-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DRAW AMT BY DRAW PERIOD CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2360-ACCUM-DRAW-PERIOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HEWAIV-DRAW-PERIOD-CD
                   ADD HEWAIV-DRAW-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HEWAIV-DRAW-PERIOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HEWAIV-DRAW-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2370-LIMIT-LIMIT.
           IF HEWAIV-LIMIT-YTD < 0.00
               MOVE 0.00 TO HEWAIV-LIMIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HEWAIV-LIMIT-YTD > 50000.00
               MOVE 50000.00 TO HEWAIV-LIMIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2380-FLAG-LIMIT.
           IF HEWAIV-LIMIT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HEWAIV-LIMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LIMIT YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2390-ACCRUE-LIMIT.
           IF HEWAIV-LIMIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HEWAIV-LIMIT-YTD * HEWAIV-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO HEWAIV-DRAW-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ANNUAL FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2400-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HEWAIV-ANNUAL-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * KST -> NWS WHEN EFFECTIVE DATE IS PAST
      * NWS -> ZJM WHEN EFFECTIVE DATE IS PAST
      * ZJM -> KST WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2410-SET-REASON.
           EVALUATE HEWAIV-REASON-CD
               WHEN 'KST'
                   IF HEWAIV-EFFECTIVE-DATE > 0 AND
                       HEWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'NWS' TO HEWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NWS'
                   IF HEWAIV-EFFECTIVE-DATE > 0 AND
                       HEWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZJM' TO HEWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZJM'
                   IF HEWAIV-EFFECTIVE-DATE > 0 AND
                       HEWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'KST' TO HEWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2420-LIMIT-LIMIT.
           IF HEWAIV-LIMIT-AMT < 25.00
               MOVE 25.00 TO HEWAIV-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HEWAIV-LIMIT-AMT > 50000.00
               MOVE 50000.00 TO HEWAIV-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2430-LIMIT-LIMIT.
           IF HEWAIV-LIMIT-AMT < 0.00
               MOVE 0.00 TO HEWAIV-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HEWAIV-LIMIT-AMT > 5000.00
               MOVE 5000.00 TO HEWAIV-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2440-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2450-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF HEWAIV-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEWAIV-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO HEWAIV-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2460-FLAG-INTEREST.
           IF HEWAIV-INTEREST-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HEWAIV-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LIMIT FEE ON THE DRAW AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2470-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN HEWAIV-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO HEWAIV-LIMIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HEWAIV-HOLD-FLAG = 'Y'
                   COMPUTE HEWAIV-LIMIT-AMT ROUNDED = HEWAIV-DRAW-AMT *
                       HEWAIV-PENALTY-PCT / 200
               WHEN HEWAIV-REGION-CD = 'BSD'
                   MOVE 15.00 TO HEWAIV-LIMIT-AMT
               WHEN HEWAIV-REGION-CD = 'HKW'
                   MOVE 20.00 TO HEWAIV-LIMIT-AMT
               WHEN OTHER
                   COMPUTE HEWAIV-LIMIT-AMT ROUNDED = HEWAIV-DRAW-AMT *
                       HEWAIV-PENALTY-PCT / 100
           END-EVALUATE
           IF HEWAIV-LIMIT-AMT > 0 AND HEWAIV-LIMIT-AMT < 1.00
               MOVE 1.00 TO HEWAIV-LIMIT-AMT
           END-IF
           IF HEWAIV-LIMIT-AMT > 50.00
               MOVE 50.00 TO HEWAIV-LIMIT-AMT
           END-IF
           ADD HEWAIV-LIMIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE ANNUAL F TIER TIER FOR THE ANNUAL FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2480-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ANNUAL-F-TIER-RATE(1) TO HEWAIV-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF HEWAIV-ANNUAL-FEE-AMT NOT <
                   WS-ANNUAL-F-TIER-LOW(WS-TX)
                   MOVE WS-ANNUAL-F-TIER-RATE(WS-TX) TO
                       HEWAIV-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HEWAIV-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HEWAIV-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ANNUAL FEE AMT BY DRAW PERIOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2490-ACCUM-DRAW-PERIOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HEWAIV-DRAW-PERIOD-CD
                   ADD HEWAIV-ANNUAL-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HEWAIV-DRAW-PERIOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HEWAIV-ANNUAL-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2500-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HEWAIV-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEWAIV-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO HEWAIV-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2510-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF HEWAIV-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO HEWAIV-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRAW AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       2520-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HEWAIV-LIMIT-AMT ROUNDED =
                   HEWAIV-DRAW-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HEWAIV-LIMIT-AMT
           END-IF
           ADD HEWAIV-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE ANNUAL F TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2530-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ANNUAL-F-TIER-RATE(1) TO HEWAIV-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-ANNUAL-F-TIER-LOW(WS-TX)
                   MOVE WS-ANNUAL-F-TIER-RATE(WS-TX) TO
                       HEWAIV-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HEWAIV-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HEWAIV-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2540-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF HEWAIV-LAST-MAINT-DATE > 0 AND HEWAIV-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEWAIV-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HEWAIV-BALANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD HEWAIV-BALANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD HEWAIV-BALANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HEWAIV-BALANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HEWAIV-BALANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2550-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF HEWAIV-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO HEWAIV-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ANNUAL F TIER TIER FOR THE ANNUAL FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2560-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ANNUAL-F-TIER-RATE(1) TO HEWAIV-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF HEWAIV-ANNUAL-FEE-AMT NOT <
                   WS-ANNUAL-F-TIER-LOW(WS-TX)
                   MOVE WS-ANNUAL-F-TIER-RATE(WS-TX) TO
                       HEWAIV-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HEWAIV-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HEWAIV-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2570-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF HEWAIV-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO HEWAIV-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2580-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HEWAIV-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HEWAIV-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HEWAIV-HOLD-FLAG = 'Y'
                   COMPUTE HEWAIV-INTEREST-AMT ROUNDED = WS-WORK-AMT-2 *
                       HEWAIV-MARGIN / 200
               WHEN HEWAIV-DRAW-PERIOD-CD = 'CCV'
                   MOVE 3.00 TO HEWAIV-INTEREST-AMT
               WHEN HEWAIV-DRAW-PERIOD-CD = 'JSU'
                   MOVE 3.00 TO HEWAIV-INTEREST-AMT
               WHEN OTHER
                   COMPUTE HEWAIV-INTEREST-AMT ROUNDED = WS-WORK-AMT-2 *
                       HEWAIV-MARGIN / 100
           END-EVALUATE
           IF HEWAIV-INTEREST-AMT > 0 AND HEWAIV-INTEREST-AMT < 5.00
               MOVE 5.00 TO HEWAIV-INTEREST-AMT
           END-IF
           IF HEWAIV-INTEREST-AMT > 25.00
               MOVE 25.00 TO HEWAIV-INTEREST-AMT
           END-IF
           ADD HEWAIV-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DRAW AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2590-LIMIT-DRAW.
           IF HEWAIV-DRAW-AMT < 50.00
               MOVE 50.00 TO HEWAIV-DRAW-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HEWAIV-DRAW-AMT > 1000.00
               MOVE 1000.00 TO HEWAIV-DRAW-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2600-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HEWAIV-REASON-CD
                   ADD HEWAIV-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HEWAIV-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HEWAIV-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ANNUAL F TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2610-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ANNUAL-F-TIER-RATE(1) TO HEWAIV-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-ANNUAL-F-TIER-LOW(WS-TX)
                   MOVE WS-ANNUAL-F-TIER-RATE(WS-TX) TO HEWAIV-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HEWAIV-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HEWAIV-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ANNUAL F TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2620-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ANNUAL-F-TIER-RATE(1) TO HEWAIV-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-ANNUAL-F-TIER-LOW(WS-TX)
                   MOVE WS-ANNUAL-F-TIER-RATE(WS-TX) TO HEWAIV-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HEWAIV-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HEWAIV-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2630-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HEWAIV-REGION-CD
                   ADD HEWAIV-BALANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HEWAIV-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HEWAIV-BALANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DRAW AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2640-FLAG-DRAW.
           IF HEWAIV-DRAW-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HEWAIV-DRAW-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2650-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF HEWAIV-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO HEWAIV-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * KST -> NWS WHEN MATURITY DATE IS PAST
      * NWS -> ZJM WHEN MATURITY DATE IS PAST
      * ZJM -> KST WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2660-SET-REASON.
           EVALUATE HEWAIV-REASON-CD
               WHEN 'KST'
                   IF HEWAIV-MATURITY-DATE > 0 AND HEWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'NWS' TO HEWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NWS'
                   IF HEWAIV-MATURITY-DATE > 0 AND HEWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ZJM' TO HEWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZJM'
                   IF HEWAIV-MATURITY-DATE > 0 AND HEWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KST' TO HEWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E894 DESC REQUIRED
      * E903 WORK AMT 2 NEGATIVE
      * E157 WORK AMT 2 OVER 10000
      * E846 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2670-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HEWAIV-DESC = SPACES
                   MOVE 'E894' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E903' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E157' TO WS-REJECT-CODE
               WHEN HEWAIV-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E846' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2680-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2690-DESCRIBE-DRAW-PERIOD.
           EVALUATE HEWAIV-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2700-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY LIEN POS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2710-ACCUM-LIEN-POS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HEWAIV-LIEN-POS-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HEWAIV-LIEN-POS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO HEWAIV-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO HEWAIV-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO HEWAIV-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'HEWAIV EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
