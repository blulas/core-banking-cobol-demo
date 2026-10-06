      *================================================================*
      * PROGRAM  : YKLIM                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : DATE WINDOW BRIDGE RULE - TIERING. CALLED BY THE    *
      *            DATE WINDOW BRIDGE BATCH AND ONLINE PROGRAMS.       *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-07-22 GAF  ORIGINAL                                       *
      * 1992-12-02 EWS  PERFORMANCE - AMOUNT-1 READ AHEAD REQ4355      *
      * 1992-03-26 TWH  CONTROL NOTICE TEXT PER LEGAL INC6161          *
      * 1994-08-28 BHS  CONTROL CUTOFF MOVED REQ8067                   *
      * 1994-11-20 SKN  AMOUNT-1 TABLE EXPANDED REQ0236                *
      * 1997-08-07 JLM  AUDIT FINDING - AMOUNT-2 CONTROL TOTALS CHG5995*
      * 1998-11-23 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-10-19 CRP  AMOUNT-1 CALCULATION CORRECTED PRB1704         *
      * 1999-06-16 EWS  CONTROL LIMIT REVISED CHG8893                  *
      * 2001-11-10 LMB  PERFORMANCE - AMOUNT-1 READ AHEAD REQ8752      *
      * 2003-05-02 BHS  AMOUNT-2 EXCEPTIONS TO OPS QUEUE CHG2075       *
      * 2006-09-21 MKL  NEW AMOUNT-1 REPORT COLUMNS PRB6447            *
      * 2007-07-02 EWS  AMOUNT-1 CALCULATION CORRECTED CHG2974         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YKLIM.
       AUTHOR. M K LOWE.

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
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 91.
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
      * WS-CONTROL-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTROL-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000238000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000785500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000266500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002650000000131500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002700000000717000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000323500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002800000000120000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002850000000220000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003850000000638000'.
       01  WS-CONTROL-GRID-TBL REDEFINES WS-CONTROL-GRID-VALUES.
           05  WS-CONTROL-GRID-ROW OCCURS 9 TIMES.
               10  WS-CONTROL-GRID-LOW     PIC 9(09)V99.
               10  WS-CONTROL-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONTROL-GRIDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTROL-GRIDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000829000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000199500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000055500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000596000'.
       01  WS-CONTROL-GRIDX-TBL REDEFINES WS-CONTROL-GRIDX-VALUES.
           05  WS-CONTROL-GRIDX-ROW OCCURS 4 TIMES.
               10  WS-CONTROL-GRIDX-LOW    PIC 9(09)V99.
               10  WS-CONTROL-GRIDX-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY YKLIMP.

       PROCEDURE DIVISION USING YKLIM-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SELECT-DISCOUNT
           PERFORM 2010-CHECK-LAST-MAINT
           PERFORM 2020-DESCRIBE-REGION
           PERFORM 2030-FLAG-CONTROL
           PERFORM 2040-SPLIT-AMOUNT-2
           PERFORM 2050-FLAG-CONTROL
           PERFORM 2060-EDIT-DESC
           PERFORM 2070-ACCRUE-AMOUNT-2
           PERFORM 2080-SET-REGION
           PERFORM 2090-SELECT-PENALTY-PCT
           PERFORM 2100-SET-REGION
           PERFORM 2110-FLAG-ITEM
           PERFORM 2120-ACCUM-PIVOT
           PERFORM 2130-LIMIT-ITEM
           PERFORM 2140-LIMIT-CONTROL
           PERFORM 2150-ACCRUE-AMOUNT-2
           PERFORM 2160-CHECK-LAST-MAINT
           PERFORM 2170-ASSESS-PREFERRED
           PERFORM 2180-SELECT-DISCOUNT
           PERFORM 2190-SET-WINDOW-RULE
           PERFORM 2200-AGE-OPEN
           PERFORM 2210-FLAG-AMOUNT-2
           PERFORM 2220-SET-WINDOW-RULE
           PERFORM 2230-SPLIT-AMOUNT-2
           PERFORM 2240-EDIT-CONTACT
           PERFORM 2250-COUNT-PAYMENT
           PERFORM 2260-SELECT-PENALTY-PCT
           PERFORM 2270-ASSESS-OVERRIDE
           PERFORM 2280-LIMIT-AMOUNT-1
           PERFORM 2290-ACCUM-REGION
           PERFORM 2300-DESCRIBE-PIVOT
           PERFORM 2310-LIMIT-AMOUNT-1
           PERFORM 2320-LIMIT-CONTROL
           PERFORM 2330-ACCUM-SOURCE
           PERFORM 2340-CHECK-LAST-STMT
           PERFORM 2350-SELECT-DISCOUNT
           PERFORM 2360-FLAG-CONTROL
           PERFORM 2370-EDIT-DESC
           PERFORM 2380-ACCRUE-AMOUNT-2
           PERFORM 2390-DESCRIBE-PIVOT
           PERFORM 2400-SPLIT-CONTROL
           PERFORM 2410-SPLIT-ITEM
           PERFORM 2420-ACCRUE-AMOUNT-1
           PERFORM 2430-DESCRIBE-PIVOT
           PERFORM 2440-ASSESS-NOTICE-SENT
           PERFORM 2450-COUNT-PAYMENT
           PERFORM 2460-AGE-OPEN
           PERFORM 2470-ACCUM-PIVOT
           PERFORM 2480-AGE-LAST-PAY
           PERFORM 2490-FLAG-CONTROL
           PERFORM 2500-EDIT-DESC
           PERFORM 2510-AGE-OPEN
           PERFORM 2520-DESCRIBE-PIVOT
           PERFORM 2530-AGE-LAST-MAINT
           PERFORM 2540-SET-SOURCE
           PERFORM 2550-LIMIT-CONTROL
           PERFORM 2560-COUNT-PAYMENT
           PERFORM 2570-COUNT-PAYMENT
           PERFORM 2580-SPLIT-AMOUNT-2
           PERFORM 2590-COUNT-CYCLE
           PERFORM 2600-SPLIT-AMOUNT-2
           PERFORM 2610-ACCUM-SOURCE
           PERFORM 2620-COUNT-PAYMENT
           PERFORM 2630-ASSESS-OVERRIDE
           PERFORM 2640-AGE-LAST-PAY
           PERFORM 2650-COUNT-PAYMENT
           PERFORM 2660-COUNT-CYCLE
           PERFORM 2670-EDIT-CONTACT
           PERFORM 2680-AGE-LAST-MAINT
           PERFORM 2690-SELECT-PENALTY-PCT
           PERFORM 2700-ACCRUE-AMOUNT-1
           PERFORM 2710-SET-PIVOT
           PERFORM 2720-COUNT-CYCLE
           PERFORM 2730-CHECK-LAST-MAINT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO YKLIM-RETURN-CODE.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL GRID TIER FOR THE AMOUNT 2 YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2000-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-GRID-RATE(1) TO YKLIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF YKLIM-AMOUNT-2-YTD NOT < WS-CONTROL-GRID-LOW(WS-TX)
                   MOVE WS-CONTROL-GRID-RATE(WS-TX) TO YKLIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKLIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKLIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2010-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YKLIM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKLIM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO YKLIM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2020-DESCRIBE-REGION.
           EVALUATE YKLIM-REGION-CD
               WHEN 'C'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'K'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'X'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONTROL YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2030-FLAG-CONTROL.
           IF YKLIM-CONTROL-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YKLIM-CONTROL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2040-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YKLIM-AMOUNT-2-YTD / 12
           COMPUTE WS-WORK-AMT-2 = YKLIM-AMOUNT-2-YTD - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2050-FLAG-CONTROL.
           IF YKLIM-CONTROL-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YKLIM-CONTROL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E808 DESC REQUIRED
      * E078 AMOUNT 1 AMT NEGATIVE
      * E505 AMOUNT 1 AMT OVER 250000
      * E756 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2060-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKLIM-DESC = SPACES
                   MOVE 'E808' TO WS-REJECT-CODE
               WHEN YKLIM-AMOUNT-1-AMT < 0
                   MOVE 'E078' TO WS-REJECT-CODE
               WHEN YKLIM-AMOUNT-1-AMT > 250000.00
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN YKLIM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E756' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2070-ACCRUE-AMOUNT-2.
           IF YKLIM-AMOUNT-2-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKLIM-AMOUNT-2-YTD * YKLIM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO YKLIM-CONTROL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> H WHEN LAST PAY DATE IS PAST
      * H -> K WHEN LAST PAY DATE IS PAST
      * K -> S WHEN LAST PAY DATE IS PAST
      * S -> V WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2080-SET-REGION.
           EVALUATE YKLIM-REGION-CD
               WHEN 'C'
                   IF YKLIM-LAST-PAY-DATE > 0 AND YKLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO YKLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF YKLIM-LAST-PAY-DATE > 0 AND YKLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO YKLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF YKLIM-LAST-PAY-DATE > 0 AND YKLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO YKLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF YKLIM-LAST-PAY-DATE > 0 AND YKLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO YKLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL GRID TIER FOR THE AMOUNT 2 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2090-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-GRID-RATE(1) TO YKLIM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF YKLIM-AMOUNT-2-AMT NOT < WS-CONTROL-GRID-LOW(WS-TX)
                   MOVE WS-CONTROL-GRID-RATE(WS-TX) TO YKLIM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKLIM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKLIM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> H WHEN LAST STMT DATE IS PAST
      * H -> K WHEN LAST STMT DATE IS PAST
      * K -> S WHEN LAST STMT DATE IS PAST
      * S -> V WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2100-SET-REGION.
           EVALUATE YKLIM-REGION-CD
               WHEN 'C'
                   IF YKLIM-LAST-STMT-DATE > 0 AND YKLIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'H' TO YKLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF YKLIM-LAST-STMT-DATE > 0 AND YKLIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'K' TO YKLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF YKLIM-LAST-STMT-DATE > 0 AND YKLIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'S' TO YKLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF YKLIM-LAST-STMT-DATE > 0 AND YKLIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'V' TO YKLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2110-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTROL YTD BY PIVOT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2120-ACCUM-PIVOT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKLIM-PIVOT-CD
                   ADD YKLIM-CONTROL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKLIM-PIVOT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YKLIM-CONTROL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2130-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2140-LIMIT-CONTROL.
           IF YKLIM-CONTROL-YTD < 10.00
               MOVE 10.00 TO YKLIM-CONTROL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKLIM-CONTROL-YTD > 10000.00
               MOVE 10000.00 TO YKLIM-CONTROL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2150-ACCRUE-AMOUNT-2.
           IF YKLIM-AMOUNT-2-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKLIM-AMOUNT-2-YTD * YKLIM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2160-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YKLIM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKLIM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO YKLIM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 1 YTD FEE ON THE AMOUNT 1 AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2170-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN YKLIM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO YKLIM-AMOUNT-1-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKLIM-OVERRIDE-FLAG = 'Y'
                   COMPUTE YKLIM-AMOUNT-1-YTD ROUNDED =
                       YKLIM-AMOUNT-1-AMT * YKLIM-DISCOUNT / 200
               WHEN YKLIM-REGION-CD = 'C'
                   MOVE 3.00 TO YKLIM-AMOUNT-1-YTD
               WHEN YKLIM-REGION-CD = 'H'
                   MOVE 10.00 TO YKLIM-AMOUNT-1-YTD
               WHEN OTHER
                   COMPUTE YKLIM-AMOUNT-1-YTD ROUNDED =
                       YKLIM-AMOUNT-1-AMT * YKLIM-DISCOUNT / 100
           END-EVALUATE
           IF YKLIM-AMOUNT-1-YTD > 0 AND YKLIM-AMOUNT-1-YTD < 10.00
               MOVE 10.00 TO YKLIM-AMOUNT-1-YTD
           END-IF
           IF YKLIM-AMOUNT-1-YTD > 35.00
               MOVE 35.00 TO YKLIM-AMOUNT-1-YTD
           END-IF
           ADD YKLIM-AMOUNT-1-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL GRID TIER FOR THE AMOUNT 1 YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2180-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-GRID-RATE(1) TO YKLIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF YKLIM-AMOUNT-1-YTD NOT < WS-CONTROL-GRID-LOW(WS-TX)
                   MOVE WS-CONTROL-GRID-RATE(WS-TX) TO YKLIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKLIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKLIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR WINDOW RULE CD:
      * A -> D WHEN LAST PAY DATE IS PAST
      * D -> S WHEN LAST PAY DATE IS PAST
      * S -> T WHEN LAST PAY DATE IS PAST
      * T -> A WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2190-SET-WINDOW-RULE.
           EVALUATE YKLIM-WINDOW-RULE-CD
               WHEN 'A'
                   IF YKLIM-LAST-PAY-DATE > 0 AND YKLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO YKLIM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF YKLIM-LAST-PAY-DATE > 0 AND YKLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO YKLIM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF YKLIM-LAST-PAY-DATE > 0 AND YKLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO YKLIM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF YKLIM-LAST-PAY-DATE > 0 AND YKLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'A' TO YKLIM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2200-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF YKLIM-OPEN-DATE > 0 AND YKLIM-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKLIM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKLIM-CONTROL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD YKLIM-CONTROL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD YKLIM-CONTROL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YKLIM-CONTROL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKLIM-CONTROL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2210-FLAG-AMOUNT-2.
           IF YKLIM-AMOUNT-2-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YKLIM-AMOUNT-2-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR WINDOW RULE CD:
      * A -> D WHEN LAST MAINT DATE IS PAST
      * D -> S WHEN LAST MAINT DATE IS PAST
      * S -> T WHEN LAST MAINT DATE IS PAST
      * T -> A WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2220-SET-WINDOW-RULE.
           EVALUATE YKLIM-WINDOW-RULE-CD
               WHEN 'A'
                   IF YKLIM-LAST-MAINT-DATE > 0 AND
                       YKLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'D' TO YKLIM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF YKLIM-LAST-MAINT-DATE > 0 AND
                       YKLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'S' TO YKLIM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF YKLIM-LAST-MAINT-DATE > 0 AND
                       YKLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'T' TO YKLIM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF YKLIM-LAST-MAINT-DATE > 0 AND
                       YKLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'A' TO YKLIM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2230-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YKLIM-AMOUNT-2-AMT / 3
           COMPUTE WS-WORK-AMT-2 = YKLIM-AMOUNT-2-AMT - (WS-WORK-AMT *
               3)
           COMPUTE YKLIM-CONTROL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E967 CONTACT REQUIRED
      * E306 AMOUNT 1 YTD NEGATIVE
      * E050 AMOUNT 1 YTD OVER 10000
      * E168 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2240-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKLIM-CONTACT = SPACES
                   MOVE 'E967' TO WS-REJECT-CODE
               WHEN YKLIM-AMOUNT-1-YTD < 0
                   MOVE 'E306' TO WS-REJECT-CODE
               WHEN YKLIM-AMOUNT-1-YTD > 10000.00
                   MOVE 'E050' TO WS-REJECT-CODE
               WHEN YKLIM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E168' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2250-COUNT-PAYMENT.
           IF YKLIM-PAYMENT-CNT < 9999999
               ADD 1 TO YKLIM-PAYMENT-CNT
           END-IF
           IF YKLIM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO YKLIM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2260-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-GRID-RATE(1) TO YKLIM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-CONTROL-GRID-LOW(WS-TX)
                   MOVE WS-CONTROL-GRID-RATE(WS-TX) TO YKLIM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKLIM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKLIM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 2 FEE ON THE AMOUNT 1 AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2270-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN YKLIM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO YKLIM-AMOUNT-2-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKLIM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE YKLIM-AMOUNT-2-AMT ROUNDED =
                       YKLIM-AMOUNT-1-AMT * YKLIM-PENALTY-PCT / 200
               WHEN YKLIM-WINDOW-RULE-CD = 'A'
                   MOVE 20.00 TO YKLIM-AMOUNT-2-AMT
               WHEN YKLIM-WINDOW-RULE-CD = 'D'
                   MOVE 12.00 TO YKLIM-AMOUNT-2-AMT
               WHEN OTHER
                   COMPUTE YKLIM-AMOUNT-2-AMT ROUNDED =
                       YKLIM-AMOUNT-1-AMT * YKLIM-PENALTY-PCT / 100
           END-EVALUATE
           IF YKLIM-AMOUNT-2-AMT > 0 AND YKLIM-AMOUNT-2-AMT < 5.00
               MOVE 5.00 TO YKLIM-AMOUNT-2-AMT
           END-IF
           IF YKLIM-AMOUNT-2-AMT > 150.00
               MOVE 150.00 TO YKLIM-AMOUNT-2-AMT
           END-IF
           ADD YKLIM-AMOUNT-2-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AMOUNT 1 YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2280-LIMIT-AMOUNT-1.
           IF YKLIM-AMOUNT-1-YTD < 0.00
               MOVE 0.00 TO YKLIM-AMOUNT-1-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKLIM-AMOUNT-1-YTD > 1000.00
               MOVE 1000.00 TO YKLIM-AMOUNT-1-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTROL YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2290-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKLIM-REGION-CD
                   ADD YKLIM-CONTROL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKLIM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YKLIM-CONTROL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PIVOT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2300-DESCRIBE-PIVOT.
           EVALUATE YKLIM-PIVOT-CD
               WHEN 'KGT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NNG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PTC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WZW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2310-LIMIT-AMOUNT-1.
           IF YKLIM-AMOUNT-1-AMT < 10.00
               MOVE 10.00 TO YKLIM-AMOUNT-1-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKLIM-AMOUNT-1-AMT > 50000.00
               MOVE 50000.00 TO YKLIM-AMOUNT-1-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2320-LIMIT-CONTROL.
           IF YKLIM-CONTROL-AMT < 0.00
               MOVE 0.00 TO YKLIM-CONTROL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKLIM-CONTROL-AMT > 5000.00
               MOVE 5000.00 TO YKLIM-CONTROL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2330-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKLIM-SOURCE-CD
                   ADD YKLIM-AMOUNT-2-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKLIM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YKLIM-AMOUNT-2-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2340-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF YKLIM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKLIM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO YKLIM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL GRIDX TIER FOR THE AMOUNT 2 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2350-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-GRIDX-RATE(1) TO YKLIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF YKLIM-AMOUNT-2-AMT NOT < WS-CONTROL-GRIDX-LOW(WS-TX)
                   MOVE WS-CONTROL-GRIDX-RATE(WS-TX) TO YKLIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKLIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKLIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2360-FLAG-CONTROL.
           IF YKLIM-CONTROL-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YKLIM-CONTROL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E482 DESC REQUIRED
      * E062 WORK AMT 2 NEGATIVE
      * E580 WORK AMT 2 OVER 10000
      * E485 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2370-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKLIM-DESC = SPACES
                   MOVE 'E482' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E062' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E580' TO WS-REJECT-CODE
               WHEN YKLIM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E485' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2380-ACCRUE-AMOUNT-2.
           IF YKLIM-AMOUNT-2-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKLIM-AMOUNT-2-AMT * YKLIM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO YKLIM-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PIVOT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2390-DESCRIBE-PIVOT.
           EVALUATE YKLIM-PIVOT-CD
               WHEN 'KGT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NNG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PTC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WZW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2400-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE YKLIM-CONTROL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2410-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 1 AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2420-ACCRUE-AMOUNT-1.
           IF YKLIM-AMOUNT-1-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKLIM-AMOUNT-1-AMT * YKLIM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO YKLIM-CONTROL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PIVOT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2430-DESCRIBE-PIVOT.
           EVALUATE YKLIM-PIVOT-CD
               WHEN 'KGT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NNG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PTC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WZW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2440-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN YKLIM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKLIM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       YKLIM-DISCOUNT / 200
               WHEN YKLIM-PIVOT-CD = 'KGT'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN YKLIM-PIVOT-CD = 'NNG'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       YKLIM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2450-COUNT-PAYMENT.
           IF YKLIM-PAYMENT-CNT < 9999999
               ADD 1 TO YKLIM-PAYMENT-CNT
           END-IF
           IF YKLIM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO YKLIM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2460-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF YKLIM-OPEN-DATE > 0 AND YKLIM-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKLIM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKLIM-AMOUNT-2-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YKLIM-AMOUNT-2-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD YKLIM-AMOUNT-2-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YKLIM-AMOUNT-2-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKLIM-AMOUNT-2-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY PIVOT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2470-ACCUM-PIVOT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKLIM-PIVOT-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKLIM-PIVOT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2480-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF YKLIM-LAST-PAY-DATE > 0 AND YKLIM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKLIM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKLIM-AMOUNT-1-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD YKLIM-AMOUNT-1-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD YKLIM-AMOUNT-1-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YKLIM-AMOUNT-1-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKLIM-AMOUNT-1-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONTROL YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2490-FLAG-CONTROL.
           IF YKLIM-CONTROL-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YKLIM-CONTROL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E748 DESC REQUIRED
      * E408 AMOUNT 2 AMT NEGATIVE
      * E120 AMOUNT 2 AMT OVER 25000
      * E297 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2500-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKLIM-DESC = SPACES
                   MOVE 'E748' TO WS-REJECT-CODE
               WHEN YKLIM-AMOUNT-2-AMT < 0
                   MOVE 'E408' TO WS-REJECT-CODE
               WHEN YKLIM-AMOUNT-2-AMT > 25000.00
                   MOVE 'E120' TO WS-REJECT-CODE
               WHEN YKLIM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E297' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2510-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF YKLIM-OPEN-DATE > 0 AND YKLIM-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKLIM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKLIM-CONTROL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD YKLIM-CONTROL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YKLIM-CONTROL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YKLIM-CONTROL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKLIM-CONTROL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PIVOT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2520-DESCRIBE-PIVOT.
           EVALUATE YKLIM-PIVOT-CD
               WHEN 'KGT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NNG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PTC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WZW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2530-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YKLIM-LAST-MAINT-DATE > 0 AND YKLIM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKLIM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKLIM-CONTROL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YKLIM-CONTROL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YKLIM-CONTROL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD YKLIM-CONTROL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKLIM-CONTROL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN OPEN DATE IS PAST
      * FTB -> GAP WHEN OPEN DATE IS PAST
      * GAP -> PAP WHEN OPEN DATE IS PAST
      * PAP -> VGC WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2540-SET-SOURCE.
           EVALUATE YKLIM-SOURCE-CD
               WHEN 'BNX'
                   IF YKLIM-OPEN-DATE > 0 AND YKLIM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YKLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YKLIM-OPEN-DATE > 0 AND YKLIM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YKLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YKLIM-OPEN-DATE > 0 AND YKLIM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YKLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YKLIM-OPEN-DATE > 0 AND YKLIM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YKLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONTROL YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2550-LIMIT-CONTROL.
           IF YKLIM-CONTROL-YTD < 0.00
               MOVE 0.00 TO YKLIM-CONTROL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKLIM-CONTROL-YTD > 500.00
               MOVE 500.00 TO YKLIM-CONTROL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2560-COUNT-PAYMENT.
           IF YKLIM-PAYMENT-CNT < 9999999
               ADD 1 TO YKLIM-PAYMENT-CNT
           END-IF
           IF YKLIM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO YKLIM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2570-COUNT-PAYMENT.
           IF YKLIM-PAYMENT-CNT < 9999999
               ADD 1 TO YKLIM-PAYMENT-CNT
           END-IF
           IF YKLIM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO YKLIM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2580-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YKLIM-AMOUNT-2-AMT / 2
           COMPUTE WS-WORK-AMT-2 = YKLIM-AMOUNT-2-AMT - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2590-COUNT-CYCLE.
           IF YKLIM-CYCLE-CNT < 9999999
               ADD 1 TO YKLIM-CYCLE-CNT
           END-IF
           IF YKLIM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO YKLIM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2600-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE YKLIM-AMOUNT-2-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2610-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKLIM-SOURCE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKLIM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2620-COUNT-PAYMENT.
           IF YKLIM-PAYMENT-CNT < 9999999
               ADD 1 TO YKLIM-PAYMENT-CNT
           END-IF
           IF YKLIM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO YKLIM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 2 FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2630-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN YKLIM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO YKLIM-AMOUNT-2-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKLIM-PREFERRED-FLAG = 'Y'
                   COMPUTE YKLIM-AMOUNT-2-AMT ROUNDED = WS-WORK-AMT *
                       YKLIM-DISCOUNT / 200
               WHEN YKLIM-PIVOT-CD = 'KGT'
                   MOVE 12.00 TO YKLIM-AMOUNT-2-AMT
               WHEN YKLIM-PIVOT-CD = 'NNG'
                   MOVE 5.00 TO YKLIM-AMOUNT-2-AMT
               WHEN OTHER
                   COMPUTE YKLIM-AMOUNT-2-AMT ROUNDED = WS-WORK-AMT *
                       YKLIM-DISCOUNT / 100
           END-EVALUATE
           IF YKLIM-AMOUNT-2-AMT > 0 AND YKLIM-AMOUNT-2-AMT < 10.00
               MOVE 10.00 TO YKLIM-AMOUNT-2-AMT
           END-IF
           IF YKLIM-AMOUNT-2-AMT > 100.00
               MOVE 100.00 TO YKLIM-AMOUNT-2-AMT
           END-IF
           ADD YKLIM-AMOUNT-2-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2640-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF YKLIM-LAST-PAY-DATE > 0 AND YKLIM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKLIM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKLIM-AMOUNT-1-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD YKLIM-AMOUNT-1-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD YKLIM-AMOUNT-1-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YKLIM-AMOUNT-1-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKLIM-AMOUNT-1-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2650-COUNT-PAYMENT.
           IF YKLIM-PAYMENT-CNT < 9999999
               ADD 1 TO YKLIM-PAYMENT-CNT
           END-IF
           IF YKLIM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO YKLIM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2660-COUNT-CYCLE.
           IF YKLIM-CYCLE-CNT < 9999999
               ADD 1 TO YKLIM-CYCLE-CNT
           END-IF
           IF YKLIM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO YKLIM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E814 CONTACT REQUIRED
      * E277 CONTROL AMT NEGATIVE
      * E908 CONTROL AMT OVER 25000
      * E637 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2670-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKLIM-CONTACT = SPACES
                   MOVE 'E814' TO WS-REJECT-CODE
               WHEN YKLIM-CONTROL-AMT < 0
                   MOVE 'E277' TO WS-REJECT-CODE
               WHEN YKLIM-CONTROL-AMT > 25000.00
                   MOVE 'E908' TO WS-REJECT-CODE
               WHEN YKLIM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E637' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2680-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YKLIM-LAST-MAINT-DATE > 0 AND YKLIM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKLIM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKLIM-AMOUNT-1-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YKLIM-AMOUNT-1-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD YKLIM-AMOUNT-1-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD YKLIM-AMOUNT-1-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKLIM-AMOUNT-1-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2690-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-GRID-RATE(1) TO YKLIM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-CONTROL-GRID-LOW(WS-TX)
                   MOVE WS-CONTROL-GRID-RATE(WS-TX) TO YKLIM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKLIM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKLIM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 1 YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2700-ACCRUE-AMOUNT-1.
           IF YKLIM-AMOUNT-1-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKLIM-AMOUNT-1-YTD * YKLIM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PIVOT CD:
      * KGT -> NNG WHEN LAST STMT DATE IS PAST
      * NNG -> PTC WHEN LAST STMT DATE IS PAST
      * PTC -> WZW WHEN LAST STMT DATE IS PAST
      * WZW -> KGT WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2710-SET-PIVOT.
           EVALUATE YKLIM-PIVOT-CD
               WHEN 'KGT'
                   IF YKLIM-LAST-STMT-DATE > 0 AND YKLIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'NNG' TO YKLIM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NNG'
                   IF YKLIM-LAST-STMT-DATE > 0 AND YKLIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PTC' TO YKLIM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PTC'
                   IF YKLIM-LAST-STMT-DATE > 0 AND YKLIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WZW' TO YKLIM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WZW'
                   IF YKLIM-LAST-STMT-DATE > 0 AND YKLIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KGT' TO YKLIM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2720-COUNT-CYCLE.
           IF YKLIM-CYCLE-CNT < 9999999
               ADD 1 TO YKLIM-CYCLE-CNT
           END-IF
           IF YKLIM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO YKLIM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2730-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YKLIM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKLIM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO YKLIM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO YKLIM-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO YKLIM-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO YKLIM-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'YKLIM EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
