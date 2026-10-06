      *================================================================*
      * PROGRAM  : HEFEE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : HOME EQUITY LINES RULE - DATE RULES. CALLED BY THE  *
      *            HOME EQUITY LINES BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-11-16 TWH  ORIGINAL                                       *
      * 1994-10-09 RJT  BALANCE EXCEPTIONS TO OPS QUEUE PRB6008        *
      * 1996-07-06 SKN  PERFORMANCE - BALANCE READ AHEAD REQ1231       *
      * 1996-07-04 SKN  BALANCE CALCULATION CORRECTED PRB8956          *
      * 1996-05-05 PQR  AUDIT FINDING - DRAW CONTROL TOTALS REQ3486    *
      * 1998-12-02 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-12-02 LMB  MIN-PAY STATUS CODES ADDED CHG2347             *
      * 2000-07-26 DPS  APPRAISE CUTOFF MOVED REQ2676                  *
      * 2002-01-15 GAF  PERFORMANCE - INTEREST READ AHEAD PRB4269      *
      * 2004-03-28 LMB  MIN-PAY FEE SCHEDULE UPDATE INC4166            *
      * 2005-02-15 BHS  LIMIT CUTOFF MOVED CHG2475                     *
      * 2005-01-10 GAF  INTEREST RERUN PROTECTION PRB1071              *
      * 2008-10-10 DPS  NEW APPRAISE REPORT COLUMNS PRB9604            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HEFEE.
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
                                           VALUE 19.000000.
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
      * WS-MIN-PAY-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MIN-PAY-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000655500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000160000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000222500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007750000000026000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008000000000388000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018000000000402000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018500000000426000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019500000000411500'.
       01  WS-MIN-PAY-GRID-TBL REDEFINES WS-MIN-PAY-GRID-VALUES.
           05  WS-MIN-PAY-GRID-ROW OCCURS 8 TIMES.
               10  WS-MIN-PAY-GRID-LOW     PIC 9(09)V99.
               10  WS-MIN-PAY-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ANNUAL-F-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ANNUAL-F-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000770000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000719000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000836000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000537000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020500000000450500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021500000000764000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021750000000429000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021850000000855000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022350000000031000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022850000000847500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023350000000288000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025850000000055000'.
       01  WS-ANNUAL-F-SCHED-TBL REDEFINES WS-ANNUAL-F-SCHED-VALUES.
           05  WS-ANNUAL-F-SCHED-ROW OCCURS 12 TIMES.
               10  WS-ANNUAL-F-SCHED-LOW   PIC 9(09)V99.
               10  WS-ANNUAL-F-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INTEREST-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000747500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000334000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000051000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000262000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008550000000489500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008650000000764000'.
       01  WS-INTEREST-GRID-TBL REDEFINES WS-INTEREST-GRID-VALUES.
           05  WS-INTEREST-GRID-ROW OCCURS 6 TIMES.
               10  WS-INTEREST-GRID-LOW    PIC 9(09)V99.
               10  WS-INTEREST-GRID-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY HEFEEP.

       PROCEDURE DIVISION USING HEFEE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-LIMIT-BALANCE
           PERFORM 2010-ACCRUE-INTEREST
           PERFORM 2020-SPLIT-DRAW
           PERFORM 2030-ACCRUE-BALANCE
           PERFORM 2040-CHECK-NEXT-REVIEW
           PERFORM 2050-EDIT-CONTACT
           PERFORM 2060-LIMIT-ANNUAL-FEE
           PERFORM 2070-LIMIT-ITEM
           PERFORM 2080-DESCRIBE-DRAW-PERIOD
           PERFORM 2090-PRORATE-DRAW
           PERFORM 2100-SET-TYPE
           PERFORM 2110-FLAG-BALANCE
           PERFORM 2120-ASSESS-HOLD
           PERFORM 2130-ACCUM-DRAW-PERIOD
           PERFORM 2140-SET-TYPE
           PERFORM 2150-LIMIT-INTEREST
           PERFORM 2160-SPLIT-DRAW
           PERFORM 2170-PRORATE-ANNUAL-FEE
           PERFORM 2180-ASSESS-PREFERRED
           PERFORM 2190-PRORATE-INTEREST
           PERFORM 2200-CHECK-NEXT-REVIEW
           PERFORM 2210-SPLIT-INTEREST
           PERFORM 2220-ACCUM-STATUS
           PERFORM 2230-AGE-OPEN
           PERFORM 2240-LIMIT-ANNUAL-FEE
           PERFORM 2250-ACCUM-STATUS
           PERFORM 2260-SET-DRAW-PERIOD
           PERFORM 2270-LIMIT-ITEM
           PERFORM 2280-COUNT-RETRY
           PERFORM 2290-DESCRIBE-STATUS
           PERFORM 2300-PRORATE-BALANCE
           PERFORM 2310-PRORATE-BALANCE
           PERFORM 2320-SELECT-DISCOUNT
           PERFORM 2330-PRORATE-INTEREST
           PERFORM 2340-ACCRUE-INTEREST
           PERFORM 2350-PRORATE-DRAW
           PERFORM 2360-SET-REASON
           PERFORM 2370-ACCUM-REASON
           PERFORM 2380-AGE-LAST-PAY
           PERFORM 2390-SPLIT-INTEREST
           PERFORM 2400-LIMIT-INTEREST
           PERFORM 2410-FLAG-ANNUAL-FEE
           PERFORM 2420-COUNT-NOTICE
           PERFORM 2430-DESCRIBE-REASON
           PERFORM 2440-PRORATE-ANNUAL-FEE
           PERFORM 2450-FLAG-ANNUAL-FEE
           PERFORM 2460-SPLIT-INTEREST
           PERFORM 2470-SELECT-PENALTY-PCT
           PERFORM 2480-DESCRIBE-TYPE
           PERFORM 2490-EDIT-CONTACT
           PERFORM 2500-AGE-OPEN
           PERFORM 2510-ACCUM-DRAW-PERIOD
           PERFORM 2520-ACCRUE-ANNUAL-FEE
           PERFORM 2530-SELECT-DISCOUNT
           PERFORM 2540-COUNT-RETRY
           PERFORM 2550-LIMIT-ITEM
           PERFORM 2560-SELECT-PENALTY-PCT
           PERFORM 2570-SET-REASON
           PERFORM 2580-ACCUM-STATUS
           PERFORM 2590-FLAG-ANNUAL-FEE
           PERFORM 2600-AGE-OPEN
           PERFORM 2610-ASSESS-NOTICE-SENT
           PERFORM 2620-SET-REASON
           PERFORM 2630-EDIT-CONTACT
           PERFORM 2640-EDIT-NAME
           PERFORM 2650-DESCRIBE-DRAW-PERIOD
           PERFORM 2660-SELECT-DISCOUNT
           PERFORM 2670-SET-TYPE
           PERFORM 2680-LIMIT-ANNUAL-FEE
           PERFORM 2690-SPLIT-ANNUAL-FEE
           PERFORM 2700-AGE-NEXT-REVIEW
           PERFORM 2710-AGE-NEXT-REVIEW
           PERFORM 2720-ASSESS-HOLD
           PERFORM 2730-PRORATE-BALANCE
           PERFORM 2740-PRORATE-DRAW
           PERFORM 2750-COUNT-NOTICE
           PERFORM 2760-DESCRIBE-DRAW-PERIOD
           PERFORM 2770-AGE-OPEN
           PERFORM 2780-LIMIT-INTEREST
           PERFORM 2790-ACCUM-TYPE
           PERFORM 2800-ACCRUE-PENALTY-PCT
           PERFORM 2810-ACCUM-DRAW-PERIOD
           PERFORM 2820-ACCRUE-BALANCE
           PERFORM 2830-ACCRUE-INTEREST
           PERFORM 2840-COUNT-RETRY
           PERFORM 2850-AGE-NEXT-DUE
           PERFORM 2860-AGE-NEXT-REVIEW
           PERFORM 2870-ACCUM-DRAW-PERIOD
           PERFORM 2880-EDIT-CONTACT
           PERFORM 2890-COUNT-RETRY
           PERFORM 2900-EDIT-NAME
           PERFORM 2910-CHECK-NEXT-REVIEW
           PERFORM 2920-AGE-NEXT-REVIEW
           PERFORM 2930-ASSESS-NOTICE-SENT
           PERFORM 2940-ACCRUE-PENALTY-PCT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO HEFEE-RETURN-CODE.

      *----------------------------------------------------------------*
      * BALANCE YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2000-LIMIT-BALANCE.
           IF HEFEE-BALANCE-YTD < 50.00
               MOVE 50.00 TO HEFEE-BALANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HEFEE-BALANCE-YTD > 2500.00
               MOVE 2500.00 TO HEFEE-BALANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2010-ACCRUE-INTEREST.
           IF HEFEE-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HEFEE-INTEREST-AMT * HEFEE-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO HEFEE-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DRAW AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2020-SPLIT-DRAW.
           COMPUTE WS-WORK-AMT = HEFEE-DRAW-AMT / 4
           COMPUTE WS-WORK-AMT-2 = HEFEE-DRAW-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BALANCE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2030-ACCRUE-BALANCE.
           IF HEFEE-BALANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HEFEE-BALANCE-YTD * HEFEE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO HEFEE-ANNUAL-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2040-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF HEFEE-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO HEFEE-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E184 CONTACT REQUIRED
      * E337 INTEREST AMT NEGATIVE
      * E880 INTEREST AMT OVER 1000000
      * E081 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2050-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HEFEE-CONTACT = SPACES
                   MOVE 'E184' TO WS-REJECT-CODE
               WHEN HEFEE-INTEREST-AMT < 0
                   MOVE 'E337' TO WS-REJECT-CODE
               WHEN HEFEE-INTEREST-AMT > 1000000.00
                   MOVE 'E880' TO WS-REJECT-CODE
               WHEN HEFEE-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E081' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2060-LIMIT-ANNUAL-FEE.
           IF HEFEE-ANNUAL-FEE-AMT < 0.00
               MOVE 0.00 TO HEFEE-ANNUAL-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HEFEE-ANNUAL-FEE-AMT > 50000.00
               MOVE 50000.00 TO HEFEE-ANNUAL-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2070-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2080-DESCRIBE-DRAW-PERIOD.
           EVALUATE HEFEE-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DRAW AMT.
      *----------------------------------------------------------------*
       2090-PRORATE-DRAW.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HEFEE-DRAW-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HEFEE-DRAW-AMT
           END-IF
           ADD HEFEE-DRAW-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CD -> JW WHEN NEXT DUE DATE IS PAST
      * JW -> KV WHEN NEXT DUE DATE IS PAST
      * KV -> WS WHEN NEXT DUE DATE IS PAST
      * WS -> XL WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2100-SET-TYPE.
           EVALUATE HEFEE-TYPE-CD
               WHEN 'CD'
                   IF HEFEE-NEXT-DUE-DATE > 0 AND HEFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JW' TO HEFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JW'
                   IF HEFEE-NEXT-DUE-DATE > 0 AND HEFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KV' TO HEFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF HEFEE-NEXT-DUE-DATE > 0 AND HEFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WS' TO HEFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WS'
                   IF HEFEE-NEXT-DUE-DATE > 0 AND HEFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO HEFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BALANCE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2110-FLAG-BALANCE.
           IF HEFEE-BALANCE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HEFEE-BALANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2120-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN HEFEE-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HEFEE-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       HEFEE-DISCOUNT / 200
               WHEN HEFEE-STATUS-CD = 'GR'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN HEFEE-STATUS-CD = 'JC'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       HEFEE-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE YTD BY DRAW PERIOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2130-ACCUM-DRAW-PERIOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HEFEE-DRAW-PERIOD-CD
                   ADD HEFEE-BALANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HEFEE-DRAW-PERIOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HEFEE-BALANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CD -> JW WHEN OPEN DATE IS PAST
      * JW -> KV WHEN OPEN DATE IS PAST
      * KV -> WS WHEN OPEN DATE IS PAST
      * WS -> XL WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2140-SET-TYPE.
           EVALUATE HEFEE-TYPE-CD
               WHEN 'CD'
                   IF HEFEE-OPEN-DATE > 0 AND HEFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JW' TO HEFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JW'
                   IF HEFEE-OPEN-DATE > 0 AND HEFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KV' TO HEFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF HEFEE-OPEN-DATE > 0 AND HEFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WS' TO HEFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WS'
                   IF HEFEE-OPEN-DATE > 0 AND HEFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO HEFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2150-LIMIT-INTEREST.
           IF HEFEE-INTEREST-AMT < 25.00
               MOVE 25.00 TO HEFEE-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HEFEE-INTEREST-AMT > 2500.00
               MOVE 2500.00 TO HEFEE-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2160-SPLIT-DRAW.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE HEFEE-DRAW-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ANNUAL FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2170-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HEFEE-ANNUAL-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE ANNUAL FEE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2180-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HEFEE-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HEFEE-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HEFEE-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE HEFEE-INTEREST-YTD ROUNDED =
                       HEFEE-ANNUAL-FEE-AMT * HEFEE-PENALTY-PCT / 200
               WHEN HEFEE-TYPE-CD = 'CD'
                   MOVE 3.00 TO HEFEE-INTEREST-YTD
               WHEN HEFEE-TYPE-CD = 'JW'
                   MOVE 20.00 TO HEFEE-INTEREST-YTD
               WHEN OTHER
                   COMPUTE HEFEE-INTEREST-YTD ROUNDED =
                       HEFEE-ANNUAL-FEE-AMT * HEFEE-PENALTY-PCT / 100
           END-EVALUATE
           IF HEFEE-INTEREST-YTD > 0 AND HEFEE-INTEREST-YTD < 2.00
               MOVE 2.00 TO HEFEE-INTEREST-YTD
           END-IF
           IF HEFEE-INTEREST-YTD > 100.00
               MOVE 100.00 TO HEFEE-INTEREST-YTD
           END-IF
           ADD HEFEE-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE INTEREST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2190-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   HEFEE-INTEREST-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2200-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF HEFEE-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO HEFEE-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2210-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = HEFEE-INTEREST-YTD / 4
           COMPUTE WS-WORK-AMT-2 = HEFEE-INTEREST-YTD - (WS-WORK-AMT *
               4)
           COMPUTE HEFEE-ANNUAL-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2220-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HEFEE-STATUS-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HEFEE-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2230-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF HEFEE-OPEN-DATE > 0 AND HEFEE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEFEE-OPEN-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2240-LIMIT-ANNUAL-FEE.
           IF HEFEE-ANNUAL-FEE-AMT < 0.00
               MOVE 0.00 TO HEFEE-ANNUAL-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HEFEE-ANNUAL-FEE-AMT > 2500.00
               MOVE 2500.00 TO HEFEE-ANNUAL-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2250-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HEFEE-STATUS-CD
                   ADD HEFEE-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HEFEE-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HEFEE-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DRAW PERIOD CD:
      * CCV -> JSU WHEN OPEN DATE IS PAST
      * JSU -> KXA WHEN OPEN DATE IS PAST
      * KXA -> VMR WHEN OPEN DATE IS PAST
      * VMR -> WLJ WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2260-SET-DRAW-PERIOD.
           EVALUATE HEFEE-DRAW-PERIOD-CD
               WHEN 'CCV'
                   IF HEFEE-OPEN-DATE > 0 AND HEFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JSU' TO HEFEE-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JSU'
                   IF HEFEE-OPEN-DATE > 0 AND HEFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KXA' TO HEFEE-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KXA'
                   IF HEFEE-OPEN-DATE > 0 AND HEFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VMR' TO HEFEE-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VMR'
                   IF HEFEE-OPEN-DATE > 0 AND HEFEE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLJ' TO HEFEE-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2270-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2280-COUNT-RETRY.
           IF HEFEE-RETRY-CNT < 9999999
               ADD 1 TO HEFEE-RETRY-CNT
           END-IF
           IF HEFEE-RETRY-CNT NOT < 10
               MOVE 'Y' TO HEFEE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2290-DESCRIBE-STATUS.
           EVALUATE HEFEE-STATUS-CD
               WHEN 'GR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BALANCE YTD.
      *----------------------------------------------------------------*
       2300-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HEFEE-BALANCE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HEFEE-BALANCE-YTD
           END-IF
           ADD HEFEE-BALANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE INTEREST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BALANCE YTD.
      *----------------------------------------------------------------*
       2310-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HEFEE-BALANCE-YTD ROUNDED =
                   HEFEE-INTEREST-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HEFEE-BALANCE-YTD
           END-IF
           ADD HEFEE-BALANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST GRID TIER FOR THE DRAW AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2320-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-GRID-RATE(1) TO HEFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HEFEE-DRAW-AMT NOT < WS-INTEREST-GRID-LOW(WS-TX)
                   MOVE WS-INTEREST-GRID-RATE(WS-TX) TO HEFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HEFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HEFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       2330-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HEFEE-INTEREST-AMT ROUNDED =
                   HEFEE-INTEREST-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HEFEE-INTEREST-AMT
           END-IF
           ADD HEFEE-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2340-ACCRUE-INTEREST.
           IF HEFEE-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HEFEE-INTEREST-YTD * HEFEE-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO HEFEE-BALANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRAW AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2350-PRORATE-DRAW.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   HEFEE-DRAW-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * KST -> NWS WHEN NEXT REVIEW DATE IS PAST
      * NWS -> ZJM WHEN NEXT REVIEW DATE IS PAST
      * ZJM -> KST WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2360-SET-REASON.
           EVALUATE HEFEE-REASON-CD
               WHEN 'KST'
                   IF HEFEE-NEXT-REVIEW-DATE > 0 AND
                       HEFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'NWS' TO HEFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NWS'
                   IF HEFEE-NEXT-REVIEW-DATE > 0 AND
                       HEFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZJM' TO HEFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZJM'
                   IF HEFEE-NEXT-REVIEW-DATE > 0 AND
                       HEFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KST' TO HEFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2370-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HEFEE-REASON-CD
                   ADD HEFEE-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HEFEE-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HEFEE-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2380-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF HEFEE-LAST-PAY-DATE > 0 AND HEFEE-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HEFEE-ANNUAL-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD HEFEE-ANNUAL-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD HEFEE-ANNUAL-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HEFEE-ANNUAL-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HEFEE-ANNUAL-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2390-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = HEFEE-INTEREST-YTD / 3
           COMPUTE WS-WORK-AMT-2 = HEFEE-INTEREST-YTD - (WS-WORK-AMT *
               3)
           COMPUTE HEFEE-ANNUAL-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2400-LIMIT-INTEREST.
           IF HEFEE-INTEREST-AMT < 0.00
               MOVE 0.00 TO HEFEE-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HEFEE-INTEREST-AMT > 50000.00
               MOVE 50000.00 TO HEFEE-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2410-FLAG-ANNUAL-FEE.
           IF HEFEE-ANNUAL-FEE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HEFEE-ANNUAL-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2420-COUNT-NOTICE.
           IF HEFEE-NOTICE-CNT < 9999999
               ADD 1 TO HEFEE-NOTICE-CNT
           END-IF
           IF HEFEE-NOTICE-CNT NOT < 10
               MOVE 'Y' TO HEFEE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2430-DESCRIBE-REASON.
           EVALUATE HEFEE-REASON-CD
               WHEN 'KST'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NWS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZJM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ANNUAL FEE AMT.
      *----------------------------------------------------------------*
       2440-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HEFEE-ANNUAL-FEE-AMT ROUNDED =
                   HEFEE-INTEREST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HEFEE-ANNUAL-FEE-AMT
           END-IF
           ADD HEFEE-ANNUAL-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2450-FLAG-ANNUAL-FEE.
           IF HEFEE-ANNUAL-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HEFEE-ANNUAL-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2460-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE HEFEE-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ANNUAL F SCHED TIER FOR THE DRAW AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2470-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ANNUAL-F-SCHED-RATE(1) TO HEFEE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF HEFEE-DRAW-AMT NOT < WS-ANNUAL-F-SCHED-LOW(WS-TX)
                   MOVE WS-ANNUAL-F-SCHED-RATE(WS-TX) TO
                       HEFEE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HEFEE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HEFEE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2480-DESCRIBE-TYPE.
           EVALUATE HEFEE-TYPE-CD
               WHEN 'CD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E800 CONTACT REQUIRED
      * E243 INTEREST AMT NEGATIVE
      * E512 INTEREST AMT OVER 250000
      * E789 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2490-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HEFEE-CONTACT = SPACES
                   MOVE 'E800' TO WS-REJECT-CODE
               WHEN HEFEE-INTEREST-AMT < 0
                   MOVE 'E243' TO WS-REJECT-CODE
               WHEN HEFEE-INTEREST-AMT > 250000.00
                   MOVE 'E512' TO WS-REJECT-CODE
               WHEN HEFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E789' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2500-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF HEFEE-OPEN-DATE > 0 AND HEFEE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HEFEE-DRAW-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD HEFEE-DRAW-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HEFEE-DRAW-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HEFEE-DRAW-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HEFEE-DRAW-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY DRAW PERIOD CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2510-ACCUM-DRAW-PERIOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HEFEE-DRAW-PERIOD-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HEFEE-DRAW-PERIOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2520-ACCRUE-ANNUAL-FEE.
           IF HEFEE-ANNUAL-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HEFEE-ANNUAL-FEE-AMT * HEFEE-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST GRID TIER FOR THE ANNUAL FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2530-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-GRID-RATE(1) TO HEFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HEFEE-ANNUAL-FEE-AMT NOT < WS-INTEREST-GRID-LOW(WS-TX)
                   MOVE WS-INTEREST-GRID-RATE(WS-TX) TO HEFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HEFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HEFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2540-COUNT-RETRY.
           IF HEFEE-RETRY-CNT < 9999999
               ADD 1 TO HEFEE-RETRY-CNT
           END-IF
           IF HEFEE-RETRY-CNT NOT < 6
               MOVE 'Y' TO HEFEE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2550-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST GRID TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2560-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-GRID-RATE(1) TO HEFEE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HEFEE-INTEREST-AMT NOT < WS-INTEREST-GRID-LOW(WS-TX)
                   MOVE WS-INTEREST-GRID-RATE(WS-TX) TO
                       HEFEE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HEFEE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HEFEE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * KST -> NWS WHEN LAST PAY DATE IS PAST
      * NWS -> ZJM WHEN LAST PAY DATE IS PAST
      * ZJM -> KST WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2570-SET-REASON.
           EVALUATE HEFEE-REASON-CD
               WHEN 'KST'
                   IF HEFEE-LAST-PAY-DATE > 0 AND HEFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NWS' TO HEFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NWS'
                   IF HEFEE-LAST-PAY-DATE > 0 AND HEFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZJM' TO HEFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZJM'
                   IF HEFEE-LAST-PAY-DATE > 0 AND HEFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KST' TO HEFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2580-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HEFEE-STATUS-CD
                   ADD HEFEE-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HEFEE-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HEFEE-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2590-FLAG-ANNUAL-FEE.
           IF HEFEE-ANNUAL-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HEFEE-ANNUAL-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2600-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF HEFEE-OPEN-DATE > 0 AND HEFEE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HEFEE-ANNUAL-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HEFEE-ANNUAL-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD HEFEE-ANNUAL-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD HEFEE-ANNUAL-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HEFEE-ANNUAL-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2610-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN HEFEE-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO HEFEE-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HEFEE-PREFERRED-FLAG = 'Y'
                   COMPUTE HEFEE-INTEREST-AMT ROUNDED = WS-WORK-AMT *
                       HEFEE-DISCOUNT / 200
               WHEN HEFEE-DRAW-PERIOD-CD = 'CCV'
                   MOVE 5.00 TO HEFEE-INTEREST-AMT
               WHEN HEFEE-DRAW-PERIOD-CD = 'JSU'
                   MOVE 10.00 TO HEFEE-INTEREST-AMT
               WHEN OTHER
                   COMPUTE HEFEE-INTEREST-AMT ROUNDED = WS-WORK-AMT *
                       HEFEE-DISCOUNT / 100
           END-EVALUATE
           IF HEFEE-INTEREST-AMT > 0 AND HEFEE-INTEREST-AMT < 10.00
               MOVE 10.00 TO HEFEE-INTEREST-AMT
           END-IF
           IF HEFEE-INTEREST-AMT > 35.00
               MOVE 35.00 TO HEFEE-INTEREST-AMT
           END-IF
           ADD HEFEE-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * KST -> NWS WHEN NEXT REVIEW DATE IS PAST
      * NWS -> ZJM WHEN NEXT REVIEW DATE IS PAST
      * ZJM -> KST WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2620-SET-REASON.
           EVALUATE HEFEE-REASON-CD
               WHEN 'KST'
                   IF HEFEE-NEXT-REVIEW-DATE > 0 AND
                       HEFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'NWS' TO HEFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NWS'
                   IF HEFEE-NEXT-REVIEW-DATE > 0 AND
                       HEFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZJM' TO HEFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZJM'
                   IF HEFEE-NEXT-REVIEW-DATE > 0 AND
                       HEFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KST' TO HEFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E190 CONTACT REQUIRED
      * E864 WORK AMT NEGATIVE
      * E420 WORK AMT OVER 99999
      * E261 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2630-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HEFEE-CONTACT = SPACES
                   MOVE 'E190' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E864' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E420' TO WS-REJECT-CODE
               WHEN HEFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E261' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E446 NAME REQUIRED
      * E988 ANNUAL FEE YTD NEGATIVE
      * E738 ANNUAL FEE YTD OVER 25000
      * E087 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2640-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HEFEE-NAME = SPACES
                   MOVE 'E446' TO WS-REJECT-CODE
               WHEN HEFEE-ANNUAL-FEE-YTD < 0
                   MOVE 'E988' TO WS-REJECT-CODE
               WHEN HEFEE-ANNUAL-FEE-YTD > 25000.00
                   MOVE 'E738' TO WS-REJECT-CODE
               WHEN HEFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E087' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2650-DESCRIBE-DRAW-PERIOD.
           EVALUATE HEFEE-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST GRID TIER FOR THE ANNUAL FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2660-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-GRID-RATE(1) TO HEFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HEFEE-ANNUAL-FEE-AMT NOT < WS-INTEREST-GRID-LOW(WS-TX)
                   MOVE WS-INTEREST-GRID-RATE(WS-TX) TO HEFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HEFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HEFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CD -> JW WHEN LAST PAY DATE IS PAST
      * JW -> KV WHEN LAST PAY DATE IS PAST
      * KV -> WS WHEN LAST PAY DATE IS PAST
      * WS -> XL WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2670-SET-TYPE.
           EVALUATE HEFEE-TYPE-CD
               WHEN 'CD'
                   IF HEFEE-LAST-PAY-DATE > 0 AND HEFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JW' TO HEFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JW'
                   IF HEFEE-LAST-PAY-DATE > 0 AND HEFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KV' TO HEFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF HEFEE-LAST-PAY-DATE > 0 AND HEFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WS' TO HEFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WS'
                   IF HEFEE-LAST-PAY-DATE > 0 AND HEFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO HEFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2680-LIMIT-ANNUAL-FEE.
           IF HEFEE-ANNUAL-FEE-AMT < 100.00
               MOVE 100.00 TO HEFEE-ANNUAL-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HEFEE-ANNUAL-FEE-AMT > 2500.00
               MOVE 2500.00 TO HEFEE-ANNUAL-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ANNUAL FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2690-SPLIT-ANNUAL-FEE.
           COMPUTE WS-WORK-AMT = HEFEE-ANNUAL-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = HEFEE-ANNUAL-FEE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE HEFEE-BALANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2700-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF HEFEE-NEXT-REVIEW-DATE > 0 AND HEFEE-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HEFEE-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HEFEE-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD HEFEE-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HEFEE-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HEFEE-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2710-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF HEFEE-NEXT-REVIEW-DATE > 0 AND HEFEE-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HEFEE-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HEFEE-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD HEFEE-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HEFEE-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HEFEE-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE BALANCE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2720-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN HEFEE-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HEFEE-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = HEFEE-BALANCE-YTD *
                       HEFEE-PENALTY-PCT / 200
               WHEN HEFEE-STATUS-CD = 'GR'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN HEFEE-STATUS-CD = 'JC'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = HEFEE-BALANCE-YTD *
                       HEFEE-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE BALANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2730-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HEFEE-BALANCE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE DRAW AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2740-PRORATE-DRAW.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HEFEE-DRAW-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2750-COUNT-NOTICE.
           IF HEFEE-NOTICE-CNT < 9999999
               ADD 1 TO HEFEE-NOTICE-CNT
           END-IF
           IF HEFEE-NOTICE-CNT NOT < 12
               MOVE 'Y' TO HEFEE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2760-DESCRIBE-DRAW-PERIOD.
           EVALUATE HEFEE-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2770-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF HEFEE-OPEN-DATE > 0 AND HEFEE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HEFEE-DRAW-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HEFEE-DRAW-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD HEFEE-DRAW-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HEFEE-DRAW-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HEFEE-DRAW-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2780-LIMIT-INTEREST.
           IF HEFEE-INTEREST-AMT < 10.00
               MOVE 10.00 TO HEFEE-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HEFEE-INTEREST-AMT > 500.00
               MOVE 500.00 TO HEFEE-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ANNUAL FEE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2790-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HEFEE-TYPE-CD
                   ADD HEFEE-ANNUAL-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HEFEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HEFEE-ANNUAL-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2800-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * HEFEE-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO HEFEE-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ANNUAL FEE AMT BY DRAW PERIOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2810-ACCUM-DRAW-PERIOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HEFEE-DRAW-PERIOD-CD
                   ADD HEFEE-ANNUAL-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HEFEE-DRAW-PERIOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HEFEE-ANNUAL-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BALANCE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2820-ACCRUE-BALANCE.
           IF HEFEE-BALANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HEFEE-BALANCE-YTD * HEFEE-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO HEFEE-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2830-ACCRUE-INTEREST.
           IF HEFEE-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HEFEE-INTEREST-AMT * HEFEE-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO HEFEE-ANNUAL-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2840-COUNT-RETRY.
           IF HEFEE-RETRY-CNT < 9999999
               ADD 1 TO HEFEE-RETRY-CNT
           END-IF
           IF HEFEE-RETRY-CNT NOT < 3
               MOVE 'Y' TO HEFEE-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2850-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF HEFEE-NEXT-DUE-DATE > 0 AND HEFEE-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEFEE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HEFEE-ANNUAL-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HEFEE-ANNUAL-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD HEFEE-ANNUAL-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HEFEE-ANNUAL-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HEFEE-ANNUAL-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2860-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF HEFEE-NEXT-REVIEW-DATE > 0 AND HEFEE-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY DRAW PERIOD CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2870-ACCUM-DRAW-PERIOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HEFEE-DRAW-PERIOD-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HEFEE-DRAW-PERIOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E076 CONTACT REQUIRED
      * E094 WORK AMT NEGATIVE
      * E767 WORK AMT OVER 250000
      * E961 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2880-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HEFEE-CONTACT = SPACES
                   MOVE 'E076' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E094' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E767' TO WS-REJECT-CODE
               WHEN HEFEE-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E961' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2890-COUNT-RETRY.
           IF HEFEE-RETRY-CNT < 9999999
               ADD 1 TO HEFEE-RETRY-CNT
           END-IF
           IF HEFEE-RETRY-CNT NOT < 10
               MOVE 'Y' TO HEFEE-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E992 NAME REQUIRED
      * E831 BALANCE YTD NEGATIVE
      * E922 BALANCE YTD OVER 25000
      * E042 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2900-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HEFEE-NAME = SPACES
                   MOVE 'E992' TO WS-REJECT-CODE
               WHEN HEFEE-BALANCE-YTD < 0
                   MOVE 'E831' TO WS-REJECT-CODE
               WHEN HEFEE-BALANCE-YTD > 25000.00
                   MOVE 'E922' TO WS-REJECT-CODE
               WHEN HEFEE-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E042' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2910-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF HEFEE-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO HEFEE-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2920-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF HEFEE-NEXT-REVIEW-DATE > 0 AND HEFEE-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HEFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HEFEE-DRAW-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HEFEE-DRAW-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD HEFEE-DRAW-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD HEFEE-DRAW-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HEFEE-DRAW-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2930-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN HEFEE-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO HEFEE-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HEFEE-PREFERRED-FLAG = 'Y'
                   COMPUTE HEFEE-INTEREST-AMT ROUNDED = WS-WORK-AMT-2 *
                       HEFEE-DISCOUNT / 200
               WHEN HEFEE-DRAW-PERIOD-CD = 'CCV'
                   MOVE 12.00 TO HEFEE-INTEREST-AMT
               WHEN HEFEE-DRAW-PERIOD-CD = 'JSU'
                   MOVE 15.00 TO HEFEE-INTEREST-AMT
               WHEN OTHER
                   COMPUTE HEFEE-INTEREST-AMT ROUNDED = WS-WORK-AMT-2 *
                       HEFEE-DISCOUNT / 100
           END-EVALUATE
           IF HEFEE-INTEREST-AMT > 0 AND HEFEE-INTEREST-AMT < 5.00
               MOVE 5.00 TO HEFEE-INTEREST-AMT
           END-IF
           IF HEFEE-INTEREST-AMT > 250.00
               MOVE 250.00 TO HEFEE-INTEREST-AMT
           END-IF
           ADD HEFEE-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2940-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * HEFEE-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO HEFEE-ANNUAL-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO HEFEE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO HEFEE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO HEFEE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'HEFEE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
