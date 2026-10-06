      *================================================================*
      * PROGRAM  : EUFEE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : EURO CONVERSION RULE - STATUS. CALLED BY THE EURO   *
      *            CONVERSION BATCH AND ONLINE PROGRAMS.               *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-06-24 EWS  ORIGINAL                                       *
      * 1996-12-28 RJT  ROUNDING FEE SCHEDULE UPDATE INC8898           *
      * 1997-03-08 RJT  ROUNDING CUTOFF MOVED CHG5470                  *
      * 1998-03-01 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-02-02 GAF  NEW FEE REPORT COLUMNS INC1721                 *
      * 2003-11-21 TWH  TRIANGULAR TABLE EXPANDED REQ0292              *
      * 2004-09-04 TWH  RESIDUAL FEE SCHEDULE UPDATE INC8232           *
      * 2005-07-16 EWS  NEW EURO REPORT COLUMNS INC9520                *
      * 2005-02-06 SKN  PERFORMANCE - EURO READ AHEAD REQ1405          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EUFEE.
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
                                           VALUE 15.000000.
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
      * WS-CONVERTE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONVERTE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000829000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000615500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000521500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000089500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003250000000829000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003350000000459500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013350000000524500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018350000000239500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028350000000745000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028850000000775000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031350000000186000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036350000000894000'.
       01  WS-CONVERTE-SCHED-TBL REDEFINES WS-CONVERTE-SCHED-VALUES.
           05  WS-CONVERTE-SCHED-ROW OCCURS 12 TIMES.
               10  WS-CONVERTE-SCHED-LOW   PIC 9(09)V99.
               10  WS-CONVERTE-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000174000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000489000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000838000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008500000000276500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009500000000887500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000394000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000005000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000811000'.
       01  WS-FEE-TIER-TBL REDEFINES WS-FEE-TIER-VALUES.
           05  WS-FEE-TIER-ROW OCCURS 8 TIMES.
               10  WS-FEE-TIER-LOW         PIC 9(09)V99.
               10  WS-FEE-TIER-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RESIDUAL-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RESIDUAL-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000285500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000436500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000721500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000796000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003050000000698500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000065000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013550000000502000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016050000000387000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016300000000698500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016400000000646500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026400000000773500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026450000000198000'.
       01  WS-RESIDUAL-SCHED-TBL REDEFINES WS-RESIDUAL-SCHED-VALUES.
           05  WS-RESIDUAL-SCHED-ROW OCCURS 12 TIMES.
               10  WS-RESIDUAL-SCHED-LOW   PIC 9(09)V99.
               10  WS-RESIDUAL-SCHED-RATE  PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY EUFEEP.

       PROCEDURE DIVISION USING EUFEE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-AGE-EFFECTIVE
           PERFORM 2010-CHECK-NOTICE
           PERFORM 2020-ASSESS-PREFERRED
           PERFORM 2030-FLAG-NATIONAL
           PERFORM 2040-SELECT-FEE-PCT
           PERFORM 2050-ACCUM-CHANNEL
           PERFORM 2060-FLAG-CONVERTED
           PERFORM 2070-FLAG-TRIANGULAR
           PERFORM 2080-ACCRUE-EURO
           PERFORM 2090-AGE-LAST-MAINT
           PERFORM 2100-SPLIT-FEE
           PERFORM 2110-CHECK-NOTICE
           PERFORM 2120-DESCRIBE-REGION
           PERFORM 2130-DESCRIBE-TYPE
           PERFORM 2140-EDIT-REF-NO
           PERFORM 2150-CHECK-LAST-PAY
           PERFORM 2160-FLAG-ITEM
           PERFORM 2170-EDIT-CUST-ID
           PERFORM 2180-PRORATE-TRIANGULAR
           PERFORM 2190-COUNT-PAYMENT
           PERFORM 2200-ACCUM-TYPE
           PERFORM 2210-ACCUM-CONV-METHOD
           PERFORM 2220-EDIT-CUST-ID
           PERFORM 2230-ACCRUE-CONVERTED
           PERFORM 2240-SELECT-RATE
           PERFORM 2250-COUNT-CYCLE
           PERFORM 2260-COUNT-PAYMENT
           PERFORM 2270-AGE-EFFECTIVE
           PERFORM 2280-EDIT-CUST-ID
           PERFORM 2290-SPLIT-EURO
           PERFORM 2300-AGE-LAST-PAY
           PERFORM 2310-FLAG-CONVERTED
           PERFORM 2320-FLAG-ITEM
           PERFORM 2330-ACCUM-CONV-METHOD
           PERFORM 2340-AGE-LAST-MAINT
           PERFORM 2350-ASSESS-PREFERRED
           PERFORM 2360-AGE-LAST-PAY
           PERFORM 2370-CHECK-EFFECTIVE
           PERFORM 2380-DESCRIBE-CONV-METHOD
           PERFORM 2390-ACCUM-CONV-METHOD
           PERFORM 2400-EDIT-REF-NO
           PERFORM 2410-AGE-LAST-MAINT
           PERFORM 2420-SPLIT-CONVERTED
           PERFORM 2430-EDIT-CUST-ID
           PERFORM 2440-SPLIT-EURO
           PERFORM 2450-ASSESS-PREFERRED
           PERFORM 2460-ACCUM-REGION
           PERFORM 2470-ASSESS-OVERRIDE
           PERFORM 2480-SPLIT-FEE
           PERFORM 2490-PRORATE-FEE
           PERFORM 2500-ASSESS-OVERRIDE
           PERFORM 2510-SPLIT-NATIONAL
           PERFORM 2520-EDIT-REF-NO
           PERFORM 2530-DESCRIBE-CONV-METHOD
           PERFORM 2540-FLAG-NATIONAL
           PERFORM 2550-ACCUM-REGION
           PERFORM 2560-SPLIT-FEE
           PERFORM 2570-ASSESS-OVERRIDE
           PERFORM 2580-SELECT-RATE
           PERFORM 2590-SPLIT-NATIONAL
           PERFORM 2600-EDIT-REF-NO
           PERFORM 2610-FLAG-NATIONAL
           PERFORM 2620-SET-REGION
           PERFORM 2630-CHECK-NOTICE
           PERFORM 2640-CHECK-NOTICE
           PERFORM 2650-EDIT-CUST-ID
           PERFORM 2660-SPLIT-CONVERTED
           PERFORM 2670-ASSESS-REVIEW
           PERFORM 2680-ACCRUE-CONVERTED
           PERFORM 2690-PRORATE-NATIONAL
           PERFORM 2700-PRORATE-NATIONAL
           PERFORM 2710-FLAG-TRIANGULAR
           PERFORM 2720-COUNT-PAYMENT
           PERFORM 2730-DESCRIBE-REGION
           PERFORM 2740-FLAG-NATIONAL
           PERFORM 2750-DESCRIBE-TYPE
           PERFORM 2760-SET-REGION
           PERFORM 2770-ASSESS-REVIEW
           PERFORM 2780-ASSESS-REVIEW
           PERFORM 2790-CHECK-LAST-PAY
           PERFORM 2800-SET-CHANNEL
           PERFORM 2810-COUNT-CYCLE
           PERFORM 2820-AGE-NOTICE
           PERFORM 2830-PRORATE-NATIONAL
           PERFORM 2840-FLAG-ITEM
           PERFORM 2850-COUNT-PAYMENT
           PERFORM 2860-ACCUM-CHANNEL
           PERFORM 2870-ACCRUE-NATIONAL
           PERFORM 2880-ASSESS-REVIEW
           PERFORM 2890-SELECT-FEE-PCT
           PERFORM 2900-FLAG-EURO
           PERFORM 2910-SET-CONV-METHOD
           PERFORM 2920-DESCRIBE-REGION
           PERFORM 2930-FLAG-ITEM
           PERFORM 2940-COUNT-CYCLE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO EUFEE-RETURN-CODE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2000-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF EUFEE-EFFECTIVE-DATE > 0 AND EUFEE-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUFEE-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUFEE-CONVERTED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EUFEE-CONVERTED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD EUFEE-CONVERTED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD EUFEE-CONVERTED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUFEE-CONVERTED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2010-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF EUFEE-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO EUFEE-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NATIONAL YTD FEE ON THE FEE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2020-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EUFEE-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO EUFEE-NATIONAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUFEE-OVERRIDE-FLAG = 'Y'
                   COMPUTE EUFEE-NATIONAL-YTD ROUNDED = EUFEE-FEE-YTD *
                       EUFEE-RATE / 200
               WHEN EUFEE-CHANNEL-CD = 'AEA'
                   MOVE 3.00 TO EUFEE-NATIONAL-YTD
               WHEN EUFEE-CHANNEL-CD = 'EBM'
                   MOVE 3.00 TO EUFEE-NATIONAL-YTD
               WHEN OTHER
                   COMPUTE EUFEE-NATIONAL-YTD ROUNDED = EUFEE-FEE-YTD *
                       EUFEE-RATE / 100
           END-EVALUATE
           IF EUFEE-NATIONAL-YTD > 0 AND EUFEE-NATIONAL-YTD < 1.00
               MOVE 1.00 TO EUFEE-NATIONAL-YTD
           END-IF
           IF EUFEE-NATIONAL-YTD > 150.00
               MOVE 150.00 TO EUFEE-NATIONAL-YTD
           END-IF
           ADD EUFEE-NATIONAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NATIONAL YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2030-FLAG-NATIONAL.
           IF EUFEE-NATIONAL-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EUFEE-NATIONAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE SCHED TIER FOR THE NATIONAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2040-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-SCHED-RATE(1) TO EUFEE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF EUFEE-NATIONAL-YTD NOT < WS-CONVERTE-SCHED-LOW(WS-TX)
                   MOVE WS-CONVERTE-SCHED-RATE(WS-TX) TO EUFEE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUFEE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUFEE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EURO YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2050-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUFEE-CHANNEL-CD
                   ADD EUFEE-EURO-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUFEE-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUFEE-EURO-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CONVERTED YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2060-FLAG-CONVERTED.
           IF EUFEE-CONVERTED-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EUFEE-CONVERTED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRIANGULAR YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2070-FLAG-TRIANGULAR.
           IF EUFEE-TRIANGULAR-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EUFEE-TRIANGULAR-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EURO YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2080-ACCRUE-EURO.
           IF EUFEE-EURO-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUFEE-EURO-YTD * EUFEE-RATE / 100 / 365
               ADD WS-WORK-AMT TO EUFEE-CONVERTED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2090-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF EUFEE-LAST-MAINT-DATE > 0 AND EUFEE-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUFEE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUFEE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EUFEE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD EUFEE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD EUFEE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUFEE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2100-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = EUFEE-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = EUFEE-FEE-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2110-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF EUFEE-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO EUFEE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2120-DESCRIBE-REGION.
           EVALUATE EUFEE-REGION-CD
               WHEN 'DTL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HYD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JUL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WSE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YAU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'YJB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2130-DESCRIBE-TYPE.
           EVALUATE EUFEE-TYPE-CD
               WHEN 'AGL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'SMF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UEC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VJA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WPH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E561 REF NO REQUIRED
      * E787 FEE YTD NEGATIVE
      * E128 FEE YTD OVER 50000
      * E264 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2140-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUFEE-REF-NO = SPACES
                   MOVE 'E561' TO WS-REJECT-CODE
               WHEN EUFEE-FEE-YTD < 0
                   MOVE 'E787' TO WS-REJECT-CODE
               WHEN EUFEE-FEE-YTD > 50000.00
                   MOVE 'E128' TO WS-REJECT-CODE
               WHEN EUFEE-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E264' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2150-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EUFEE-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO EUFEE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2160-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E786 CUST ID REQUIRED
      * E553 NATIONAL YTD NEGATIVE
      * E459 NATIONAL YTD OVER 5000
      * E252 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2170-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUFEE-CUST-ID = SPACES
                   MOVE 'E786' TO WS-REJECT-CODE
               WHEN EUFEE-NATIONAL-YTD < 0
                   MOVE 'E553' TO WS-REJECT-CODE
               WHEN EUFEE-NATIONAL-YTD > 5000.00
                   MOVE 'E459' TO WS-REJECT-CODE
               WHEN EUFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E252' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EURO YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TRIANGULAR YTD.
      *----------------------------------------------------------------*
       2180-PRORATE-TRIANGULAR.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUFEE-TRIANGULAR-YTD ROUNDED =
                   EUFEE-EURO-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EUFEE-TRIANGULAR-YTD
           END-IF
           ADD EUFEE-TRIANGULAR-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2190-COUNT-PAYMENT.
           IF EUFEE-PAYMENT-CNT < 9999999
               ADD 1 TO EUFEE-PAYMENT-CNT
           END-IF
           IF EUFEE-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO EUFEE-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EURO YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2200-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUFEE-TYPE-CD
                   ADD EUFEE-EURO-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUFEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUFEE-EURO-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CONV METHOD CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2210-ACCUM-CONV-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUFEE-CONV-METHOD-CD
                   ADD EUFEE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUFEE-CONV-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUFEE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E390 CUST ID REQUIRED
      * E315 NATIONAL AMT NEGATIVE
      * E033 NATIONAL AMT OVER 25000
      * E533 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2220-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUFEE-CUST-ID = SPACES
                   MOVE 'E390' TO WS-REJECT-CODE
               WHEN EUFEE-NATIONAL-AMT < 0
                   MOVE 'E315' TO WS-REJECT-CODE
               WHEN EUFEE-NATIONAL-AMT > 25000.00
                   MOVE 'E033' TO WS-REJECT-CODE
               WHEN EUFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E533' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONVERTED YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2230-ACCRUE-CONVERTED.
           IF EUFEE-CONVERTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUFEE-CONVERTED-YTD * EUFEE-RATE / 100 / 365
               ADD WS-WORK-AMT TO EUFEE-NATIONAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE SCHED TIER FOR THE CONVERTED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2240-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-SCHED-RATE(1) TO EUFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF EUFEE-CONVERTED-YTD NOT < WS-CONVERTE-SCHED-LOW(WS-TX)
                   MOVE WS-CONVERTE-SCHED-RATE(WS-TX) TO EUFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2250-COUNT-CYCLE.
           IF EUFEE-CYCLE-CNT < 9999999
               ADD 1 TO EUFEE-CYCLE-CNT
           END-IF
           IF EUFEE-CYCLE-CNT NOT < 6
               MOVE 'Y' TO EUFEE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2260-COUNT-PAYMENT.
           IF EUFEE-PAYMENT-CNT < 9999999
               ADD 1 TO EUFEE-PAYMENT-CNT
           END-IF
           IF EUFEE-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO EUFEE-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2270-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF EUFEE-EFFECTIVE-DATE > 0 AND EUFEE-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUFEE-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUFEE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EUFEE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EUFEE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EUFEE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUFEE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E361 CUST ID REQUIRED
      * E933 NATIONAL YTD NEGATIVE
      * E449 NATIONAL YTD OVER 5000
      * E238 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2280-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUFEE-CUST-ID = SPACES
                   MOVE 'E361' TO WS-REJECT-CODE
               WHEN EUFEE-NATIONAL-YTD < 0
                   MOVE 'E933' TO WS-REJECT-CODE
               WHEN EUFEE-NATIONAL-YTD > 5000.00
                   MOVE 'E449' TO WS-REJECT-CODE
               WHEN EUFEE-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E238' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EURO YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2290-SPLIT-EURO.
           COMPUTE WS-WORK-AMT = EUFEE-EURO-YTD / 2
           COMPUTE WS-WORK-AMT-2 = EUFEE-EURO-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2300-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EUFEE-LAST-PAY-DATE > 0 AND EUFEE-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUFEE-NATIONAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EUFEE-NATIONAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EUFEE-NATIONAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EUFEE-NATIONAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUFEE-NATIONAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONVERTED YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2310-FLAG-CONVERTED.
           IF EUFEE-CONVERTED-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EUFEE-CONVERTED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2320-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONVERTED YTD BY CONV METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2330-ACCUM-CONV-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUFEE-CONV-METHOD-CD
                   ADD EUFEE-CONVERTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUFEE-CONV-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUFEE-CONVERTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2340-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF EUFEE-LAST-MAINT-DATE > 0 AND EUFEE-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUFEE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUFEE-NATIONAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EUFEE-NATIONAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD EUFEE-NATIONAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EUFEE-NATIONAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUFEE-NATIONAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2350-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EUFEE-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO EUFEE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUFEE-OVERRIDE-FLAG = 'Y'
                   COMPUTE EUFEE-FEE-YTD ROUNDED = WS-WORK-AMT-2 *
                       EUFEE-RATE / 200
               WHEN EUFEE-REGION-CD = 'DTL'
                   MOVE 5.00 TO EUFEE-FEE-YTD
               WHEN EUFEE-REGION-CD = 'HYD'
                   MOVE 15.00 TO EUFEE-FEE-YTD
               WHEN OTHER
                   COMPUTE EUFEE-FEE-YTD ROUNDED = WS-WORK-AMT-2 *
                       EUFEE-RATE / 100
           END-EVALUATE
           IF EUFEE-FEE-YTD > 0 AND EUFEE-FEE-YTD < 1.00
               MOVE 1.00 TO EUFEE-FEE-YTD
           END-IF
           IF EUFEE-FEE-YTD > 25.00
               MOVE 25.00 TO EUFEE-FEE-YTD
           END-IF
           ADD EUFEE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2360-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EUFEE-LAST-PAY-DATE > 0 AND EUFEE-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2370-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF EUFEE-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUFEE-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO EUFEE-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CONV METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2380-DESCRIBE-CONV-METHOD.
           EVALUATE EUFEE-CONV-METHOD-CD
               WHEN 'GL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CONV METHOD CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2390-ACCUM-CONV-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUFEE-CONV-METHOD-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUFEE-CONV-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E350 REF NO REQUIRED
      * E902 WORK AMT 2 NEGATIVE
      * E631 WORK AMT 2 OVER 250000
      * E063 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2400-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUFEE-REF-NO = SPACES
                   MOVE 'E350' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E902' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E631' TO WS-REJECT-CODE
               WHEN EUFEE-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E063' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2410-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF EUFEE-LAST-MAINT-DATE > 0 AND EUFEE-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUFEE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CONVERTED YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2420-SPLIT-CONVERTED.
           COMPUTE WS-WORK-AMT = EUFEE-CONVERTED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = EUFEE-CONVERTED-YTD - (WS-WORK-AMT *
               3)
           COMPUTE EUFEE-EURO-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E821 CUST ID REQUIRED
      * E193 WORK AMT NEGATIVE
      * E510 WORK AMT OVER 99999
      * E799 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2430-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUFEE-CUST-ID = SPACES
                   MOVE 'E821' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E193' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E510' TO WS-REJECT-CODE
               WHEN EUFEE-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E799' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EURO YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2440-SPLIT-EURO.
           COMPUTE WS-WORK-AMT = EUFEE-EURO-YTD / 3
           COMPUTE WS-WORK-AMT-2 = EUFEE-EURO-YTD - (WS-WORK-AMT * 3)
           COMPUTE EUFEE-NATIONAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NATIONAL FEE ON THE TRIANGULAR YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2450-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EUFEE-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO EUFEE-NATIONAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUFEE-OVERRIDE-FLAG = 'Y'
                   COMPUTE EUFEE-NATIONAL-AMT ROUNDED =
                       EUFEE-TRIANGULAR-YTD * EUFEE-RATE / 200
               WHEN EUFEE-REGION-CD = 'DTL'
                   MOVE 5.00 TO EUFEE-NATIONAL-AMT
               WHEN EUFEE-REGION-CD = 'HYD'
                   MOVE 10.00 TO EUFEE-NATIONAL-AMT
               WHEN OTHER
                   COMPUTE EUFEE-NATIONAL-AMT ROUNDED =
                       EUFEE-TRIANGULAR-YTD * EUFEE-RATE / 100
           END-EVALUATE
           IF EUFEE-NATIONAL-AMT > 0 AND EUFEE-NATIONAL-AMT < 10.00
               MOVE 10.00 TO EUFEE-NATIONAL-AMT
           END-IF
           IF EUFEE-NATIONAL-AMT > 25.00
               MOVE 25.00 TO EUFEE-NATIONAL-AMT
           END-IF
           ADD EUFEE-NATIONAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE NATIONAL YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2460-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUFEE-REGION-CD
                   ADD EUFEE-NATIONAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUFEE-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUFEE-NATIONAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NATIONAL YTD FEE ON THE CONVERTED YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2470-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN EUFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO EUFEE-NATIONAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUFEE-PREFERRED-FLAG = 'Y'
                   COMPUTE EUFEE-NATIONAL-YTD ROUNDED =
                       EUFEE-CONVERTED-YTD * EUFEE-RATE / 200
               WHEN EUFEE-CHANNEL-CD = 'AEA'
                   MOVE 15.00 TO EUFEE-NATIONAL-YTD
               WHEN EUFEE-CHANNEL-CD = 'EBM'
                   MOVE 3.00 TO EUFEE-NATIONAL-YTD
               WHEN OTHER
                   COMPUTE EUFEE-NATIONAL-YTD ROUNDED =
                       EUFEE-CONVERTED-YTD * EUFEE-RATE / 100
           END-EVALUATE
           IF EUFEE-NATIONAL-YTD > 0 AND EUFEE-NATIONAL-YTD < 2.00
               MOVE 2.00 TO EUFEE-NATIONAL-YTD
           END-IF
           IF EUFEE-NATIONAL-YTD > 25.00
               MOVE 25.00 TO EUFEE-NATIONAL-YTD
           END-IF
           ADD EUFEE-NATIONAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2480-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = EUFEE-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = EUFEE-FEE-YTD - (WS-WORK-AMT * 3)
           COMPUTE EUFEE-NATIONAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2490-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   EUFEE-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE EURO YTD FEE ON THE NATIONAL AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2500-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN EUFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO EUFEE-EURO-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUFEE-REVIEW-FLAG = 'Y'
                   COMPUTE EUFEE-EURO-YTD ROUNDED = EUFEE-NATIONAL-AMT *
                       EUFEE-FEE-PCT / 200
               WHEN EUFEE-CONV-METHOD-CD = 'GL'
                   MOVE 5.00 TO EUFEE-EURO-YTD
               WHEN EUFEE-CONV-METHOD-CD = 'JA'
                   MOVE 7.00 TO EUFEE-EURO-YTD
               WHEN OTHER
                   COMPUTE EUFEE-EURO-YTD ROUNDED = EUFEE-NATIONAL-AMT *
                       EUFEE-FEE-PCT / 100
           END-EVALUATE
           IF EUFEE-EURO-YTD > 0 AND EUFEE-EURO-YTD < 1.00
               MOVE 1.00 TO EUFEE-EURO-YTD
           END-IF
           IF EUFEE-EURO-YTD > 35.00
               MOVE 35.00 TO EUFEE-EURO-YTD
           END-IF
           ADD EUFEE-EURO-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT NATIONAL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2510-SPLIT-NATIONAL.
           COMPUTE WS-WORK-AMT = EUFEE-NATIONAL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = EUFEE-NATIONAL-AMT - (WS-WORK-AMT *
               2)
           COMPUTE EUFEE-NATIONAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E654 REF NO REQUIRED
      * E499 NATIONAL AMT NEGATIVE
      * E711 NATIONAL AMT OVER 250000
      * E299 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2520-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUFEE-REF-NO = SPACES
                   MOVE 'E654' TO WS-REJECT-CODE
               WHEN EUFEE-NATIONAL-AMT < 0
                   MOVE 'E499' TO WS-REJECT-CODE
               WHEN EUFEE-NATIONAL-AMT > 250000.00
                   MOVE 'E711' TO WS-REJECT-CODE
               WHEN EUFEE-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E299' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CONV METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2530-DESCRIBE-CONV-METHOD.
           EVALUATE EUFEE-CONV-METHOD-CD
               WHEN 'GL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NATIONAL AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2540-FLAG-NATIONAL.
           IF EUFEE-NATIONAL-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EUFEE-NATIONAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NATIONAL AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2550-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUFEE-REGION-CD
                   ADD EUFEE-NATIONAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUFEE-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUFEE-NATIONAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2560-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = EUFEE-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = EUFEE-FEE-YTD - (WS-WORK-AMT * 12)
           COMPUTE EUFEE-NATIONAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE CONVERTED YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2570-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN EUFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO EUFEE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUFEE-REVIEW-FLAG = 'Y'
                   COMPUTE EUFEE-FEE-YTD ROUNDED = EUFEE-CONVERTED-YTD *
                       EUFEE-FEE-PCT / 200
               WHEN EUFEE-CHANNEL-CD = 'AEA'
                   MOVE 7.00 TO EUFEE-FEE-YTD
               WHEN EUFEE-CHANNEL-CD = 'EBM'
                   MOVE 7.00 TO EUFEE-FEE-YTD
               WHEN OTHER
                   COMPUTE EUFEE-FEE-YTD ROUNDED = EUFEE-CONVERTED-YTD *
                       EUFEE-FEE-PCT / 100
           END-EVALUATE
           IF EUFEE-FEE-YTD > 0 AND EUFEE-FEE-YTD < 1.00
               MOVE 1.00 TO EUFEE-FEE-YTD
           END-IF
           IF EUFEE-FEE-YTD > 25.00
               MOVE 25.00 TO EUFEE-FEE-YTD
           END-IF
           ADD EUFEE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2580-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-SCHED-RATE(1) TO EUFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-CONVERTE-SCHED-LOW(WS-TX)
                   MOVE WS-CONVERTE-SCHED-RATE(WS-TX) TO EUFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT NATIONAL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2590-SPLIT-NATIONAL.
           COMPUTE WS-WORK-AMT = EUFEE-NATIONAL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = EUFEE-NATIONAL-AMT - (WS-WORK-AMT *
               3)
           COMPUTE EUFEE-EURO-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E650 REF NO REQUIRED
      * E274 NATIONAL AMT NEGATIVE
      * E792 NATIONAL AMT OVER 50000
      * E394 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2600-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUFEE-REF-NO = SPACES
                   MOVE 'E650' TO WS-REJECT-CODE
               WHEN EUFEE-NATIONAL-AMT < 0
                   MOVE 'E274' TO WS-REJECT-CODE
               WHEN EUFEE-NATIONAL-AMT > 50000.00
                   MOVE 'E792' TO WS-REJECT-CODE
               WHEN EUFEE-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E394' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NATIONAL YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2610-FLAG-NATIONAL.
           IF EUFEE-NATIONAL-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EUFEE-NATIONAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DTL -> HYD WHEN EFFECTIVE DATE IS PAST
      * HYD -> JUL WHEN EFFECTIVE DATE IS PAST
      * JUL -> WSE WHEN EFFECTIVE DATE IS PAST
      * WSE -> YAU WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2620-SET-REGION.
           EVALUATE EUFEE-REGION-CD
               WHEN 'DTL'
                   IF EUFEE-EFFECTIVE-DATE > 0 AND EUFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HYD' TO EUFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HYD'
                   IF EUFEE-EFFECTIVE-DATE > 0 AND EUFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JUL' TO EUFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JUL'
                   IF EUFEE-EFFECTIVE-DATE > 0 AND EUFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WSE' TO EUFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WSE'
                   IF EUFEE-EFFECTIVE-DATE > 0 AND EUFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'YAU' TO EUFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2630-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF EUFEE-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO EUFEE-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2640-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF EUFEE-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO EUFEE-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E155 CUST ID REQUIRED
      * E834 NATIONAL AMT NEGATIVE
      * E268 NATIONAL AMT OVER 50000
      * E188 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2650-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUFEE-CUST-ID = SPACES
                   MOVE 'E155' TO WS-REJECT-CODE
               WHEN EUFEE-NATIONAL-AMT < 0
                   MOVE 'E834' TO WS-REJECT-CODE
               WHEN EUFEE-NATIONAL-AMT > 50000.00
                   MOVE 'E268' TO WS-REJECT-CODE
               WHEN EUFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E188' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONVERTED YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2660-SPLIT-CONVERTED.
           COMPUTE WS-WORK-AMT = EUFEE-CONVERTED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = EUFEE-CONVERTED-YTD - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONVERTED YTD FEE ON THE NATIONAL YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2670-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN EUFEE-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO EUFEE-CONVERTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUFEE-OVERRIDE-FLAG = 'Y'
                   COMPUTE EUFEE-CONVERTED-YTD ROUNDED =
                       EUFEE-NATIONAL-YTD * EUFEE-FEE-PCT / 200
               WHEN EUFEE-TYPE-CD = 'AGL'
                   MOVE 5.00 TO EUFEE-CONVERTED-YTD
               WHEN EUFEE-TYPE-CD = 'SMF'
                   MOVE 12.00 TO EUFEE-CONVERTED-YTD
               WHEN OTHER
                   COMPUTE EUFEE-CONVERTED-YTD ROUNDED =
                       EUFEE-NATIONAL-YTD * EUFEE-FEE-PCT / 100
           END-EVALUATE
           IF EUFEE-CONVERTED-YTD > 0 AND EUFEE-CONVERTED-YTD < 5.00
               MOVE 5.00 TO EUFEE-CONVERTED-YTD
           END-IF
           IF EUFEE-CONVERTED-YTD > 50.00
               MOVE 50.00 TO EUFEE-CONVERTED-YTD
           END-IF
           ADD EUFEE-CONVERTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONVERTED YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2680-ACCRUE-CONVERTED.
           IF EUFEE-CONVERTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUFEE-CONVERTED-YTD * EUFEE-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NATIONAL AMT.
      *----------------------------------------------------------------*
       2690-PRORATE-NATIONAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUFEE-NATIONAL-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EUFEE-NATIONAL-AMT
           END-IF
           ADD EUFEE-NATIONAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NATIONAL AMT.
      *----------------------------------------------------------------*
       2700-PRORATE-NATIONAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUFEE-NATIONAL-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EUFEE-NATIONAL-AMT
           END-IF
           ADD EUFEE-NATIONAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRIANGULAR YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2710-FLAG-TRIANGULAR.
           IF EUFEE-TRIANGULAR-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EUFEE-TRIANGULAR-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2720-COUNT-PAYMENT.
           IF EUFEE-PAYMENT-CNT < 9999999
               ADD 1 TO EUFEE-PAYMENT-CNT
           END-IF
           IF EUFEE-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO EUFEE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2730-DESCRIBE-REGION.
           EVALUATE EUFEE-REGION-CD
               WHEN 'DTL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HYD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JUL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WSE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YAU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YJB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NATIONAL YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2740-FLAG-NATIONAL.
           IF EUFEE-NATIONAL-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EUFEE-NATIONAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2750-DESCRIBE-TYPE.
           EVALUATE EUFEE-TYPE-CD
               WHEN 'AGL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SMF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UEC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VJA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WPH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DTL -> HYD WHEN NOTICE DATE IS PAST
      * HYD -> JUL WHEN NOTICE DATE IS PAST
      * JUL -> WSE WHEN NOTICE DATE IS PAST
      * WSE -> YAU WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2760-SET-REGION.
           EVALUATE EUFEE-REGION-CD
               WHEN 'DTL'
                   IF EUFEE-NOTICE-DATE > 0 AND EUFEE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HYD' TO EUFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HYD'
                   IF EUFEE-NOTICE-DATE > 0 AND EUFEE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JUL' TO EUFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JUL'
                   IF EUFEE-NOTICE-DATE > 0 AND EUFEE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WSE' TO EUFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WSE'
                   IF EUFEE-NOTICE-DATE > 0 AND EUFEE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YAU' TO EUFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE NATIONAL AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2770-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN EUFEE-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUFEE-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = EUFEE-NATIONAL-AMT *
                       EUFEE-RATE / 200
               WHEN EUFEE-TYPE-CD = 'AGL'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN EUFEE-TYPE-CD = 'SMF'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = EUFEE-NATIONAL-AMT *
                       EUFEE-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CONVERTED YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2780-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN EUFEE-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO EUFEE-CONVERTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUFEE-PREFERRED-FLAG = 'Y'
                   COMPUTE EUFEE-CONVERTED-YTD ROUNDED = WS-WORK-AMT-2 *
                       EUFEE-FEE-PCT / 200
               WHEN EUFEE-CONV-METHOD-CD = 'GL'
                   MOVE 20.00 TO EUFEE-CONVERTED-YTD
               WHEN EUFEE-CONV-METHOD-CD = 'JA'
                   MOVE 12.00 TO EUFEE-CONVERTED-YTD
               WHEN OTHER
                   COMPUTE EUFEE-CONVERTED-YTD ROUNDED = WS-WORK-AMT-2 *
                       EUFEE-FEE-PCT / 100
           END-EVALUATE
           IF EUFEE-CONVERTED-YTD > 0 AND EUFEE-CONVERTED-YTD < 10.00
               MOVE 10.00 TO EUFEE-CONVERTED-YTD
           END-IF
           IF EUFEE-CONVERTED-YTD > 75.00
               MOVE 75.00 TO EUFEE-CONVERTED-YTD
           END-IF
           ADD EUFEE-CONVERTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2790-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EUFEE-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO EUFEE-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AEA -> EBM WHEN NOTICE DATE IS PAST
      * EBM -> GAU WHEN NOTICE DATE IS PAST
      * GAU -> GSU WHEN NOTICE DATE IS PAST
      * GSU -> VWP WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2800-SET-CHANNEL.
           EVALUATE EUFEE-CHANNEL-CD
               WHEN 'AEA'
                   IF EUFEE-NOTICE-DATE > 0 AND EUFEE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EBM' TO EUFEE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EBM'
                   IF EUFEE-NOTICE-DATE > 0 AND EUFEE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GAU' TO EUFEE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAU'
                   IF EUFEE-NOTICE-DATE > 0 AND EUFEE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GSU' TO EUFEE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GSU'
                   IF EUFEE-NOTICE-DATE > 0 AND EUFEE-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VWP' TO EUFEE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2810-COUNT-CYCLE.
           IF EUFEE-CYCLE-CNT < 9999999
               ADD 1 TO EUFEE-CYCLE-CNT
           END-IF
           IF EUFEE-CYCLE-CNT NOT < 10
               MOVE 'Y' TO EUFEE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2820-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF EUFEE-NOTICE-DATE > 0 AND EUFEE-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUFEE-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUFEE-NATIONAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EUFEE-NATIONAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EUFEE-NATIONAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EUFEE-NATIONAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUFEE-NATIONAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NATIONAL AMT.
      *----------------------------------------------------------------*
       2830-PRORATE-NATIONAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUFEE-NATIONAL-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EUFEE-NATIONAL-AMT
           END-IF
           ADD EUFEE-NATIONAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2840-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2850-COUNT-PAYMENT.
           IF EUFEE-PAYMENT-CNT < 9999999
               ADD 1 TO EUFEE-PAYMENT-CNT
           END-IF
           IF EUFEE-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO EUFEE-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONVERTED YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2860-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUFEE-CHANNEL-CD
                   ADD EUFEE-CONVERTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUFEE-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUFEE-CONVERTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NATIONAL YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2870-ACCRUE-NATIONAL.
           IF EUFEE-NATIONAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUFEE-NATIONAL-YTD * EUFEE-RATE / 100 / 360
               ADD WS-WORK-AMT TO EUFEE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRIANGULAR YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2880-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN EUFEE-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO EUFEE-TRIANGULAR-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUFEE-OVERRIDE-FLAG = 'Y'
                   COMPUTE EUFEE-TRIANGULAR-YTD ROUNDED = WS-WORK-AMT-2
                       * EUFEE-RATE / 200
               WHEN EUFEE-CONV-METHOD-CD = 'GL'
                   MOVE 20.00 TO EUFEE-TRIANGULAR-YTD
               WHEN EUFEE-CONV-METHOD-CD = 'JA'
                   MOVE 20.00 TO EUFEE-TRIANGULAR-YTD
               WHEN OTHER
                   COMPUTE EUFEE-TRIANGULAR-YTD ROUNDED = WS-WORK-AMT-2
                       * EUFEE-RATE / 100
           END-EVALUATE
           IF EUFEE-TRIANGULAR-YTD > 0 AND EUFEE-TRIANGULAR-YTD < 2.00
               MOVE 2.00 TO EUFEE-TRIANGULAR-YTD
           END-IF
           IF EUFEE-TRIANGULAR-YTD > 35.00
               MOVE 35.00 TO EUFEE-TRIANGULAR-YTD
           END-IF
           ADD EUFEE-TRIANGULAR-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE CONVERTED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2890-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO EUFEE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF EUFEE-CONVERTED-YTD NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO EUFEE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUFEE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUFEE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EURO YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2900-FLAG-EURO.
           IF EUFEE-EURO-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EUFEE-EURO-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CONV METHOD CD:
      * GL -> JA WHEN EFFECTIVE DATE IS PAST
      * JA -> LD WHEN EFFECTIVE DATE IS PAST
      * LD -> MR WHEN EFFECTIVE DATE IS PAST
      * MR -> VC WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2910-SET-CONV-METHOD.
           EVALUATE EUFEE-CONV-METHOD-CD
               WHEN 'GL'
                   IF EUFEE-EFFECTIVE-DATE > 0 AND EUFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JA' TO EUFEE-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF EUFEE-EFFECTIVE-DATE > 0 AND EUFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LD' TO EUFEE-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF EUFEE-EFFECTIVE-DATE > 0 AND EUFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MR' TO EUFEE-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF EUFEE-EFFECTIVE-DATE > 0 AND EUFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VC' TO EUFEE-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2920-DESCRIBE-REGION.
           EVALUATE EUFEE-REGION-CD
               WHEN 'DTL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HYD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JUL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WSE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YAU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YJB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2930-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2940-COUNT-CYCLE.
           IF EUFEE-CYCLE-CNT < 9999999
               ADD 1 TO EUFEE-CYCLE-CNT
           END-IF
           IF EUFEE-CYCLE-CNT NOT < 10
               MOVE 'Y' TO EUFEE-OVERRIDE-FLAG
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO EUFEE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO EUFEE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO EUFEE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'EUFEE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
