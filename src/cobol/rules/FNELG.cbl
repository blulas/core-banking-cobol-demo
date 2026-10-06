      *================================================================*
      * PROGRAM  : FNELG                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : FOREIGN BANKNOTES RULE - DATE RULES. CALLED BY THE  *
      *            FOREIGN BANKNOTES BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-11-21 MKL  ORIGINAL                                       *
      * 1997-09-03 LMB  SHIPPING LIMIT REVISED REQ6178                 *
      * 1999-04-08 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-01-02 BHS  AUDIT FINDING - MARGIN CONTROL TOTALS INC0951  *
      * 2000-02-23 DPS  FOREIGN FEE SCHEDULE UPDATE PRB7162            *
      * 2001-06-03 SKN  USD LIMIT REVISED CHG2753                      *
      * 2002-12-15 LMB  MARGIN CALCULATION CORRECTED REQ0401           *
      * 2003-10-07 MKL  BUYBACK LIMIT REVISED REQ8814                  *
      * 2004-01-03 PQR  NEW USD REPORT COLUMNS PRB3262                 *
      * 2005-07-01 BHS  SHIPPING EXCEPTIONS TO OPS QUEUE PRB2180       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FNELG.
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
                                           VALUE 20.000000.
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
      * WS-BUYBACK-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BUYBACK-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000659000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000576000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000894000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006250000000589000'.
       01  WS-BUYBACK-GRID-TBL REDEFINES WS-BUYBACK-GRID-VALUES.
           05  WS-BUYBACK-GRID-ROW OCCURS 4 TIMES.
               10  WS-BUYBACK-GRID-LOW     PIC 9(09)V99.
               10  WS-BUYBACK-GRID-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY FNELGP.

       PROCEDURE DIVISION USING FNELG-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-PRORATE-MARGIN
           PERFORM 2010-ASSESS-OVERRIDE
           PERFORM 2020-FLAG-BUYBACK
           PERFORM 2030-PRORATE-SHIPPING
           PERFORM 2040-LIMIT-FOREIGN
           PERFORM 2050-COUNT-PAYMENT
           PERFORM 2060-FLAG-FOREIGN
           PERFORM 2070-EDIT-ADDR
           PERFORM 2080-SELECT-RATE
           PERFORM 2090-EDIT-ADDR
           PERFORM 2100-PRORATE-USD
           PERFORM 2110-LIMIT-BUYBACK
           PERFORM 2120-COUNT-PAYMENT
           PERFORM 2130-DESCRIBE-ORDER-TYPE
           PERFORM 2140-AGE-OPEN
           PERFORM 2150-SET-SOURCE
           PERFORM 2160-ASSESS-PREFERRED
           PERFORM 2170-DESCRIBE-BRANCH
           PERFORM 2180-LIMIT-ITEM
           PERFORM 2190-SPLIT-MARGIN
           PERFORM 2200-AGE-OPEN
           PERFORM 2210-ACCUM-STATUS
           PERFORM 2220-DESCRIBE-BRANCH
           PERFORM 2230-FLAG-SHIPPING
           PERFORM 2240-ASSESS-OVERRIDE
           PERFORM 2250-ASSESS-PREFERRED
           PERFORM 2260-PRORATE-MARGIN
           PERFORM 2270-EDIT-DESC
           PERFORM 2280-ASSESS-PREFERRED
           PERFORM 2290-ASSESS-REVIEW
           PERFORM 2300-ASSESS-PREFERRED
           PERFORM 2310-ACCRUE-FOREIGN
           PERFORM 2320-PRORATE-SHIPPING
           PERFORM 2330-ASSESS-REVIEW
           PERFORM 2340-SET-BRANCH
           PERFORM 2350-SET-BRANCH
           PERFORM 2360-EDIT-DESC
           PERFORM 2370-AGE-LAST-PAY
           PERFORM 2380-EDIT-DESC
           PERFORM 2390-COUNT-PAYMENT
           PERFORM 2400-ASSESS-PREFERRED
           PERFORM 2410-COUNT-CYCLE
           PERFORM 2420-EDIT-ADDR
           PERFORM 2430-CHECK-NOTICE
           PERFORM 2440-CHECK-LAST-PAY
           PERFORM 2450-AGE-LAST-MAINT
           PERFORM 2460-DESCRIBE-STATUS
           PERFORM 2470-LIMIT-ITEM
           PERFORM 2480-PRORATE-FOREIGN
           PERFORM 2490-CHECK-NOTICE
           PERFORM 2500-SET-BRANCH
           PERFORM 2510-AGE-NOTICE
           PERFORM 2520-EDIT-ADDR
           PERFORM 2530-COUNT-CYCLE
           PERFORM 2540-LIMIT-USD
           PERFORM 2550-LIMIT-MARGIN
           PERFORM 2560-COUNT-CYCLE
           PERFORM 2570-SELECT-PENALTY-PCT
           PERFORM 2580-SET-SOURCE
           PERFORM 2590-LIMIT-ITEM
           PERFORM 2600-DESCRIBE-ORDER-TYPE
           PERFORM 2610-PRORATE-MARGIN
           PERFORM 2620-PRORATE-SHIPPING
           PERFORM 2630-ACCUM-SOURCE
           PERFORM 2640-PRORATE-FOREIGN
           PERFORM 2650-CHECK-LAST-PAY
           PERFORM 2660-SELECT-PENALTY-PCT
           PERFORM 2670-EDIT-DESC
           PERFORM 2680-ACCUM-SOURCE
           PERFORM 2690-EDIT-ADDR
           PERFORM 2700-SET-SOURCE
           PERFORM 2710-FLAG-FOREIGN
           PERFORM 2720-COUNT-PAYMENT
           PERFORM 2730-SPLIT-MARGIN
           PERFORM 2740-COUNT-PAYMENT
           PERFORM 2750-ACCRUE-FOREIGN
           PERFORM 2760-ASSESS-PREFERRED
           PERFORM 2770-PRORATE-MARGIN
           PERFORM 2780-ACCRUE-SHIPPING
           PERFORM 2790-AGE-NOTICE
           PERFORM 2800-SELECT-PENALTY-PCT
           PERFORM 2810-ACCUM-ORDER-TYPE
           PERFORM 2820-ASSESS-OVERRIDE
           PERFORM 2830-LIMIT-FOREIGN
           PERFORM 2840-DESCRIBE-BRANCH
           PERFORM 2850-DESCRIBE-ORDER-TYPE
           PERFORM 2860-PRORATE-SHIPPING
           PERFORM 2870-AGE-NOTICE
           PERFORM 2880-SPLIT-SHIPPING
           PERFORM 2890-ACCUM-ORDER-TYPE
           PERFORM 2900-COUNT-CYCLE
           PERFORM 2910-FLAG-ITEM
           PERFORM 2920-PRORATE-FOREIGN
           PERFORM 2930-EDIT-DESC
           PERFORM 2940-FLAG-MARGIN
           PERFORM 2950-COUNT-PAYMENT
           PERFORM 2960-EDIT-DESC
           PERFORM 2970-AGE-NOTICE
           PERFORM 2980-SET-BRANCH
           PERFORM 2990-PRORATE-FOREIGN
           PERFORM 3000-CHECK-OPEN
           PERFORM 3010-ACCRUE-USD
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO FNELG-RETURN-CODE.

      *----------------------------------------------------------------*
      * PRORATE SHIPPING AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MARGIN YTD.
      *----------------------------------------------------------------*
       2000-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNELG-MARGIN-YTD ROUNDED =
                   FNELG-SHIPPING-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FNELG-MARGIN-YTD
           END-IF
           ADD FNELG-MARGIN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN FEE ON THE USD AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2010-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN FNELG-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO FNELG-FOREIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNELG-REVIEW-FLAG = 'Y'
                   COMPUTE FNELG-FOREIGN-AMT ROUNDED = FNELG-USD-AMT *
                       FNELG-PENALTY-PCT / 200
               WHEN FNELG-ORDER-TYPE-CD = 'DTU'
                   MOVE 10.00 TO FNELG-FOREIGN-AMT
               WHEN FNELG-ORDER-TYPE-CD = 'KZV'
                   MOVE 5.00 TO FNELG-FOREIGN-AMT
               WHEN OTHER
                   COMPUTE FNELG-FOREIGN-AMT ROUNDED = FNELG-USD-AMT *
                       FNELG-PENALTY-PCT / 100
           END-EVALUATE
           IF FNELG-FOREIGN-AMT > 0 AND FNELG-FOREIGN-AMT < 5.00
               MOVE 5.00 TO FNELG-FOREIGN-AMT
           END-IF
           IF FNELG-FOREIGN-AMT > 25.00
               MOVE 25.00 TO FNELG-FOREIGN-AMT
           END-IF
           ADD FNELG-FOREIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * BUYBACK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2020-FLAG-BUYBACK.
           IF FNELG-BUYBACK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FNELG-BUYBACK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHIPPING AMT.
      *----------------------------------------------------------------*
       2030-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNELG-SHIPPING-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FNELG-SHIPPING-AMT
           END-IF
           ADD FNELG-SHIPPING-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2040-LIMIT-FOREIGN.
           IF FNELG-FOREIGN-AMT < 25.00
               MOVE 25.00 TO FNELG-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNELG-FOREIGN-AMT > 1000.00
               MOVE 1000.00 TO FNELG-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2050-COUNT-PAYMENT.
           IF FNELG-PAYMENT-CNT < 9999999
               ADD 1 TO FNELG-PAYMENT-CNT
           END-IF
           IF FNELG-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO FNELG-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2060-FLAG-FOREIGN.
           IF FNELG-FOREIGN-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FNELG-FOREIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E594 ADDR REQUIRED
      * E681 FOREIGN AMT NEGATIVE
      * E976 FOREIGN AMT OVER 99999
      * E783 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2070-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNELG-ADDR = SPACES
                   MOVE 'E594' TO WS-REJECT-CODE
               WHEN FNELG-FOREIGN-AMT < 0
                   MOVE 'E681' TO WS-REJECT-CODE
               WHEN FNELG-FOREIGN-AMT > 99999.00
                   MOVE 'E976' TO WS-REJECT-CODE
               WHEN FNELG-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E783' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK GRID TIER FOR THE MARGIN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2080-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-GRID-RATE(1) TO FNELG-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF FNELG-MARGIN-YTD NOT < WS-BUYBACK-GRID-LOW(WS-TX)
                   MOVE WS-BUYBACK-GRID-RATE(WS-TX) TO FNELG-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNELG-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNELG-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E719 ADDR REQUIRED
      * E734 USD AMT NEGATIVE
      * E250 USD AMT OVER 250000
      * E015 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2090-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNELG-ADDR = SPACES
                   MOVE 'E719' TO WS-REJECT-CODE
               WHEN FNELG-USD-AMT < 0
                   MOVE 'E734' TO WS-REJECT-CODE
               WHEN FNELG-USD-AMT > 250000.00
                   MOVE 'E250' TO WS-REJECT-CODE
               WHEN FNELG-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E015' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHIPPING AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO USD AMT.
      *----------------------------------------------------------------*
       2100-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNELG-USD-AMT ROUNDED =
                   FNELG-SHIPPING-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FNELG-USD-AMT
           END-IF
           ADD FNELG-USD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * BUYBACK AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2110-LIMIT-BUYBACK.
           IF FNELG-BUYBACK-AMT < 0.00
               MOVE 0.00 TO FNELG-BUYBACK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNELG-BUYBACK-AMT > 500.00
               MOVE 500.00 TO FNELG-BUYBACK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2120-COUNT-PAYMENT.
           IF FNELG-PAYMENT-CNT < 9999999
               ADD 1 TO FNELG-PAYMENT-CNT
           END-IF
           IF FNELG-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO FNELG-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ORDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2130-DESCRIBE-ORDER-TYPE.
           EVALUATE FNELG-ORDER-TYPE-CD
               WHEN 'DTU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KZV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PHV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PZZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WMC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XWF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2140-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FNELG-OPEN-DATE > 0 AND FNELG-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNELG-OPEN-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN OPEN DATE IS PAST
      * FS -> HL WHEN OPEN DATE IS PAST
      * HL -> HS WHEN OPEN DATE IS PAST
      * HS -> NW WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2150-SET-SOURCE.
           EVALUATE FNELG-SOURCE-CD
               WHEN 'DG'
                   IF FNELG-OPEN-DATE > 0 AND FNELG-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FS' TO FNELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FS'
                   IF FNELG-OPEN-DATE > 0 AND FNELG-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO FNELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF FNELG-OPEN-DATE > 0 AND FNELG-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HS' TO FNELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HS'
                   IF FNELG-OPEN-DATE > 0 AND FNELG-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NW' TO FNELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE MARGIN YTD FEE ON THE FOREIGN AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2160-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FNELG-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO FNELG-MARGIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNELG-REVIEW-FLAG = 'Y'
                   COMPUTE FNELG-MARGIN-YTD ROUNDED = FNELG-FOREIGN-AMT
                       * FNELG-PENALTY-PCT / 200
               WHEN FNELG-BRANCH-CD = 'APZ'
                   MOVE 5.00 TO FNELG-MARGIN-YTD
               WHEN FNELG-BRANCH-CD = 'BBP'
                   MOVE 12.00 TO FNELG-MARGIN-YTD
               WHEN OTHER
                   COMPUTE FNELG-MARGIN-YTD ROUNDED = FNELG-FOREIGN-AMT
                       * FNELG-PENALTY-PCT / 100
           END-EVALUATE
           IF FNELG-MARGIN-YTD > 0 AND FNELG-MARGIN-YTD < 10.00
               MOVE 10.00 TO FNELG-MARGIN-YTD
           END-IF
           IF FNELG-MARGIN-YTD > 50.00
               MOVE 50.00 TO FNELG-MARGIN-YTD
           END-IF
           ADD FNELG-MARGIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2170-DESCRIBE-BRANCH.
           EVALUATE FNELG-BRANCH-CD
               WHEN 'APZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BBP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ENL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'EZB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KHV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TDN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2180-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MARGIN YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2190-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = FNELG-MARGIN-YTD / 4
           COMPUTE WS-WORK-AMT-2 = FNELG-MARGIN-YTD - (WS-WORK-AMT * 4)
           COMPUTE FNELG-USD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2200-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FNELG-OPEN-DATE > 0 AND FNELG-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNELG-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2210-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNELG-STATUS-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNELG-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2220-DESCRIBE-BRANCH.
           EVALUATE FNELG-BRANCH-CD
               WHEN 'APZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BBP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ENL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'EZB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KHV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TDN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHIPPING AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2230-FLAG-SHIPPING.
           IF FNELG-SHIPPING-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FNELG-SHIPPING-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE MARGIN AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2240-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN FNELG-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNELG-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = FNELG-MARGIN-AMT *
                       FNELG-PENALTY-PCT / 200
               WHEN FNELG-STATUS-CD = 'CXH'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN FNELG-STATUS-CD = 'ESS'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = FNELG-MARGIN-AMT *
                       FNELG-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE MARGIN FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2250-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FNELG-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO FNELG-MARGIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNELG-REVIEW-FLAG = 'Y'
                   COMPUTE FNELG-MARGIN-AMT ROUNDED = WS-WORK-AMT-2 *
                       FNELG-RATE / 200
               WHEN FNELG-ORDER-TYPE-CD = 'DTU'
                   MOVE 3.00 TO FNELG-MARGIN-AMT
               WHEN FNELG-ORDER-TYPE-CD = 'KZV'
                   MOVE 7.00 TO FNELG-MARGIN-AMT
               WHEN OTHER
                   COMPUTE FNELG-MARGIN-AMT ROUNDED = WS-WORK-AMT-2 *
                       FNELG-RATE / 100
           END-EVALUATE
           IF FNELG-MARGIN-AMT > 0 AND FNELG-MARGIN-AMT < 10.00
               MOVE 10.00 TO FNELG-MARGIN-AMT
           END-IF
           IF FNELG-MARGIN-AMT > 25.00
               MOVE 25.00 TO FNELG-MARGIN-AMT
           END-IF
           ADD FNELG-MARGIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MARGIN YTD.
      *----------------------------------------------------------------*
       2260-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNELG-MARGIN-YTD ROUNDED =
                   FNELG-FOREIGN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FNELG-MARGIN-YTD
           END-IF
           ADD FNELG-MARGIN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E425 DESC REQUIRED
      * E754 WORK AMT 2 NEGATIVE
      * E504 WORK AMT 2 OVER 5000
      * E678 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2270-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNELG-DESC = SPACES
                   MOVE 'E425' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E754' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E504' TO WS-REJECT-CODE
               WHEN FNELG-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E678' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MARGIN FEE ON THE FOREIGN AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2280-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FNELG-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO FNELG-MARGIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNELG-REVIEW-FLAG = 'Y'
                   COMPUTE FNELG-MARGIN-AMT ROUNDED = FNELG-FOREIGN-AMT
                       * FNELG-PENALTY-PCT / 200
               WHEN FNELG-STATUS-CD = 'CXH'
                   MOVE 10.00 TO FNELG-MARGIN-AMT
               WHEN FNELG-STATUS-CD = 'ESS'
                   MOVE 7.00 TO FNELG-MARGIN-AMT
               WHEN OTHER
                   COMPUTE FNELG-MARGIN-AMT ROUNDED = FNELG-FOREIGN-AMT
                       * FNELG-PENALTY-PCT / 100
           END-EVALUATE
           IF FNELG-MARGIN-AMT > 0 AND FNELG-MARGIN-AMT < 1.00
               MOVE 1.00 TO FNELG-MARGIN-AMT
           END-IF
           IF FNELG-MARGIN-AMT > 250.00
               MOVE 250.00 TO FNELG-MARGIN-AMT
           END-IF
           ADD FNELG-MARGIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE USD FEE ON THE MARGIN YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2290-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN FNELG-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO FNELG-USD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNELG-PREFERRED-FLAG = 'Y'
                   COMPUTE FNELG-USD-AMT ROUNDED = FNELG-MARGIN-YTD *
                       FNELG-RATE / 200
               WHEN FNELG-BRANCH-CD = 'APZ'
                   MOVE 7.00 TO FNELG-USD-AMT
               WHEN FNELG-BRANCH-CD = 'BBP'
                   MOVE 10.00 TO FNELG-USD-AMT
               WHEN OTHER
                   COMPUTE FNELG-USD-AMT ROUNDED = FNELG-MARGIN-YTD *
                       FNELG-RATE / 100
           END-EVALUATE
           IF FNELG-USD-AMT > 0 AND FNELG-USD-AMT < 2.00
               MOVE 2.00 TO FNELG-USD-AMT
           END-IF
           IF FNELG-USD-AMT > 50.00
               MOVE 50.00 TO FNELG-USD-AMT
           END-IF
           ADD FNELG-USD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE MARGIN YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2300-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FNELG-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNELG-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = FNELG-MARGIN-YTD *
                       FNELG-RATE / 200
               WHEN FNELG-SOURCE-CD = 'DG'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN FNELG-SOURCE-CD = 'FS'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = FNELG-MARGIN-YTD *
                       FNELG-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2310-ACCRUE-FOREIGN.
           IF FNELG-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNELG-FOREIGN-AMT * FNELG-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO FNELG-MARGIN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPPING AMT.
      *----------------------------------------------------------------*
       2320-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNELG-SHIPPING-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FNELG-SHIPPING-AMT
           END-IF
           ADD FNELG-SHIPPING-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPPING FEE ON THE WORK AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2330-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN FNELG-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO FNELG-SHIPPING-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNELG-OVERRIDE-FLAG = 'Y'
                   COMPUTE FNELG-SHIPPING-AMT ROUNDED = WS-WORK-AMT *
                       FNELG-RATE / 200
               WHEN FNELG-SOURCE-CD = 'DG'
                   MOVE 12.00 TO FNELG-SHIPPING-AMT
               WHEN FNELG-SOURCE-CD = 'FS'
                   MOVE 5.00 TO FNELG-SHIPPING-AMT
               WHEN OTHER
                   COMPUTE FNELG-SHIPPING-AMT ROUNDED = WS-WORK-AMT *
                       FNELG-RATE / 100
           END-EVALUATE
           IF FNELG-SHIPPING-AMT > 0 AND FNELG-SHIPPING-AMT < 10.00
               MOVE 10.00 TO FNELG-SHIPPING-AMT
           END-IF
           IF FNELG-SHIPPING-AMT > 150.00
               MOVE 150.00 TO FNELG-SHIPPING-AMT
           END-IF
           ADD FNELG-SHIPPING-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * APZ -> BBP WHEN NOTICE DATE IS PAST
      * BBP -> ENL WHEN NOTICE DATE IS PAST
      * ENL -> EZB WHEN NOTICE DATE IS PAST
      * EZB -> KHV WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2340-SET-BRANCH.
           EVALUATE FNELG-BRANCH-CD
               WHEN 'APZ'
                   IF FNELG-NOTICE-DATE > 0 AND FNELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BBP' TO FNELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BBP'
                   IF FNELG-NOTICE-DATE > 0 AND FNELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ENL' TO FNELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ENL'
                   IF FNELG-NOTICE-DATE > 0 AND FNELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EZB' TO FNELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EZB'
                   IF FNELG-NOTICE-DATE > 0 AND FNELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHV' TO FNELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * APZ -> BBP WHEN NOTICE DATE IS PAST
      * BBP -> ENL WHEN NOTICE DATE IS PAST
      * ENL -> EZB WHEN NOTICE DATE IS PAST
      * EZB -> KHV WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2350-SET-BRANCH.
           EVALUATE FNELG-BRANCH-CD
               WHEN 'APZ'
                   IF FNELG-NOTICE-DATE > 0 AND FNELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BBP' TO FNELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BBP'
                   IF FNELG-NOTICE-DATE > 0 AND FNELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ENL' TO FNELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ENL'
                   IF FNELG-NOTICE-DATE > 0 AND FNELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EZB' TO FNELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EZB'
                   IF FNELG-NOTICE-DATE > 0 AND FNELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHV' TO FNELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E488 DESC REQUIRED
      * E787 WORK AMT 2 NEGATIVE
      * E584 WORK AMT 2 OVER 5000
      * E719 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2360-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNELG-DESC = SPACES
                   MOVE 'E488' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E787' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E584' TO WS-REJECT-CODE
               WHEN FNELG-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E719' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2370-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF FNELG-LAST-PAY-DATE > 0 AND FNELG-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNELG-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNELG-FOREIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FNELG-FOREIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD FNELG-FOREIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FNELG-FOREIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNELG-FOREIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E381 DESC REQUIRED
      * E875 FOREIGN AMT NEGATIVE
      * E848 FOREIGN AMT OVER 50000
      * E842 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2380-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNELG-DESC = SPACES
                   MOVE 'E381' TO WS-REJECT-CODE
               WHEN FNELG-FOREIGN-AMT < 0
                   MOVE 'E875' TO WS-REJECT-CODE
               WHEN FNELG-FOREIGN-AMT > 50000.00
                   MOVE 'E848' TO WS-REJECT-CODE
               WHEN FNELG-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E842' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2390-COUNT-PAYMENT.
           IF FNELG-PAYMENT-CNT < 9999999
               ADD 1 TO FNELG-PAYMENT-CNT
           END-IF
           IF FNELG-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO FNELG-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MARGIN FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2400-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FNELG-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO FNELG-MARGIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNELG-OVERRIDE-FLAG = 'Y'
                   COMPUTE FNELG-MARGIN-AMT ROUNDED = WS-WORK-AMT-2 *
                       FNELG-RATE / 200
               WHEN FNELG-BRANCH-CD = 'APZ'
                   MOVE 10.00 TO FNELG-MARGIN-AMT
               WHEN FNELG-BRANCH-CD = 'BBP'
                   MOVE 15.00 TO FNELG-MARGIN-AMT
               WHEN OTHER
                   COMPUTE FNELG-MARGIN-AMT ROUNDED = WS-WORK-AMT-2 *
                       FNELG-RATE / 100
           END-EVALUATE
           IF FNELG-MARGIN-AMT > 0 AND FNELG-MARGIN-AMT < 5.00
               MOVE 5.00 TO FNELG-MARGIN-AMT
           END-IF
           IF FNELG-MARGIN-AMT > 75.00
               MOVE 75.00 TO FNELG-MARGIN-AMT
           END-IF
           ADD FNELG-MARGIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2410-COUNT-CYCLE.
           IF FNELG-CYCLE-CNT < 9999999
               ADD 1 TO FNELG-CYCLE-CNT
           END-IF
           IF FNELG-CYCLE-CNT NOT < 12
               MOVE 'Y' TO FNELG-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E915 ADDR REQUIRED
      * E586 FOREIGN AMT NEGATIVE
      * E499 FOREIGN AMT OVER 50000
      * E133 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2420-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNELG-ADDR = SPACES
                   MOVE 'E915' TO WS-REJECT-CODE
               WHEN FNELG-FOREIGN-AMT < 0
                   MOVE 'E586' TO WS-REJECT-CODE
               WHEN FNELG-FOREIGN-AMT > 50000.00
                   MOVE 'E499' TO WS-REJECT-CODE
               WHEN FNELG-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E133' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2430-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FNELG-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNELG-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO FNELG-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2440-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF FNELG-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNELG-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO FNELG-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2450-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF FNELG-LAST-MAINT-DATE > 0 AND FNELG-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNELG-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNELG-FOREIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FNELG-FOREIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FNELG-FOREIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FNELG-FOREIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNELG-FOREIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2460-DESCRIBE-STATUS.
           EVALUATE FNELG-STATUS-CD
               WHEN 'CXH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ESS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XDE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XFD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2470-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE USD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FOREIGN AMT.
      *----------------------------------------------------------------*
       2480-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNELG-FOREIGN-AMT ROUNDED =
                   FNELG-USD-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FNELG-FOREIGN-AMT
           END-IF
           ADD FNELG-FOREIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2490-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FNELG-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNELG-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO FNELG-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * APZ -> BBP WHEN OPEN DATE IS PAST
      * BBP -> ENL WHEN OPEN DATE IS PAST
      * ENL -> EZB WHEN OPEN DATE IS PAST
      * EZB -> KHV WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2500-SET-BRANCH.
           EVALUATE FNELG-BRANCH-CD
               WHEN 'APZ'
                   IF FNELG-OPEN-DATE > 0 AND FNELG-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BBP' TO FNELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BBP'
                   IF FNELG-OPEN-DATE > 0 AND FNELG-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ENL' TO FNELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ENL'
                   IF FNELG-OPEN-DATE > 0 AND FNELG-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EZB' TO FNELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EZB'
                   IF FNELG-OPEN-DATE > 0 AND FNELG-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHV' TO FNELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2510-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FNELG-NOTICE-DATE > 0 AND FNELG-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNELG-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNELG-MARGIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FNELG-MARGIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD FNELG-MARGIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FNELG-MARGIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNELG-MARGIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E616 ADDR REQUIRED
      * E640 USD AMT NEGATIVE
      * E844 USD AMT OVER 250000
      * E546 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2520-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNELG-ADDR = SPACES
                   MOVE 'E616' TO WS-REJECT-CODE
               WHEN FNELG-USD-AMT < 0
                   MOVE 'E640' TO WS-REJECT-CODE
               WHEN FNELG-USD-AMT > 250000.00
                   MOVE 'E844' TO WS-REJECT-CODE
               WHEN FNELG-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E546' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2530-COUNT-CYCLE.
           IF FNELG-CYCLE-CNT < 9999999
               ADD 1 TO FNELG-CYCLE-CNT
           END-IF
           IF FNELG-CYCLE-CNT NOT < 10
               MOVE 'Y' TO FNELG-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * USD AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2540-LIMIT-USD.
           IF FNELG-USD-AMT < 10.00
               MOVE 10.00 TO FNELG-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNELG-USD-AMT > 500.00
               MOVE 500.00 TO FNELG-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2550-LIMIT-MARGIN.
           IF FNELG-MARGIN-AMT < 50.00
               MOVE 50.00 TO FNELG-MARGIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNELG-MARGIN-AMT > 1000.00
               MOVE 1000.00 TO FNELG-MARGIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2560-COUNT-CYCLE.
           IF FNELG-CYCLE-CNT < 9999999
               ADD 1 TO FNELG-CYCLE-CNT
           END-IF
           IF FNELG-CYCLE-CNT NOT < 10
               MOVE 'Y' TO FNELG-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK GRID TIER FOR THE FOREIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2570-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-GRID-RATE(1) TO FNELG-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF FNELG-FOREIGN-AMT NOT < WS-BUYBACK-GRID-LOW(WS-TX)
                   MOVE WS-BUYBACK-GRID-RATE(WS-TX) TO FNELG-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNELG-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNELG-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN LAST PAY DATE IS PAST
      * FS -> HL WHEN LAST PAY DATE IS PAST
      * HL -> HS WHEN LAST PAY DATE IS PAST
      * HS -> NW WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2580-SET-SOURCE.
           EVALUATE FNELG-SOURCE-CD
               WHEN 'DG'
                   IF FNELG-LAST-PAY-DATE > 0 AND FNELG-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FS' TO FNELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FS'
                   IF FNELG-LAST-PAY-DATE > 0 AND FNELG-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO FNELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF FNELG-LAST-PAY-DATE > 0 AND FNELG-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HS' TO FNELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HS'
                   IF FNELG-LAST-PAY-DATE > 0 AND FNELG-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NW' TO FNELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2590-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ORDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2600-DESCRIBE-ORDER-TYPE.
           EVALUATE FNELG-ORDER-TYPE-CD
               WHEN 'DTU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KZV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PHV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PZZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WMC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XWF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE MARGIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2610-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNELG-MARGIN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE SHIPPING AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2620-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNELG-SHIPPING-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE MARGIN AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2630-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNELG-SOURCE-CD
                   ADD FNELG-MARGIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNELG-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FNELG-MARGIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FOREIGN AMT.
      *----------------------------------------------------------------*
       2640-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNELG-FOREIGN-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FNELG-FOREIGN-AMT
           END-IF
           ADD FNELG-FOREIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2650-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF FNELG-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNELG-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO FNELG-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK GRID TIER FOR THE USD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2660-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-GRID-RATE(1) TO FNELG-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF FNELG-USD-AMT NOT < WS-BUYBACK-GRID-LOW(WS-TX)
                   MOVE WS-BUYBACK-GRID-RATE(WS-TX) TO FNELG-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNELG-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNELG-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E917 DESC REQUIRED
      * E582 SHIPPING AMT NEGATIVE
      * E316 SHIPPING AMT OVER 10000
      * E789 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2670-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNELG-DESC = SPACES
                   MOVE 'E917' TO WS-REJECT-CODE
               WHEN FNELG-SHIPPING-AMT < 0
                   MOVE 'E582' TO WS-REJECT-CODE
               WHEN FNELG-SHIPPING-AMT > 10000.00
                   MOVE 'E316' TO WS-REJECT-CODE
               WHEN FNELG-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E789' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MARGIN YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2680-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNELG-SOURCE-CD
                   ADD FNELG-MARGIN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNELG-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FNELG-MARGIN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E310 ADDR REQUIRED
      * E733 USD AMT NEGATIVE
      * E534 USD AMT OVER 5000
      * E284 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2690-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNELG-ADDR = SPACES
                   MOVE 'E310' TO WS-REJECT-CODE
               WHEN FNELG-USD-AMT < 0
                   MOVE 'E733' TO WS-REJECT-CODE
               WHEN FNELG-USD-AMT > 5000.00
                   MOVE 'E534' TO WS-REJECT-CODE
               WHEN FNELG-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E284' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN LAST MAINT DATE IS PAST
      * FS -> HL WHEN LAST MAINT DATE IS PAST
      * HL -> HS WHEN LAST MAINT DATE IS PAST
      * HS -> NW WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2700-SET-SOURCE.
           EVALUATE FNELG-SOURCE-CD
               WHEN 'DG'
                   IF FNELG-LAST-MAINT-DATE > 0 AND
                       FNELG-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'FS' TO FNELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FS'
                   IF FNELG-LAST-MAINT-DATE > 0 AND
                       FNELG-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'HL' TO FNELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF FNELG-LAST-MAINT-DATE > 0 AND
                       FNELG-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'HS' TO FNELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HS'
                   IF FNELG-LAST-MAINT-DATE > 0 AND
                       FNELG-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'NW' TO FNELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FOREIGN AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2710-FLAG-FOREIGN.
           IF FNELG-FOREIGN-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FNELG-FOREIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2720-COUNT-PAYMENT.
           IF FNELG-PAYMENT-CNT < 9999999
               ADD 1 TO FNELG-PAYMENT-CNT
           END-IF
           IF FNELG-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO FNELG-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2730-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE FNELG-MARGIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2740-COUNT-PAYMENT.
           IF FNELG-PAYMENT-CNT < 9999999
               ADD 1 TO FNELG-PAYMENT-CNT
           END-IF
           IF FNELG-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO FNELG-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2750-ACCRUE-FOREIGN.
           IF FNELG-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNELG-FOREIGN-AMT * FNELG-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE SHIPPING AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2760-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FNELG-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNELG-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = FNELG-SHIPPING-AMT *
                       FNELG-RATE / 200
               WHEN FNELG-ORDER-TYPE-CD = 'DTU'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN FNELG-ORDER-TYPE-CD = 'KZV'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = FNELG-SHIPPING-AMT *
                       FNELG-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE MARGIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MARGIN AMT.
      *----------------------------------------------------------------*
       2770-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNELG-MARGIN-AMT ROUNDED =
                   FNELG-MARGIN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FNELG-MARGIN-AMT
           END-IF
           ADD FNELG-MARGIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPPING AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2780-ACCRUE-SHIPPING.
           IF FNELG-SHIPPING-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNELG-SHIPPING-AMT * FNELG-RATE / 100 / 360
               ADD WS-WORK-AMT TO FNELG-MARGIN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2790-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FNELG-NOTICE-DATE > 0 AND FNELG-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNELG-NOTICE-DATE TO DTU-DATE-1
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
      * SELECT THE BUYBACK GRID TIER FOR THE FOREIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2800-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-GRID-RATE(1) TO FNELG-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF FNELG-FOREIGN-AMT NOT < WS-BUYBACK-GRID-LOW(WS-TX)
                   MOVE WS-BUYBACK-GRID-RATE(WS-TX) TO FNELG-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNELG-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNELG-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY ORDER TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2810-ACCUM-ORDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNELG-ORDER-TYPE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNELG-ORDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE MARGIN YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2820-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN FNELG-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNELG-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = FNELG-MARGIN-YTD *
                       FNELG-PENALTY-PCT / 200
               WHEN FNELG-ORDER-TYPE-CD = 'DTU'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN FNELG-ORDER-TYPE-CD = 'KZV'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = FNELG-MARGIN-YTD *
                       FNELG-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2830-LIMIT-FOREIGN.
           IF FNELG-FOREIGN-AMT < 25.00
               MOVE 25.00 TO FNELG-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNELG-FOREIGN-AMT > 2500.00
               MOVE 2500.00 TO FNELG-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2840-DESCRIBE-BRANCH.
           EVALUATE FNELG-BRANCH-CD
               WHEN 'APZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BBP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ENL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'EZB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KHV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TDN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ORDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2850-DESCRIBE-ORDER-TYPE.
           EVALUATE FNELG-ORDER-TYPE-CD
               WHEN 'DTU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KZV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PHV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PZZ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WMC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XWF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHIPPING AMT.
      *----------------------------------------------------------------*
       2860-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNELG-SHIPPING-AMT ROUNDED =
                   FNELG-FOREIGN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FNELG-SHIPPING-AMT
           END-IF
           ADD FNELG-SHIPPING-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2870-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FNELG-NOTICE-DATE > 0 AND FNELG-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNELG-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNELG-MARGIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FNELG-MARGIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD FNELG-MARGIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FNELG-MARGIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNELG-MARGIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SHIPPING AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2880-SPLIT-SHIPPING.
           COMPUTE WS-WORK-AMT = FNELG-SHIPPING-AMT / 12
           COMPUTE WS-WORK-AMT-2 = FNELG-SHIPPING-AMT - (WS-WORK-AMT *
               12)
           COMPUTE FNELG-BUYBACK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MARGIN AMT BY ORDER TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2890-ACCUM-ORDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNELG-ORDER-TYPE-CD
                   ADD FNELG-MARGIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNELG-ORDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FNELG-MARGIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2900-COUNT-CYCLE.
           IF FNELG-CYCLE-CNT < 9999999
               ADD 1 TO FNELG-CYCLE-CNT
           END-IF
           IF FNELG-CYCLE-CNT NOT < 10
               MOVE 'Y' TO FNELG-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2910-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FOREIGN AMT.
      *----------------------------------------------------------------*
       2920-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNELG-FOREIGN-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FNELG-FOREIGN-AMT
           END-IF
           ADD FNELG-FOREIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E330 DESC REQUIRED
      * E116 USD AMT NEGATIVE
      * E124 USD AMT OVER 25000
      * E024 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2930-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNELG-DESC = SPACES
                   MOVE 'E330' TO WS-REJECT-CODE
               WHEN FNELG-USD-AMT < 0
                   MOVE 'E116' TO WS-REJECT-CODE
               WHEN FNELG-USD-AMT > 25000.00
                   MOVE 'E124' TO WS-REJECT-CODE
               WHEN FNELG-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E024' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2940-FLAG-MARGIN.
           IF FNELG-MARGIN-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FNELG-MARGIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2950-COUNT-PAYMENT.
           IF FNELG-PAYMENT-CNT < 9999999
               ADD 1 TO FNELG-PAYMENT-CNT
           END-IF
           IF FNELG-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO FNELG-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E963 DESC REQUIRED
      * E715 WORK AMT 2 NEGATIVE
      * E487 WORK AMT 2 OVER 250000
      * E852 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2960-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNELG-DESC = SPACES
                   MOVE 'E963' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E715' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E487' TO WS-REJECT-CODE
               WHEN FNELG-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E852' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2970-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FNELG-NOTICE-DATE > 0 AND FNELG-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNELG-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNELG-MARGIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FNELG-MARGIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FNELG-MARGIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FNELG-MARGIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNELG-MARGIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * APZ -> BBP WHEN OPEN DATE IS PAST
      * BBP -> ENL WHEN OPEN DATE IS PAST
      * ENL -> EZB WHEN OPEN DATE IS PAST
      * EZB -> KHV WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2980-SET-BRANCH.
           EVALUATE FNELG-BRANCH-CD
               WHEN 'APZ'
                   IF FNELG-OPEN-DATE > 0 AND FNELG-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BBP' TO FNELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BBP'
                   IF FNELG-OPEN-DATE > 0 AND FNELG-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ENL' TO FNELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ENL'
                   IF FNELG-OPEN-DATE > 0 AND FNELG-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EZB' TO FNELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EZB'
                   IF FNELG-OPEN-DATE > 0 AND FNELG-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHV' TO FNELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE BUYBACK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FOREIGN AMT.
      *----------------------------------------------------------------*
       2990-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNELG-FOREIGN-AMT ROUNDED =
                   FNELG-BUYBACK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FNELG-FOREIGN-AMT
           END-IF
           ADD FNELG-FOREIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3000-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FNELG-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNELG-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO FNELG-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON USD AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-USD.
           IF FNELG-USD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNELG-USD-AMT * FNELG-RATE / 100 / 360
               ADD WS-WORK-AMT TO FNELG-MARGIN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO FNELG-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO FNELG-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO FNELG-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'FNELG EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
