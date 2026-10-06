      *================================================================*
      * PROGRAM  : FNWAIV                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : FOREIGN BANKNOTES RULE - PRICING. CALLED BY THE     *
      *            FOREIGN BANKNOTES BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-06-13 SKN  ORIGINAL                                       *
      * 1995-08-28 RJT  PERFORMANCE - BUYBACK READ AHEAD PRB9785       *
      * 1997-02-09 PQR  FOREIGN EXCEPTIONS TO OPS QUEUE CHG3566        *
      * 1998-09-16 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-02-16 BHS  BUYBACK CALCULATION CORRECTED PRB3515          *
      * 2002-07-07 BHS  MARGIN EXCEPTIONS TO OPS QUEUE REQ6186         *
      * 2003-09-21 PQR  MARGIN RERUN PROTECTION REQ4231                *
      * 2003-06-08 PQR  AUDIT FINDING - SHIPPING CONTROL TOTALS CHG5483*
      * 2003-05-03 PQR  FOREIGN EXCEPTIONS TO OPS QUEUE REQ3307        *
      * 2003-10-15 GAF  SHIPPING TABLE EXPANDED REQ6339                *
      * 2004-09-15 MKL  FOREIGN TABLE EXPANDED CHG3766                 *
      * 2004-08-26 PQR  SHIPPING STATUS CODES ADDED CHG5910            *
      * 2005-07-28 MKL  FOREIGN ROUNDING - HALF UP PRB4146             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FNWAIV.
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
      * WS-MARGIN-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MARGIN-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000404000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000322000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000819000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000744500'.
       01  WS-MARGIN-GRID-TBL REDEFINES WS-MARGIN-GRID-VALUES.
           05  WS-MARGIN-GRID-ROW OCCURS 4 TIMES.
               10  WS-MARGIN-GRID-LOW      PIC 9(09)V99.
               10  WS-MARGIN-GRID-RATE     PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY FNWAIVP.

       PROCEDURE DIVISION USING FNWAIV-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCRUE-BUYBACK
           PERFORM 2010-PRORATE-USD
           PERFORM 2020-LIMIT-MARGIN
           PERFORM 2030-CHECK-EFFECTIVE
           PERFORM 2040-FLAG-ITEM
           PERFORM 2050-SET-SOURCE
           PERFORM 2060-EDIT-CUST-ID
           PERFORM 2070-FLAG-FOREIGN
           PERFORM 2080-ASSESS-EXEMPT
           PERFORM 2090-ACCRUE-USD
           PERFORM 2100-SPLIT-USD
           PERFORM 2110-AGE-EFFECTIVE
           PERFORM 2120-COUNT-EXCEPT
           PERFORM 2130-ASSESS-REVIEW
           PERFORM 2140-ACCRUE-USD
           PERFORM 2150-AGE-EFFECTIVE
           PERFORM 2160-LIMIT-BUYBACK
           PERFORM 2170-FLAG-FOREIGN
           PERFORM 2180-CHECK-NOTICE
           PERFORM 2190-LIMIT-MARGIN
           PERFORM 2200-ACCRUE-MARGIN
           PERFORM 2210-ACCUM-TYPE
           PERFORM 2220-ASSESS-REVIEW
           PERFORM 2230-ACCUM-CURRENCY
           PERFORM 2240-ASSESS-EXEMPT
           PERFORM 2250-DESCRIBE-ORDER-TYPE
           PERFORM 2260-ACCRUE-RATE
           PERFORM 2270-ACCUM-CURRENCY
           PERFORM 2280-SPLIT-USD
           PERFORM 2290-ACCUM-CURRENCY
           PERFORM 2300-DESCRIBE-SOURCE
           PERFORM 2310-COUNT-EXCEPT
           PERFORM 2320-CHECK-NOTICE
           PERFORM 2330-PRORATE-USD
           PERFORM 2340-DESCRIBE-ORDER-TYPE
           PERFORM 2350-SPLIT-USD
           PERFORM 2360-SET-CURRENCY
           PERFORM 2370-FLAG-ITEM
           PERFORM 2380-SET-CURRENCY
           PERFORM 2390-DESCRIBE-SOURCE
           PERFORM 2400-ACCUM-CURRENCY
           PERFORM 2410-EDIT-NAME
           PERFORM 2420-ACCUM-SOURCE
           PERFORM 2430-SELECT-RATE
           PERFORM 2440-DESCRIBE-TYPE
           PERFORM 2450-ASSESS-PREFERRED
           PERFORM 2460-ACCRUE-PENALTY-PCT
           PERFORM 2470-FLAG-ITEM
           PERFORM 2480-SELECT-PENALTY-PCT
           PERFORM 2490-ACCUM-ORDER-TYPE
           PERFORM 2500-PRORATE-MARGIN
           PERFORM 2510-EDIT-NAME
           PERFORM 2520-EDIT-CUST-ID
           PERFORM 2530-CHECK-LAST-ACT
           PERFORM 2540-ACCUM-CURRENCY
           PERFORM 2550-EDIT-CUST-ID
           PERFORM 2560-LIMIT-ITEM
           PERFORM 2570-CHECK-LAST-MAINT
           PERFORM 2580-AGE-NOTICE
           PERFORM 2590-AGE-EFFECTIVE
           PERFORM 2600-FLAG-USD
           PERFORM 2610-LIMIT-USD
           PERFORM 2620-ASSESS-REVIEW
           PERFORM 2630-SPLIT-USD
           PERFORM 2640-AGE-NOTICE
           PERFORM 2650-ASSESS-EXEMPT
           PERFORM 2660-SPLIT-BUYBACK
           PERFORM 2670-LIMIT-BUYBACK
           PERFORM 2680-COUNT-EXCEPT
           PERFORM 2690-LIMIT-USD
           PERFORM 2700-FLAG-USD
           PERFORM 2710-SET-TYPE
           PERFORM 2720-SET-TYPE
           PERFORM 2730-ACCRUE-FOREIGN
           PERFORM 2740-FLAG-FOREIGN
           PERFORM 2750-ACCRUE-USD
           PERFORM 2760-ACCRUE-MARGIN
           PERFORM 2770-FLAG-FOREIGN
           PERFORM 2780-SPLIT-ITEM
           PERFORM 2790-LIMIT-FOREIGN
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO FNWAIV-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUYBACK YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2000-ACCRUE-BUYBACK.
           IF FNWAIV-BUYBACK-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNWAIV-BUYBACK-YTD * FNWAIV-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE USD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2010-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNWAIV-USD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * MARGIN AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2020-LIMIT-MARGIN.
           IF FNWAIV-MARGIN-AMT < 10.00
               MOVE 10.00 TO FNWAIV-MARGIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNWAIV-MARGIN-AMT > 1000.00
               MOVE 1000.00 TO FNWAIV-MARGIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2030-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF FNWAIV-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNWAIV-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO FNWAIV-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2040-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN LAST ACT DATE IS PAST
      * FS -> HL WHEN LAST ACT DATE IS PAST
      * HL -> HS WHEN LAST ACT DATE IS PAST
      * HS -> NW WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2050-SET-SOURCE.
           EVALUATE FNWAIV-SOURCE-CD
               WHEN 'DG'
                   IF FNWAIV-LAST-ACT-DATE > 0 AND FNWAIV-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FS' TO FNWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FS'
                   IF FNWAIV-LAST-ACT-DATE > 0 AND FNWAIV-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HL' TO FNWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF FNWAIV-LAST-ACT-DATE > 0 AND FNWAIV-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HS' TO FNWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HS'
                   IF FNWAIV-LAST-ACT-DATE > 0 AND FNWAIV-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'NW' TO FNWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E199 CUST ID REQUIRED
      * E140 USD YTD NEGATIVE
      * E188 USD YTD OVER 99999
      * E512 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2060-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNWAIV-CUST-ID = SPACES
                   MOVE 'E199' TO WS-REJECT-CODE
               WHEN FNWAIV-USD-YTD < 0
                   MOVE 'E140' TO WS-REJECT-CODE
               WHEN FNWAIV-USD-YTD > 99999.00
                   MOVE 'E188' TO WS-REJECT-CODE
               WHEN FNWAIV-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E512' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2070-FLAG-FOREIGN.
           IF FNWAIV-FOREIGN-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FNWAIV-FOREIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE USD AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2080-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN FNWAIV-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNWAIV-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = FNWAIV-USD-AMT *
                       FNWAIV-PENALTY-PCT / 200
               WHEN FNWAIV-SOURCE-CD = 'DG'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN FNWAIV-SOURCE-CD = 'FS'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = FNWAIV-USD-AMT *
                       FNWAIV-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON USD YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2090-ACCRUE-USD.
           IF FNWAIV-USD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNWAIV-USD-YTD * FNWAIV-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO FNWAIV-BUYBACK-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT USD YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2100-SPLIT-USD.
           COMPUTE WS-WORK-AMT = FNWAIV-USD-YTD / 3
           COMPUTE WS-WORK-AMT-2 = FNWAIV-USD-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2110-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF FNWAIV-EFFECTIVE-DATE > 0 AND FNWAIV-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNWAIV-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNWAIV-FOREIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD FNWAIV-FOREIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD FNWAIV-FOREIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FNWAIV-FOREIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNWAIV-FOREIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2120-COUNT-EXCEPT.
           IF FNWAIV-EXCEPT-CNT < 9999999
               ADD 1 TO FNWAIV-EXCEPT-CNT
           END-IF
           IF FNWAIV-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO FNWAIV-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2130-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN FNWAIV-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNWAIV-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       FNWAIV-PENALTY-PCT / 200
               WHEN FNWAIV-CURRENCY-CD = 'C'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN FNWAIV-CURRENCY-CD = 'E'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       FNWAIV-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON USD AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2140-ACCRUE-USD.
           IF FNWAIV-USD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNWAIV-USD-AMT * FNWAIV-RATE / 100 / 360
               ADD WS-WORK-AMT TO FNWAIV-FOREIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2150-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF FNWAIV-EFFECTIVE-DATE > 0 AND FNWAIV-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNWAIV-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNWAIV-MARGIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FNWAIV-MARGIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD FNWAIV-MARGIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD FNWAIV-MARGIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNWAIV-MARGIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BUYBACK YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2160-LIMIT-BUYBACK.
           IF FNWAIV-BUYBACK-YTD < 25.00
               MOVE 25.00 TO FNWAIV-BUYBACK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNWAIV-BUYBACK-YTD > 10000.00
               MOVE 10000.00 TO FNWAIV-BUYBACK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2170-FLAG-FOREIGN.
           IF FNWAIV-FOREIGN-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FNWAIV-FOREIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2180-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FNWAIV-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNWAIV-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO FNWAIV-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2190-LIMIT-MARGIN.
           IF FNWAIV-MARGIN-AMT < 50.00
               MOVE 50.00 TO FNWAIV-MARGIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNWAIV-MARGIN-AMT > 5000.00
               MOVE 5000.00 TO FNWAIV-MARGIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARGIN YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2200-ACCRUE-MARGIN.
           IF FNWAIV-MARGIN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNWAIV-MARGIN-YTD * FNWAIV-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2210-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNWAIV-TYPE-CD
                   ADD FNWAIV-BUYBACK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNWAIV-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FNWAIV-BUYBACK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2220-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN FNWAIV-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO FNWAIV-BUYBACK-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNWAIV-EXEMPT-FLAG = 'Y'
                   COMPUTE FNWAIV-BUYBACK-YTD ROUNDED = WS-WORK-AMT-2 *
                       FNWAIV-PENALTY-PCT / 200
               WHEN FNWAIV-TYPE-CD = 'CFP'
                   MOVE 5.00 TO FNWAIV-BUYBACK-YTD
               WHEN FNWAIV-TYPE-CD = 'FFU'
                   MOVE 20.00 TO FNWAIV-BUYBACK-YTD
               WHEN OTHER
                   COMPUTE FNWAIV-BUYBACK-YTD ROUNDED = WS-WORK-AMT-2 *
                       FNWAIV-PENALTY-PCT / 100
           END-EVALUATE
           IF FNWAIV-BUYBACK-YTD > 0 AND FNWAIV-BUYBACK-YTD < 2.00
               MOVE 2.00 TO FNWAIV-BUYBACK-YTD
           END-IF
           IF FNWAIV-BUYBACK-YTD > 25.00
               MOVE 25.00 TO FNWAIV-BUYBACK-YTD
           END-IF
           ADD FNWAIV-BUYBACK-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE MARGIN YTD BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2230-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNWAIV-CURRENCY-CD
                   ADD FNWAIV-MARGIN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNWAIV-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FNWAIV-MARGIN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MARGIN YTD FEE ON THE BUYBACK YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2240-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN FNWAIV-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO FNWAIV-MARGIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNWAIV-PREFERRED-FLAG = 'Y'
                   COMPUTE FNWAIV-MARGIN-YTD ROUNDED =
                       FNWAIV-BUYBACK-YTD * FNWAIV-RATE / 200
               WHEN FNWAIV-TYPE-CD = 'CFP'
                   MOVE 7.00 TO FNWAIV-MARGIN-YTD
               WHEN FNWAIV-TYPE-CD = 'FFU'
                   MOVE 7.00 TO FNWAIV-MARGIN-YTD
               WHEN OTHER
                   COMPUTE FNWAIV-MARGIN-YTD ROUNDED =
                       FNWAIV-BUYBACK-YTD * FNWAIV-RATE / 100
           END-EVALUATE
           IF FNWAIV-MARGIN-YTD > 0 AND FNWAIV-MARGIN-YTD < 10.00
               MOVE 10.00 TO FNWAIV-MARGIN-YTD
           END-IF
           IF FNWAIV-MARGIN-YTD > 150.00
               MOVE 150.00 TO FNWAIV-MARGIN-YTD
           END-IF
           ADD FNWAIV-MARGIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE ORDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2250-DESCRIBE-ORDER-TYPE.
           EVALUATE FNWAIV-ORDER-TYPE-CD
               WHEN 'DTU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KZV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PHV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PZZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WMC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XWF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2260-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * FNWAIV-RATE / 100 / 360
               ADD WS-WORK-AMT TO FNWAIV-FOREIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE USD AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2270-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNWAIV-CURRENCY-CD
                   ADD FNWAIV-USD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNWAIV-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FNWAIV-USD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT USD YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2280-SPLIT-USD.
           COMPUTE WS-WORK-AMT = FNWAIV-USD-YTD / 2
           COMPUTE WS-WORK-AMT-2 = FNWAIV-USD-YTD - (WS-WORK-AMT * 2)
           COMPUTE FNWAIV-MARGIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE USD YTD BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2290-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNWAIV-CURRENCY-CD
                   ADD FNWAIV-USD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNWAIV-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FNWAIV-USD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2300-DESCRIBE-SOURCE.
           EVALUATE FNWAIV-SOURCE-CD
               WHEN 'DG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2310-COUNT-EXCEPT.
           IF FNWAIV-EXCEPT-CNT < 9999999
               ADD 1 TO FNWAIV-EXCEPT-CNT
           END-IF
           IF FNWAIV-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO FNWAIV-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2320-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FNWAIV-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNWAIV-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO FNWAIV-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MARGIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO USD YTD.
      *----------------------------------------------------------------*
       2330-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNWAIV-USD-YTD ROUNDED =
                   FNWAIV-MARGIN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FNWAIV-USD-YTD
           END-IF
           ADD FNWAIV-USD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE ORDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2340-DESCRIBE-ORDER-TYPE.
           EVALUATE FNWAIV-ORDER-TYPE-CD
               WHEN 'DTU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KZV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PHV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PZZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WMC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XWF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT USD YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2350-SPLIT-USD.
           COMPUTE WS-WORK-AMT = FNWAIV-USD-YTD / 2
           COMPUTE WS-WORK-AMT-2 = FNWAIV-USD-YTD - (WS-WORK-AMT * 2)
           COMPUTE FNWAIV-BUYBACK-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * C -> E WHEN EFFECTIVE DATE IS PAST
      * E -> G WHEN EFFECTIVE DATE IS PAST
      * G -> P WHEN EFFECTIVE DATE IS PAST
      * P -> W WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2360-SET-CURRENCY.
           EVALUATE FNWAIV-CURRENCY-CD
               WHEN 'C'
                   IF FNWAIV-EFFECTIVE-DATE > 0 AND
                       FNWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'E' TO FNWAIV-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF FNWAIV-EFFECTIVE-DATE > 0 AND
                       FNWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'G' TO FNWAIV-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF FNWAIV-EFFECTIVE-DATE > 0 AND
                       FNWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'P' TO FNWAIV-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF FNWAIV-EFFECTIVE-DATE > 0 AND
                       FNWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'W' TO FNWAIV-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2370-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * C -> E WHEN EFFECTIVE DATE IS PAST
      * E -> G WHEN EFFECTIVE DATE IS PAST
      * G -> P WHEN EFFECTIVE DATE IS PAST
      * P -> W WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2380-SET-CURRENCY.
           EVALUATE FNWAIV-CURRENCY-CD
               WHEN 'C'
                   IF FNWAIV-EFFECTIVE-DATE > 0 AND
                       FNWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'E' TO FNWAIV-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF FNWAIV-EFFECTIVE-DATE > 0 AND
                       FNWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'G' TO FNWAIV-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF FNWAIV-EFFECTIVE-DATE > 0 AND
                       FNWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'P' TO FNWAIV-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF FNWAIV-EFFECTIVE-DATE > 0 AND
                       FNWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'W' TO FNWAIV-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2390-DESCRIBE-SOURCE.
           EVALUATE FNWAIV-SOURCE-CD
               WHEN 'DG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'NW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE MARGIN AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2400-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNWAIV-CURRENCY-CD
                   ADD FNWAIV-MARGIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNWAIV-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FNWAIV-MARGIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E647 NAME REQUIRED
      * E812 USD YTD NEGATIVE
      * E356 USD YTD OVER 250000
      * E840 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2410-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNWAIV-NAME = SPACES
                   MOVE 'E647' TO WS-REJECT-CODE
               WHEN FNWAIV-USD-YTD < 0
                   MOVE 'E812' TO WS-REJECT-CODE
               WHEN FNWAIV-USD-YTD > 250000.00
                   MOVE 'E356' TO WS-REJECT-CODE
               WHEN FNWAIV-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E840' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2420-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNWAIV-SOURCE-CD
                   ADD FNWAIV-BUYBACK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNWAIV-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FNWAIV-BUYBACK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN GRID TIER FOR THE MARGIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2430-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-GRID-RATE(1) TO FNWAIV-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF FNWAIV-MARGIN-AMT NOT < WS-MARGIN-GRID-LOW(WS-TX)
                   MOVE WS-MARGIN-GRID-RATE(WS-TX) TO FNWAIV-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNWAIV-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNWAIV-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2440-DESCRIBE-TYPE.
           EVALUATE FNWAIV-TYPE-CD
               WHEN 'CFP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE MARGIN YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2450-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FNWAIV-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNWAIV-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = FNWAIV-MARGIN-YTD *
                       FNWAIV-RATE / 200
               WHEN FNWAIV-CURRENCY-CD = 'C'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN FNWAIV-CURRENCY-CD = 'E'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = FNWAIV-MARGIN-YTD *
                       FNWAIV-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2460-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * FNWAIV-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2470-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN GRID TIER FOR THE USD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2480-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-GRID-RATE(1) TO FNWAIV-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF FNWAIV-USD-AMT NOT < WS-MARGIN-GRID-LOW(WS-TX)
                   MOVE WS-MARGIN-GRID-RATE(WS-TX) TO FNWAIV-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNWAIV-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNWAIV-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY ORDER TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2490-ACCUM-ORDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNWAIV-ORDER-TYPE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNWAIV-ORDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MARGIN YTD.
      *----------------------------------------------------------------*
       2500-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNWAIV-MARGIN-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FNWAIV-MARGIN-YTD
           END-IF
           ADD FNWAIV-MARGIN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E419 NAME REQUIRED
      * E622 BUYBACK YTD NEGATIVE
      * E150 BUYBACK YTD OVER 250000
      * E944 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2510-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNWAIV-NAME = SPACES
                   MOVE 'E419' TO WS-REJECT-CODE
               WHEN FNWAIV-BUYBACK-YTD < 0
                   MOVE 'E622' TO WS-REJECT-CODE
               WHEN FNWAIV-BUYBACK-YTD > 250000.00
                   MOVE 'E150' TO WS-REJECT-CODE
               WHEN FNWAIV-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E944' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E681 CUST ID REQUIRED
      * E313 FOREIGN AMT NEGATIVE
      * E389 FOREIGN AMT OVER 5000
      * E990 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2520-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNWAIV-CUST-ID = SPACES
                   MOVE 'E681' TO WS-REJECT-CODE
               WHEN FNWAIV-FOREIGN-AMT < 0
                   MOVE 'E313' TO WS-REJECT-CODE
               WHEN FNWAIV-FOREIGN-AMT > 5000.00
                   MOVE 'E389' TO WS-REJECT-CODE
               WHEN FNWAIV-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E990' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2530-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF FNWAIV-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNWAIV-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO FNWAIV-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MARGIN AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2540-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNWAIV-CURRENCY-CD
                   ADD FNWAIV-MARGIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNWAIV-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FNWAIV-MARGIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E127 CUST ID REQUIRED
      * E587 WORK AMT 2 NEGATIVE
      * E252 WORK AMT 2 OVER 10000
      * E629 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2550-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNWAIV-CUST-ID = SPACES
                   MOVE 'E127' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E587' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E252' TO WS-REJECT-CODE
               WHEN FNWAIV-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E629' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2560-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2570-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF FNWAIV-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNWAIV-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO FNWAIV-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2580-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FNWAIV-NOTICE-DATE > 0 AND FNWAIV-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNWAIV-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2590-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF FNWAIV-EFFECTIVE-DATE > 0 AND FNWAIV-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNWAIV-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNWAIV-BUYBACK-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FNWAIV-BUYBACK-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FNWAIV-BUYBACK-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FNWAIV-BUYBACK-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNWAIV-BUYBACK-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * USD YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2600-FLAG-USD.
           IF FNWAIV-USD-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FNWAIV-USD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * USD AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2610-LIMIT-USD.
           IF FNWAIV-USD-AMT < 50.00
               MOVE 50.00 TO FNWAIV-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNWAIV-USD-AMT > 1000.00
               MOVE 1000.00 TO FNWAIV-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MARGIN YTD FEE ON THE FOREIGN AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2620-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN FNWAIV-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO FNWAIV-MARGIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNWAIV-PREFERRED-FLAG = 'Y'
                   COMPUTE FNWAIV-MARGIN-YTD ROUNDED =
                       FNWAIV-FOREIGN-AMT * FNWAIV-PENALTY-PCT / 200
               WHEN FNWAIV-SOURCE-CD = 'DG'
                   MOVE 15.00 TO FNWAIV-MARGIN-YTD
               WHEN FNWAIV-SOURCE-CD = 'FS'
                   MOVE 3.00 TO FNWAIV-MARGIN-YTD
               WHEN OTHER
                   COMPUTE FNWAIV-MARGIN-YTD ROUNDED =
                       FNWAIV-FOREIGN-AMT * FNWAIV-PENALTY-PCT / 100
           END-EVALUATE
           IF FNWAIV-MARGIN-YTD > 0 AND FNWAIV-MARGIN-YTD < 2.00
               MOVE 2.00 TO FNWAIV-MARGIN-YTD
           END-IF
           IF FNWAIV-MARGIN-YTD > 150.00
               MOVE 150.00 TO FNWAIV-MARGIN-YTD
           END-IF
           ADD FNWAIV-MARGIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT USD YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2630-SPLIT-USD.
           COMPUTE WS-WORK-AMT = FNWAIV-USD-YTD / 2
           COMPUTE WS-WORK-AMT-2 = FNWAIV-USD-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2640-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FNWAIV-NOTICE-DATE > 0 AND FNWAIV-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNWAIV-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNWAIV-BUYBACK-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD FNWAIV-BUYBACK-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD FNWAIV-BUYBACK-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD FNWAIV-BUYBACK-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNWAIV-BUYBACK-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK YTD FEE ON THE FOREIGN AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2650-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN FNWAIV-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO FNWAIV-BUYBACK-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNWAIV-REVIEW-FLAG = 'Y'
                   COMPUTE FNWAIV-BUYBACK-YTD ROUNDED =
                       FNWAIV-FOREIGN-AMT * FNWAIV-RATE / 200
               WHEN FNWAIV-ORDER-TYPE-CD = 'DTU'
                   MOVE 3.00 TO FNWAIV-BUYBACK-YTD
               WHEN FNWAIV-ORDER-TYPE-CD = 'KZV'
                   MOVE 15.00 TO FNWAIV-BUYBACK-YTD
               WHEN OTHER
                   COMPUTE FNWAIV-BUYBACK-YTD ROUNDED =
                       FNWAIV-FOREIGN-AMT * FNWAIV-RATE / 100
           END-EVALUATE
           IF FNWAIV-BUYBACK-YTD > 0 AND FNWAIV-BUYBACK-YTD < 10.00
               MOVE 10.00 TO FNWAIV-BUYBACK-YTD
           END-IF
           IF FNWAIV-BUYBACK-YTD > 250.00
               MOVE 250.00 TO FNWAIV-BUYBACK-YTD
           END-IF
           ADD FNWAIV-BUYBACK-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT BUYBACK YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2660-SPLIT-BUYBACK.
           COMPUTE WS-WORK-AMT = FNWAIV-BUYBACK-YTD / 3
           COMPUTE WS-WORK-AMT-2 = FNWAIV-BUYBACK-YTD - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2670-LIMIT-BUYBACK.
           IF FNWAIV-BUYBACK-YTD < 50.00
               MOVE 50.00 TO FNWAIV-BUYBACK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNWAIV-BUYBACK-YTD > 500.00
               MOVE 500.00 TO FNWAIV-BUYBACK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2680-COUNT-EXCEPT.
           IF FNWAIV-EXCEPT-CNT < 9999999
               ADD 1 TO FNWAIV-EXCEPT-CNT
           END-IF
           IF FNWAIV-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO FNWAIV-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * USD AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2690-LIMIT-USD.
           IF FNWAIV-USD-AMT < 10.00
               MOVE 10.00 TO FNWAIV-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNWAIV-USD-AMT > 1000.00
               MOVE 1000.00 TO FNWAIV-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * USD YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2700-FLAG-USD.
           IF FNWAIV-USD-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FNWAIV-USD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CFP -> FFU WHEN EFFECTIVE DATE IS PAST
      * FFU -> JNR WHEN EFFECTIVE DATE IS PAST
      * JNR -> UGS WHEN EFFECTIVE DATE IS PAST
      * UGS -> XUM WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2710-SET-TYPE.
           EVALUATE FNWAIV-TYPE-CD
               WHEN 'CFP'
                   IF FNWAIV-EFFECTIVE-DATE > 0 AND
                       FNWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'FFU' TO FNWAIV-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FFU'
                   IF FNWAIV-EFFECTIVE-DATE > 0 AND
                       FNWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'JNR' TO FNWAIV-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JNR'
                   IF FNWAIV-EFFECTIVE-DATE > 0 AND
                       FNWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'UGS' TO FNWAIV-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UGS'
                   IF FNWAIV-EFFECTIVE-DATE > 0 AND
                       FNWAIV-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'XUM' TO FNWAIV-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CFP -> FFU WHEN NOTICE DATE IS PAST
      * FFU -> JNR WHEN NOTICE DATE IS PAST
      * JNR -> UGS WHEN NOTICE DATE IS PAST
      * UGS -> XUM WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2720-SET-TYPE.
           EVALUATE FNWAIV-TYPE-CD
               WHEN 'CFP'
                   IF FNWAIV-NOTICE-DATE > 0 AND FNWAIV-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FFU' TO FNWAIV-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FFU'
                   IF FNWAIV-NOTICE-DATE > 0 AND FNWAIV-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JNR' TO FNWAIV-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JNR'
                   IF FNWAIV-NOTICE-DATE > 0 AND FNWAIV-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UGS' TO FNWAIV-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UGS'
                   IF FNWAIV-NOTICE-DATE > 0 AND FNWAIV-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XUM' TO FNWAIV-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2730-ACCRUE-FOREIGN.
           IF FNWAIV-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNWAIV-FOREIGN-AMT * FNWAIV-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2740-FLAG-FOREIGN.
           IF FNWAIV-FOREIGN-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FNWAIV-FOREIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON USD YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2750-ACCRUE-USD.
           IF FNWAIV-USD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNWAIV-USD-YTD * FNWAIV-RATE / 100 / 360
               ADD WS-WORK-AMT TO FNWAIV-MARGIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARGIN YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2760-ACCRUE-MARGIN.
           IF FNWAIV-MARGIN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNWAIV-MARGIN-YTD * FNWAIV-RATE / 100 / 360
               ADD WS-WORK-AMT TO FNWAIV-MARGIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2770-FLAG-FOREIGN.
           IF FNWAIV-FOREIGN-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FNWAIV-FOREIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2780-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2790-LIMIT-FOREIGN.
           IF FNWAIV-FOREIGN-AMT < 10.00
               MOVE 10.00 TO FNWAIV-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNWAIV-FOREIGN-AMT > 2500.00
               MOVE 2500.00 TO FNWAIV-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO FNWAIV-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO FNWAIV-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO FNWAIV-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'FNWAIV EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
