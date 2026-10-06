      *================================================================*
      * PROGRAM  : TRVAL                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : PERSONAL TRUST RULE - VALIDATION. CALLED BY THE     *
      *            PERSONAL TRUST BATCH AND ONLINE PROGRAMS.           *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-07-03 BHS  ORIGINAL                                       *
      * 1990-01-23 RJT  INCOME FEE SCHEDULE UPDATE INC3635             *
      * 1994-10-17 MKL  TAX ROUNDING - HALF UP PRB5292                 *
      * 1996-06-22 LMB  TRUSTEE-FEE NOTICE TEXT PER LEGAL REQ7444      *
      * 1996-06-27 RJT  TAX FEE SCHEDULE UPDATE CHG4074                *
      * 2002-12-07 CRP  PRINCIPAL FEE SCHEDULE UPDATE CHG0742          *
      * 2002-10-14 LMB  DISTRIB NOTICE TEXT PER LEGAL REQ5757          *
      * 2002-01-03 JLM  PRINCIPAL RERUN PROTECTION CHG7953             *
      * 2003-12-13 BHS  TRUSTEE-FEE LIMIT REVISED CHG2886              *
      * 2004-05-22 JLM  INCOME NOTICE TEXT PER LEGAL REQ0985           *
      * 2005-06-27 TWH  AUDIT FINDING - INCOME CONTROL TOTALS CHG7690  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRVAL.
       AUTHOR. D P SHAW.

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
      * WS-MARKET-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MARKET-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000027500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000284000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000347000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000316500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010750000000148000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000605000'.
       01  WS-MARKET-BAND-TBL REDEFINES WS-MARKET-BAND-VALUES.
           05  WS-MARKET-BAND-ROW OCCURS 6 TIMES.
               10  WS-MARKET-BAND-LOW      PIC 9(09)V99.
               10  WS-MARKET-BAND-RATE     PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY TRVALP.

       PROCEDURE DIVISION USING TRVAL-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCRUE-PRINCIPAL
           PERFORM 2010-ACCRUE-TAX
           PERFORM 2020-PRORATE-DISTRIB
           PERFORM 2030-LIMIT-PRINCIPAL
           PERFORM 2040-PRORATE-MARKET
           PERFORM 2050-LIMIT-MARKET
           PERFORM 2060-SPLIT-DISTRIB
           PERFORM 2070-SPLIT-DISTRIB
           PERFORM 2080-ACCUM-REASON
           PERFORM 2090-FLAG-PRINCIPAL
           PERFORM 2100-SPLIT-PRINCIPAL
           PERFORM 2110-EDIT-CONTACT
           PERFORM 2120-PRORATE-TAX
           PERFORM 2130-DESCRIBE-TRUST-TYPE
           PERFORM 2140-COUNT-NOTICE
           PERFORM 2150-ACCRUE-MARGIN
           PERFORM 2160-CHECK-NOTICE
           PERFORM 2170-PRORATE-PRINCIPAL
           PERFORM 2180-SELECT-RATE
           PERFORM 2190-DESCRIBE-STATUS
           PERFORM 2200-SELECT-MARGIN
           PERFORM 2210-FLAG-MARKET
           PERFORM 2220-LIMIT-PRINCIPAL
           PERFORM 2230-LIMIT-MARKET
           PERFORM 2240-CHECK-NOTICE
           PERFORM 2250-EDIT-CONTACT
           PERFORM 2260-LIMIT-DISTRIB
           PERFORM 2270-ASSESS-NOTICE-SENT
           PERFORM 2280-ASSESS-EXEMPT
           PERFORM 2290-SET-TRUST-TYPE
           PERFORM 2300-CHECK-MATURITY
           PERFORM 2310-CHECK-CLOSE
           PERFORM 2320-AGE-CLOSE
           PERFORM 2330-DESCRIBE-TRUST-TYPE
           PERFORM 2340-ASSESS-EXEMPT
           PERFORM 2350-ASSESS-NOTICE-SENT
           PERFORM 2360-SPLIT-TAX
           PERFORM 2370-CHECK-LAST-MAINT
           PERFORM 2380-SPLIT-MARKET
           PERFORM 2390-AGE-LAST-MAINT
           PERFORM 2400-SET-TRUST-TYPE
           PERFORM 2410-PRORATE-MARKET
           PERFORM 2420-ASSESS-NOTICE-SENT
           PERFORM 2430-EDIT-CONTACT
           PERFORM 2440-PRORATE-PRINCIPAL
           PERFORM 2450-DESCRIBE-REGION
           PERFORM 2460-SELECT-RATE
           PERFORM 2470-ASSESS-REVIEW
           PERFORM 2480-SPLIT-MARKET
           PERFORM 2490-LIMIT-DISTRIB
           PERFORM 2500-DESCRIBE-REASON
           PERFORM 2510-CHECK-LAST-MAINT
           PERFORM 2520-SET-REGION
           PERFORM 2530-SET-TRUST-TYPE
           PERFORM 2540-CHECK-CLOSE
           PERFORM 2550-DESCRIBE-REGION
           PERFORM 2560-CHECK-CLOSE
           PERFORM 2570-SET-REASON
           PERFORM 2580-LIMIT-ITEM
           PERFORM 2590-FLAG-MARKET
           PERFORM 2600-ASSESS-NOTICE-SENT
           PERFORM 2610-DESCRIBE-REASON
           PERFORM 2620-LIMIT-TAX
           PERFORM 2630-SET-REGION
           PERFORM 2640-ACCRUE-RATE
           PERFORM 2650-COUNT-NOTICE
           PERFORM 2660-SET-TRUST-TYPE
           PERFORM 2670-ACCRUE-MARGIN
           PERFORM 2680-FLAG-MARKET
           PERFORM 2690-CHECK-NOTICE
           PERFORM 2700-ACCUM-STATUS
           PERFORM 2710-AGE-LAST-MAINT
           PERFORM 2720-COUNT-NOTICE
           PERFORM 2730-LIMIT-ITEM
           PERFORM 2740-EDIT-ADDR
           PERFORM 2750-ACCRUE-MARKET
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO TRVAL-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2000-ACCRUE-PRINCIPAL.
           IF TRVAL-PRINCIPAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRVAL-PRINCIPAL-YTD * TRVAL-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRVAL-MARKET-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2010-ACCRUE-TAX.
           IF TRVAL-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRVAL-TAX-AMT * TRVAL-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2020-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TRVAL-DISTRIB-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2030-LIMIT-PRINCIPAL.
           IF TRVAL-PRINCIPAL-AMT < 10.00
               MOVE 10.00 TO TRVAL-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRVAL-PRINCIPAL-AMT > 10000.00
               MOVE 10000.00 TO TRVAL-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MARKET YTD.
      *----------------------------------------------------------------*
       2040-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRVAL-MARKET-YTD ROUNDED =
                   TRVAL-PRINCIPAL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRVAL-MARKET-YTD
           END-IF
           ADD TRVAL-MARKET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * MARKET YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2050-LIMIT-MARKET.
           IF TRVAL-MARKET-YTD < 10.00
               MOVE 10.00 TO TRVAL-MARKET-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRVAL-MARKET-YTD > 5000.00
               MOVE 5000.00 TO TRVAL-MARKET-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2060-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = TRVAL-DISTRIB-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TRVAL-DISTRIB-YTD - (WS-WORK-AMT * 2)
           COMPUTE TRVAL-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2070-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = TRVAL-DISTRIB-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TRVAL-DISTRIB-YTD - (WS-WORK-AMT * 4)
           COMPUTE TRVAL-MARKET-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MARKET AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2080-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRVAL-REASON-CD
                   ADD TRVAL-MARKET-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRVAL-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRVAL-MARKET-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2090-FLAG-PRINCIPAL.
           IF TRVAL-PRINCIPAL-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRVAL-PRINCIPAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2100-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = TRVAL-PRINCIPAL-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TRVAL-PRINCIPAL-YTD - (WS-WORK-AMT *
               3)
           COMPUTE TRVAL-DISTRIB-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E714 CONTACT REQUIRED
      * E792 TAX AMT NEGATIVE
      * E623 TAX AMT OVER 5000
      * E614 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2110-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRVAL-CONTACT = SPACES
                   MOVE 'E714' TO WS-REJECT-CODE
               WHEN TRVAL-TAX-AMT < 0
                   MOVE 'E792' TO WS-REJECT-CODE
               WHEN TRVAL-TAX-AMT > 5000.00
                   MOVE 'E623' TO WS-REJECT-CODE
               WHEN TRVAL-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E614' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MARKET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TAX AMT.
      *----------------------------------------------------------------*
       2120-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRVAL-TAX-AMT ROUNDED =
                   TRVAL-MARKET-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRVAL-TAX-AMT
           END-IF
           ADD TRVAL-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TRUST TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2130-DESCRIBE-TRUST-TYPE.
           EVALUATE TRVAL-TRUST-TYPE-CD
               WHEN 'FLJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MNG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VFN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XPL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2140-COUNT-NOTICE.
           IF TRVAL-NOTICE-CNT < 9999999
               ADD 1 TO TRVAL-NOTICE-CNT
           END-IF
           IF TRVAL-NOTICE-CNT NOT < 10
               MOVE 'Y' TO TRVAL-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2150-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TRVAL-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRVAL-PRINCIPAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2160-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TRVAL-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRVAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TRVAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINCIPAL YTD.
      *----------------------------------------------------------------*
       2170-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRVAL-PRINCIPAL-YTD ROUNDED =
                   TRVAL-TAX-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRVAL-PRINCIPAL-YTD
           END-IF
           ADD TRVAL-PRINCIPAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE MARKET BAND TIER FOR THE MARKET YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2180-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-MARKET-BAND-RATE(1) TO TRVAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TRVAL-MARKET-YTD NOT < WS-MARKET-BAND-LOW(WS-TX)
                   MOVE WS-MARKET-BAND-RATE(WS-TX) TO TRVAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRVAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRVAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2190-DESCRIBE-STATUS.
           EVALUATE TRVAL-STATUS-CD
               WHEN 'EH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE MARKET BAND TIER FOR THE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2200-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-MARKET-BAND-RATE(1) TO TRVAL-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TRVAL-TAX-AMT NOT < WS-MARKET-BAND-LOW(WS-TX)
                   MOVE WS-MARKET-BAND-RATE(WS-TX) TO TRVAL-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRVAL-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRVAL-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARKET AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2210-FLAG-MARKET.
           IF TRVAL-MARKET-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRVAL-MARKET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2220-LIMIT-PRINCIPAL.
           IF TRVAL-PRINCIPAL-YTD < 0.00
               MOVE 0.00 TO TRVAL-PRINCIPAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRVAL-PRINCIPAL-YTD > 10000.00
               MOVE 10000.00 TO TRVAL-PRINCIPAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARKET YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2230-LIMIT-MARKET.
           IF TRVAL-MARKET-YTD < 50.00
               MOVE 50.00 TO TRVAL-MARKET-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRVAL-MARKET-YTD > 50000.00
               MOVE 50000.00 TO TRVAL-MARKET-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2240-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TRVAL-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRVAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TRVAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E111 CONTACT REQUIRED
      * E180 TAX AMT NEGATIVE
      * E361 TAX AMT OVER 10000
      * E036 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2250-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRVAL-CONTACT = SPACES
                   MOVE 'E111' TO WS-REJECT-CODE
               WHEN TRVAL-TAX-AMT < 0
                   MOVE 'E180' TO WS-REJECT-CODE
               WHEN TRVAL-TAX-AMT > 10000.00
                   MOVE 'E361' TO WS-REJECT-CODE
               WHEN TRVAL-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E036' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2260-LIMIT-DISTRIB.
           IF TRVAL-DISTRIB-YTD < 50.00
               MOVE 50.00 TO TRVAL-DISTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRVAL-DISTRIB-YTD > 10000.00
               MOVE 10000.00 TO TRVAL-DISTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAX FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2270-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRVAL-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRVAL-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRVAL-EXEMPT-FLAG = 'Y'
                   COMPUTE TRVAL-TAX-AMT ROUNDED = WS-WORK-AMT-2 *
                       TRVAL-RATE / 200
               WHEN TRVAL-TRUST-TYPE-CD = 'FLJ'
                   MOVE 3.00 TO TRVAL-TAX-AMT
               WHEN TRVAL-TRUST-TYPE-CD = 'MNG'
                   MOVE 3.00 TO TRVAL-TAX-AMT
               WHEN OTHER
                   COMPUTE TRVAL-TAX-AMT ROUNDED = WS-WORK-AMT-2 *
                       TRVAL-RATE / 100
           END-EVALUATE
           IF TRVAL-TAX-AMT > 0 AND TRVAL-TAX-AMT < 2.00
               MOVE 2.00 TO TRVAL-TAX-AMT
           END-IF
           IF TRVAL-TAX-AMT > 35.00
               MOVE 35.00 TO TRVAL-TAX-AMT
           END-IF
           ADD TRVAL-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE TAX FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2280-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRVAL-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRVAL-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRVAL-REVIEW-FLAG = 'Y'
                   COMPUTE TRVAL-TAX-AMT ROUNDED = WS-WORK-AMT *
                       TRVAL-RATE / 200
               WHEN TRVAL-REASON-CD = 'H'
                   MOVE 5.00 TO TRVAL-TAX-AMT
               WHEN TRVAL-REASON-CD = 'M'
                   MOVE 3.00 TO TRVAL-TAX-AMT
               WHEN OTHER
                   COMPUTE TRVAL-TAX-AMT ROUNDED = WS-WORK-AMT *
                       TRVAL-RATE / 100
           END-EVALUATE
           IF TRVAL-TAX-AMT > 0 AND TRVAL-TAX-AMT < 2.00
               MOVE 2.00 TO TRVAL-TAX-AMT
           END-IF
           IF TRVAL-TAX-AMT > 150.00
               MOVE 150.00 TO TRVAL-TAX-AMT
           END-IF
           ADD TRVAL-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN MATURITY DATE IS PAST
      * MNG -> VFN WHEN MATURITY DATE IS PAST
      * VFN -> XPL WHEN MATURITY DATE IS PAST
      * XPL -> FLJ WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2290-SET-TRUST-TYPE.
           EVALUATE TRVAL-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRVAL-MATURITY-DATE > 0 AND TRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRVAL-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRVAL-MATURITY-DATE > 0 AND TRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRVAL-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRVAL-MATURITY-DATE > 0 AND TRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRVAL-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRVAL-MATURITY-DATE > 0 AND TRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRVAL-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2300-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TRVAL-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRVAL-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TRVAL-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2310-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRVAL-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRVAL-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TRVAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2320-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRVAL-CLOSE-DATE > 0 AND TRVAL-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRVAL-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRVAL-MARKET-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRVAL-MARKET-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRVAL-MARKET-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRVAL-MARKET-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRVAL-MARKET-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRUST TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2330-DESCRIBE-TRUST-TYPE.
           EVALUATE TRVAL-TRUST-TYPE-CD
               WHEN 'FLJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MNG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VFN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XPL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE MARKET FEE ON THE DISTRIB YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2340-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRVAL-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRVAL-MARKET-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRVAL-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TRVAL-MARKET-AMT ROUNDED = TRVAL-DISTRIB-YTD
                       * TRVAL-MARGIN / 200
               WHEN TRVAL-TRUST-TYPE-CD = 'FLJ'
                   MOVE 15.00 TO TRVAL-MARKET-AMT
               WHEN TRVAL-TRUST-TYPE-CD = 'MNG'
                   MOVE 15.00 TO TRVAL-MARKET-AMT
               WHEN OTHER
                   COMPUTE TRVAL-MARKET-AMT ROUNDED = TRVAL-DISTRIB-YTD
                       * TRVAL-MARGIN / 100
           END-EVALUATE
           IF TRVAL-MARKET-AMT > 0 AND TRVAL-MARKET-AMT < 2.00
               MOVE 2.00 TO TRVAL-MARKET-AMT
           END-IF
           IF TRVAL-MARKET-AMT > 250.00
               MOVE 250.00 TO TRVAL-MARKET-AMT
           END-IF
           ADD TRVAL-MARKET-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE TAX FEE ON THE MARKET AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2350-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRVAL-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRVAL-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRVAL-EXEMPT-FLAG = 'Y'
                   COMPUTE TRVAL-TAX-AMT ROUNDED = TRVAL-MARKET-AMT *
                       TRVAL-MARGIN / 200
               WHEN TRVAL-TRUST-TYPE-CD = 'FLJ'
                   MOVE 10.00 TO TRVAL-TAX-AMT
               WHEN TRVAL-TRUST-TYPE-CD = 'MNG'
                   MOVE 15.00 TO TRVAL-TAX-AMT
               WHEN OTHER
                   COMPUTE TRVAL-TAX-AMT ROUNDED = TRVAL-MARKET-AMT *
                       TRVAL-MARGIN / 100
           END-EVALUATE
           IF TRVAL-TAX-AMT > 0 AND TRVAL-TAX-AMT < 2.00
               MOVE 2.00 TO TRVAL-TAX-AMT
           END-IF
           IF TRVAL-TAX-AMT > 75.00
               MOVE 75.00 TO TRVAL-TAX-AMT
           END-IF
           ADD TRVAL-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT TAX AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2360-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = TRVAL-TAX-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRVAL-TAX-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2370-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TRVAL-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TRVAL-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2380-SPLIT-MARKET.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE TRVAL-MARKET-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2390-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TRVAL-LAST-MAINT-DATE > 0 AND TRVAL-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRVAL-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRVAL-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TRVAL-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TRVAL-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRVAL-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN CLOSE DATE IS PAST
      * MNG -> VFN WHEN CLOSE DATE IS PAST
      * VFN -> XPL WHEN CLOSE DATE IS PAST
      * XPL -> FLJ WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2400-SET-TRUST-TYPE.
           EVALUATE TRVAL-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRVAL-CLOSE-DATE > 0 AND TRVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRVAL-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRVAL-CLOSE-DATE > 0 AND TRVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRVAL-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRVAL-CLOSE-DATE > 0 AND TRVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRVAL-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRVAL-CLOSE-DATE > 0 AND TRVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRVAL-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MARKET YTD.
      *----------------------------------------------------------------*
       2410-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRVAL-MARKET-YTD ROUNDED =
                   TRVAL-PRINCIPAL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRVAL-MARKET-YTD
           END-IF
           ADD TRVAL-MARKET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE MARKET AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2420-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRVAL-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRVAL-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRVAL-REVIEW-FLAG = 'Y'
                   COMPUTE TRVAL-PRINCIPAL-AMT ROUNDED =
                       TRVAL-MARKET-AMT * TRVAL-MARGIN / 200
               WHEN TRVAL-REASON-CD = 'H'
                   MOVE 15.00 TO TRVAL-PRINCIPAL-AMT
               WHEN TRVAL-REASON-CD = 'M'
                   MOVE 7.00 TO TRVAL-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE TRVAL-PRINCIPAL-AMT ROUNDED =
                       TRVAL-MARKET-AMT * TRVAL-MARGIN / 100
           END-EVALUATE
           IF TRVAL-PRINCIPAL-AMT > 0 AND TRVAL-PRINCIPAL-AMT < 2.00
               MOVE 2.00 TO TRVAL-PRINCIPAL-AMT
           END-IF
           IF TRVAL-PRINCIPAL-AMT > 25.00
               MOVE 25.00 TO TRVAL-PRINCIPAL-AMT
           END-IF
           ADD TRVAL-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E757 CONTACT REQUIRED
      * E136 TAX AMT NEGATIVE
      * E816 TAX AMT OVER 50000
      * E852 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2430-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRVAL-CONTACT = SPACES
                   MOVE 'E757' TO WS-REJECT-CODE
               WHEN TRVAL-TAX-AMT < 0
                   MOVE 'E136' TO WS-REJECT-CODE
               WHEN TRVAL-TAX-AMT > 50000.00
                   MOVE 'E816' TO WS-REJECT-CODE
               WHEN TRVAL-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E852' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MARKET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINCIPAL YTD.
      *----------------------------------------------------------------*
       2440-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRVAL-PRINCIPAL-YTD ROUNDED =
                   TRVAL-MARKET-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRVAL-PRINCIPAL-YTD
           END-IF
           ADD TRVAL-PRINCIPAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2450-DESCRIBE-REGION.
           EVALUATE TRVAL-REGION-CD
               WHEN 'A'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'N'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE MARKET BAND TIER FOR THE DISTRIB YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2460-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-MARKET-BAND-RATE(1) TO TRVAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TRVAL-DISTRIB-YTD NOT < WS-MARKET-BAND-LOW(WS-TX)
                   MOVE WS-MARKET-BAND-RATE(WS-TX) TO TRVAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRVAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRVAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE TAX AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2470-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TRVAL-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TRVAL-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRVAL-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TRVAL-PRINCIPAL-AMT ROUNDED = TRVAL-TAX-AMT *
                       TRVAL-RATE / 200
               WHEN TRVAL-REASON-CD = 'H'
                   MOVE 7.00 TO TRVAL-PRINCIPAL-AMT
               WHEN TRVAL-REASON-CD = 'M'
                   MOVE 3.00 TO TRVAL-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE TRVAL-PRINCIPAL-AMT ROUNDED = TRVAL-TAX-AMT *
                       TRVAL-RATE / 100
           END-EVALUATE
           IF TRVAL-PRINCIPAL-AMT > 0 AND TRVAL-PRINCIPAL-AMT < 2.00
               MOVE 2.00 TO TRVAL-PRINCIPAL-AMT
           END-IF
           IF TRVAL-PRINCIPAL-AMT > 50.00
               MOVE 50.00 TO TRVAL-PRINCIPAL-AMT
           END-IF
           ADD TRVAL-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT MARKET YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2480-SPLIT-MARKET.
           COMPUTE WS-WORK-AMT = TRVAL-MARKET-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TRVAL-MARKET-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2490-LIMIT-DISTRIB.
           IF TRVAL-DISTRIB-YTD < 25.00
               MOVE 25.00 TO TRVAL-DISTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRVAL-DISTRIB-YTD > 2500.00
               MOVE 2500.00 TO TRVAL-DISTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2500-DESCRIBE-REASON.
           EVALUATE TRVAL-REASON-CD
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'M'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2510-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TRVAL-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TRVAL-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> D WHEN NOTICE DATE IS PAST
      * D -> N WHEN NOTICE DATE IS PAST
      * N -> T WHEN NOTICE DATE IS PAST
      * T -> A WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2520-SET-REGION.
           EVALUATE TRVAL-REGION-CD
               WHEN 'A'
                   IF TRVAL-NOTICE-DATE > 0 AND TRVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF TRVAL-NOTICE-DATE > 0 AND TRVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRVAL-NOTICE-DATE > 0 AND TRVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRVAL-NOTICE-DATE > 0 AND TRVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'A' TO TRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN CLOSE DATE IS PAST
      * MNG -> VFN WHEN CLOSE DATE IS PAST
      * VFN -> XPL WHEN CLOSE DATE IS PAST
      * XPL -> FLJ WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2530-SET-TRUST-TYPE.
           EVALUATE TRVAL-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRVAL-CLOSE-DATE > 0 AND TRVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRVAL-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRVAL-CLOSE-DATE > 0 AND TRVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRVAL-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRVAL-CLOSE-DATE > 0 AND TRVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRVAL-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRVAL-CLOSE-DATE > 0 AND TRVAL-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRVAL-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2540-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRVAL-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRVAL-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TRVAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2550-DESCRIBE-REGION.
           EVALUATE TRVAL-REGION-CD
               WHEN 'A'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2560-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRVAL-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRVAL-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TRVAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * H -> M WHEN MATURITY DATE IS PAST
      * M -> T WHEN MATURITY DATE IS PAST
      * T -> H WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2570-SET-REASON.
           EVALUATE TRVAL-REASON-CD
               WHEN 'H'
                   IF TRVAL-MATURITY-DATE > 0 AND TRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TRVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TRVAL-MATURITY-DATE > 0 AND TRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRVAL-MATURITY-DATE > 0 AND TRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO TRVAL-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2580-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARKET AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2590-FLAG-MARKET.
           IF TRVAL-MARKET-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRVAL-MARKET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE MARKET YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2600-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRVAL-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRVAL-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRVAL-EXEMPT-FLAG = 'Y'
                   COMPUTE TRVAL-PRINCIPAL-AMT ROUNDED =
                       TRVAL-MARKET-YTD * TRVAL-MARGIN / 200
               WHEN TRVAL-TRUST-TYPE-CD = 'FLJ'
                   MOVE 3.00 TO TRVAL-PRINCIPAL-AMT
               WHEN TRVAL-TRUST-TYPE-CD = 'MNG'
                   MOVE 20.00 TO TRVAL-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE TRVAL-PRINCIPAL-AMT ROUNDED =
                       TRVAL-MARKET-YTD * TRVAL-MARGIN / 100
           END-EVALUATE
           IF TRVAL-PRINCIPAL-AMT > 0 AND TRVAL-PRINCIPAL-AMT < 5.00
               MOVE 5.00 TO TRVAL-PRINCIPAL-AMT
           END-IF
           IF TRVAL-PRINCIPAL-AMT > 100.00
               MOVE 100.00 TO TRVAL-PRINCIPAL-AMT
           END-IF
           ADD TRVAL-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2610-DESCRIBE-REASON.
           EVALUATE TRVAL-REASON-CD
               WHEN 'H'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TAX AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2620-LIMIT-TAX.
           IF TRVAL-TAX-AMT < 100.00
               MOVE 100.00 TO TRVAL-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRVAL-TAX-AMT > 1000.00
               MOVE 1000.00 TO TRVAL-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> D WHEN MATURITY DATE IS PAST
      * D -> N WHEN MATURITY DATE IS PAST
      * N -> T WHEN MATURITY DATE IS PAST
      * T -> A WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2630-SET-REGION.
           EVALUATE TRVAL-REGION-CD
               WHEN 'A'
                   IF TRVAL-MATURITY-DATE > 0 AND TRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF TRVAL-MATURITY-DATE > 0 AND TRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRVAL-MATURITY-DATE > 0 AND TRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRVAL-MATURITY-DATE > 0 AND TRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'A' TO TRVAL-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2640-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TRVAL-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRVAL-MARKET-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2650-COUNT-NOTICE.
           IF TRVAL-NOTICE-CNT < 9999999
               ADD 1 TO TRVAL-NOTICE-CNT
           END-IF
           IF TRVAL-NOTICE-CNT NOT < 5
               MOVE 'Y' TO TRVAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN MATURITY DATE IS PAST
      * MNG -> VFN WHEN MATURITY DATE IS PAST
      * VFN -> XPL WHEN MATURITY DATE IS PAST
      * XPL -> FLJ WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2660-SET-TRUST-TYPE.
           EVALUATE TRVAL-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRVAL-MATURITY-DATE > 0 AND TRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRVAL-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRVAL-MATURITY-DATE > 0 AND TRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRVAL-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRVAL-MATURITY-DATE > 0 AND TRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRVAL-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRVAL-MATURITY-DATE > 0 AND TRVAL-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRVAL-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2670-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TRVAL-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRVAL-PRINCIPAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARKET YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2680-FLAG-MARKET.
           IF TRVAL-MARKET-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRVAL-MARKET-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2690-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TRVAL-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRVAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TRVAL-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2700-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRVAL-STATUS-CD
                   ADD TRVAL-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRVAL-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRVAL-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2710-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TRVAL-LAST-MAINT-DATE > 0 AND TRVAL-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRVAL-PRINCIPAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRVAL-PRINCIPAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TRVAL-PRINCIPAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TRVAL-PRINCIPAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRVAL-PRINCIPAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2720-COUNT-NOTICE.
           IF TRVAL-NOTICE-CNT < 9999999
               ADD 1 TO TRVAL-NOTICE-CNT
           END-IF
           IF TRVAL-NOTICE-CNT NOT < 12
               MOVE 'Y' TO TRVAL-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2730-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E318 ADDR REQUIRED
      * E542 WORK AMT 2 NEGATIVE
      * E263 WORK AMT 2 OVER 1000000
      * E852 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2740-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRVAL-ADDR = SPACES
                   MOVE 'E318' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E542' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E263' TO WS-REJECT-CODE
               WHEN TRVAL-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E852' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARKET AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2750-ACCRUE-MARKET.
           IF TRVAL-MARKET-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRVAL-MARKET-AMT * TRVAL-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO TRVAL-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO TRVAL-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO TRVAL-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'TRVAL EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
