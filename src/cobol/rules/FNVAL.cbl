      *================================================================*
      * PROGRAM  : FNVAL                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : FOREIGN BANKNOTES RULE - FEES. CALLED BY THE FOREIGN*
      *            BANKNOTES BATCH AND ONLINE PROGRAMS.                *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-10-18 MKL  ORIGINAL                                       *
      * 1997-03-26 BHS  PERFORMANCE - MARGIN READ AHEAD CHG7384        *
      * 1998-10-08 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-05-18 JLM  USD CALCULATION CORRECTED PRB8213              *
      * 2002-04-18 BHS  NEW FOREIGN REPORT COLUMNS REQ4009             *
      * 2003-12-13 MKL  USD FEE SCHEDULE UPDATE INC3449                *
      * 2005-06-25 EWS  NEW SHIPPING REPORT COLUMNS INC0477            *
      * 2005-11-03 BHS  USD LIMIT REVISED CHG6335                      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FNVAL.
       AUTHOR. J L MARSH.

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
                                           VALUE 10.000000.
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
      * WS-SHIPPING-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHIPPING-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000718000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000390000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000381500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000467500'.
       01  WS-SHIPPING-TIER-TBL REDEFINES WS-SHIPPING-TIER-VALUES.
           05  WS-SHIPPING-TIER-ROW OCCURS 4 TIMES.
               10  WS-SHIPPING-TIER-LOW    PIC 9(09)V99.
               10  WS-SHIPPING-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FOREIGN-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FOREIGN-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000185000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000405000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000025500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000810500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005700000000042500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010700000000486000'.
       01  WS-FOREIGN-TIER-TBL REDEFINES WS-FOREIGN-TIER-VALUES.
           05  WS-FOREIGN-TIER-ROW OCCURS 6 TIMES.
               10  WS-FOREIGN-TIER-LOW     PIC 9(09)V99.
               10  WS-FOREIGN-TIER-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY FNVALP.

       PROCEDURE DIVISION USING FNVAL-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ASSESS-REVIEW
           PERFORM 2010-AGE-NOTICE
           PERFORM 2020-SPLIT-SHIPPING
           PERFORM 2030-CHECK-OPEN
           PERFORM 2040-SPLIT-ITEM
           PERFORM 2050-ACCUM-ORDER-TYPE
           PERFORM 2060-FLAG-SHIPPING
           PERFORM 2070-SPLIT-ITEM
           PERFORM 2080-AGE-EXPIRY
           PERFORM 2090-FLAG-FOREIGN
           PERFORM 2100-ACCUM-ORDER-TYPE
           PERFORM 2110-LIMIT-FOREIGN
           PERFORM 2120-SELECT-PENALTY-PCT
           PERFORM 2130-SPLIT-SHIPPING
           PERFORM 2140-CHECK-NOTICE
           PERFORM 2150-EDIT-CONTACT
           PERFORM 2160-CHECK-NOTICE
           PERFORM 2170-DESCRIBE-TYPE
           PERFORM 2180-SELECT-PENALTY-PCT
           PERFORM 2190-FLAG-FOREIGN
           PERFORM 2200-COUNT-PAYMENT
           PERFORM 2210-AGE-NOTICE
           PERFORM 2220-FLAG-FOREIGN
           PERFORM 2230-PRORATE-USD
           PERFORM 2240-EDIT-REF-NO
           PERFORM 2250-ACCRUE-RATE
           PERFORM 2260-EDIT-REF-NO
           PERFORM 2270-DESCRIBE-SOURCE
           PERFORM 2280-CHECK-LAST-ACT
           PERFORM 2290-FLAG-ITEM
           PERFORM 2300-SELECT-PENALTY-PCT
           PERFORM 2310-SET-TYPE
           PERFORM 2320-AGE-NOTICE
           PERFORM 2330-ACCRUE-MARGIN
           PERFORM 2340-FLAG-SHIPPING
           PERFORM 2350-AGE-EXPIRY
           PERFORM 2360-COUNT-PAYMENT
           PERFORM 2370-ACCUM-BRANCH
           PERFORM 2380-SELECT-RATE
           PERFORM 2390-DESCRIBE-BRANCH
           PERFORM 2400-LIMIT-FOREIGN
           PERFORM 2410-PRORATE-SHIPPING
           PERFORM 2420-SPLIT-SHIPPING
           PERFORM 2430-ACCRUE-FOREIGN
           PERFORM 2440-SPLIT-MARGIN
           PERFORM 2450-CHECK-OPEN
           PERFORM 2460-ASSESS-REVIEW
           PERFORM 2470-EDIT-REF-NO
           PERFORM 2480-LIMIT-ITEM
           PERFORM 2490-ASSESS-REVIEW
           PERFORM 2500-AGE-NOTICE
           PERFORM 2510-DESCRIBE-SOURCE
           PERFORM 2520-DESCRIBE-ORDER-TYPE
           PERFORM 2530-ACCUM-TYPE
           PERFORM 2540-SET-BRANCH
           PERFORM 2550-ACCUM-ORDER-TYPE
           PERFORM 2560-AGE-NOTICE
           PERFORM 2570-CHECK-NOTICE
           PERFORM 2580-COUNT-EXCEPT
           PERFORM 2590-PRORATE-USD
           PERFORM 2600-DESCRIBE-SOURCE
           PERFORM 2610-AGE-NOTICE
           PERFORM 2620-EDIT-CONTACT
           PERFORM 2630-CHECK-OPEN
           PERFORM 2640-CHECK-EXPIRY
           PERFORM 2650-EDIT-CONTACT
           PERFORM 2660-AGE-LAST-ACT
           PERFORM 2670-AGE-LAST-ACT
           PERFORM 2680-PRORATE-FOREIGN
           PERFORM 2690-ACCRUE-RATE
           PERFORM 2700-COUNT-EXCEPT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO FNVAL-RETURN-CODE.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPPING FEE ON THE SHIPPING YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2000-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN FNVAL-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO FNVAL-SHIPPING-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNVAL-EXEMPT-FLAG = 'Y'
                   COMPUTE FNVAL-SHIPPING-AMT ROUNDED =
                       FNVAL-SHIPPING-YTD * FNVAL-PENALTY-PCT / 200
               WHEN FNVAL-SOURCE-CD = 'DG'
                   MOVE 7.00 TO FNVAL-SHIPPING-AMT
               WHEN FNVAL-SOURCE-CD = 'FS'
                   MOVE 3.00 TO FNVAL-SHIPPING-AMT
               WHEN OTHER
                   COMPUTE FNVAL-SHIPPING-AMT ROUNDED =
                       FNVAL-SHIPPING-YTD * FNVAL-PENALTY-PCT / 100
           END-EVALUATE
           IF FNVAL-SHIPPING-AMT > 0 AND FNVAL-SHIPPING-AMT < 5.00
               MOVE 5.00 TO FNVAL-SHIPPING-AMT
           END-IF
           IF FNVAL-SHIPPING-AMT > 35.00
               MOVE 35.00 TO FNVAL-SHIPPING-AMT
           END-IF
           ADD FNVAL-SHIPPING-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2010-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FNVAL-NOTICE-DATE > 0 AND FNVAL-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNVAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNVAL-MARGIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FNVAL-MARGIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD FNVAL-MARGIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD FNVAL-MARGIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNVAL-MARGIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SHIPPING YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2020-SPLIT-SHIPPING.
           COMPUTE WS-WORK-AMT = FNVAL-SHIPPING-YTD / 4
           COMPUTE WS-WORK-AMT-2 = FNVAL-SHIPPING-YTD - (WS-WORK-AMT *
               4)
           COMPUTE FNVAL-FOREIGN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2030-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FNVAL-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNVAL-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO FNVAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2040-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE USD AMT BY ORDER TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2050-ACCUM-ORDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNVAL-ORDER-TYPE-CD
                   ADD FNVAL-USD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNVAL-ORDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FNVAL-USD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SHIPPING AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2060-FLAG-SHIPPING.
           IF FNVAL-SHIPPING-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FNVAL-SHIPPING-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2070-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2080-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF FNVAL-EXPIRY-DATE > 0 AND FNVAL-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNVAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNVAL-MARGIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FNVAL-MARGIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD FNVAL-MARGIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FNVAL-MARGIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNVAL-MARGIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FOREIGN YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2090-FLAG-FOREIGN.
           IF FNVAL-FOREIGN-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FNVAL-FOREIGN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHIPPING AMT BY ORDER TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2100-ACCUM-ORDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNVAL-ORDER-TYPE-CD
                   ADD FNVAL-SHIPPING-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNVAL-ORDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FNVAL-SHIPPING-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2110-LIMIT-FOREIGN.
           IF FNVAL-FOREIGN-AMT < 100.00
               MOVE 100.00 TO FNVAL-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNVAL-FOREIGN-AMT > 50000.00
               MOVE 50000.00 TO FNVAL-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE SHIPPING YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2120-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FNVAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF FNVAL-SHIPPING-YTD NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FNVAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNVAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNVAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHIPPING YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2130-SPLIT-SHIPPING.
           COMPUTE WS-WORK-AMT = FNVAL-SHIPPING-YTD / 12
           COMPUTE WS-WORK-AMT-2 = FNVAL-SHIPPING-YTD - (WS-WORK-AMT *
               12)
           COMPUTE FNVAL-SHIPPING-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2140-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FNVAL-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNVAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO FNVAL-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E950 CONTACT REQUIRED
      * E661 SHIPPING AMT NEGATIVE
      * E098 SHIPPING AMT OVER 5000
      * E236 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2150-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNVAL-CONTACT = SPACES
                   MOVE 'E950' TO WS-REJECT-CODE
               WHEN FNVAL-SHIPPING-AMT < 0
                   MOVE 'E661' TO WS-REJECT-CODE
               WHEN FNVAL-SHIPPING-AMT > 5000.00
                   MOVE 'E098' TO WS-REJECT-CODE
               WHEN FNVAL-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E236' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2160-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FNVAL-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNVAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO FNVAL-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2170-DESCRIBE-TYPE.
           EVALUATE FNVAL-TYPE-CD
               WHEN 'CFP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE MARGIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2180-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FNVAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF FNVAL-MARGIN-AMT NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FNVAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNVAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNVAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2190-FLAG-FOREIGN.
           IF FNVAL-FOREIGN-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FNVAL-FOREIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2200-COUNT-PAYMENT.
           IF FNVAL-PAYMENT-CNT < 9999999
               ADD 1 TO FNVAL-PAYMENT-CNT
           END-IF
           IF FNVAL-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO FNVAL-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2210-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FNVAL-NOTICE-DATE > 0 AND FNVAL-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNVAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNVAL-FOREIGN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FNVAL-FOREIGN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FNVAL-FOREIGN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD FNVAL-FOREIGN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNVAL-FOREIGN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FOREIGN AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2220-FLAG-FOREIGN.
           IF FNVAL-FOREIGN-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FNVAL-FOREIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MARGIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO USD AMT.
      *----------------------------------------------------------------*
       2230-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNVAL-USD-AMT ROUNDED =
                   FNVAL-MARGIN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FNVAL-USD-AMT
           END-IF
           ADD FNVAL-USD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E565 REF NO REQUIRED
      * E890 SHIPPING YTD NEGATIVE
      * E838 SHIPPING YTD OVER 1000000
      * E440 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2240-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNVAL-REF-NO = SPACES
                   MOVE 'E565' TO WS-REJECT-CODE
               WHEN FNVAL-SHIPPING-YTD < 0
                   MOVE 'E890' TO WS-REJECT-CODE
               WHEN FNVAL-SHIPPING-YTD > 1000000.00
                   MOVE 'E838' TO WS-REJECT-CODE
               WHEN FNVAL-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E440' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2250-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * FNVAL-RATE / 100 / 360
               ADD WS-WORK-AMT TO FNVAL-FOREIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E301 REF NO REQUIRED
      * E816 WORK AMT 2 NEGATIVE
      * E018 WORK AMT 2 OVER 99999
      * E070 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2260-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNVAL-REF-NO = SPACES
                   MOVE 'E301' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E816' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E018' TO WS-REJECT-CODE
               WHEN FNVAL-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E070' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2270-DESCRIBE-SOURCE.
           EVALUATE FNVAL-SOURCE-CD
               WHEN 'DG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2280-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF FNVAL-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNVAL-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO FNVAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2290-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHIPPING TIER TIER FOR THE MARGIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2300-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPPING-TIER-RATE(1) TO FNVAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF FNVAL-MARGIN-AMT NOT < WS-SHIPPING-TIER-LOW(WS-TX)
                   MOVE WS-SHIPPING-TIER-RATE(WS-TX) TO
                       FNVAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNVAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNVAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CFP -> FFU WHEN NOTICE DATE IS PAST
      * FFU -> JNR WHEN NOTICE DATE IS PAST
      * JNR -> UGS WHEN NOTICE DATE IS PAST
      * UGS -> XUM WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2310-SET-TYPE.
           EVALUATE FNVAL-TYPE-CD
               WHEN 'CFP'
                   IF FNVAL-NOTICE-DATE > 0 AND FNVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FFU' TO FNVAL-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FFU'
                   IF FNVAL-NOTICE-DATE > 0 AND FNVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JNR' TO FNVAL-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JNR'
                   IF FNVAL-NOTICE-DATE > 0 AND FNVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UGS' TO FNVAL-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UGS'
                   IF FNVAL-NOTICE-DATE > 0 AND FNVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XUM' TO FNVAL-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2320-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FNVAL-NOTICE-DATE > 0 AND FNVAL-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNVAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNVAL-SHIPPING-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FNVAL-SHIPPING-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD FNVAL-SHIPPING-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD FNVAL-SHIPPING-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNVAL-SHIPPING-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARGIN AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2330-ACCRUE-MARGIN.
           IF FNVAL-MARGIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNVAL-MARGIN-AMT * FNVAL-RATE / 100 / 365
               ADD WS-WORK-AMT TO FNVAL-USD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHIPPING AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2340-FLAG-SHIPPING.
           IF FNVAL-SHIPPING-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FNVAL-SHIPPING-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2350-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF FNVAL-EXPIRY-DATE > 0 AND FNVAL-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNVAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2360-COUNT-PAYMENT.
           IF FNVAL-PAYMENT-CNT < 9999999
               ADD 1 TO FNVAL-PAYMENT-CNT
           END-IF
           IF FNVAL-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO FNVAL-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2370-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNVAL-BRANCH-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNVAL-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHIPPING TIER TIER FOR THE SHIPPING AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2380-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPPING-TIER-RATE(1) TO FNVAL-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF FNVAL-SHIPPING-AMT NOT < WS-SHIPPING-TIER-LOW(WS-TX)
                   MOVE WS-SHIPPING-TIER-RATE(WS-TX) TO FNVAL-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNVAL-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNVAL-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2390-DESCRIBE-BRANCH.
           EVALUATE FNVAL-BRANCH-CD
               WHEN 'APZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BBP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ENL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'EZB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KHV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TDN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FOREIGN YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2400-LIMIT-FOREIGN.
           IF FNVAL-FOREIGN-YTD < 0.00
               MOVE 0.00 TO FNVAL-FOREIGN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNVAL-FOREIGN-YTD > 1000.00
               MOVE 1000.00 TO FNVAL-FOREIGN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPPING AMT.
      *----------------------------------------------------------------*
       2410-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNVAL-SHIPPING-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FNVAL-SHIPPING-AMT
           END-IF
           ADD FNVAL-SHIPPING-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2420-SPLIT-SHIPPING.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE FNVAL-SHIPPING-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2430-ACCRUE-FOREIGN.
           IF FNVAL-FOREIGN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNVAL-FOREIGN-YTD * FNVAL-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO FNVAL-USD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MARGIN AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2440-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = FNVAL-MARGIN-AMT / 4
           COMPUTE WS-WORK-AMT-2 = FNVAL-MARGIN-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2450-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FNVAL-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNVAL-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO FNVAL-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2460-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN FNVAL-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO FNVAL-FOREIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNVAL-EXEMPT-FLAG = 'Y'
                   COMPUTE FNVAL-FOREIGN-AMT ROUNDED = WS-WORK-AMT-2 *
                       FNVAL-RATE / 200
               WHEN FNVAL-TYPE-CD = 'CFP'
                   MOVE 7.00 TO FNVAL-FOREIGN-AMT
               WHEN FNVAL-TYPE-CD = 'FFU'
                   MOVE 10.00 TO FNVAL-FOREIGN-AMT
               WHEN OTHER
                   COMPUTE FNVAL-FOREIGN-AMT ROUNDED = WS-WORK-AMT-2 *
                       FNVAL-RATE / 100
           END-EVALUATE
           IF FNVAL-FOREIGN-AMT > 0 AND FNVAL-FOREIGN-AMT < 2.00
               MOVE 2.00 TO FNVAL-FOREIGN-AMT
           END-IF
           IF FNVAL-FOREIGN-AMT > 150.00
               MOVE 150.00 TO FNVAL-FOREIGN-AMT
           END-IF
           ADD FNVAL-FOREIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E702 REF NO REQUIRED
      * E830 MARGIN AMT NEGATIVE
      * E933 MARGIN AMT OVER 50000
      * E575 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2470-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNVAL-REF-NO = SPACES
                   MOVE 'E702' TO WS-REJECT-CODE
               WHEN FNVAL-MARGIN-AMT < 0
                   MOVE 'E830' TO WS-REJECT-CODE
               WHEN FNVAL-MARGIN-AMT > 50000.00
                   MOVE 'E933' TO WS-REJECT-CODE
               WHEN FNVAL-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E575' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2480-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MARGIN FEE ON THE FOREIGN YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2490-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN FNVAL-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO FNVAL-MARGIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNVAL-EXEMPT-FLAG = 'Y'
                   COMPUTE FNVAL-MARGIN-AMT ROUNDED = FNVAL-FOREIGN-YTD
                       * FNVAL-PENALTY-PCT / 200
               WHEN FNVAL-ORDER-TYPE-CD = 'DTU'
                   MOVE 7.00 TO FNVAL-MARGIN-AMT
               WHEN FNVAL-ORDER-TYPE-CD = 'KZV'
                   MOVE 20.00 TO FNVAL-MARGIN-AMT
               WHEN OTHER
                   COMPUTE FNVAL-MARGIN-AMT ROUNDED = FNVAL-FOREIGN-YTD
                       * FNVAL-PENALTY-PCT / 100
           END-EVALUATE
           IF FNVAL-MARGIN-AMT > 0 AND FNVAL-MARGIN-AMT < 10.00
               MOVE 10.00 TO FNVAL-MARGIN-AMT
           END-IF
           IF FNVAL-MARGIN-AMT > 50.00
               MOVE 50.00 TO FNVAL-MARGIN-AMT
           END-IF
           ADD FNVAL-MARGIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2500-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FNVAL-NOTICE-DATE > 0 AND FNVAL-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNVAL-NOTICE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2510-DESCRIBE-SOURCE.
           EVALUATE FNVAL-SOURCE-CD
               WHEN 'DG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'NW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ORDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2520-DESCRIBE-ORDER-TYPE.
           EVALUATE FNVAL-ORDER-TYPE-CD
               WHEN 'DTU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KZV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PHV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PZZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WMC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XWF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FOREIGN YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2530-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNVAL-TYPE-CD
                   ADD FNVAL-FOREIGN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNVAL-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FNVAL-FOREIGN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * APZ -> BBP WHEN NOTICE DATE IS PAST
      * BBP -> ENL WHEN NOTICE DATE IS PAST
      * ENL -> EZB WHEN NOTICE DATE IS PAST
      * EZB -> KHV WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2540-SET-BRANCH.
           EVALUATE FNVAL-BRANCH-CD
               WHEN 'APZ'
                   IF FNVAL-NOTICE-DATE > 0 AND FNVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BBP' TO FNVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BBP'
                   IF FNVAL-NOTICE-DATE > 0 AND FNVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ENL' TO FNVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ENL'
                   IF FNVAL-NOTICE-DATE > 0 AND FNVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EZB' TO FNVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EZB'
                   IF FNVAL-NOTICE-DATE > 0 AND FNVAL-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHV' TO FNVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY ORDER TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2550-ACCUM-ORDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNVAL-ORDER-TYPE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNVAL-ORDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2560-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FNVAL-NOTICE-DATE > 0 AND FNVAL-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNVAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNVAL-SHIPPING-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FNVAL-SHIPPING-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD FNVAL-SHIPPING-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD FNVAL-SHIPPING-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNVAL-SHIPPING-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2570-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FNVAL-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNVAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO FNVAL-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2580-COUNT-EXCEPT.
           IF FNVAL-EXCEPT-CNT < 9999999
               ADD 1 TO FNVAL-EXCEPT-CNT
           END-IF
           IF FNVAL-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO FNVAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO USD AMT.
      *----------------------------------------------------------------*
       2590-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNVAL-USD-AMT ROUNDED =
                   FNVAL-FOREIGN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FNVAL-USD-AMT
           END-IF
           ADD FNVAL-USD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2600-DESCRIBE-SOURCE.
           EVALUATE FNVAL-SOURCE-CD
               WHEN 'DG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2610-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FNVAL-NOTICE-DATE > 0 AND FNVAL-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNVAL-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNVAL-SHIPPING-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD FNVAL-SHIPPING-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD FNVAL-SHIPPING-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FNVAL-SHIPPING-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNVAL-SHIPPING-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E299 CONTACT REQUIRED
      * E385 USD AMT NEGATIVE
      * E188 USD AMT OVER 1000000
      * E033 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2620-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNVAL-CONTACT = SPACES
                   MOVE 'E299' TO WS-REJECT-CODE
               WHEN FNVAL-USD-AMT < 0
                   MOVE 'E385' TO WS-REJECT-CODE
               WHEN FNVAL-USD-AMT > 1000000.00
                   MOVE 'E188' TO WS-REJECT-CODE
               WHEN FNVAL-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E033' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2630-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FNVAL-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNVAL-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO FNVAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2640-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF FNVAL-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNVAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO FNVAL-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E208 CONTACT REQUIRED
      * E225 SHIPPING YTD NEGATIVE
      * E807 SHIPPING YTD OVER 250000
      * E809 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2650-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNVAL-CONTACT = SPACES
                   MOVE 'E208' TO WS-REJECT-CODE
               WHEN FNVAL-SHIPPING-YTD < 0
                   MOVE 'E225' TO WS-REJECT-CODE
               WHEN FNVAL-SHIPPING-YTD > 250000.00
                   MOVE 'E807' TO WS-REJECT-CODE
               WHEN FNVAL-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E809' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2660-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF FNVAL-LAST-ACT-DATE > 0 AND FNVAL-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNVAL-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNVAL-FOREIGN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD FNVAL-FOREIGN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD FNVAL-FOREIGN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FNVAL-FOREIGN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNVAL-FOREIGN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2670-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF FNVAL-LAST-ACT-DATE > 0 AND FNVAL-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNVAL-LAST-ACT-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FOREIGN AMT.
      *----------------------------------------------------------------*
       2680-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNVAL-FOREIGN-AMT ROUNDED =
                   FNVAL-FOREIGN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FNVAL-FOREIGN-AMT
           END-IF
           ADD FNVAL-FOREIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2690-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * FNVAL-RATE / 100 / 360
               ADD WS-WORK-AMT TO FNVAL-MARGIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2700-COUNT-EXCEPT.
           IF FNVAL-EXCEPT-CNT < 9999999
               ADD 1 TO FNVAL-EXCEPT-CNT
           END-IF
           IF FNVAL-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO FNVAL-REVIEW-FLAG
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO FNVAL-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO FNVAL-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO FNVAL-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'FNVAL EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
