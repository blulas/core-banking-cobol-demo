      *================================================================*
      * PROGRAM  : TLELG                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : TELLER BALANCING RULE - VALIDATION. CALLED BY THE   *
      *            TELLER BALANCING BATCH AND ONLINE PROGRAMS.         *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-05-13 TWH  ORIGINAL                                       *
      * 1993-03-25 DPS  NEW CASH-OUT REPORT COLUMNS REQ8335            *
      * 1994-09-17 EWS  BATCH RERUN PROTECTION REQ5713                 *
      * 1996-10-27 JLM  OVERSHORT TABLE EXPANDED CHG4383               *
      * 1997-02-28 RJT  PERFORMANCE - VAULT-SELL READ AHEAD INC3974    *
      * 1998-03-01 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-01-24 GAF  VAULT-BUY FEE SCHEDULE UPDATE REQ1139          *
      * 2004-01-21 LMB  CASH-OUT ROUNDING - HALF UP REQ0151            *
      * 2006-02-23 TWH  OVERSHORT FEE SCHEDULE UPDATE REQ9277          *
      * 2006-05-19 GAF  NEW OVERSHORT REPORT COLUMNS PRB7384           *
      * 2007-12-24 CRP  AUDIT FINDING - VAULT-SELL CONTROL TOTALS INC07*
      * 2008-12-17 BHS  AUDIT FINDING - CASH-IN CONTROL TOTALS PRB8071 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLELG.
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
                                           VALUE 23.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 28.
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
      * WS-OVERSHOR-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-OVERSHOR-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000099500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000305000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000789500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011250000000733500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011350000000832000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021350000000872000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026350000000504500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026400000000617000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026650000000742500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036650000000259000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036700000000037000'.
       01  WS-OVERSHOR-GRID-TBL REDEFINES WS-OVERSHOR-GRID-VALUES.
           05  WS-OVERSHOR-GRID-ROW OCCURS 11 TIMES.
               10  WS-OVERSHOR-GRID-LOW    PIC 9(09)V99.
               10  WS-OVERSHOR-GRID-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY TLELGP.

       PROCEDURE DIVISION USING TLELG-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-FLAG-CASH-OUT
           PERFORM 2010-AGE-EXPIRY
           PERFORM 2020-EDIT-DESC
           PERFORM 2030-EDIT-DESC
           PERFORM 2040-FLAG-ITEM
           PERFORM 2050-COUNT-ITEM
           PERFORM 2060-FLAG-OVERSHORT
           PERFORM 2070-SPLIT-VAULT-SELL
           PERFORM 2080-SELECT-DISCOUNT
           PERFORM 2090-FLAG-ITEM
           PERFORM 2100-PRORATE-CASH-OUT
           PERFORM 2110-FLAG-ITEM
           PERFORM 2120-FLAG-OVERSHORT
           PERFORM 2130-ACCUM-PROOF-TYPE
           PERFORM 2140-COUNT-ITEM
           PERFORM 2150-FLAG-ITEM
           PERFORM 2160-AGE-LAST-ACT
           PERFORM 2170-FLAG-VAULT-SELL
           PERFORM 2180-SET-PROOF-TYPE
           PERFORM 2190-EDIT-ADDR
           PERFORM 2200-FLAG-OVERSHORT
           PERFORM 2210-PRORATE-OVERSHORT
           PERFORM 2220-ACCUM-CYCLE
           PERFORM 2230-AGE-LAST-ACT
           PERFORM 2240-DESCRIBE-CHANNEL
           PERFORM 2250-ACCUM-CYCLE
           PERFORM 2260-EDIT-DESC
           PERFORM 2270-CHECK-EFFECTIVE
           PERFORM 2280-EDIT-DESC
           PERFORM 2290-CHECK-LAST-PAY
           PERFORM 2300-COUNT-NOTICE
           PERFORM 2310-ACCUM-CYCLE
           PERFORM 2320-SPLIT-OVERSHORT
           PERFORM 2330-SET-PROOF-TYPE
           PERFORM 2340-SET-CYCLE
           PERFORM 2350-SELECT-DISCOUNT
           PERFORM 2360-ASSESS-REVIEW
           PERFORM 2370-SELECT-RATE
           PERFORM 2380-ACCUM-CHANNEL
           PERFORM 2390-SET-SOURCE
           PERFORM 2400-SET-CYCLE
           PERFORM 2410-PRORATE-VAULT-SELL
           PERFORM 2420-SET-PROOF-TYPE
           PERFORM 2430-CHECK-EXPIRY
           PERFORM 2440-SPLIT-OVERSHORT
           PERFORM 2450-ASSESS-OVERRIDE
           PERFORM 2460-CHECK-EXPIRY
           PERFORM 2470-EDIT-ADDR
           PERFORM 2480-SPLIT-VAULT-SELL
           PERFORM 2490-SPLIT-CASH-IN
           PERFORM 2500-ACCUM-SOURCE
           PERFORM 2510-SPLIT-OVERSHORT
           PERFORM 2520-LIMIT-OVERSHORT
           PERFORM 2530-SET-CHANNEL
           PERFORM 2540-CHECK-EXPIRY
           PERFORM 2550-FLAG-CASH-OUT
           PERFORM 2560-SELECT-DISCOUNT
           PERFORM 2570-AGE-EFFECTIVE
           PERFORM 2580-DESCRIBE-CYCLE
           PERFORM 2590-SPLIT-CASH-OUT
           PERFORM 2600-ASSESS-STOP
           PERFORM 2610-SPLIT-OVERSHORT
           PERFORM 2620-SET-CYCLE
           PERFORM 2630-COUNT-ITEM
           PERFORM 2640-PRORATE-CASH-OUT
           PERFORM 2650-EDIT-DESC
           PERFORM 2660-ACCUM-CYCLE
           PERFORM 2670-ASSESS-REVIEW
           PERFORM 2680-SPLIT-OVERSHORT
           PERFORM 2690-ASSESS-REVIEW
           PERFORM 2700-PRORATE-CASH-OUT
           PERFORM 2710-PRORATE-CASH-OUT
           PERFORM 2720-COUNT-NOTICE
           PERFORM 2730-SPLIT-OVERSHORT
           PERFORM 2740-LIMIT-VAULT-SELL
           PERFORM 2750-PRORATE-CASH-OUT
           PERFORM 2760-COUNT-ITEM
           PERFORM 2770-AGE-EFFECTIVE
           PERFORM 2780-FLAG-CASH-OUT
           PERFORM 2790-SPLIT-CASH-OUT
           PERFORM 2800-SPLIT-VAULT-SELL
           PERFORM 2810-ACCUM-PROOF-TYPE
           PERFORM 2820-FLAG-CASH-OUT
           PERFORM 2830-ACCUM-CYCLE
           PERFORM 2840-SPLIT-OVERSHORT
           PERFORM 2850-ACCUM-SOURCE
           PERFORM 2860-SPLIT-VAULT-SELL
           PERFORM 2870-COUNT-ITEM
           PERFORM 2880-COUNT-ITEM
           PERFORM 2890-EDIT-DESC
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO TLELG-RETURN-CODE.

      *----------------------------------------------------------------*
      * CASH OUT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2000-FLAG-CASH-OUT.
           IF TLELG-CASH-OUT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLELG-CASH-OUT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2010-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TLELG-EXPIRY-DATE > 0 AND TLELG-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLELG-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLELG-OVERSHORT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TLELG-OVERSHORT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TLELG-OVERSHORT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TLELG-OVERSHORT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLELG-OVERSHORT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E936 DESC REQUIRED
      * E412 CASH OUT AMT NEGATIVE
      * E552 CASH OUT AMT OVER 10000
      * E077 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2020-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLELG-DESC = SPACES
                   MOVE 'E936' TO WS-REJECT-CODE
               WHEN TLELG-CASH-OUT-AMT < 0
                   MOVE 'E412' TO WS-REJECT-CODE
               WHEN TLELG-CASH-OUT-AMT > 10000.00
                   MOVE 'E552' TO WS-REJECT-CODE
               WHEN TLELG-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E077' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E536 DESC REQUIRED
      * E952 CASH OUT AMT NEGATIVE
      * E617 CASH OUT AMT OVER 10000
      * E097 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2030-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLELG-DESC = SPACES
                   MOVE 'E536' TO WS-REJECT-CODE
               WHEN TLELG-CASH-OUT-AMT < 0
                   MOVE 'E952' TO WS-REJECT-CODE
               WHEN TLELG-CASH-OUT-AMT > 10000.00
                   MOVE 'E617' TO WS-REJECT-CODE
               WHEN TLELG-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E097' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2040-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2050-COUNT-ITEM.
           IF TLELG-ITEM-CNT < 9999999
               ADD 1 TO TLELG-ITEM-CNT
           END-IF
           IF TLELG-ITEM-CNT NOT < 3
               MOVE 'Y' TO TLELG-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2060-FLAG-OVERSHORT.
           IF TLELG-OVERSHORT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLELG-OVERSHORT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2070-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE TLELG-VAULT-SELL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OVERSHOR GRID TIER FOR THE OVERSHORT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2080-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OVERSHOR-GRID-RATE(1) TO TLELG-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TLELG-OVERSHORT-AMT NOT < WS-OVERSHOR-GRID-LOW(WS-TX)
                   MOVE WS-OVERSHOR-GRID-RATE(WS-TX) TO TLELG-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLELG-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLELG-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2090-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH OUT AMT.
      *----------------------------------------------------------------*
       2100-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLELG-CASH-OUT-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLELG-CASH-OUT-AMT
           END-IF
           ADD TLELG-CASH-OUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2110-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2120-FLAG-OVERSHORT.
           IF TLELG-OVERSHORT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLELG-OVERSHORT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT YTD BY PROOF TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2130-ACCUM-PROOF-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLELG-PROOF-TYPE-CD
                   ADD TLELG-OVERSHORT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLELG-PROOF-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLELG-OVERSHORT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2140-COUNT-ITEM.
           IF TLELG-ITEM-CNT < 9999999
               ADD 1 TO TLELG-ITEM-CNT
           END-IF
           IF TLELG-ITEM-CNT NOT < 3
               MOVE 'Y' TO TLELG-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2150-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2160-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLELG-LAST-ACT-DATE > 0 AND TLELG-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLELG-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLELG-CASH-OUT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLELG-CASH-OUT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TLELG-CASH-OUT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLELG-CASH-OUT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLELG-CASH-OUT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT SELL YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2170-FLAG-VAULT-SELL.
           IF TLELG-VAULT-SELL-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLELG-VAULT-SELL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PROOF TYPE CD:
      * JG -> RX WHEN EFFECTIVE DATE IS PAST
      * RX -> TW WHEN EFFECTIVE DATE IS PAST
      * TW -> UG WHEN EFFECTIVE DATE IS PAST
      * UG -> XY WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2180-SET-PROOF-TYPE.
           EVALUATE TLELG-PROOF-TYPE-CD
               WHEN 'JG'
                   IF TLELG-EFFECTIVE-DATE > 0 AND TLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RX' TO TLELG-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RX'
                   IF TLELG-EFFECTIVE-DATE > 0 AND TLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TW' TO TLELG-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TW'
                   IF TLELG-EFFECTIVE-DATE > 0 AND TLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UG' TO TLELG-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF TLELG-EFFECTIVE-DATE > 0 AND TLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XY' TO TLELG-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E112 ADDR REQUIRED
      * E960 CASH IN AMT NEGATIVE
      * E752 CASH IN AMT OVER 1000000
      * E173 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2190-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLELG-ADDR = SPACES
                   MOVE 'E112' TO WS-REJECT-CODE
               WHEN TLELG-CASH-IN-AMT < 0
                   MOVE 'E960' TO WS-REJECT-CODE
               WHEN TLELG-CASH-IN-AMT > 1000000.00
                   MOVE 'E752' TO WS-REJECT-CODE
               WHEN TLELG-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E173' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2200-FLAG-OVERSHORT.
           IF TLELG-OVERSHORT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TLELG-OVERSHORT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH OUT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OVERSHORT YTD.
      *----------------------------------------------------------------*
       2210-PRORATE-OVERSHORT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLELG-OVERSHORT-YTD ROUNDED =
                   TLELG-CASH-OUT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLELG-OVERSHORT-YTD
           END-IF
           ADD TLELG-OVERSHORT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2220-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLELG-CYCLE-CD
                   ADD TLELG-CASH-OUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLELG-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLELG-CASH-OUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2230-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLELG-LAST-ACT-DATE > 0 AND TLELG-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLELG-LAST-ACT-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2240-DESCRIBE-CHANNEL.
           EVALUATE TLELG-CHANNEL-CD
               WHEN 'D'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'F'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'M'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'N'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2250-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLELG-CYCLE-CD
                   ADD TLELG-OVERSHORT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLELG-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLELG-OVERSHORT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E874 DESC REQUIRED
      * E671 OVERSHORT YTD NEGATIVE
      * E626 OVERSHORT YTD OVER 50000
      * E139 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2260-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLELG-DESC = SPACES
                   MOVE 'E874' TO WS-REJECT-CODE
               WHEN TLELG-OVERSHORT-YTD < 0
                   MOVE 'E671' TO WS-REJECT-CODE
               WHEN TLELG-OVERSHORT-YTD > 50000.00
                   MOVE 'E626' TO WS-REJECT-CODE
               WHEN TLELG-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E139' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2270-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLELG-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLELG-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TLELG-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E547 DESC REQUIRED
      * E715 CASH OUT AMT NEGATIVE
      * E654 CASH OUT AMT OVER 250000
      * E700 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2280-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLELG-DESC = SPACES
                   MOVE 'E547' TO WS-REJECT-CODE
               WHEN TLELG-CASH-OUT-AMT < 0
                   MOVE 'E715' TO WS-REJECT-CODE
               WHEN TLELG-CASH-OUT-AMT > 250000.00
                   MOVE 'E654' TO WS-REJECT-CODE
               WHEN TLELG-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E700' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2290-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TLELG-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLELG-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TLELG-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2300-COUNT-NOTICE.
           IF TLELG-NOTICE-CNT < 9999999
               ADD 1 TO TLELG-NOTICE-CNT
           END-IF
           IF TLELG-NOTICE-CNT NOT < 12
               MOVE 'Y' TO TLELG-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2310-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLELG-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLELG-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OVERSHORT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2320-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = TLELG-OVERSHORT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TLELG-OVERSHORT-YTD - (WS-WORK-AMT *
               4)
           COMPUTE TLELG-CASH-OUT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PROOF TYPE CD:
      * JG -> RX WHEN EFFECTIVE DATE IS PAST
      * RX -> TW WHEN EFFECTIVE DATE IS PAST
      * TW -> UG WHEN EFFECTIVE DATE IS PAST
      * UG -> XY WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2330-SET-PROOF-TYPE.
           EVALUATE TLELG-PROOF-TYPE-CD
               WHEN 'JG'
                   IF TLELG-EFFECTIVE-DATE > 0 AND TLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RX' TO TLELG-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RX'
                   IF TLELG-EFFECTIVE-DATE > 0 AND TLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TW' TO TLELG-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TW'
                   IF TLELG-EFFECTIVE-DATE > 0 AND TLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UG' TO TLELG-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF TLELG-EFFECTIVE-DATE > 0 AND TLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XY' TO TLELG-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * KX -> TA WHEN LAST ACT DATE IS PAST
      * TA -> TG WHEN LAST ACT DATE IS PAST
      * TG -> WG WHEN LAST ACT DATE IS PAST
      * WG -> KX WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2340-SET-CYCLE.
           EVALUATE TLELG-CYCLE-CD
               WHEN 'KX'
                   IF TLELG-LAST-ACT-DATE > 0 AND TLELG-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TA' TO TLELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF TLELG-LAST-ACT-DATE > 0 AND TLELG-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TG' TO TLELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TG'
                   IF TLELG-LAST-ACT-DATE > 0 AND TLELG-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WG' TO TLELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WG'
                   IF TLELG-LAST-ACT-DATE > 0 AND TLELG-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO TLELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE OVERSHOR GRID TIER FOR THE CASH OUT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2350-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OVERSHOR-GRID-RATE(1) TO TLELG-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TLELG-CASH-OUT-AMT NOT < WS-OVERSHOR-GRID-LOW(WS-TX)
                   MOVE WS-OVERSHOR-GRID-RATE(WS-TX) TO TLELG-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLELG-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLELG-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT YTD FEE ON THE CASH IN AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2360-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TLELG-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TLELG-OVERSHORT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLELG-OVERRIDE-FLAG = 'Y'
                   COMPUTE TLELG-OVERSHORT-YTD ROUNDED =
                       TLELG-CASH-IN-AMT * TLELG-DISCOUNT / 200
               WHEN TLELG-PROOF-TYPE-CD = 'JG'
                   MOVE 7.00 TO TLELG-OVERSHORT-YTD
               WHEN TLELG-PROOF-TYPE-CD = 'RX'
                   MOVE 10.00 TO TLELG-OVERSHORT-YTD
               WHEN OTHER
                   COMPUTE TLELG-OVERSHORT-YTD ROUNDED =
                       TLELG-CASH-IN-AMT * TLELG-DISCOUNT / 100
           END-EVALUATE
           IF TLELG-OVERSHORT-YTD > 0 AND TLELG-OVERSHORT-YTD < 5.00
               MOVE 5.00 TO TLELG-OVERSHORT-YTD
           END-IF
           IF TLELG-OVERSHORT-YTD > 250.00
               MOVE 250.00 TO TLELG-OVERSHORT-YTD
           END-IF
           ADD TLELG-OVERSHORT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE OVERSHOR GRID TIER FOR THE CASH OUT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2370-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-OVERSHOR-GRID-RATE(1) TO TLELG-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TLELG-CASH-OUT-YTD NOT < WS-OVERSHOR-GRID-LOW(WS-TX)
                   MOVE WS-OVERSHOR-GRID-RATE(WS-TX) TO TLELG-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLELG-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLELG-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2380-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLELG-CHANNEL-CD
                   ADD TLELG-CASH-OUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLELG-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLELG-CASH-OUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AF -> PT WHEN LAST PAY DATE IS PAST
      * PT -> SZ WHEN LAST PAY DATE IS PAST
      * SZ -> VK WHEN LAST PAY DATE IS PAST
      * VK -> VW WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2390-SET-SOURCE.
           EVALUATE TLELG-SOURCE-CD
               WHEN 'AF'
                   IF TLELG-LAST-PAY-DATE > 0 AND TLELG-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TLELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TLELG-LAST-PAY-DATE > 0 AND TLELG-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SZ' TO TLELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SZ'
                   IF TLELG-LAST-PAY-DATE > 0 AND TLELG-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VK' TO TLELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF TLELG-LAST-PAY-DATE > 0 AND TLELG-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VW' TO TLELG-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * KX -> TA WHEN EXPIRY DATE IS PAST
      * TA -> TG WHEN EXPIRY DATE IS PAST
      * TG -> WG WHEN EXPIRY DATE IS PAST
      * WG -> KX WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2400-SET-CYCLE.
           EVALUATE TLELG-CYCLE-CD
               WHEN 'KX'
                   IF TLELG-EXPIRY-DATE > 0 AND TLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TA' TO TLELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF TLELG-EXPIRY-DATE > 0 AND TLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TG' TO TLELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TG'
                   IF TLELG-EXPIRY-DATE > 0 AND TLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WG' TO TLELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WG'
                   IF TLELG-EXPIRY-DATE > 0 AND TLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO TLELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VAULT SELL YTD.
      *----------------------------------------------------------------*
       2410-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLELG-VAULT-SELL-YTD ROUNDED =
                   TLELG-OVERSHORT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLELG-VAULT-SELL-YTD
           END-IF
           ADD TLELG-VAULT-SELL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PROOF TYPE CD:
      * JG -> RX WHEN EXPIRY DATE IS PAST
      * RX -> TW WHEN EXPIRY DATE IS PAST
      * TW -> UG WHEN EXPIRY DATE IS PAST
      * UG -> XY WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2420-SET-PROOF-TYPE.
           EVALUATE TLELG-PROOF-TYPE-CD
               WHEN 'JG'
                   IF TLELG-EXPIRY-DATE > 0 AND TLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RX' TO TLELG-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RX'
                   IF TLELG-EXPIRY-DATE > 0 AND TLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TW' TO TLELG-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TW'
                   IF TLELG-EXPIRY-DATE > 0 AND TLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO TLELG-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF TLELG-EXPIRY-DATE > 0 AND TLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XY' TO TLELG-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2430-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TLELG-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLELG-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TLELG-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OVERSHORT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2440-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = TLELG-OVERSHORT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TLELG-OVERSHORT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE TLELG-CASH-OUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN FEE ON THE OVERSHORT YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2450-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TLELG-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TLELG-CASH-IN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLELG-REVIEW-FLAG = 'Y'
                   COMPUTE TLELG-CASH-IN-AMT ROUNDED =
                       TLELG-OVERSHORT-YTD * TLELG-RATE / 200
               WHEN TLELG-CYCLE-CD = 'KX'
                   MOVE 20.00 TO TLELG-CASH-IN-AMT
               WHEN TLELG-CYCLE-CD = 'TA'
                   MOVE 20.00 TO TLELG-CASH-IN-AMT
               WHEN OTHER
                   COMPUTE TLELG-CASH-IN-AMT ROUNDED =
                       TLELG-OVERSHORT-YTD * TLELG-RATE / 100
           END-EVALUATE
           IF TLELG-CASH-IN-AMT > 0 AND TLELG-CASH-IN-AMT < 1.00
               MOVE 1.00 TO TLELG-CASH-IN-AMT
           END-IF
           IF TLELG-CASH-IN-AMT > 50.00
               MOVE 50.00 TO TLELG-CASH-IN-AMT
           END-IF
           ADD TLELG-CASH-IN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2460-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TLELG-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLELG-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TLELG-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E689 ADDR REQUIRED
      * E902 WORK AMT NEGATIVE
      * E675 WORK AMT OVER 1000000
      * E008 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2470-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLELG-ADDR = SPACES
                   MOVE 'E689' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E902' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E675' TO WS-REJECT-CODE
               WHEN TLELG-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E008' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2480-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TLELG-VAULT-SELL-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TLELG-VAULT-SELL-YTD - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH IN AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2490-SPLIT-CASH-IN.
           COMPUTE WS-WORK-AMT = TLELG-CASH-IN-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TLELG-CASH-IN-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2500-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLELG-SOURCE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLELG-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OVERSHORT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2510-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = TLELG-OVERSHORT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TLELG-OVERSHORT-YTD - (WS-WORK-AMT *
               3)
           COMPUTE TLELG-CASH-IN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2520-LIMIT-OVERSHORT.
           IF TLELG-OVERSHORT-YTD < 0.00
               MOVE 0.00 TO TLELG-OVERSHORT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLELG-OVERSHORT-YTD > 50000.00
               MOVE 50000.00 TO TLELG-OVERSHORT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * D -> F WHEN EXPIRY DATE IS PAST
      * F -> M WHEN EXPIRY DATE IS PAST
      * M -> N WHEN EXPIRY DATE IS PAST
      * N -> R WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2530-SET-CHANNEL.
           EVALUATE TLELG-CHANNEL-CD
               WHEN 'D'
                   IF TLELG-EXPIRY-DATE > 0 AND TLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO TLELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TLELG-EXPIRY-DATE > 0 AND TLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TLELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLELG-EXPIRY-DATE > 0 AND TLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TLELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TLELG-EXPIRY-DATE > 0 AND TLELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO TLELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2540-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TLELG-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLELG-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TLELG-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2550-FLAG-CASH-OUT.
           IF TLELG-CASH-OUT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TLELG-CASH-OUT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OVERSHOR GRID TIER FOR THE OVERSHORT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2560-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OVERSHOR-GRID-RATE(1) TO TLELG-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TLELG-OVERSHORT-AMT NOT < WS-OVERSHOR-GRID-LOW(WS-TX)
                   MOVE WS-OVERSHOR-GRID-RATE(WS-TX) TO TLELG-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLELG-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLELG-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2570-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLELG-EFFECTIVE-DATE > 0 AND TLELG-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLELG-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLELG-CASH-OUT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TLELG-CASH-OUT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLELG-CASH-OUT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLELG-CASH-OUT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLELG-CASH-OUT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2580-DESCRIBE-CYCLE.
           EVALUATE TLELG-CYCLE-CD
               WHEN 'KX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CASH OUT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2590-SPLIT-CASH-OUT.
           COMPUTE WS-WORK-AMT = TLELG-CASH-OUT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TLELG-CASH-OUT-YTD - (WS-WORK-AMT *
               3)
           COMPUTE TLELG-OVERSHORT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN FEE ON THE WORK AMT 2.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2600-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TLELG-STOP-FLAG = 'Y'
                   MOVE ZERO TO TLELG-CASH-IN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLELG-OVERRIDE-FLAG = 'Y'
                   COMPUTE TLELG-CASH-IN-AMT ROUNDED = WS-WORK-AMT-2 *
                       TLELG-RATE / 200
               WHEN TLELG-SOURCE-CD = 'AF'
                   MOVE 12.00 TO TLELG-CASH-IN-AMT
               WHEN TLELG-SOURCE-CD = 'PT'
                   MOVE 12.00 TO TLELG-CASH-IN-AMT
               WHEN OTHER
                   COMPUTE TLELG-CASH-IN-AMT ROUNDED = WS-WORK-AMT-2 *
                       TLELG-RATE / 100
           END-EVALUATE
           IF TLELG-CASH-IN-AMT > 0 AND TLELG-CASH-IN-AMT < 2.00
               MOVE 2.00 TO TLELG-CASH-IN-AMT
           END-IF
           IF TLELG-CASH-IN-AMT > 150.00
               MOVE 150.00 TO TLELG-CASH-IN-AMT
           END-IF
           ADD TLELG-CASH-IN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2610-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE TLELG-OVERSHORT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * KX -> TA WHEN EFFECTIVE DATE IS PAST
      * TA -> TG WHEN EFFECTIVE DATE IS PAST
      * TG -> WG WHEN EFFECTIVE DATE IS PAST
      * WG -> KX WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2620-SET-CYCLE.
           EVALUATE TLELG-CYCLE-CD
               WHEN 'KX'
                   IF TLELG-EFFECTIVE-DATE > 0 AND TLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TA' TO TLELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF TLELG-EFFECTIVE-DATE > 0 AND TLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TG' TO TLELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TG'
                   IF TLELG-EFFECTIVE-DATE > 0 AND TLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WG' TO TLELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WG'
                   IF TLELG-EFFECTIVE-DATE > 0 AND TLELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KX' TO TLELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2630-COUNT-ITEM.
           IF TLELG-ITEM-CNT < 9999999
               ADD 1 TO TLELG-ITEM-CNT
           END-IF
           IF TLELG-ITEM-CNT NOT < 3
               MOVE 'Y' TO TLELG-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH OUT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2640-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TLELG-CASH-OUT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E219 DESC REQUIRED
      * E905 CASH OUT YTD NEGATIVE
      * E595 CASH OUT YTD OVER 10000
      * E223 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2650-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLELG-DESC = SPACES
                   MOVE 'E219' TO WS-REJECT-CODE
               WHEN TLELG-CASH-OUT-YTD < 0
                   MOVE 'E905' TO WS-REJECT-CODE
               WHEN TLELG-CASH-OUT-YTD > 10000.00
                   MOVE 'E595' TO WS-REJECT-CODE
               WHEN TLELG-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E223' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH IN AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2660-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLELG-CYCLE-CD
                   ADD TLELG-CASH-IN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLELG-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLELG-CASH-IN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2670-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TLELG-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TLELG-OVERSHORT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLELG-OVERRIDE-FLAG = 'Y'
                   COMPUTE TLELG-OVERSHORT-YTD ROUNDED = WS-WORK-AMT-2 *
                       TLELG-RATE / 200
               WHEN TLELG-SOURCE-CD = 'AF'
                   MOVE 10.00 TO TLELG-OVERSHORT-YTD
               WHEN TLELG-SOURCE-CD = 'PT'
                   MOVE 15.00 TO TLELG-OVERSHORT-YTD
               WHEN OTHER
                   COMPUTE TLELG-OVERSHORT-YTD ROUNDED = WS-WORK-AMT-2 *
                       TLELG-RATE / 100
           END-EVALUATE
           IF TLELG-OVERSHORT-YTD > 0 AND TLELG-OVERSHORT-YTD < 5.00
               MOVE 5.00 TO TLELG-OVERSHORT-YTD
           END-IF
           IF TLELG-OVERSHORT-YTD > 150.00
               MOVE 150.00 TO TLELG-OVERSHORT-YTD
           END-IF
           ADD TLELG-OVERSHORT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT OVERSHORT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2680-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = TLELG-OVERSHORT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TLELG-OVERSHORT-YTD - (WS-WORK-AMT *
               4)
           COMPUTE TLELG-CASH-IN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE WORK AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2690-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TLELG-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLELG-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       TLELG-RATE / 200
               WHEN TLELG-SOURCE-CD = 'AF'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN TLELG-SOURCE-CD = 'PT'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       TLELG-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH OUT AMT.
      *----------------------------------------------------------------*
       2700-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLELG-CASH-OUT-AMT ROUNDED =
                   TLELG-VAULT-SELL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLELG-CASH-OUT-AMT
           END-IF
           ADD TLELG-CASH-OUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH OUT YTD.
      *----------------------------------------------------------------*
       2710-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLELG-CASH-OUT-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLELG-CASH-OUT-YTD
           END-IF
           ADD TLELG-CASH-OUT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2720-COUNT-NOTICE.
           IF TLELG-NOTICE-CNT < 9999999
               ADD 1 TO TLELG-NOTICE-CNT
           END-IF
           IF TLELG-NOTICE-CNT NOT < 10
               MOVE 'Y' TO TLELG-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OVERSHORT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2730-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = TLELG-OVERSHORT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TLELG-OVERSHORT-AMT - (WS-WORK-AMT *
               3)
           COMPUTE TLELG-CASH-IN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VAULT SELL YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2740-LIMIT-VAULT-SELL.
           IF TLELG-VAULT-SELL-YTD < 50.00
               MOVE 50.00 TO TLELG-VAULT-SELL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLELG-VAULT-SELL-YTD > 1000.00
               MOVE 1000.00 TO TLELG-VAULT-SELL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH OUT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH OUT YTD.
      *----------------------------------------------------------------*
       2750-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLELG-CASH-OUT-YTD ROUNDED =
                   TLELG-CASH-OUT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLELG-CASH-OUT-YTD
           END-IF
           ADD TLELG-CASH-OUT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2760-COUNT-ITEM.
           IF TLELG-ITEM-CNT < 9999999
               ADD 1 TO TLELG-ITEM-CNT
           END-IF
           IF TLELG-ITEM-CNT NOT < 5
               MOVE 'Y' TO TLELG-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2770-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLELG-EFFECTIVE-DATE > 0 AND TLELG-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLELG-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLELG-OVERSHORT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD TLELG-OVERSHORT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TLELG-OVERSHORT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLELG-OVERSHORT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLELG-OVERSHORT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH OUT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2780-FLAG-CASH-OUT.
           IF TLELG-CASH-OUT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLELG-CASH-OUT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH OUT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2790-SPLIT-CASH-OUT.
           COMPUTE WS-WORK-AMT = TLELG-CASH-OUT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TLELG-CASH-OUT-YTD - (WS-WORK-AMT *
               3)
           COMPUTE TLELG-VAULT-SELL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2800-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TLELG-VAULT-SELL-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TLELG-VAULT-SELL-YTD - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT AMT BY PROOF TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2810-ACCUM-PROOF-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLELG-PROOF-TYPE-CD
                   ADD TLELG-OVERSHORT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLELG-PROOF-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLELG-OVERSHORT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2820-FLAG-CASH-OUT.
           IF TLELG-CASH-OUT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLELG-CASH-OUT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2830-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLELG-CYCLE-CD
                   ADD TLELG-OVERSHORT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLELG-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLELG-OVERSHORT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OVERSHORT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2840-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = TLELG-OVERSHORT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TLELG-OVERSHORT-AMT - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2850-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLELG-SOURCE-CD
                   ADD TLELG-OVERSHORT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLELG-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLELG-OVERSHORT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2860-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TLELG-VAULT-SELL-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TLELG-VAULT-SELL-YTD - (WS-WORK-AMT *
               12)
           COMPUTE TLELG-CASH-IN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2870-COUNT-ITEM.
           IF TLELG-ITEM-CNT < 9999999
               ADD 1 TO TLELG-ITEM-CNT
           END-IF
           IF TLELG-ITEM-CNT NOT < 12
               MOVE 'Y' TO TLELG-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2880-COUNT-ITEM.
           IF TLELG-ITEM-CNT < 9999999
               ADD 1 TO TLELG-ITEM-CNT
           END-IF
           IF TLELG-ITEM-CNT NOT < 3
               MOVE 'Y' TO TLELG-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E735 DESC REQUIRED
      * E822 CASH IN AMT NEGATIVE
      * E764 CASH IN AMT OVER 25000
      * E855 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2890-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLELG-DESC = SPACES
                   MOVE 'E735' TO WS-REJECT-CODE
               WHEN TLELG-CASH-IN-AMT < 0
                   MOVE 'E822' TO WS-REJECT-CODE
               WHEN TLELG-CASH-IN-AMT > 25000.00
                   MOVE 'E764' TO WS-REJECT-CODE
               WHEN TLELG-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E855' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO TLELG-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO TLELG-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO TLELG-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'TLELG EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
