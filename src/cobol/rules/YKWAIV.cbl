      *================================================================*
      * PROGRAM  : YKWAIV                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : DATE WINDOW BRIDGE RULE - ELIGIBILITY. CALLED BY THE*
      *            DATE WINDOW BRIDGE BATCH AND ONLINE PROGRAMS.       *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-01-04 LMB  ORIGINAL                                       *
      * 1992-06-14 JLM  AUDIT FINDING - AMOUNT-2 CONTROL TOTALS REQ6079*
      * 1992-08-26 PQR  CONTROL CUTOFF MOVED CHG4856                   *
      * 1999-05-24 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-08-25 TWH  CONTROL LIMIT REVISED PRB5869                  *
      * 2001-05-15 DPS  AMOUNT-1 NOTICE TEXT PER LEGAL INC3650         *
      * 2002-08-23 JLM  CONTROL LIMIT REVISED CHG8294                  *
      * 2007-10-14 CRP  AMOUNT-1 FEE SCHEDULE UPDATE PRB2513           *
      * 2008-05-23 TWH  CONTROL CUTOFF MOVED CHG6624                   *
      * 2008-02-10 MKL  CONTROL TABLE EXPANDED CHG0131                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YKWAIV.
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
      * WS-CONTROL-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTROL-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000055000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000180000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000362500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000511000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002700000000150000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000633500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003750000000703500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006250000000141000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011250000000329500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012250000000078500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000070500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012750000000336000'.
       01  WS-CONTROL-TIER-TBL REDEFINES WS-CONTROL-TIER-VALUES.
           05  WS-CONTROL-TIER-ROW OCCURS 12 TIMES.
               10  WS-CONTROL-TIER-LOW     PIC 9(09)V99.
               10  WS-CONTROL-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONTROL-TIERX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTROL-TIERX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000800500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000670000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000778000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006050000000127000'.
       01  WS-CONTROL-TIERX-TBL REDEFINES WS-CONTROL-TIERX-VALUES.
           05  WS-CONTROL-TIERX-ROW OCCURS 4 TIMES.
               10  WS-CONTROL-TIERX-LOW    PIC 9(09)V99.
               10  WS-CONTROL-TIERX-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY YKWAIVP.

       PROCEDURE DIVISION USING YKWAIV-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SPLIT-CONTROL
           PERFORM 2010-ACCUM-SOURCE
           PERFORM 2020-LIMIT-AMOUNT-1
           PERFORM 2030-DESCRIBE-REGION
           PERFORM 2040-COUNT-CYCLE
           PERFORM 2050-SET-SOURCE
           PERFORM 2060-EDIT-CONTACT
           PERFORM 2070-SET-CYCLE
           PERFORM 2080-SELECT-MARGIN
           PERFORM 2090-AGE-NEXT-DUE
           PERFORM 2100-ASSESS-PREFERRED
           PERFORM 2110-ACCUM-REGION
           PERFORM 2120-DESCRIBE-REGION
           PERFORM 2130-AGE-LAST-ACT
           PERFORM 2140-EDIT-CUST-ID
           PERFORM 2150-SELECT-DISCOUNT
           PERFORM 2160-LIMIT-CONTROL
           PERFORM 2170-CHECK-NEXT-DUE
           PERFORM 2180-SELECT-DISCOUNT
           PERFORM 2190-ASSESS-AUTO-RENEW
           PERFORM 2200-SELECT-DISCOUNT
           PERFORM 2210-DESCRIBE-SOURCE
           PERFORM 2220-ACCRUE-MARGIN
           PERFORM 2230-EDIT-CONTACT
           PERFORM 2240-FLAG-CONTROL
           PERFORM 2250-FLAG-AMOUNT-2
           PERFORM 2260-DESCRIBE-REASON
           PERFORM 2270-ACCRUE-AMOUNT-2
           PERFORM 2280-LIMIT-AMOUNT-2
           PERFORM 2290-EDIT-CUST-ID
           PERFORM 2300-FLAG-AMOUNT-1
           PERFORM 2310-ACCUM-REASON
           PERFORM 2320-DESCRIBE-SOURCE
           PERFORM 2330-FLAG-AMOUNT-1
           PERFORM 2340-FLAG-CONTROL
           PERFORM 2350-EDIT-CUST-ID
           PERFORM 2360-ASSESS-NOTICE-SENT
           PERFORM 2370-SPLIT-AMOUNT-2
           PERFORM 2380-ACCRUE-CONTROL
           PERFORM 2390-LIMIT-ITEM
           PERFORM 2400-AGE-MATURITY
           PERFORM 2410-PRORATE-AMOUNT-1
           PERFORM 2420-ACCUM-SOURCE
           PERFORM 2430-LIMIT-AMOUNT-2
           PERFORM 2440-CHECK-NOTICE
           PERFORM 2450-ACCRUE-AMOUNT-2
           PERFORM 2460-SPLIT-CONTROL
           PERFORM 2470-EDIT-CUST-ID
           PERFORM 2480-LIMIT-ITEM
           PERFORM 2490-CHECK-LAST-ACT
           PERFORM 2500-ACCRUE-AMOUNT-2
           PERFORM 2510-ASSESS-AUTO-RENEW
           PERFORM 2520-ACCRUE-AMOUNT-1
           PERFORM 2530-FLAG-AMOUNT-1
           PERFORM 2540-LIMIT-AMOUNT-1
           PERFORM 2550-ACCRUE-DISCOUNT
           PERFORM 2560-SELECT-MARGIN
           PERFORM 2570-ASSESS-AUTO-RENEW
           PERFORM 2580-LIMIT-AMOUNT-1
           PERFORM 2590-ACCRUE-AMOUNT-1
           PERFORM 2600-SET-REGION
           PERFORM 2610-SELECT-MARGIN
           PERFORM 2620-SET-SOURCE
           PERFORM 2630-CHECK-NEXT-DUE
           PERFORM 2640-SET-REASON
           PERFORM 2650-SPLIT-AMOUNT-2
           PERFORM 2660-DESCRIBE-REGION
           PERFORM 2670-DESCRIBE-REASON
           PERFORM 2680-FLAG-ITEM
           PERFORM 2690-FLAG-AMOUNT-1
           PERFORM 2700-EDIT-CONTACT
           PERFORM 2710-LIMIT-AMOUNT-1
           PERFORM 2720-PRORATE-AMOUNT-1
           PERFORM 2730-DESCRIBE-SOURCE
           PERFORM 2740-DESCRIBE-REASON
           PERFORM 2750-ACCRUE-AMOUNT-1
           PERFORM 2760-SPLIT-AMOUNT-1
           PERFORM 2770-COUNT-CYCLE
           PERFORM 2780-SPLIT-AMOUNT-1
           PERFORM 2790-EDIT-CUST-ID
           PERFORM 2800-ACCRUE-AMOUNT-1
           PERFORM 2810-ASSESS-NOTICE-SENT
           PERFORM 2820-FLAG-ITEM
           PERFORM 2830-EDIT-CONTACT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO YKWAIV-RETURN-CODE.

      *----------------------------------------------------------------*
      * SPLIT CONTROL YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2000-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YKWAIV-CONTROL-YTD / 2
           COMPUTE WS-WORK-AMT-2 = YKWAIV-CONTROL-YTD - (WS-WORK-AMT *
               2)
           COMPUTE YKWAIV-CONTROL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2010-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKWAIV-SOURCE-CD
                   ADD YKWAIV-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKWAIV-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YKWAIV-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2020-LIMIT-AMOUNT-1.
           IF YKWAIV-AMOUNT-1-YTD < 50.00
               MOVE 50.00 TO YKWAIV-AMOUNT-1-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKWAIV-AMOUNT-1-YTD > 2500.00
               MOVE 2500.00 TO YKWAIV-AMOUNT-1-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2030-DESCRIBE-REGION.
           EVALUATE YKWAIV-REGION-CD
               WHEN 'C'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'S'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'V'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2040-COUNT-CYCLE.
           IF YKWAIV-CYCLE-CNT < 9999999
               ADD 1 TO YKWAIV-CYCLE-CNT
           END-IF
           IF YKWAIV-CYCLE-CNT NOT < 3
               MOVE 'Y' TO YKWAIV-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN NOTICE DATE IS PAST
      * FTB -> GAP WHEN NOTICE DATE IS PAST
      * GAP -> PAP WHEN NOTICE DATE IS PAST
      * PAP -> VGC WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2050-SET-SOURCE.
           EVALUATE YKWAIV-SOURCE-CD
               WHEN 'BNX'
                   IF YKWAIV-NOTICE-DATE > 0 AND YKWAIV-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YKWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YKWAIV-NOTICE-DATE > 0 AND YKWAIV-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YKWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YKWAIV-NOTICE-DATE > 0 AND YKWAIV-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YKWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YKWAIV-NOTICE-DATE > 0 AND YKWAIV-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YKWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E700 CONTACT REQUIRED
      * E076 CONTROL YTD NEGATIVE
      * E674 CONTROL YTD OVER 50000
      * E180 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2060-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKWAIV-CONTACT = SPACES
                   MOVE 'E700' TO WS-REJECT-CODE
               WHEN YKWAIV-CONTROL-YTD < 0
                   MOVE 'E076' TO WS-REJECT-CODE
               WHEN YKWAIV-CONTROL-YTD > 50000.00
                   MOVE 'E674' TO WS-REJECT-CODE
               WHEN YKWAIV-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E180' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * FJ -> LA WHEN MATURITY DATE IS PAST
      * LA -> SP WHEN MATURITY DATE IS PAST
      * SP -> UC WHEN MATURITY DATE IS PAST
      * UC -> FJ WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2070-SET-CYCLE.
           EVALUATE YKWAIV-CYCLE-CD
               WHEN 'FJ'
                   IF YKWAIV-MATURITY-DATE > 0 AND YKWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LA' TO YKWAIV-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LA'
                   IF YKWAIV-MATURITY-DATE > 0 AND YKWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'SP' TO YKWAIV-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF YKWAIV-MATURITY-DATE > 0 AND YKWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UC' TO YKWAIV-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UC'
                   IF YKWAIV-MATURITY-DATE > 0 AND YKWAIV-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FJ' TO YKWAIV-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIERX TIER FOR THE CONTROL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2080-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIERX-RATE(1) TO YKWAIV-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF YKWAIV-CONTROL-AMT NOT < WS-CONTROL-TIERX-LOW(WS-TX)
                   MOVE WS-CONTROL-TIERX-RATE(WS-TX) TO YKWAIV-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKWAIV-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKWAIV-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2090-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF YKWAIV-NEXT-DUE-DATE > 0 AND YKWAIV-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKWAIV-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 2 YTD FEE ON THE AMOUNT 1 YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2100-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN YKWAIV-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO YKWAIV-AMOUNT-2-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKWAIV-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE YKWAIV-AMOUNT-2-YTD ROUNDED =
                       YKWAIV-AMOUNT-1-YTD * YKWAIV-MARGIN / 200
               WHEN YKWAIV-SOURCE-CD = 'BNX'
                   MOVE 20.00 TO YKWAIV-AMOUNT-2-YTD
               WHEN YKWAIV-SOURCE-CD = 'FTB'
                   MOVE 15.00 TO YKWAIV-AMOUNT-2-YTD
               WHEN OTHER
                   COMPUTE YKWAIV-AMOUNT-2-YTD ROUNDED =
                       YKWAIV-AMOUNT-1-YTD * YKWAIV-MARGIN / 100
           END-EVALUATE
           IF YKWAIV-AMOUNT-2-YTD > 0 AND YKWAIV-AMOUNT-2-YTD < 1.00
               MOVE 1.00 TO YKWAIV-AMOUNT-2-YTD
           END-IF
           IF YKWAIV-AMOUNT-2-YTD > 100.00
               MOVE 100.00 TO YKWAIV-AMOUNT-2-YTD
           END-IF
           ADD YKWAIV-AMOUNT-2-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2110-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKWAIV-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKWAIV-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2120-DESCRIBE-REGION.
           EVALUATE YKWAIV-REGION-CD
               WHEN 'C'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'K'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'W'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2130-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF YKWAIV-LAST-ACT-DATE > 0 AND YKWAIV-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKWAIV-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YKWAIV-AMOUNT-1-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD YKWAIV-AMOUNT-1-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YKWAIV-AMOUNT-1-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YKWAIV-AMOUNT-1-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YKWAIV-AMOUNT-1-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E097 CUST ID REQUIRED
      * E455 CONTROL YTD NEGATIVE
      * E005 CONTROL YTD OVER 99999
      * E915 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2140-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKWAIV-CUST-ID = SPACES
                   MOVE 'E097' TO WS-REJECT-CODE
               WHEN YKWAIV-CONTROL-YTD < 0
                   MOVE 'E455' TO WS-REJECT-CODE
               WHEN YKWAIV-CONTROL-YTD > 99999.00
                   MOVE 'E005' TO WS-REJECT-CODE
               WHEN YKWAIV-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E915' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIERX TIER FOR THE AMOUNT 2 YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2150-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIERX-RATE(1) TO YKWAIV-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF YKWAIV-AMOUNT-2-YTD NOT < WS-CONTROL-TIERX-LOW(WS-TX)
                   MOVE WS-CONTROL-TIERX-RATE(WS-TX) TO YKWAIV-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKWAIV-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKWAIV-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2160-LIMIT-CONTROL.
           IF YKWAIV-CONTROL-YTD < 50.00
               MOVE 50.00 TO YKWAIV-CONTROL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKWAIV-CONTROL-YTD > 50000.00
               MOVE 50000.00 TO YKWAIV-CONTROL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2170-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF YKWAIV-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKWAIV-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO YKWAIV-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2180-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIER-RATE(1) TO YKWAIV-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-CONTROL-TIER-LOW(WS-TX)
                   MOVE WS-CONTROL-TIER-RATE(WS-TX) TO YKWAIV-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKWAIV-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKWAIV-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 1 YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2190-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN YKWAIV-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO YKWAIV-AMOUNT-1-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKWAIV-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE YKWAIV-AMOUNT-1-YTD ROUNDED = WS-WORK-AMT-2 *
                       YKWAIV-MARGIN / 200
               WHEN YKWAIV-CYCLE-CD = 'FJ'
                   MOVE 7.00 TO YKWAIV-AMOUNT-1-YTD
               WHEN YKWAIV-CYCLE-CD = 'LA'
                   MOVE 3.00 TO YKWAIV-AMOUNT-1-YTD
               WHEN OTHER
                   COMPUTE YKWAIV-AMOUNT-1-YTD ROUNDED = WS-WORK-AMT-2 *
                       YKWAIV-MARGIN / 100
           END-EVALUATE
           IF YKWAIV-AMOUNT-1-YTD > 0 AND YKWAIV-AMOUNT-1-YTD < 2.00
               MOVE 2.00 TO YKWAIV-AMOUNT-1-YTD
           END-IF
           IF YKWAIV-AMOUNT-1-YTD > 100.00
               MOVE 100.00 TO YKWAIV-AMOUNT-1-YTD
           END-IF
           ADD YKWAIV-AMOUNT-1-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIER TIER FOR THE AMOUNT 2 YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2200-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIER-RATE(1) TO YKWAIV-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF YKWAIV-AMOUNT-2-YTD NOT < WS-CONTROL-TIER-LOW(WS-TX)
                   MOVE WS-CONTROL-TIER-RATE(WS-TX) TO YKWAIV-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKWAIV-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKWAIV-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2210-DESCRIBE-SOURCE.
           EVALUATE YKWAIV-SOURCE-CD
               WHEN 'BNX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FTB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GAP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PAP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VGC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VJV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2220-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * YKWAIV-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO YKWAIV-AMOUNT-2-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E778 CONTACT REQUIRED
      * E378 CONTROL YTD NEGATIVE
      * E678 CONTROL YTD OVER 1000000
      * E427 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2230-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKWAIV-CONTACT = SPACES
                   MOVE 'E778' TO WS-REJECT-CODE
               WHEN YKWAIV-CONTROL-YTD < 0
                   MOVE 'E378' TO WS-REJECT-CODE
               WHEN YKWAIV-CONTROL-YTD > 1000000.00
                   MOVE 'E678' TO WS-REJECT-CODE
               WHEN YKWAIV-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E427' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2240-FLAG-CONTROL.
           IF YKWAIV-CONTROL-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YKWAIV-CONTROL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2250-FLAG-AMOUNT-2.
           IF YKWAIV-AMOUNT-2-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YKWAIV-AMOUNT-2-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2260-DESCRIBE-REASON.
           EVALUATE YKWAIV-REASON-CD
               WHEN 'AX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2270-ACCRUE-AMOUNT-2.
           IF YKWAIV-AMOUNT-2-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKWAIV-AMOUNT-2-AMT * YKWAIV-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2280-LIMIT-AMOUNT-2.
           IF YKWAIV-AMOUNT-2-AMT < 50.00
               MOVE 50.00 TO YKWAIV-AMOUNT-2-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKWAIV-AMOUNT-2-AMT > 500.00
               MOVE 500.00 TO YKWAIV-AMOUNT-2-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E009 CUST ID REQUIRED
      * E326 CONTROL AMT NEGATIVE
      * E253 CONTROL AMT OVER 50000
      * E477 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2290-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKWAIV-CUST-ID = SPACES
                   MOVE 'E009' TO WS-REJECT-CODE
               WHEN YKWAIV-CONTROL-AMT < 0
                   MOVE 'E326' TO WS-REJECT-CODE
               WHEN YKWAIV-CONTROL-AMT > 50000.00
                   MOVE 'E253' TO WS-REJECT-CODE
               WHEN YKWAIV-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E477' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2300-FLAG-AMOUNT-1.
           IF YKWAIV-AMOUNT-1-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YKWAIV-AMOUNT-1-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2310-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKWAIV-REASON-CD
                   ADD YKWAIV-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKWAIV-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YKWAIV-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2320-DESCRIBE-SOURCE.
           EVALUATE YKWAIV-SOURCE-CD
               WHEN 'BNX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FTB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GAP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PAP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VGC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VJV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 1 YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2330-FLAG-AMOUNT-1.
           IF YKWAIV-AMOUNT-1-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YKWAIV-AMOUNT-1-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2340-FLAG-CONTROL.
           IF YKWAIV-CONTROL-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YKWAIV-CONTROL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E689 CUST ID REQUIRED
      * E473 CONTROL AMT NEGATIVE
      * E737 CONTROL AMT OVER 250000
      * E895 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2350-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKWAIV-CUST-ID = SPACES
                   MOVE 'E689' TO WS-REJECT-CODE
               WHEN YKWAIV-CONTROL-AMT < 0
                   MOVE 'E473' TO WS-REJECT-CODE
               WHEN YKWAIV-CONTROL-AMT > 250000.00
                   MOVE 'E737' TO WS-REJECT-CODE
               WHEN YKWAIV-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E895' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL YTD FEE ON THE AMOUNT 2 AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2360-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN YKWAIV-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO YKWAIV-CONTROL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKWAIV-PREFERRED-FLAG = 'Y'
                   COMPUTE YKWAIV-CONTROL-YTD ROUNDED =
                       YKWAIV-AMOUNT-2-AMT * YKWAIV-DISCOUNT / 200
               WHEN YKWAIV-CYCLE-CD = 'FJ'
                   MOVE 3.00 TO YKWAIV-CONTROL-YTD
               WHEN YKWAIV-CYCLE-CD = 'LA'
                   MOVE 12.00 TO YKWAIV-CONTROL-YTD
               WHEN OTHER
                   COMPUTE YKWAIV-CONTROL-YTD ROUNDED =
                       YKWAIV-AMOUNT-2-AMT * YKWAIV-DISCOUNT / 100
           END-EVALUATE
           IF YKWAIV-CONTROL-YTD > 0 AND YKWAIV-CONTROL-YTD < 2.00
               MOVE 2.00 TO YKWAIV-CONTROL-YTD
           END-IF
           IF YKWAIV-CONTROL-YTD > 50.00
               MOVE 50.00 TO YKWAIV-CONTROL-YTD
           END-IF
           ADD YKWAIV-CONTROL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2370-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YKWAIV-AMOUNT-2-YTD / 12
           COMPUTE WS-WORK-AMT-2 = YKWAIV-AMOUNT-2-YTD - (WS-WORK-AMT *
               12)
           COMPUTE YKWAIV-AMOUNT-1-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTROL AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2380-ACCRUE-CONTROL.
           IF YKWAIV-CONTROL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKWAIV-CONTROL-AMT * YKWAIV-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO YKWAIV-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2390-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2400-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF YKWAIV-MATURITY-DATE > 0 AND YKWAIV-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKWAIV-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 2 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 1 AMT.
      *----------------------------------------------------------------*
       2410-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YKWAIV-AMOUNT-1-AMT ROUNDED =
                   YKWAIV-AMOUNT-2-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YKWAIV-AMOUNT-1-AMT
           END-IF
           ADD YKWAIV-AMOUNT-1-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2420-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YKWAIV-SOURCE-CD
                   ADD YKWAIV-AMOUNT-2-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YKWAIV-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YKWAIV-AMOUNT-2-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2430-LIMIT-AMOUNT-2.
           IF YKWAIV-AMOUNT-2-AMT < 50.00
               MOVE 50.00 TO YKWAIV-AMOUNT-2-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKWAIV-AMOUNT-2-AMT > 500.00
               MOVE 500.00 TO YKWAIV-AMOUNT-2-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2440-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF YKWAIV-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKWAIV-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO YKWAIV-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2450-ACCRUE-AMOUNT-2.
           IF YKWAIV-AMOUNT-2-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKWAIV-AMOUNT-2-YTD * YKWAIV-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO YKWAIV-CONTROL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2460-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE YKWAIV-CONTROL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E949 CUST ID REQUIRED
      * E377 CONTROL AMT NEGATIVE
      * E015 CONTROL AMT OVER 50000
      * E964 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2470-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKWAIV-CUST-ID = SPACES
                   MOVE 'E949' TO WS-REJECT-CODE
               WHEN YKWAIV-CONTROL-AMT < 0
                   MOVE 'E377' TO WS-REJECT-CODE
               WHEN YKWAIV-CONTROL-AMT > 50000.00
                   MOVE 'E015' TO WS-REJECT-CODE
               WHEN YKWAIV-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E964' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2480-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2490-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF YKWAIV-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKWAIV-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO YKWAIV-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2500-ACCRUE-AMOUNT-2.
           IF YKWAIV-AMOUNT-2-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKWAIV-AMOUNT-2-AMT * YKWAIV-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO YKWAIV-CONTROL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 2 YTD FEE ON THE CONTROL YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2510-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN YKWAIV-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO YKWAIV-AMOUNT-2-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKWAIV-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE YKWAIV-AMOUNT-2-YTD ROUNDED =
                       YKWAIV-CONTROL-YTD * YKWAIV-DISCOUNT / 200
               WHEN YKWAIV-REGION-CD = 'C'
                   MOVE 12.00 TO YKWAIV-AMOUNT-2-YTD
               WHEN YKWAIV-REGION-CD = 'H'
                   MOVE 20.00 TO YKWAIV-AMOUNT-2-YTD
               WHEN OTHER
                   COMPUTE YKWAIV-AMOUNT-2-YTD ROUNDED =
                       YKWAIV-CONTROL-YTD * YKWAIV-DISCOUNT / 100
           END-EVALUATE
           IF YKWAIV-AMOUNT-2-YTD > 0 AND YKWAIV-AMOUNT-2-YTD < 5.00
               MOVE 5.00 TO YKWAIV-AMOUNT-2-YTD
           END-IF
           IF YKWAIV-AMOUNT-2-YTD > 25.00
               MOVE 25.00 TO YKWAIV-AMOUNT-2-YTD
           END-IF
           ADD YKWAIV-AMOUNT-2-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 1 YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2520-ACCRUE-AMOUNT-1.
           IF YKWAIV-AMOUNT-1-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKWAIV-AMOUNT-1-YTD * YKWAIV-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO YKWAIV-AMOUNT-2-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2530-FLAG-AMOUNT-1.
           IF YKWAIV-AMOUNT-1-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YKWAIV-AMOUNT-1-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2540-LIMIT-AMOUNT-1.
           IF YKWAIV-AMOUNT-1-YTD < 25.00
               MOVE 25.00 TO YKWAIV-AMOUNT-1-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKWAIV-AMOUNT-1-YTD > 500.00
               MOVE 500.00 TO YKWAIV-AMOUNT-1-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2550-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * YKWAIV-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO YKWAIV-AMOUNT-2-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIERX TIER FOR THE CONTROL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2560-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIERX-RATE(1) TO YKWAIV-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF YKWAIV-CONTROL-YTD NOT < WS-CONTROL-TIERX-LOW(WS-TX)
                   MOVE WS-CONTROL-TIERX-RATE(WS-TX) TO YKWAIV-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKWAIV-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKWAIV-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 2 FEE ON THE AMOUNT 2 YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2570-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN YKWAIV-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO YKWAIV-AMOUNT-2-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKWAIV-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE YKWAIV-AMOUNT-2-AMT ROUNDED =
                       YKWAIV-AMOUNT-2-YTD * YKWAIV-MARGIN / 200
               WHEN YKWAIV-REGION-CD = 'C'
                   MOVE 15.00 TO YKWAIV-AMOUNT-2-AMT
               WHEN YKWAIV-REGION-CD = 'H'
                   MOVE 12.00 TO YKWAIV-AMOUNT-2-AMT
               WHEN OTHER
                   COMPUTE YKWAIV-AMOUNT-2-AMT ROUNDED =
                       YKWAIV-AMOUNT-2-YTD * YKWAIV-MARGIN / 100
           END-EVALUATE
           IF YKWAIV-AMOUNT-2-AMT > 0 AND YKWAIV-AMOUNT-2-AMT < 10.00
               MOVE 10.00 TO YKWAIV-AMOUNT-2-AMT
           END-IF
           IF YKWAIV-AMOUNT-2-AMT > 25.00
               MOVE 25.00 TO YKWAIV-AMOUNT-2-AMT
           END-IF
           ADD YKWAIV-AMOUNT-2-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AMOUNT 1 YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2580-LIMIT-AMOUNT-1.
           IF YKWAIV-AMOUNT-1-YTD < 10.00
               MOVE 10.00 TO YKWAIV-AMOUNT-1-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKWAIV-AMOUNT-1-YTD > 2500.00
               MOVE 2500.00 TO YKWAIV-AMOUNT-1-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 1 AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2590-ACCRUE-AMOUNT-1.
           IF YKWAIV-AMOUNT-1-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKWAIV-AMOUNT-1-AMT * YKWAIV-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO YKWAIV-CONTROL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> H WHEN LAST ACT DATE IS PAST
      * H -> K WHEN LAST ACT DATE IS PAST
      * K -> S WHEN LAST ACT DATE IS PAST
      * S -> V WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2600-SET-REGION.
           EVALUATE YKWAIV-REGION-CD
               WHEN 'C'
                   IF YKWAIV-LAST-ACT-DATE > 0 AND YKWAIV-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'H' TO YKWAIV-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF YKWAIV-LAST-ACT-DATE > 0 AND YKWAIV-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'K' TO YKWAIV-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF YKWAIV-LAST-ACT-DATE > 0 AND YKWAIV-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'S' TO YKWAIV-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF YKWAIV-LAST-ACT-DATE > 0 AND YKWAIV-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'V' TO YKWAIV-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIERX TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2610-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIERX-RATE(1) TO YKWAIV-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-CONTROL-TIERX-LOW(WS-TX)
                   MOVE WS-CONTROL-TIERX-RATE(WS-TX) TO YKWAIV-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YKWAIV-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YKWAIV-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN NOTICE DATE IS PAST
      * FTB -> GAP WHEN NOTICE DATE IS PAST
      * GAP -> PAP WHEN NOTICE DATE IS PAST
      * PAP -> VGC WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2620-SET-SOURCE.
           EVALUATE YKWAIV-SOURCE-CD
               WHEN 'BNX'
                   IF YKWAIV-NOTICE-DATE > 0 AND YKWAIV-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YKWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YKWAIV-NOTICE-DATE > 0 AND YKWAIV-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YKWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YKWAIV-NOTICE-DATE > 0 AND YKWAIV-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YKWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YKWAIV-NOTICE-DATE > 0 AND YKWAIV-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YKWAIV-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2630-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF YKWAIV-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YKWAIV-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO YKWAIV-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> LK WHEN NOTICE DATE IS PAST
      * LK -> NT WHEN NOTICE DATE IS PAST
      * NT -> UW WHEN NOTICE DATE IS PAST
      * UW -> AX WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2640-SET-REASON.
           EVALUATE YKWAIV-REASON-CD
               WHEN 'AX'
                   IF YKWAIV-NOTICE-DATE > 0 AND YKWAIV-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LK' TO YKWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LK'
                   IF YKWAIV-NOTICE-DATE > 0 AND YKWAIV-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NT' TO YKWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NT'
                   IF YKWAIV-NOTICE-DATE > 0 AND YKWAIV-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UW' TO YKWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UW'
                   IF YKWAIV-NOTICE-DATE > 0 AND YKWAIV-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AX' TO YKWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2650-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE YKWAIV-AMOUNT-2-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2660-DESCRIBE-REGION.
           EVALUATE YKWAIV-REGION-CD
               WHEN 'C'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'K'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'S'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'V'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'X'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2670-DESCRIBE-REASON.
           EVALUATE YKWAIV-REASON-CD
               WHEN 'AX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2680-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2690-FLAG-AMOUNT-1.
           IF YKWAIV-AMOUNT-1-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YKWAIV-AMOUNT-1-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E134 CONTACT REQUIRED
      * E681 WORK AMT NEGATIVE
      * E019 WORK AMT OVER 10000
      * E833 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2700-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKWAIV-CONTACT = SPACES
                   MOVE 'E134' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E681' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E019' TO WS-REJECT-CODE
               WHEN YKWAIV-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E833' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2710-LIMIT-AMOUNT-1.
           IF YKWAIV-AMOUNT-1-YTD < 100.00
               MOVE 100.00 TO YKWAIV-AMOUNT-1-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YKWAIV-AMOUNT-1-YTD > 500.00
               MOVE 500.00 TO YKWAIV-AMOUNT-1-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 1 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT 1 YTD.
      *----------------------------------------------------------------*
       2720-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YKWAIV-AMOUNT-1-YTD ROUNDED =
                   YKWAIV-AMOUNT-1-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YKWAIV-AMOUNT-1-YTD
           END-IF
           ADD YKWAIV-AMOUNT-1-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2730-DESCRIBE-SOURCE.
           EVALUATE YKWAIV-SOURCE-CD
               WHEN 'BNX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FTB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GAP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PAP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VGC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VJV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2740-DESCRIBE-REASON.
           EVALUATE YKWAIV-REASON-CD
               WHEN 'AX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 1 AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2750-ACCRUE-AMOUNT-1.
           IF YKWAIV-AMOUNT-1-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKWAIV-AMOUNT-1-AMT * YKWAIV-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO YKWAIV-AMOUNT-2-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2760-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YKWAIV-AMOUNT-1-YTD / 2
           COMPUTE WS-WORK-AMT-2 = YKWAIV-AMOUNT-1-YTD - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2770-COUNT-CYCLE.
           IF YKWAIV-CYCLE-CNT < 9999999
               ADD 1 TO YKWAIV-CYCLE-CNT
           END-IF
           IF YKWAIV-CYCLE-CNT NOT < 5
               MOVE 'Y' TO YKWAIV-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2780-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YKWAIV-AMOUNT-1-AMT / 3
           COMPUTE WS-WORK-AMT-2 = YKWAIV-AMOUNT-1-AMT - (WS-WORK-AMT *
               3)
           COMPUTE YKWAIV-CONTROL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E587 CUST ID REQUIRED
      * E012 AMOUNT 1 AMT NEGATIVE
      * E716 AMOUNT 1 AMT OVER 25000
      * E490 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2790-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKWAIV-CUST-ID = SPACES
                   MOVE 'E587' TO WS-REJECT-CODE
               WHEN YKWAIV-AMOUNT-1-AMT < 0
                   MOVE 'E012' TO WS-REJECT-CODE
               WHEN YKWAIV-AMOUNT-1-AMT > 25000.00
                   MOVE 'E716' TO WS-REJECT-CODE
               WHEN YKWAIV-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E490' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 1 YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2800-ACCRUE-AMOUNT-1.
           IF YKWAIV-AMOUNT-1-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YKWAIV-AMOUNT-1-YTD * YKWAIV-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO YKWAIV-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 1 FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2810-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN YKWAIV-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO YKWAIV-AMOUNT-1-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YKWAIV-PREFERRED-FLAG = 'Y'
                   COMPUTE YKWAIV-AMOUNT-1-AMT ROUNDED = WS-WORK-AMT-2 *
                       YKWAIV-MARGIN / 200
               WHEN YKWAIV-SOURCE-CD = 'BNX'
                   MOVE 12.00 TO YKWAIV-AMOUNT-1-AMT
               WHEN YKWAIV-SOURCE-CD = 'FTB'
                   MOVE 10.00 TO YKWAIV-AMOUNT-1-AMT
               WHEN OTHER
                   COMPUTE YKWAIV-AMOUNT-1-AMT ROUNDED = WS-WORK-AMT-2 *
                       YKWAIV-MARGIN / 100
           END-EVALUATE
           IF YKWAIV-AMOUNT-1-AMT > 0 AND YKWAIV-AMOUNT-1-AMT < 5.00
               MOVE 5.00 TO YKWAIV-AMOUNT-1-AMT
           END-IF
           IF YKWAIV-AMOUNT-1-AMT > 35.00
               MOVE 35.00 TO YKWAIV-AMOUNT-1-AMT
           END-IF
           ADD YKWAIV-AMOUNT-1-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2820-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E106 CONTACT REQUIRED
      * E942 AMOUNT 2 AMT NEGATIVE
      * E286 AMOUNT 2 AMT OVER 25000
      * E117 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2830-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YKWAIV-CONTACT = SPACES
                   MOVE 'E106' TO WS-REJECT-CODE
               WHEN YKWAIV-AMOUNT-2-AMT < 0
                   MOVE 'E942' TO WS-REJECT-CODE
               WHEN YKWAIV-AMOUNT-2-AMT > 25000.00
                   MOVE 'E286' TO WS-REJECT-CODE
               WHEN YKWAIV-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E117' TO WS-REJECT-CODE
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
                   MOVE 08 TO YKWAIV-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO YKWAIV-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO YKWAIV-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'YKWAIV EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
