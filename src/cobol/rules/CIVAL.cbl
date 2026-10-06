      *================================================================*
      * PROGRAM  : CIVAL                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : CHECK IMAGE ARCHIVE RULE - VALIDATION. CALLED BY THE*
      *            CHECK IMAGE ARCHIVE BATCH AND ONLINE PROGRAMS.      *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-12-08 RJT  ORIGINAL                                       *
      * 1988-12-27 RJT  AUDIT FINDING - IMAGE-FEE CONTROL TOTALS CHG313*
      * 2002-01-05 RJT  RETRIEVE-FEE CALCULATION CORRECTED PRB6797     *
      * 2004-10-17 JLM  ITEM FEE SCHEDULE UPDATE REQ9482               *
      * 2005-02-07 EWS  NEW STORAGE REPORT COLUMNS INC7987             *
      * 2005-11-08 CRP  NEW STORAGE REPORT COLUMNS PRB3130             *
      * 2007-06-22 TWH  STORAGE TABLE EXPANDED REQ7426                 *
      * 2008-08-17 CRP  PERFORMANCE - STORAGE READ AHEAD INC2035       *
      * 2008-09-04 GAF  NEW RETRIEVE-FEE REPORT COLUMNS PRB6955        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIVAL.
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
                                           VALUE 8.000000.
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
      * WS-IMAGE-FE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-IMAGE-FE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000064000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000801000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000407500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003250000000307500'.
       01  WS-IMAGE-FE-BAND-TBL REDEFINES WS-IMAGE-FE-BAND-VALUES.
           05  WS-IMAGE-FE-BAND-ROW OCCURS 4 TIMES.
               10  WS-IMAGE-FE-BAND-LOW    PIC 9(09)V99.
               10  WS-IMAGE-FE-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RETRIEVE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RETRIEVE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000037500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000631500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000071000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000782000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000099000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000742000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016500000000786000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026500000000437000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031500000000631500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031550000000513500'.
       01  WS-RETRIEVE-BAND-TBL REDEFINES WS-RETRIEVE-BAND-VALUES.
           05  WS-RETRIEVE-BAND-ROW OCCURS 10 TIMES.
               10  WS-RETRIEVE-BAND-LOW    PIC 9(09)V99.
               10  WS-RETRIEVE-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-STORAGE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STORAGE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000366000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000149500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000633500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000858500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015050000000231500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000247500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015200000000133000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015300000000753500'.
       01  WS-STORAGE-GRID-TBL REDEFINES WS-STORAGE-GRID-VALUES.
           05  WS-STORAGE-GRID-ROW OCCURS 8 TIMES.
               10  WS-STORAGE-GRID-LOW     PIC 9(09)V99.
               10  WS-STORAGE-GRID-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY CIVALP.

       PROCEDURE DIVISION USING CIVAL-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCRUE-RETRIEVE-FEE
           PERFORM 2010-EDIT-ADDR
           PERFORM 2020-ASSESS-EXEMPT
           PERFORM 2030-SPLIT-STORAGE
           PERFORM 2040-ASSESS-HOLD
           PERFORM 2050-AGE-EFFECTIVE
           PERFORM 2060-ASSESS-EXEMPT
           PERFORM 2070-SPLIT-ITEM
           PERFORM 2080-COUNT-NOTICE
           PERFORM 2090-SELECT-PENALTY-PCT
           PERFORM 2100-ACCUM-CHANNEL
           PERFORM 2110-EDIT-NAME
           PERFORM 2120-SET-ITEM-TYPE
           PERFORM 2130-SPLIT-STORAGE
           PERFORM 2140-ACCUM-BRANCH
           PERFORM 2150-COUNT-PAYMENT
           PERFORM 2160-FLAG-ITEM
           PERFORM 2170-PRORATE-ITEM
           PERFORM 2180-CHECK-LAST-STMT
           PERFORM 2190-SELECT-MARGIN
           PERFORM 2200-CHECK-LAST-MAINT
           PERFORM 2210-SET-ITEM-TYPE
           PERFORM 2220-DESCRIBE-BRANCH
           PERFORM 2230-LIMIT-IMAGE-FEE
           PERFORM 2240-SPLIT-STORAGE
           PERFORM 2250-EDIT-ADDR
           PERFORM 2260-COUNT-PAYMENT
           PERFORM 2270-LIMIT-IMAGE-FEE
           PERFORM 2280-AGE-LAST-MAINT
           PERFORM 2290-ACCUM-ITEM-TYPE
           PERFORM 2300-ACCUM-CHANNEL
           PERFORM 2310-COUNT-NOTICE
           PERFORM 2320-AGE-LAST-MAINT
           PERFORM 2330-SPLIT-STORAGE
           PERFORM 2340-LIMIT-STORAGE
           PERFORM 2350-PRORATE-STORAGE
           PERFORM 2360-AGE-LAST-STMT
           PERFORM 2370-AGE-LAST-STMT
           PERFORM 2380-SPLIT-IMAGE-FEE
           PERFORM 2390-ASSESS-HOLD
           PERFORM 2400-DESCRIBE-ITEM-TYPE
           PERFORM 2410-COUNT-PAYMENT
           PERFORM 2420-SET-CAPTURE-SITE
           PERFORM 2430-DESCRIBE-ITEM-TYPE
           PERFORM 2440-ACCUM-ITEM-TYPE
           PERFORM 2450-SET-CHANNEL
           PERFORM 2460-CHECK-LAST-MAINT
           PERFORM 2470-ACCUM-ITEM-TYPE
           PERFORM 2480-DESCRIBE-CAPTURE-SITE
           PERFORM 2490-ACCRUE-ITEM
           PERFORM 2500-DESCRIBE-BRANCH
           PERFORM 2510-LIMIT-ITEM
           PERFORM 2520-LIMIT-RETRIEVE-FEE
           PERFORM 2530-SET-BRANCH
           PERFORM 2540-LIMIT-ITEM
           PERFORM 2550-FLAG-ITEM
           PERFORM 2560-ACCUM-CAPTURE-SITE
           PERFORM 2570-ACCRUE-IMAGE-FEE
           PERFORM 2580-ACCUM-CAPTURE-SITE
           PERFORM 2590-ACCUM-CAPTURE-SITE
           PERFORM 2600-ASSESS-HOLD
           PERFORM 2610-FLAG-ITEM
           PERFORM 2620-ASSESS-EXEMPT
           PERFORM 2630-COUNT-PAYMENT
           PERFORM 2640-ASSESS-EXEMPT
           PERFORM 2650-EDIT-ADDR
           PERFORM 2660-PRORATE-STORAGE
           PERFORM 2670-DESCRIBE-BRANCH
           PERFORM 2680-COUNT-NOTICE
           PERFORM 2690-LIMIT-RETRIEVE-FEE
           PERFORM 2700-FLAG-IMAGE-FEE
           PERFORM 2710-LIMIT-STORAGE
           PERFORM 2720-COUNT-PAYMENT
           PERFORM 2730-ACCUM-CAPTURE-SITE
           PERFORM 2740-PRORATE-ITEM
           PERFORM 2750-CHECK-EFFECTIVE
           PERFORM 2760-FLAG-ITEM
           PERFORM 2770-SPLIT-RETRIEVE-FEE
           PERFORM 2780-SELECT-MARGIN
           PERFORM 2790-SPLIT-ITEM
           PERFORM 2800-AGE-OPEN
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO CIVAL-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETRIEVE FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2000-ACCRUE-RETRIEVE-FEE.
           IF CIVAL-RETRIEVE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIVAL-RETRIEVE-FEE-AMT * CIVAL-PENALTY-PCT / 100 /
                       365
               ADD WS-WORK-AMT TO CIVAL-IMAGE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E526 ADDR REQUIRED
      * E573 IMAGE FEE AMT NEGATIVE
      * E222 IMAGE FEE AMT OVER 99999
      * E705 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2010-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIVAL-ADDR = SPACES
                   MOVE 'E526' TO WS-REJECT-CODE
               WHEN CIVAL-IMAGE-FEE-AMT < 0
                   MOVE 'E573' TO WS-REJECT-CODE
               WHEN CIVAL-IMAGE-FEE-AMT > 99999.00
                   MOVE 'E222' TO WS-REJECT-CODE
               WHEN CIVAL-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E705' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ITEM YTD FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2020-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CIVAL-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CIVAL-ITEM-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIVAL-OVERRIDE-FLAG = 'Y'
                   COMPUTE CIVAL-ITEM-YTD ROUNDED = WS-WORK-AMT *
                       CIVAL-MARGIN / 200
               WHEN CIVAL-CHANNEL-CD = 'FW'
                   MOVE 5.00 TO CIVAL-ITEM-YTD
               WHEN CIVAL-CHANNEL-CD = 'KL'
                   MOVE 5.00 TO CIVAL-ITEM-YTD
               WHEN OTHER
                   COMPUTE CIVAL-ITEM-YTD ROUNDED = WS-WORK-AMT *
                       CIVAL-MARGIN / 100
           END-EVALUATE
           IF CIVAL-ITEM-YTD > 0 AND CIVAL-ITEM-YTD < 10.00
               MOVE 10.00 TO CIVAL-ITEM-YTD
           END-IF
           IF CIVAL-ITEM-YTD > 25.00
               MOVE 25.00 TO CIVAL-ITEM-YTD
           END-IF
           ADD CIVAL-ITEM-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT STORAGE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2030-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = CIVAL-STORAGE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CIVAL-STORAGE-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE ITEM AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2040-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CIVAL-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CIVAL-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIVAL-EXEMPT-FLAG = 'Y'
                   COMPUTE CIVAL-STORAGE-AMT ROUNDED = CIVAL-ITEM-AMT *
                       CIVAL-MARGIN / 200
               WHEN CIVAL-ITEM-TYPE-CD = 'CW'
                   MOVE 15.00 TO CIVAL-STORAGE-AMT
               WHEN CIVAL-ITEM-TYPE-CD = 'FB'
                   MOVE 12.00 TO CIVAL-STORAGE-AMT
               WHEN OTHER
                   COMPUTE CIVAL-STORAGE-AMT ROUNDED = CIVAL-ITEM-AMT *
                       CIVAL-MARGIN / 100
           END-EVALUATE
           IF CIVAL-STORAGE-AMT > 0 AND CIVAL-STORAGE-AMT < 5.00
               MOVE 5.00 TO CIVAL-STORAGE-AMT
           END-IF
           IF CIVAL-STORAGE-AMT > 35.00
               MOVE 35.00 TO CIVAL-STORAGE-AMT
           END-IF
           ADD CIVAL-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2050-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CIVAL-EFFECTIVE-DATE > 0 AND CIVAL-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIVAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIVAL-IMAGE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CIVAL-IMAGE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CIVAL-IMAGE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIVAL-IMAGE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIVAL-IMAGE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ITEM FEE ON THE STORAGE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2060-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CIVAL-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CIVAL-ITEM-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIVAL-HOLD-FLAG = 'Y'
                   COMPUTE CIVAL-ITEM-AMT ROUNDED = CIVAL-STORAGE-AMT *
                       CIVAL-PENALTY-PCT / 200
               WHEN CIVAL-CAPTURE-SITE-CD = 'BB'
                   MOVE 20.00 TO CIVAL-ITEM-AMT
               WHEN CIVAL-CAPTURE-SITE-CD = 'CA'
                   MOVE 15.00 TO CIVAL-ITEM-AMT
               WHEN OTHER
                   COMPUTE CIVAL-ITEM-AMT ROUNDED = CIVAL-STORAGE-AMT *
                       CIVAL-PENALTY-PCT / 100
           END-EVALUATE
           IF CIVAL-ITEM-AMT > 0 AND CIVAL-ITEM-AMT < 10.00
               MOVE 10.00 TO CIVAL-ITEM-AMT
           END-IF
           IF CIVAL-ITEM-AMT > 150.00
               MOVE 150.00 TO CIVAL-ITEM-AMT
           END-IF
           ADD CIVAL-ITEM-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT ITEM YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2070-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIVAL-ITEM-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CIVAL-ITEM-YTD - (WS-WORK-AMT * 12)
           COMPUTE CIVAL-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2080-COUNT-NOTICE.
           IF CIVAL-NOTICE-CNT < 9999999
               ADD 1 TO CIVAL-NOTICE-CNT
           END-IF
           IF CIVAL-NOTICE-CNT NOT < 5
               MOVE 'Y' TO CIVAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE IMAGE FE BAND TIER FOR THE STORAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2090-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-IMAGE-FE-BAND-RATE(1) TO CIVAL-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CIVAL-STORAGE-YTD NOT < WS-IMAGE-FE-BAND-LOW(WS-TX)
                   MOVE WS-IMAGE-FE-BAND-RATE(WS-TX) TO
                       CIVAL-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIVAL-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIVAL-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE IMAGE FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2100-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIVAL-CHANNEL-CD
                   ADD CIVAL-IMAGE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIVAL-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIVAL-IMAGE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E493 NAME REQUIRED
      * E599 STORAGE AMT NEGATIVE
      * E444 STORAGE AMT OVER 50000
      * E539 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2110-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIVAL-NAME = SPACES
                   MOVE 'E493' TO WS-REJECT-CODE
               WHEN CIVAL-STORAGE-AMT < 0
                   MOVE 'E599' TO WS-REJECT-CODE
               WHEN CIVAL-STORAGE-AMT > 50000.00
                   MOVE 'E444' TO WS-REJECT-CODE
               WHEN CIVAL-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E539' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * CW -> FB WHEN LAST STMT DATE IS PAST
      * FB -> HN WHEN LAST STMT DATE IS PAST
      * HN -> KX WHEN LAST STMT DATE IS PAST
      * KX -> CW WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2120-SET-ITEM-TYPE.
           EVALUATE CIVAL-ITEM-TYPE-CD
               WHEN 'CW'
                   IF CIVAL-LAST-STMT-DATE > 0 AND CIVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FB' TO CIVAL-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF CIVAL-LAST-STMT-DATE > 0 AND CIVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HN' TO CIVAL-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HN'
                   IF CIVAL-LAST-STMT-DATE > 0 AND CIVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KX' TO CIVAL-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KX'
                   IF CIVAL-LAST-STMT-DATE > 0 AND CIVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'CW' TO CIVAL-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2130-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE CIVAL-STORAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ITEM YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2140-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIVAL-BRANCH-CD
                   ADD CIVAL-ITEM-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIVAL-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIVAL-ITEM-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2150-COUNT-PAYMENT.
           IF CIVAL-PAYMENT-CNT < 9999999
               ADD 1 TO CIVAL-PAYMENT-CNT
           END-IF
           IF CIVAL-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO CIVAL-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2160-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ITEM YTD.
      *----------------------------------------------------------------*
       2170-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIVAL-ITEM-YTD ROUNDED =
                   CIVAL-IMAGE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIVAL-ITEM-YTD
           END-IF
           ADD CIVAL-ITEM-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2180-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF CIVAL-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIVAL-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO CIVAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE IMAGE FE BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2190-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-IMAGE-FE-BAND-RATE(1) TO CIVAL-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-IMAGE-FE-BAND-LOW(WS-TX)
                   MOVE WS-IMAGE-FE-BAND-RATE(WS-TX) TO CIVAL-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIVAL-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIVAL-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2200-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF CIVAL-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CIVAL-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * CW -> FB WHEN LAST STMT DATE IS PAST
      * FB -> HN WHEN LAST STMT DATE IS PAST
      * HN -> KX WHEN LAST STMT DATE IS PAST
      * KX -> CW WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2210-SET-ITEM-TYPE.
           EVALUATE CIVAL-ITEM-TYPE-CD
               WHEN 'CW'
                   IF CIVAL-LAST-STMT-DATE > 0 AND CIVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FB' TO CIVAL-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF CIVAL-LAST-STMT-DATE > 0 AND CIVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HN' TO CIVAL-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HN'
                   IF CIVAL-LAST-STMT-DATE > 0 AND CIVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KX' TO CIVAL-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KX'
                   IF CIVAL-LAST-STMT-DATE > 0 AND CIVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'CW' TO CIVAL-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2220-DESCRIBE-BRANCH.
           EVALUATE CIVAL-BRANCH-CD
               WHEN 'DJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2230-LIMIT-IMAGE-FEE.
           IF CIVAL-IMAGE-FEE-AMT < 50.00
               MOVE 50.00 TO CIVAL-IMAGE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIVAL-IMAGE-FEE-AMT > 5000.00
               MOVE 5000.00 TO CIVAL-IMAGE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2240-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE CIVAL-STORAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E540 ADDR REQUIRED
      * E028 STORAGE YTD NEGATIVE
      * E769 STORAGE YTD OVER 99999
      * E605 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2250-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIVAL-ADDR = SPACES
                   MOVE 'E540' TO WS-REJECT-CODE
               WHEN CIVAL-STORAGE-YTD < 0
                   MOVE 'E028' TO WS-REJECT-CODE
               WHEN CIVAL-STORAGE-YTD > 99999.00
                   MOVE 'E769' TO WS-REJECT-CODE
               WHEN CIVAL-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2260-COUNT-PAYMENT.
           IF CIVAL-PAYMENT-CNT < 9999999
               ADD 1 TO CIVAL-PAYMENT-CNT
           END-IF
           IF CIVAL-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO CIVAL-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2270-LIMIT-IMAGE-FEE.
           IF CIVAL-IMAGE-FEE-AMT < 100.00
               MOVE 100.00 TO CIVAL-IMAGE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIVAL-IMAGE-FEE-AMT > 10000.00
               MOVE 10000.00 TO CIVAL-IMAGE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2280-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF CIVAL-LAST-MAINT-DATE > 0 AND CIVAL-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIVAL-ITEM-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CIVAL-ITEM-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CIVAL-ITEM-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIVAL-ITEM-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIVAL-ITEM-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ITEM YTD BY ITEM TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2290-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIVAL-ITEM-TYPE-CD
                   ADD CIVAL-ITEM-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIVAL-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIVAL-ITEM-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE IMAGE FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2300-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIVAL-CHANNEL-CD
                   ADD CIVAL-IMAGE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIVAL-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIVAL-IMAGE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2310-COUNT-NOTICE.
           IF CIVAL-NOTICE-CNT < 9999999
               ADD 1 TO CIVAL-NOTICE-CNT
           END-IF
           IF CIVAL-NOTICE-CNT NOT < 10
               MOVE 'Y' TO CIVAL-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2320-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF CIVAL-LAST-MAINT-DATE > 0 AND CIVAL-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIVAL-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD CIVAL-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CIVAL-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIVAL-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIVAL-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT STORAGE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2330-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = CIVAL-STORAGE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CIVAL-STORAGE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE CIVAL-RETRIEVE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2340-LIMIT-STORAGE.
           IF CIVAL-STORAGE-AMT < 0.00
               MOVE 0.00 TO CIVAL-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIVAL-STORAGE-AMT > 1000.00
               MOVE 1000.00 TO CIVAL-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       2350-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIVAL-STORAGE-AMT ROUNDED =
                   CIVAL-IMAGE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIVAL-STORAGE-AMT
           END-IF
           ADD CIVAL-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2360-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF CIVAL-LAST-STMT-DATE > 0 AND CIVAL-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIVAL-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIVAL-ITEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIVAL-ITEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CIVAL-ITEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIVAL-ITEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIVAL-ITEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2370-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF CIVAL-LAST-STMT-DATE > 0 AND CIVAL-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIVAL-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIVAL-ITEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CIVAL-ITEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CIVAL-ITEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIVAL-ITEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIVAL-ITEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT IMAGE FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2380-SPLIT-IMAGE-FEE.
           COMPUTE WS-WORK-AMT = CIVAL-IMAGE-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CIVAL-IMAGE-FEE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE CIVAL-ITEM-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETRIEVE FEE FEE ON THE IMAGE FEE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2390-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CIVAL-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CIVAL-RETRIEVE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIVAL-EXEMPT-FLAG = 'Y'
                   COMPUTE CIVAL-RETRIEVE-FEE-AMT ROUNDED =
                       CIVAL-IMAGE-FEE-AMT * CIVAL-PENALTY-PCT / 200
               WHEN CIVAL-CAPTURE-SITE-CD = 'BB'
                   MOVE 15.00 TO CIVAL-RETRIEVE-FEE-AMT
               WHEN CIVAL-CAPTURE-SITE-CD = 'CA'
                   MOVE 5.00 TO CIVAL-RETRIEVE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIVAL-RETRIEVE-FEE-AMT ROUNDED =
                       CIVAL-IMAGE-FEE-AMT * CIVAL-PENALTY-PCT / 100
           END-EVALUATE
           IF CIVAL-RETRIEVE-FEE-AMT > 0 AND CIVAL-RETRIEVE-FEE-AMT <
               5.00
               MOVE 5.00 TO CIVAL-RETRIEVE-FEE-AMT
           END-IF
           IF CIVAL-RETRIEVE-FEE-AMT > 250.00
               MOVE 250.00 TO CIVAL-RETRIEVE-FEE-AMT
           END-IF
           ADD CIVAL-RETRIEVE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2400-DESCRIBE-ITEM-TYPE.
           EVALUATE CIVAL-ITEM-TYPE-CD
               WHEN 'CW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2410-COUNT-PAYMENT.
           IF CIVAL-PAYMENT-CNT < 9999999
               ADD 1 TO CIVAL-PAYMENT-CNT
           END-IF
           IF CIVAL-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO CIVAL-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CAPTURE SITE CD:
      * BB -> CA WHEN LAST STMT DATE IS PAST
      * CA -> PC WHEN LAST STMT DATE IS PAST
      * PC -> BB WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2420-SET-CAPTURE-SITE.
           EVALUATE CIVAL-CAPTURE-SITE-CD
               WHEN 'BB'
                   IF CIVAL-LAST-STMT-DATE > 0 AND CIVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'CA' TO CIVAL-CAPTURE-SITE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CA'
                   IF CIVAL-LAST-STMT-DATE > 0 AND CIVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PC' TO CIVAL-CAPTURE-SITE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF CIVAL-LAST-STMT-DATE > 0 AND CIVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BB' TO CIVAL-CAPTURE-SITE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2430-DESCRIBE-ITEM-TYPE.
           EVALUATE CIVAL-ITEM-TYPE-CD
               WHEN 'CW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE RETRIEVE FEE AMT BY ITEM TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2440-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIVAL-ITEM-TYPE-CD
                   ADD CIVAL-RETRIEVE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIVAL-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIVAL-RETRIEVE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FW -> KL WHEN LAST STMT DATE IS PAST
      * KL -> NB WHEN LAST STMT DATE IS PAST
      * NB -> TP WHEN LAST STMT DATE IS PAST
      * TP -> ZZ WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2450-SET-CHANNEL.
           EVALUATE CIVAL-CHANNEL-CD
               WHEN 'FW'
                   IF CIVAL-LAST-STMT-DATE > 0 AND CIVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KL' TO CIVAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KL'
                   IF CIVAL-LAST-STMT-DATE > 0 AND CIVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'NB' TO CIVAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NB'
                   IF CIVAL-LAST-STMT-DATE > 0 AND CIVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TP' TO CIVAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TP'
                   IF CIVAL-LAST-STMT-DATE > 0 AND CIVAL-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ZZ' TO CIVAL-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2460-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF CIVAL-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CIVAL-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ITEM AMT BY ITEM TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2470-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIVAL-ITEM-TYPE-CD
                   ADD CIVAL-ITEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIVAL-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIVAL-ITEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CAPTURE SITE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2480-DESCRIBE-CAPTURE-SITE.
           EVALUATE CIVAL-CAPTURE-SITE-CD
               WHEN 'BB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ITEM AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2490-ACCRUE-ITEM.
           IF CIVAL-ITEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIVAL-ITEM-AMT * CIVAL-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIVAL-ITEM-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2500-DESCRIBE-BRANCH.
           EVALUATE CIVAL-BRANCH-CD
               WHEN 'DJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2510-LIMIT-ITEM.
           IF CIVAL-ITEM-YTD < 25.00
               MOVE 25.00 TO CIVAL-ITEM-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIVAL-ITEM-YTD > 500.00
               MOVE 500.00 TO CIVAL-ITEM-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRIEVE FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2520-LIMIT-RETRIEVE-FEE.
           IF CIVAL-RETRIEVE-FEE-AMT < 0.00
               MOVE 0.00 TO CIVAL-RETRIEVE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIVAL-RETRIEVE-FEE-AMT > 50000.00
               MOVE 50000.00 TO CIVAL-RETRIEVE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DJ -> FX WHEN OPEN DATE IS PAST
      * FX -> MT WHEN OPEN DATE IS PAST
      * MT -> UA WHEN OPEN DATE IS PAST
      * UA -> VL WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2530-SET-BRANCH.
           EVALUATE CIVAL-BRANCH-CD
               WHEN 'DJ'
                   IF CIVAL-OPEN-DATE > 0 AND CIVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FX' TO CIVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FX'
                   IF CIVAL-OPEN-DATE > 0 AND CIVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MT' TO CIVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MT'
                   IF CIVAL-OPEN-DATE > 0 AND CIVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UA' TO CIVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF CIVAL-OPEN-DATE > 0 AND CIVAL-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VL' TO CIVAL-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2540-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2550-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ITEM AMT BY CAPTURE SITE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2560-ACCUM-CAPTURE-SITE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIVAL-CAPTURE-SITE-CD
                   ADD CIVAL-ITEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIVAL-CAPTURE-SITE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIVAL-ITEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON IMAGE FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2570-ACCRUE-IMAGE-FEE.
           IF CIVAL-IMAGE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIVAL-IMAGE-FEE-AMT * CIVAL-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CIVAL-ITEM-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE YTD BY CAPTURE SITE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2580-ACCUM-CAPTURE-SITE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIVAL-CAPTURE-SITE-CD
                   ADD CIVAL-STORAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIVAL-CAPTURE-SITE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIVAL-STORAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE IMAGE FEE AMT BY CAPTURE SITE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2590-ACCUM-CAPTURE-SITE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIVAL-CAPTURE-SITE-CD
                   ADD CIVAL-IMAGE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIVAL-CAPTURE-SITE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIVAL-IMAGE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE IMAGE FEE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2600-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CIVAL-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIVAL-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CIVAL-IMAGE-FEE-AMT *
                       CIVAL-MARGIN / 200
               WHEN CIVAL-BRANCH-CD = 'DJ'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN CIVAL-BRANCH-CD = 'FX'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CIVAL-IMAGE-FEE-AMT *
                       CIVAL-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2610-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETRIEVE FEE FEE ON THE ITEM YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2620-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CIVAL-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CIVAL-RETRIEVE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIVAL-OVERRIDE-FLAG = 'Y'
                   COMPUTE CIVAL-RETRIEVE-FEE-AMT ROUNDED =
                       CIVAL-ITEM-YTD * CIVAL-MARGIN / 200
               WHEN CIVAL-CHANNEL-CD = 'FW'
                   MOVE 20.00 TO CIVAL-RETRIEVE-FEE-AMT
               WHEN CIVAL-CHANNEL-CD = 'KL'
                   MOVE 20.00 TO CIVAL-RETRIEVE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIVAL-RETRIEVE-FEE-AMT ROUNDED =
                       CIVAL-ITEM-YTD * CIVAL-MARGIN / 100
           END-EVALUATE
           IF CIVAL-RETRIEVE-FEE-AMT > 0 AND CIVAL-RETRIEVE-FEE-AMT <
               10.00
               MOVE 10.00 TO CIVAL-RETRIEVE-FEE-AMT
           END-IF
           IF CIVAL-RETRIEVE-FEE-AMT > 150.00
               MOVE 150.00 TO CIVAL-RETRIEVE-FEE-AMT
           END-IF
           ADD CIVAL-RETRIEVE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2630-COUNT-PAYMENT.
           IF CIVAL-PAYMENT-CNT < 9999999
               ADD 1 TO CIVAL-PAYMENT-CNT
           END-IF
           IF CIVAL-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO CIVAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETRIEVE FEE FEE ON THE IMAGE FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2640-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CIVAL-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CIVAL-RETRIEVE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIVAL-HOLD-FLAG = 'Y'
                   COMPUTE CIVAL-RETRIEVE-FEE-AMT ROUNDED =
                       CIVAL-IMAGE-FEE-AMT * CIVAL-MARGIN / 200
               WHEN CIVAL-CHANNEL-CD = 'FW'
                   MOVE 20.00 TO CIVAL-RETRIEVE-FEE-AMT
               WHEN CIVAL-CHANNEL-CD = 'KL'
                   MOVE 3.00 TO CIVAL-RETRIEVE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIVAL-RETRIEVE-FEE-AMT ROUNDED =
                       CIVAL-IMAGE-FEE-AMT * CIVAL-MARGIN / 100
           END-EVALUATE
           IF CIVAL-RETRIEVE-FEE-AMT > 0 AND CIVAL-RETRIEVE-FEE-AMT <
               1.00
               MOVE 1.00 TO CIVAL-RETRIEVE-FEE-AMT
           END-IF
           IF CIVAL-RETRIEVE-FEE-AMT > 25.00
               MOVE 25.00 TO CIVAL-RETRIEVE-FEE-AMT
           END-IF
           ADD CIVAL-RETRIEVE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E145 ADDR REQUIRED
      * E084 ITEM YTD NEGATIVE
      * E081 ITEM YTD OVER 25000
      * E093 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2650-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIVAL-ADDR = SPACES
                   MOVE 'E145' TO WS-REJECT-CODE
               WHEN CIVAL-ITEM-YTD < 0
                   MOVE 'E084' TO WS-REJECT-CODE
               WHEN CIVAL-ITEM-YTD > 25000.00
                   MOVE 'E081' TO WS-REJECT-CODE
               WHEN CIVAL-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E093' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ITEM YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       2660-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIVAL-STORAGE-AMT ROUNDED =
                   CIVAL-ITEM-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIVAL-STORAGE-AMT
           END-IF
           ADD CIVAL-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2670-DESCRIBE-BRANCH.
           EVALUATE CIVAL-BRANCH-CD
               WHEN 'DJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2680-COUNT-NOTICE.
           IF CIVAL-NOTICE-CNT < 9999999
               ADD 1 TO CIVAL-NOTICE-CNT
           END-IF
           IF CIVAL-NOTICE-CNT NOT < 5
               MOVE 'Y' TO CIVAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRIEVE FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2690-LIMIT-RETRIEVE-FEE.
           IF CIVAL-RETRIEVE-FEE-AMT < 25.00
               MOVE 25.00 TO CIVAL-RETRIEVE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIVAL-RETRIEVE-FEE-AMT > 50000.00
               MOVE 50000.00 TO CIVAL-RETRIEVE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2700-FLAG-IMAGE-FEE.
           IF CIVAL-IMAGE-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIVAL-IMAGE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2710-LIMIT-STORAGE.
           IF CIVAL-STORAGE-AMT < 50.00
               MOVE 50.00 TO CIVAL-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIVAL-STORAGE-AMT > 2500.00
               MOVE 2500.00 TO CIVAL-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2720-COUNT-PAYMENT.
           IF CIVAL-PAYMENT-CNT < 9999999
               ADD 1 TO CIVAL-PAYMENT-CNT
           END-IF
           IF CIVAL-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO CIVAL-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CAPTURE SITE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2730-ACCUM-CAPTURE-SITE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIVAL-CAPTURE-SITE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIVAL-CAPTURE-SITE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2740-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2750-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CIVAL-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIVAL-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CIVAL-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2760-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RETRIEVE FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2770-SPLIT-RETRIEVE-FEE.
           COMPUTE WS-WORK-AMT = CIVAL-RETRIEVE-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CIVAL-RETRIEVE-FEE-AMT - (WS-WORK-AMT
               * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE GRID TIER FOR THE ITEM AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2780-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-GRID-RATE(1) TO CIVAL-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CIVAL-ITEM-AMT NOT < WS-STORAGE-GRID-LOW(WS-TX)
                   MOVE WS-STORAGE-GRID-RATE(WS-TX) TO CIVAL-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIVAL-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIVAL-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2790-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE CIVAL-ITEM-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2800-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CIVAL-OPEN-DATE > 0 AND CIVAL-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIVAL-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIVAL-RETRIEVE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CIVAL-RETRIEVE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CIVAL-RETRIEVE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIVAL-RETRIEVE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIVAL-RETRIEVE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO CIVAL-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO CIVAL-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO CIVAL-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'CIVAL EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
