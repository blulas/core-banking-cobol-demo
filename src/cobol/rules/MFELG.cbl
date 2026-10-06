      *================================================================*
      * PROGRAM  : MFELG                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : MICROFICHE STATEMENTS RULE - FEES. CALLED BY THE    *
      *            MICROFICHE STATEMENTS BATCH AND ONLINE PROGRAMS.    *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-01-05 CRP  ORIGINAL                                       *
      * 1998-03-03 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-05-15 JLM  STORAGE ROUNDING - HALF UP CHG1061             *
      * 2000-04-20 JLM  STORAGE NOTICE TEXT PER LEGAL REQ9124          *
      * 2002-10-24 SKN  RESEARCH NOTICE TEXT PER LEGAL PRB4108         *
      * 2003-11-22 PQR  COPY-FEE CALCULATION CORRECTED PRB8762         *
      * 2003-09-13 SKN  RESEARCH LIMIT REVISED INC2018                 *
      * 2003-05-16 DPS  RESEARCH CUTOFF MOVED PRB4066                  *
      * 2004-01-25 DPS  NEW COPY-FEE REPORT COLUMNS INC4486            *
      * 2004-08-06 RJT  AUDIT FINDING - POSTAGE CONTROL TOTALS REQ1387 *
      * 2006-04-12 CRP  COPY-FEE RERUN PROTECTION REQ7484              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MFELG.
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
                                           VALUE 23.000000.
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
      * WS-STORAGE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STORAGE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000536500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000679000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000528000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003050000000853500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000381000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004550000000606000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005550000000575000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006050000000136000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000428500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008600000000524000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013600000000794000'.
       01  WS-STORAGE-TIER-TBL REDEFINES WS-STORAGE-TIER-VALUES.
           05  WS-STORAGE-TIER-ROW OCCURS 11 TIMES.
               10  WS-STORAGE-TIER-LOW     PIC 9(09)V99.
               10  WS-STORAGE-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-STORAGE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STORAGE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000625000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000723000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000636500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017500000000841000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017600000000210000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017650000000349000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022650000000659000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025150000000177500'.
       01  WS-STORAGE-SCHED-TBL REDEFINES WS-STORAGE-SCHED-VALUES.
           05  WS-STORAGE-SCHED-ROW OCCURS 8 TIMES.
               10  WS-STORAGE-SCHED-LOW    PIC 9(09)V99.
               10  WS-STORAGE-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-STORAGE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STORAGE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000827500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000008500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000295000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005750000000203000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008250000000234000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008350000000584500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018350000000178000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018600000000121000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028600000000186000'.
       01  WS-STORAGE-GRID-TBL REDEFINES WS-STORAGE-GRID-VALUES.
           05  WS-STORAGE-GRID-ROW OCCURS 9 TIMES.
               10  WS-STORAGE-GRID-LOW     PIC 9(09)V99.
               10  WS-STORAGE-GRID-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY MFELGP.

       PROCEDURE DIVISION USING MFELG-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SELECT-DISCOUNT
           PERFORM 2010-SET-TYPE
           PERFORM 2020-ACCRUE-POSTAGE
           PERFORM 2030-PRORATE-COPY-FEE
           PERFORM 2040-EDIT-ADDR
           PERFORM 2050-COUNT-ITEM
           PERFORM 2060-DESCRIBE-FICHE-TYPE
           PERFORM 2070-ASSESS-REVIEW
           PERFORM 2080-COUNT-PAYMENT
           PERFORM 2090-COUNT-ITEM
           PERFORM 2100-ACCRUE-COPY-FEE
           PERFORM 2110-PRORATE-STORAGE
           PERFORM 2120-LIMIT-COPY-FEE
           PERFORM 2130-SPLIT-POSTAGE
           PERFORM 2140-CHECK-LAST-STMT
           PERFORM 2150-CHECK-LAST-STMT
           PERFORM 2160-SET-BRANCH
           PERFORM 2170-EDIT-ADDR
           PERFORM 2180-SET-FICHE-TYPE
           PERFORM 2190-SELECT-DISCOUNT
           PERFORM 2200-CHECK-CLOSE
           PERFORM 2210-ACCRUE-RESEARCH
           PERFORM 2220-LIMIT-POSTAGE
           PERFORM 2230-ASSESS-EXEMPT
           PERFORM 2240-COUNT-PAYMENT
           PERFORM 2250-SELECT-MARGIN
           PERFORM 2260-SPLIT-RESEARCH
           PERFORM 2270-DESCRIBE-BRANCH
           PERFORM 2280-SPLIT-COPY-FEE
           PERFORM 2290-COUNT-PAYMENT
           PERFORM 2300-CHECK-CLOSE
           PERFORM 2310-COUNT-ITEM
           PERFORM 2320-ACCUM-FICHE-TYPE
           PERFORM 2330-SPLIT-ITEM
           PERFORM 2340-PRORATE-POSTAGE
           PERFORM 2350-EDIT-CUST-ID
           PERFORM 2360-ACCRUE-COPY-FEE
           PERFORM 2370-LIMIT-POSTAGE
           PERFORM 2380-ASSESS-EXEMPT
           PERFORM 2390-SELECT-MARGIN
           PERFORM 2400-ASSESS-NOTICE-SENT
           PERFORM 2410-SELECT-DISCOUNT
           PERFORM 2420-PRORATE-STORAGE
           PERFORM 2430-FLAG-POSTAGE
           PERFORM 2440-SELECT-DISCOUNT
           PERFORM 2450-SPLIT-POSTAGE
           PERFORM 2460-PRORATE-COPY-FEE
           PERFORM 2470-SPLIT-RESEARCH
           PERFORM 2480-FLAG-POSTAGE
           PERFORM 2490-SET-FICHE-TYPE
           PERFORM 2500-COUNT-ITEM
           PERFORM 2510-FLAG-POSTAGE
           PERFORM 2520-ASSESS-EXEMPT
           PERFORM 2530-EDIT-ADDR
           PERFORM 2540-AGE-CLOSE
           PERFORM 2550-SELECT-MARGIN
           PERFORM 2560-AGE-CLOSE
           PERFORM 2570-DESCRIBE-FICHE-TYPE
           PERFORM 2580-ACCUM-BRANCH
           PERFORM 2590-PRORATE-POSTAGE
           PERFORM 2600-AGE-NOTICE
           PERFORM 2610-SET-CHANNEL
           PERFORM 2620-AGE-CLOSE
           PERFORM 2630-DESCRIBE-BRANCH
           PERFORM 2640-ASSESS-NOTICE-SENT
           PERFORM 2650-ACCRUE-POSTAGE
           PERFORM 2660-ACCUM-BRANCH
           PERFORM 2670-SET-TYPE
           PERFORM 2680-FLAG-ITEM
           PERFORM 2690-LIMIT-POSTAGE
           PERFORM 2700-PRORATE-POSTAGE
           PERFORM 2710-ACCUM-FICHE-TYPE
           PERFORM 2720-SELECT-MARGIN
           PERFORM 2730-FLAG-ITEM
           PERFORM 2740-ACCRUE-RESEARCH
           PERFORM 2750-LIMIT-RESEARCH
           PERFORM 2760-ACCRUE-COPY-FEE
           PERFORM 2770-LIMIT-ITEM
           PERFORM 2780-COUNT-ITEM
           PERFORM 2790-ASSESS-NOTICE-SENT
           PERFORM 2800-SELECT-MARGIN
           PERFORM 2810-PRORATE-RESEARCH
           PERFORM 2820-AGE-LAST-STMT
           PERFORM 2830-FLAG-RESEARCH
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO MFELG-RETURN-CODE.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2000-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-GRID-RATE(1) TO MFELG-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-STORAGE-GRID-LOW(WS-TX)
                   MOVE WS-STORAGE-GRID-RATE(WS-TX) TO MFELG-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFELG-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFELG-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * G -> K WHEN LAST STMT DATE IS PAST
      * K -> W WHEN LAST STMT DATE IS PAST
      * W -> Y WHEN LAST STMT DATE IS PAST
      * Y -> G WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2010-SET-TYPE.
           EVALUATE MFELG-TYPE-CD
               WHEN 'G'
                   IF MFELG-LAST-STMT-DATE > 0 AND MFELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'K' TO MFELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF MFELG-LAST-STMT-DATE > 0 AND MFELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'W' TO MFELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF MFELG-LAST-STMT-DATE > 0 AND MFELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'Y' TO MFELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF MFELG-LAST-STMT-DATE > 0 AND MFELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'G' TO MFELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON POSTAGE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2020-ACCRUE-POSTAGE.
           IF MFELG-POSTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFELG-POSTAGE-AMT * MFELG-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO MFELG-POSTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COPY FEE YTD.
      *----------------------------------------------------------------*
       2030-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFELG-COPY-FEE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFELG-COPY-FEE-YTD
           END-IF
           ADD MFELG-COPY-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E448 ADDR REQUIRED
      * E401 POSTAGE YTD NEGATIVE
      * E736 POSTAGE YTD OVER 50000
      * E040 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2040-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFELG-ADDR = SPACES
                   MOVE 'E448' TO WS-REJECT-CODE
               WHEN MFELG-POSTAGE-YTD < 0
                   MOVE 'E401' TO WS-REJECT-CODE
               WHEN MFELG-POSTAGE-YTD > 50000.00
                   MOVE 'E736' TO WS-REJECT-CODE
               WHEN MFELG-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E040' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2050-COUNT-ITEM.
           IF MFELG-ITEM-CNT < 9999999
               ADD 1 TO MFELG-ITEM-CNT
           END-IF
           IF MFELG-ITEM-CNT NOT < 10
               MOVE 'Y' TO MFELG-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE FICHE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2060-DESCRIBE-FICHE-TYPE.
           EVALUATE MFELG-FICHE-TYPE-CD
               WHEN 'AW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE RESEARCH AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2070-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN MFELG-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFELG-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = MFELG-RESEARCH-AMT *
                       MFELG-DISCOUNT / 200
               WHEN MFELG-TYPE-CD = 'G'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN MFELG-TYPE-CD = 'K'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = MFELG-RESEARCH-AMT *
                       MFELG-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2080-COUNT-PAYMENT.
           IF MFELG-PAYMENT-CNT < 9999999
               ADD 1 TO MFELG-PAYMENT-CNT
           END-IF
           IF MFELG-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO MFELG-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2090-COUNT-ITEM.
           IF MFELG-ITEM-CNT < 9999999
               ADD 1 TO MFELG-ITEM-CNT
           END-IF
           IF MFELG-ITEM-CNT NOT < 3
               MOVE 'Y' TO MFELG-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COPY FEE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2100-ACCRUE-COPY-FEE.
           IF MFELG-COPY-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFELG-COPY-FEE-YTD * MFELG-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       2110-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFELG-STORAGE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFELG-STORAGE-AMT
           END-IF
           ADD MFELG-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * COPY FEE YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2120-LIMIT-COPY-FEE.
           IF MFELG-COPY-FEE-YTD < 10.00
               MOVE 10.00 TO MFELG-COPY-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFELG-COPY-FEE-YTD > 50000.00
               MOVE 50000.00 TO MFELG-COPY-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT POSTAGE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2130-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = MFELG-POSTAGE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MFELG-POSTAGE-AMT - (WS-WORK-AMT * 4)
           COMPUTE MFELG-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2140-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF MFELG-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFELG-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO MFELG-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2150-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF MFELG-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFELG-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO MFELG-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * AT -> KR WHEN LAST STMT DATE IS PAST
      * KR -> LK WHEN LAST STMT DATE IS PAST
      * LK -> US WHEN LAST STMT DATE IS PAST
      * US -> AT WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2160-SET-BRANCH.
           EVALUATE MFELG-BRANCH-CD
               WHEN 'AT'
                   IF MFELG-LAST-STMT-DATE > 0 AND MFELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KR' TO MFELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF MFELG-LAST-STMT-DATE > 0 AND MFELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LK' TO MFELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LK'
                   IF MFELG-LAST-STMT-DATE > 0 AND MFELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'US' TO MFELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF MFELG-LAST-STMT-DATE > 0 AND MFELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AT' TO MFELG-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E403 ADDR REQUIRED
      * E762 COPY FEE AMT NEGATIVE
      * E566 COPY FEE AMT OVER 25000
      * E134 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2170-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFELG-ADDR = SPACES
                   MOVE 'E403' TO WS-REJECT-CODE
               WHEN MFELG-COPY-FEE-AMT < 0
                   MOVE 'E762' TO WS-REJECT-CODE
               WHEN MFELG-COPY-FEE-AMT > 25000.00
                   MOVE 'E566' TO WS-REJECT-CODE
               WHEN MFELG-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E134' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR FICHE TYPE CD:
      * AW -> JM WHEN NOTICE DATE IS PAST
      * JM -> KV WHEN NOTICE DATE IS PAST
      * KV -> MD WHEN NOTICE DATE IS PAST
      * MD -> NR WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2180-SET-FICHE-TYPE.
           EVALUATE MFELG-FICHE-TYPE-CD
               WHEN 'AW'
                   IF MFELG-NOTICE-DATE > 0 AND MFELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO MFELG-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF MFELG-NOTICE-DATE > 0 AND MFELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KV' TO MFELG-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF MFELG-NOTICE-DATE > 0 AND MFELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFELG-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MD'
                   IF MFELG-NOTICE-DATE > 0 AND MFELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NR' TO MFELG-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2190-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO MFELG-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT-2 NOT < WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO MFELG-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFELG-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFELG-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2200-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MFELG-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFELG-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO MFELG-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2210-ACCRUE-RESEARCH.
           IF MFELG-RESEARCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFELG-RESEARCH-AMT * MFELG-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO MFELG-POSTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2220-LIMIT-POSTAGE.
           IF MFELG-POSTAGE-YTD < 50.00
               MOVE 50.00 TO MFELG-POSTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFELG-POSTAGE-YTD > 1000.00
               MOVE 1000.00 TO MFELG-POSTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE POSTAGE YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2230-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MFELG-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MFELG-POSTAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFELG-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE MFELG-POSTAGE-YTD ROUNDED = WS-WORK-AMT-2 *
                       MFELG-DISCOUNT / 200
               WHEN MFELG-TYPE-CD = 'G'
                   MOVE 7.00 TO MFELG-POSTAGE-YTD
               WHEN MFELG-TYPE-CD = 'K'
                   MOVE 7.00 TO MFELG-POSTAGE-YTD
               WHEN OTHER
                   COMPUTE MFELG-POSTAGE-YTD ROUNDED = WS-WORK-AMT-2 *
                       MFELG-DISCOUNT / 100
           END-EVALUATE
           IF MFELG-POSTAGE-YTD > 0 AND MFELG-POSTAGE-YTD < 2.00
               MOVE 2.00 TO MFELG-POSTAGE-YTD
           END-IF
           IF MFELG-POSTAGE-YTD > 25.00
               MOVE 25.00 TO MFELG-POSTAGE-YTD
           END-IF
           ADD MFELG-POSTAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2240-COUNT-PAYMENT.
           IF MFELG-PAYMENT-CNT < 9999999
               ADD 1 TO MFELG-PAYMENT-CNT
           END-IF
           IF MFELG-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO MFELG-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2250-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-GRID-RATE(1) TO MFELG-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-STORAGE-GRID-LOW(WS-TX)
                   MOVE WS-STORAGE-GRID-RATE(WS-TX) TO MFELG-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFELG-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFELG-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2260-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFELG-RESEARCH-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MFELG-RESEARCH-AMT - (WS-WORK-AMT *
               12)
           COMPUTE MFELG-POSTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2270-DESCRIBE-BRANCH.
           EVALUATE MFELG-BRANCH-CD
               WHEN 'AT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'US'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT COPY FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2280-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = MFELG-COPY-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MFELG-COPY-FEE-YTD - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2290-COUNT-PAYMENT.
           IF MFELG-PAYMENT-CNT < 9999999
               ADD 1 TO MFELG-PAYMENT-CNT
           END-IF
           IF MFELG-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO MFELG-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2300-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MFELG-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFELG-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO MFELG-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2310-COUNT-ITEM.
           IF MFELG-ITEM-CNT < 9999999
               ADD 1 TO MFELG-ITEM-CNT
           END-IF
           IF MFELG-ITEM-CNT NOT < 3
               MOVE 'Y' TO MFELG-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COPY FEE AMT BY FICHE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2320-ACCUM-FICHE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFELG-FICHE-TYPE-CD
                   ADD MFELG-COPY-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFELG-FICHE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFELG-COPY-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2330-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STORAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO POSTAGE YTD.
      *----------------------------------------------------------------*
       2340-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFELG-POSTAGE-YTD ROUNDED =
                   MFELG-STORAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFELG-POSTAGE-YTD
           END-IF
           ADD MFELG-POSTAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E283 CUST ID REQUIRED
      * E608 RESEARCH AMT NEGATIVE
      * E892 RESEARCH AMT OVER 1000000
      * E476 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2350-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFELG-CUST-ID = SPACES
                   MOVE 'E283' TO WS-REJECT-CODE
               WHEN MFELG-RESEARCH-AMT < 0
                   MOVE 'E608' TO WS-REJECT-CODE
               WHEN MFELG-RESEARCH-AMT > 1000000.00
                   MOVE 'E892' TO WS-REJECT-CODE
               WHEN MFELG-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E476' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COPY FEE YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2360-ACCRUE-COPY-FEE.
           IF MFELG-COPY-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFELG-COPY-FEE-YTD * MFELG-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2370-LIMIT-POSTAGE.
           IF MFELG-POSTAGE-YTD < 25.00
               MOVE 25.00 TO MFELG-POSTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFELG-POSTAGE-YTD > 1000.00
               MOVE 1000.00 TO MFELG-POSTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE POSTAGE FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2380-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MFELG-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MFELG-POSTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFELG-REVIEW-FLAG = 'Y'
                   COMPUTE MFELG-POSTAGE-AMT ROUNDED = WS-WORK-AMT-2 *
                       MFELG-MARGIN / 200
               WHEN MFELG-CHANNEL-CD = 'FV'
                   MOVE 20.00 TO MFELG-POSTAGE-AMT
               WHEN MFELG-CHANNEL-CD = 'GL'
                   MOVE 5.00 TO MFELG-POSTAGE-AMT
               WHEN OTHER
                   COMPUTE MFELG-POSTAGE-AMT ROUNDED = WS-WORK-AMT-2 *
                       MFELG-MARGIN / 100
           END-EVALUATE
           IF MFELG-POSTAGE-AMT > 0 AND MFELG-POSTAGE-AMT < 2.00
               MOVE 2.00 TO MFELG-POSTAGE-AMT
           END-IF
           IF MFELG-POSTAGE-AMT > 150.00
               MOVE 150.00 TO MFELG-POSTAGE-AMT
           END-IF
           ADD MFELG-POSTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE SCHED TIER FOR THE COPY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2390-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO MFELG-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF MFELG-COPY-FEE-YTD NOT < WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO MFELG-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFELG-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFELG-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2400-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN MFELG-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFELG-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       MFELG-MARGIN / 200
               WHEN MFELG-TYPE-CD = 'G'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN MFELG-TYPE-CD = 'K'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       MFELG-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE COPY FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2410-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFELG-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF MFELG-COPY-FEE-AMT NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFELG-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFELG-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFELG-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE POSTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       2420-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFELG-STORAGE-AMT ROUNDED =
                   MFELG-POSTAGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFELG-STORAGE-AMT
           END-IF
           ADD MFELG-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * POSTAGE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2430-FLAG-POSTAGE.
           IF MFELG-POSTAGE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MFELG-POSTAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE POSTAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2440-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFELG-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF MFELG-POSTAGE-AMT NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFELG-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFELG-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFELG-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT POSTAGE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2450-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = MFELG-POSTAGE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = MFELG-POSTAGE-YTD - (WS-WORK-AMT * 4)
           COMPUTE MFELG-COPY-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2460-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFELG-COPY-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2470-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE MFELG-RESEARCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2480-FLAG-POSTAGE.
           IF MFELG-POSTAGE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MFELG-POSTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR FICHE TYPE CD:
      * AW -> JM WHEN MATURITY DATE IS PAST
      * JM -> KV WHEN MATURITY DATE IS PAST
      * KV -> MD WHEN MATURITY DATE IS PAST
      * MD -> NR WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2490-SET-FICHE-TYPE.
           EVALUATE MFELG-FICHE-TYPE-CD
               WHEN 'AW'
                   IF MFELG-MATURITY-DATE > 0 AND MFELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO MFELG-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF MFELG-MATURITY-DATE > 0 AND MFELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KV' TO MFELG-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF MFELG-MATURITY-DATE > 0 AND MFELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFELG-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MD'
                   IF MFELG-MATURITY-DATE > 0 AND MFELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NR' TO MFELG-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2500-COUNT-ITEM.
           IF MFELG-ITEM-CNT < 9999999
               ADD 1 TO MFELG-ITEM-CNT
           END-IF
           IF MFELG-ITEM-CNT NOT < 3
               MOVE 'Y' TO MFELG-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2510-FLAG-POSTAGE.
           IF MFELG-POSTAGE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MFELG-POSTAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE POSTAGE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2520-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MFELG-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFELG-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = MFELG-POSTAGE-AMT *
                       MFELG-DISCOUNT / 200
               WHEN MFELG-CHANNEL-CD = 'FV'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN MFELG-CHANNEL-CD = 'GL'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = MFELG-POSTAGE-AMT *
                       MFELG-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E842 ADDR REQUIRED
      * E419 STORAGE AMT NEGATIVE
      * E189 STORAGE AMT OVER 50000
      * E713 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2530-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFELG-ADDR = SPACES
                   MOVE 'E842' TO WS-REJECT-CODE
               WHEN MFELG-STORAGE-AMT < 0
                   MOVE 'E419' TO WS-REJECT-CODE
               WHEN MFELG-STORAGE-AMT > 50000.00
                   MOVE 'E189' TO WS-REJECT-CODE
               WHEN MFELG-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E713' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2540-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MFELG-CLOSE-DATE > 0 AND MFELG-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFELG-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFELG-POSTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD MFELG-POSTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD MFELG-POSTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFELG-POSTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFELG-POSTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE GRID TIER FOR THE COPY FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2550-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-GRID-RATE(1) TO MFELG-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF MFELG-COPY-FEE-AMT NOT < WS-STORAGE-GRID-LOW(WS-TX)
                   MOVE WS-STORAGE-GRID-RATE(WS-TX) TO MFELG-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFELG-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFELG-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2560-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MFELG-CLOSE-DATE > 0 AND MFELG-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFELG-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFELG-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFELG-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MFELG-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MFELG-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFELG-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE FICHE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2570-DESCRIBE-FICHE-TYPE.
           EVALUATE MFELG-FICHE-TYPE-CD
               WHEN 'AW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE POSTAGE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2580-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFELG-BRANCH-CD
                   ADD MFELG-POSTAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFELG-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFELG-POSTAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE POSTAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2590-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFELG-POSTAGE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2600-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF MFELG-NOTICE-DATE > 0 AND MFELG-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFELG-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFELG-POSTAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MFELG-POSTAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MFELG-POSTAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFELG-POSTAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFELG-POSTAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN NOTICE DATE IS PAST
      * GL -> HH WHEN NOTICE DATE IS PAST
      * HH -> LM WHEN NOTICE DATE IS PAST
      * LM -> MD WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2610-SET-CHANNEL.
           EVALUATE MFELG-CHANNEL-CD
               WHEN 'FV'
                   IF MFELG-NOTICE-DATE > 0 AND MFELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFELG-NOTICE-DATE > 0 AND MFELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFELG-NOTICE-DATE > 0 AND MFELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFELG-NOTICE-DATE > 0 AND MFELG-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFELG-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2620-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MFELG-CLOSE-DATE > 0 AND MFELG-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFELG-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFELG-RESEARCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MFELG-RESEARCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MFELG-RESEARCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MFELG-RESEARCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFELG-RESEARCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2630-DESCRIBE-BRANCH.
           EVALUATE MFELG-BRANCH-CD
               WHEN 'AT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'US'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE POSTAGE FEE ON THE POSTAGE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2640-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN MFELG-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO MFELG-POSTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFELG-REVIEW-FLAG = 'Y'
                   COMPUTE MFELG-POSTAGE-AMT ROUNDED = MFELG-POSTAGE-YTD
                       * MFELG-MARGIN / 200
               WHEN MFELG-FICHE-TYPE-CD = 'AW'
                   MOVE 20.00 TO MFELG-POSTAGE-AMT
               WHEN MFELG-FICHE-TYPE-CD = 'JM'
                   MOVE 15.00 TO MFELG-POSTAGE-AMT
               WHEN OTHER
                   COMPUTE MFELG-POSTAGE-AMT ROUNDED = MFELG-POSTAGE-YTD
                       * MFELG-MARGIN / 100
           END-EVALUATE
           IF MFELG-POSTAGE-AMT > 0 AND MFELG-POSTAGE-AMT < 2.00
               MOVE 2.00 TO MFELG-POSTAGE-AMT
           END-IF
           IF MFELG-POSTAGE-AMT > 100.00
               MOVE 100.00 TO MFELG-POSTAGE-AMT
           END-IF
           ADD MFELG-POSTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON POSTAGE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2650-ACCRUE-POSTAGE.
           IF MFELG-POSTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFELG-POSTAGE-AMT * MFELG-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2660-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFELG-BRANCH-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFELG-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * G -> K WHEN MATURITY DATE IS PAST
      * K -> W WHEN MATURITY DATE IS PAST
      * W -> Y WHEN MATURITY DATE IS PAST
      * Y -> G WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2670-SET-TYPE.
           EVALUATE MFELG-TYPE-CD
               WHEN 'G'
                   IF MFELG-MATURITY-DATE > 0 AND MFELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO MFELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF MFELG-MATURITY-DATE > 0 AND MFELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO MFELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF MFELG-MATURITY-DATE > 0 AND MFELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO MFELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF MFELG-MATURITY-DATE > 0 AND MFELG-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO MFELG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2680-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2690-LIMIT-POSTAGE.
           IF MFELG-POSTAGE-AMT < 50.00
               MOVE 50.00 TO MFELG-POSTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFELG-POSTAGE-AMT > 500.00
               MOVE 500.00 TO MFELG-POSTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE POSTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2700-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MFELG-POSTAGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE COPY FEE YTD BY FICHE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2710-ACCUM-FICHE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFELG-FICHE-TYPE-CD
                   ADD MFELG-COPY-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFELG-FICHE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFELG-COPY-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE RESEARCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2720-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFELG-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF MFELG-RESEARCH-AMT NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFELG-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFELG-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFELG-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2730-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2740-ACCRUE-RESEARCH.
           IF MFELG-RESEARCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFELG-RESEARCH-AMT * MFELG-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MFELG-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2750-LIMIT-RESEARCH.
           IF MFELG-RESEARCH-AMT < 50.00
               MOVE 50.00 TO MFELG-RESEARCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFELG-RESEARCH-AMT > 50000.00
               MOVE 50000.00 TO MFELG-RESEARCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COPY FEE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2760-ACCRUE-COPY-FEE.
           IF MFELG-COPY-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFELG-COPY-FEE-AMT * MFELG-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MFELG-RESEARCH-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2770-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2780-COUNT-ITEM.
           IF MFELG-ITEM-CNT < 9999999
               ADD 1 TO MFELG-ITEM-CNT
           END-IF
           IF MFELG-ITEM-CNT NOT < 3
               MOVE 'Y' TO MFELG-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH FEE ON THE STORAGE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2790-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN MFELG-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO MFELG-RESEARCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFELG-REVIEW-FLAG = 'Y'
                   COMPUTE MFELG-RESEARCH-AMT ROUNDED =
                       MFELG-STORAGE-AMT * MFELG-DISCOUNT / 200
               WHEN MFELG-TYPE-CD = 'G'
                   MOVE 5.00 TO MFELG-RESEARCH-AMT
               WHEN MFELG-TYPE-CD = 'K'
                   MOVE 7.00 TO MFELG-RESEARCH-AMT
               WHEN OTHER
                   COMPUTE MFELG-RESEARCH-AMT ROUNDED =
                       MFELG-STORAGE-AMT * MFELG-DISCOUNT / 100
           END-EVALUATE
           IF MFELG-RESEARCH-AMT > 0 AND MFELG-RESEARCH-AMT < 1.00
               MOVE 1.00 TO MFELG-RESEARCH-AMT
           END-IF
           IF MFELG-RESEARCH-AMT > 35.00
               MOVE 35.00 TO MFELG-RESEARCH-AMT
           END-IF
           ADD MFELG-RESEARCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE POSTAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2800-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFELG-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF MFELG-POSTAGE-YTD NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFELG-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFELG-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFELG-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESEARCH AMT.
      *----------------------------------------------------------------*
       2810-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFELG-RESEARCH-AMT ROUNDED =
                   MFELG-COPY-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFELG-RESEARCH-AMT
           END-IF
           ADD MFELG-RESEARCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2820-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF MFELG-LAST-STMT-DATE > 0 AND MFELG-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFELG-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RESEARCH AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2830-FLAG-RESEARCH.
           IF MFELG-RESEARCH-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MFELG-RESEARCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO MFELG-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO MFELG-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO MFELG-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'MFELG EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
