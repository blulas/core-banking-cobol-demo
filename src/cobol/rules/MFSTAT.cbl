      *================================================================*
      * PROGRAM  : MFSTAT                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : MICROFICHE STATEMENTS RULE - VALIDATION. CALLED BY  *
      *            THE MICROFICHE STATEMENTS BATCH AND ONLINE PROGRAMS.*
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-04-26 CRP  ORIGINAL                                       *
      * 1997-12-18 RJT  PERFORMANCE - COPY-FEE READ AHEAD CHG7243      *
      * 1998-08-15 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-11-27 BHS  POSTAGE TABLE EXPANDED CHG8893                 *
      * 2000-05-04 PQR  NEW COPY-FEE REPORT COLUMNS REQ9944            *
      * 2001-04-11 JLM  COPY-FEE ROUNDING - HALF UP CHG2481            *
      * 2002-03-21 TWH  AUDIT FINDING - STORAGE CONTROL TOTALS REQ8842 *
      * 2005-11-20 CRP  POSTAGE TABLE EXPANDED PRB4212                 *
      * 2008-11-10 PQR  RESEARCH STATUS CODES ADDED PRB7395            *
      * 2008-08-08 TWH  POSTAGE NOTICE TEXT PER LEGAL REQ9326          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MFSTAT.
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
                                           VALUE 14.000000.
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
      * WS-COPY-FEE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COPY-FEE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000232000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000820500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000052000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000850000000548000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000662500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001200000000598500'.
       01  WS-COPY-FEE-BAND-TBL REDEFINES WS-COPY-FEE-BAND-VALUES.
           05  WS-COPY-FEE-BAND-ROW OCCURS 6 TIMES.
               10  WS-COPY-FEE-BAND-LOW    PIC 9(09)V99.
               10  WS-COPY-FEE-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COPY-FEE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COPY-FEE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000500000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000645000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000372500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000650000000871000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000900000000488500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003400000000136000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003900000000335000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013900000000134000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023900000000040500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023950000000904500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024950000000389000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027450000000052500'.
       01  WS-COPY-FEE-GRID-TBL REDEFINES WS-COPY-FEE-GRID-VALUES.
           05  WS-COPY-FEE-GRID-ROW OCCURS 12 TIMES.
               10  WS-COPY-FEE-GRID-LOW    PIC 9(09)V99.
               10  WS-COPY-FEE-GRID-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY MFSTATP.

       PROCEDURE DIVISION USING MFSTAT-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCUM-BRANCH
           PERFORM 2010-ACCUM-STATUS
           PERFORM 2020-AGE-NEXT-DUE
           PERFORM 2030-PRORATE-POSTAGE
           PERFORM 2040-ASSESS-NOTICE-SENT
           PERFORM 2050-AGE-LAST-PAY
           PERFORM 2060-SET-CHANNEL
           PERFORM 2070-ACCRUE-POSTAGE
           PERFORM 2080-SPLIT-STORAGE
           PERFORM 2090-DESCRIBE-CHANNEL
           PERFORM 2100-FLAG-ITEM
           PERFORM 2110-DESCRIBE-RETENTION
           PERFORM 2120-ASSESS-EXEMPT
           PERFORM 2130-SPLIT-COPY-FEE
           PERFORM 2140-SPLIT-COPY-FEE
           PERFORM 2150-PRORATE-STORAGE
           PERFORM 2160-LIMIT-STORAGE
           PERFORM 2170-ACCRUE-COPY-FEE
           PERFORM 2180-COUNT-CYCLE
           PERFORM 2190-PRORATE-STORAGE
           PERFORM 2200-CHECK-OPEN
           PERFORM 2210-ACCUM-RETENTION
           PERFORM 2220-PRORATE-RESEARCH
           PERFORM 2230-DESCRIBE-RETENTION
           PERFORM 2240-CHECK-NEXT-DUE
           PERFORM 2250-AGE-LAST-PAY
           PERFORM 2260-DESCRIBE-BRANCH
           PERFORM 2270-AGE-NEXT-DUE
           PERFORM 2280-SELECT-DISCOUNT
           PERFORM 2290-SET-CHANNEL
           PERFORM 2300-COUNT-CYCLE
           PERFORM 2310-CHECK-NEXT-DUE
           PERFORM 2320-SPLIT-POSTAGE
           PERFORM 2330-PRORATE-POSTAGE
           PERFORM 2340-LIMIT-RESEARCH
           PERFORM 2350-CHECK-CLOSE
           PERFORM 2360-SET-CHANNEL
           PERFORM 2370-DESCRIBE-STATUS
           PERFORM 2380-ACCUM-CHANNEL
           PERFORM 2390-ACCRUE-POSTAGE
           PERFORM 2400-LIMIT-RESEARCH
           PERFORM 2410-ASSESS-EXEMPT
           PERFORM 2420-AGE-OPEN
           PERFORM 2430-SPLIT-POSTAGE
           PERFORM 2440-COUNT-CYCLE
           PERFORM 2450-AGE-NEXT-DUE
           PERFORM 2460-SET-BRANCH
           PERFORM 2470-ASSESS-EXEMPT
           PERFORM 2480-SELECT-MARGIN
           PERFORM 2490-SELECT-MARGIN
           PERFORM 2500-PRORATE-COPY-FEE
           PERFORM 2510-SELECT-MARGIN
           PERFORM 2520-SELECT-DISCOUNT
           PERFORM 2530-FLAG-ITEM
           PERFORM 2540-SPLIT-STORAGE
           PERFORM 2550-DESCRIBE-STATUS
           PERFORM 2560-ASSESS-AUTO-RENEW
           PERFORM 2570-DESCRIBE-CHANNEL
           PERFORM 2580-ASSESS-AUTO-RENEW
           PERFORM 2590-COUNT-CYCLE
           PERFORM 2600-ACCRUE-RESEARCH
           PERFORM 2610-AGE-NEXT-DUE
           PERFORM 2620-SELECT-MARGIN
           PERFORM 2630-FLAG-COPY-FEE
           PERFORM 2640-COUNT-CYCLE
           PERFORM 2650-ACCUM-STATUS
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO MFSTAT-RETURN-CODE.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2000-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFSTAT-BRANCH-CD
                   ADD MFSTAT-STORAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFSTAT-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFSTAT-STORAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESEARCH YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2010-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFSTAT-STATUS-CD
                   ADD MFSTAT-RESEARCH-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFSTAT-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFSTAT-RESEARCH-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2020-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF MFSTAT-NEXT-DUE-DATE > 0 AND MFSTAT-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFSTAT-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFSTAT-POSTAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MFSTAT-POSTAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MFSTAT-POSTAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFSTAT-POSTAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFSTAT-POSTAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO POSTAGE YTD.
      *----------------------------------------------------------------*
       2030-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFSTAT-POSTAGE-YTD ROUNDED =
                   MFSTAT-COPY-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFSTAT-POSTAGE-YTD
           END-IF
           ADD MFSTAT-POSTAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE POSTAGE YTD FEE ON THE POSTAGE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2040-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN MFSTAT-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO MFSTAT-POSTAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFSTAT-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MFSTAT-POSTAGE-YTD ROUNDED =
                       MFSTAT-POSTAGE-AMT * MFSTAT-MARGIN / 200
               WHEN MFSTAT-STATUS-CD = 'AST'
                   MOVE 20.00 TO MFSTAT-POSTAGE-YTD
               WHEN MFSTAT-STATUS-CD = 'JXN'
                   MOVE 20.00 TO MFSTAT-POSTAGE-YTD
               WHEN OTHER
                   COMPUTE MFSTAT-POSTAGE-YTD ROUNDED =
                       MFSTAT-POSTAGE-AMT * MFSTAT-MARGIN / 100
           END-EVALUATE
           IF MFSTAT-POSTAGE-YTD > 0 AND MFSTAT-POSTAGE-YTD < 1.00
               MOVE 1.00 TO MFSTAT-POSTAGE-YTD
           END-IF
           IF MFSTAT-POSTAGE-YTD > 150.00
               MOVE 150.00 TO MFSTAT-POSTAGE-YTD
           END-IF
           ADD MFSTAT-POSTAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2050-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MFSTAT-LAST-PAY-DATE > 0 AND MFSTAT-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFSTAT-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFSTAT-RESEARCH-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MFSTAT-RESEARCH-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MFSTAT-RESEARCH-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MFSTAT-RESEARCH-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFSTAT-RESEARCH-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN LAST PAY DATE IS PAST
      * GL -> HH WHEN LAST PAY DATE IS PAST
      * HH -> LM WHEN LAST PAY DATE IS PAST
      * LM -> MD WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2060-SET-CHANNEL.
           EVALUATE MFSTAT-CHANNEL-CD
               WHEN 'FV'
                   IF MFSTAT-LAST-PAY-DATE > 0 AND MFSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFSTAT-LAST-PAY-DATE > 0 AND MFSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFSTAT-LAST-PAY-DATE > 0 AND MFSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFSTAT-LAST-PAY-DATE > 0 AND MFSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON POSTAGE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2070-ACCRUE-POSTAGE.
           IF MFSTAT-POSTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFSTAT-POSTAGE-AMT * MFSTAT-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MFSTAT-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STORAGE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2080-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = MFSTAT-STORAGE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MFSTAT-STORAGE-YTD - (WS-WORK-AMT *
               2)
           COMPUTE MFSTAT-RESEARCH-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2090-DESCRIBE-CHANNEL.
           EVALUATE MFSTAT-CHANNEL-CD
               WHEN 'FV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ND'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2100-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE RETENTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2110-DESCRIBE-RETENTION.
           EVALUATE MFSTAT-RETENTION-CD
               WHEN 'KT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'SJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE RESEARCH YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2120-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MFSTAT-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFSTAT-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = MFSTAT-RESEARCH-YTD *
                       MFSTAT-MARGIN / 200
               WHEN MFSTAT-CHANNEL-CD = 'FV'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN MFSTAT-CHANNEL-CD = 'GL'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = MFSTAT-RESEARCH-YTD *
                       MFSTAT-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2130-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE MFSTAT-COPY-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2140-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE MFSTAT-COPY-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STORAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2150-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MFSTAT-STORAGE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2160-LIMIT-STORAGE.
           IF MFSTAT-STORAGE-AMT < 100.00
               MOVE 100.00 TO MFSTAT-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFSTAT-STORAGE-AMT > 5000.00
               MOVE 5000.00 TO MFSTAT-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COPY FEE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2170-ACCRUE-COPY-FEE.
           IF MFSTAT-COPY-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFSTAT-COPY-FEE-YTD * MFSTAT-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2180-COUNT-CYCLE.
           IF MFSTAT-CYCLE-CNT < 9999999
               ADD 1 TO MFSTAT-CYCLE-CNT
           END-IF
           IF MFSTAT-CYCLE-CNT NOT < 6
               MOVE 'Y' TO MFSTAT-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       2190-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFSTAT-STORAGE-AMT ROUNDED =
                   MFSTAT-COPY-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFSTAT-STORAGE-AMT
           END-IF
           ADD MFSTAT-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2200-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MFSTAT-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFSTAT-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO MFSTAT-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESEARCH YTD BY RETENTION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2210-ACCUM-RETENTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFSTAT-RETENTION-CD
                   ADD MFSTAT-RESEARCH-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFSTAT-RETENTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFSTAT-RESEARCH-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STORAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESEARCH YTD.
      *----------------------------------------------------------------*
       2220-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFSTAT-RESEARCH-YTD ROUNDED =
                   MFSTAT-STORAGE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFSTAT-RESEARCH-YTD
           END-IF
           ADD MFSTAT-RESEARCH-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE RETENTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2230-DESCRIBE-RETENTION.
           EVALUATE MFSTAT-RETENTION-CD
               WHEN 'KT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2240-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF MFSTAT-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFSTAT-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO MFSTAT-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2250-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MFSTAT-LAST-PAY-DATE > 0 AND MFSTAT-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFSTAT-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFSTAT-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFSTAT-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MFSTAT-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MFSTAT-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFSTAT-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2260-DESCRIBE-BRANCH.
           EVALUATE MFSTAT-BRANCH-CD
               WHEN 'AT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'US'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2270-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF MFSTAT-NEXT-DUE-DATE > 0 AND MFSTAT-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFSTAT-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFSTAT-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD MFSTAT-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MFSTAT-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFSTAT-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFSTAT-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE BAND TIER FOR THE POSTAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2280-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-BAND-RATE(1) TO MFSTAT-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF MFSTAT-POSTAGE-AMT NOT < WS-COPY-FEE-BAND-LOW(WS-TX)
                   MOVE WS-COPY-FEE-BAND-RATE(WS-TX) TO MFSTAT-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFSTAT-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFSTAT-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN NEXT DUE DATE IS PAST
      * GL -> HH WHEN NEXT DUE DATE IS PAST
      * HH -> LM WHEN NEXT DUE DATE IS PAST
      * LM -> MD WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2290-SET-CHANNEL.
           EVALUATE MFSTAT-CHANNEL-CD
               WHEN 'FV'
                   IF MFSTAT-NEXT-DUE-DATE > 0 AND MFSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFSTAT-NEXT-DUE-DATE > 0 AND MFSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFSTAT-NEXT-DUE-DATE > 0 AND MFSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFSTAT-NEXT-DUE-DATE > 0 AND MFSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2300-COUNT-CYCLE.
           IF MFSTAT-CYCLE-CNT < 9999999
               ADD 1 TO MFSTAT-CYCLE-CNT
           END-IF
           IF MFSTAT-CYCLE-CNT NOT < 10
               MOVE 'Y' TO MFSTAT-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2310-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF MFSTAT-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFSTAT-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO MFSTAT-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT POSTAGE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2320-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = MFSTAT-POSTAGE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = MFSTAT-POSTAGE-YTD - (WS-WORK-AMT *
               3)
           COMPUTE MFSTAT-STORAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE POSTAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO POSTAGE AMT.
      *----------------------------------------------------------------*
       2330-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFSTAT-POSTAGE-AMT ROUNDED =
                   MFSTAT-POSTAGE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFSTAT-POSTAGE-AMT
           END-IF
           ADD MFSTAT-POSTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RESEARCH YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2340-LIMIT-RESEARCH.
           IF MFSTAT-RESEARCH-YTD < 0.00
               MOVE 0.00 TO MFSTAT-RESEARCH-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFSTAT-RESEARCH-YTD > 500.00
               MOVE 500.00 TO MFSTAT-RESEARCH-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2350-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MFSTAT-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFSTAT-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO MFSTAT-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN LAST PAY DATE IS PAST
      * GL -> HH WHEN LAST PAY DATE IS PAST
      * HH -> LM WHEN LAST PAY DATE IS PAST
      * LM -> MD WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2360-SET-CHANNEL.
           EVALUATE MFSTAT-CHANNEL-CD
               WHEN 'FV'
                   IF MFSTAT-LAST-PAY-DATE > 0 AND MFSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFSTAT-LAST-PAY-DATE > 0 AND MFSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFSTAT-LAST-PAY-DATE > 0 AND MFSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFSTAT-LAST-PAY-DATE > 0 AND MFSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFSTAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2370-DESCRIBE-STATUS.
           EVALUATE MFSTAT-STATUS-CD
               WHEN 'AST'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JXN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KJJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TZL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XZM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2380-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFSTAT-CHANNEL-CD
                   ADD MFSTAT-STORAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFSTAT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFSTAT-STORAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON POSTAGE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2390-ACCRUE-POSTAGE.
           IF MFSTAT-POSTAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFSTAT-POSTAGE-YTD * MFSTAT-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO MFSTAT-STORAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2400-LIMIT-RESEARCH.
           IF MFSTAT-RESEARCH-YTD < 25.00
               MOVE 25.00 TO MFSTAT-RESEARCH-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFSTAT-RESEARCH-YTD > 5000.00
               MOVE 5000.00 TO MFSTAT-RESEARCH-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE COPY FEE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2410-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MFSTAT-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MFSTAT-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFSTAT-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE MFSTAT-STORAGE-AMT ROUNDED =
                       MFSTAT-COPY-FEE-YTD * MFSTAT-MARGIN / 200
               WHEN MFSTAT-RETENTION-CD = 'KT'
                   MOVE 5.00 TO MFSTAT-STORAGE-AMT
               WHEN MFSTAT-RETENTION-CD = 'SJ'
                   MOVE 12.00 TO MFSTAT-STORAGE-AMT
               WHEN OTHER
                   COMPUTE MFSTAT-STORAGE-AMT ROUNDED =
                       MFSTAT-COPY-FEE-YTD * MFSTAT-MARGIN / 100
           END-EVALUATE
           IF MFSTAT-STORAGE-AMT > 0 AND MFSTAT-STORAGE-AMT < 10.00
               MOVE 10.00 TO MFSTAT-STORAGE-AMT
           END-IF
           IF MFSTAT-STORAGE-AMT > 35.00
               MOVE 35.00 TO MFSTAT-STORAGE-AMT
           END-IF
           ADD MFSTAT-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2420-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MFSTAT-OPEN-DATE > 0 AND MFSTAT-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFSTAT-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFSTAT-POSTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFSTAT-POSTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MFSTAT-POSTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFSTAT-POSTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFSTAT-POSTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT POSTAGE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2430-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = MFSTAT-POSTAGE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = MFSTAT-POSTAGE-AMT - (WS-WORK-AMT *
               3)
           COMPUTE MFSTAT-STORAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2440-COUNT-CYCLE.
           IF MFSTAT-CYCLE-CNT < 9999999
               ADD 1 TO MFSTAT-CYCLE-CNT
           END-IF
           IF MFSTAT-CYCLE-CNT NOT < 12
               MOVE 'Y' TO MFSTAT-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2450-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF MFSTAT-NEXT-DUE-DATE > 0 AND MFSTAT-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFSTAT-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFSTAT-RESEARCH-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFSTAT-RESEARCH-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MFSTAT-RESEARCH-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFSTAT-RESEARCH-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFSTAT-RESEARCH-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * AT -> KR WHEN NEXT DUE DATE IS PAST
      * KR -> LK WHEN NEXT DUE DATE IS PAST
      * LK -> US WHEN NEXT DUE DATE IS PAST
      * US -> AT WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2460-SET-BRANCH.
           EVALUATE MFSTAT-BRANCH-CD
               WHEN 'AT'
                   IF MFSTAT-NEXT-DUE-DATE > 0 AND MFSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KR' TO MFSTAT-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF MFSTAT-NEXT-DUE-DATE > 0 AND MFSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LK' TO MFSTAT-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LK'
                   IF MFSTAT-NEXT-DUE-DATE > 0 AND MFSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'US' TO MFSTAT-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF MFSTAT-NEXT-DUE-DATE > 0 AND MFSTAT-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AT' TO MFSTAT-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE POSTAGE FEE ON THE POSTAGE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2470-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MFSTAT-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MFSTAT-POSTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFSTAT-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE MFSTAT-POSTAGE-AMT ROUNDED =
                       MFSTAT-POSTAGE-YTD * MFSTAT-MARGIN / 200
               WHEN MFSTAT-RETENTION-CD = 'KT'
                   MOVE 10.00 TO MFSTAT-POSTAGE-AMT
               WHEN MFSTAT-RETENTION-CD = 'SJ'
                   MOVE 15.00 TO MFSTAT-POSTAGE-AMT
               WHEN OTHER
                   COMPUTE MFSTAT-POSTAGE-AMT ROUNDED =
                       MFSTAT-POSTAGE-YTD * MFSTAT-MARGIN / 100
           END-EVALUATE
           IF MFSTAT-POSTAGE-AMT > 0 AND MFSTAT-POSTAGE-AMT < 5.00
               MOVE 5.00 TO MFSTAT-POSTAGE-AMT
           END-IF
           IF MFSTAT-POSTAGE-AMT > 50.00
               MOVE 50.00 TO MFSTAT-POSTAGE-AMT
           END-IF
           ADD MFSTAT-POSTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE GRID TIER FOR THE COPY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2480-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-GRID-RATE(1) TO MFSTAT-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF MFSTAT-COPY-FEE-YTD NOT < WS-COPY-FEE-GRID-LOW(WS-TX)
                   MOVE WS-COPY-FEE-GRID-RATE(WS-TX) TO MFSTAT-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFSTAT-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFSTAT-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE GRID TIER FOR THE RESEARCH YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2490-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-GRID-RATE(1) TO MFSTAT-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF MFSTAT-RESEARCH-YTD NOT < WS-COPY-FEE-GRID-LOW(WS-TX)
                   MOVE WS-COPY-FEE-GRID-RATE(WS-TX) TO MFSTAT-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFSTAT-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFSTAT-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STORAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COPY FEE YTD.
      *----------------------------------------------------------------*
       2500-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFSTAT-COPY-FEE-YTD ROUNDED =
                   MFSTAT-STORAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFSTAT-COPY-FEE-YTD
           END-IF
           ADD MFSTAT-COPY-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE BAND TIER FOR THE RESEARCH YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2510-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-BAND-RATE(1) TO MFSTAT-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF MFSTAT-RESEARCH-YTD NOT < WS-COPY-FEE-BAND-LOW(WS-TX)
                   MOVE WS-COPY-FEE-BAND-RATE(WS-TX) TO MFSTAT-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFSTAT-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFSTAT-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE GRID TIER FOR THE POSTAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2520-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-GRID-RATE(1) TO MFSTAT-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF MFSTAT-POSTAGE-YTD NOT < WS-COPY-FEE-GRID-LOW(WS-TX)
                   MOVE WS-COPY-FEE-GRID-RATE(WS-TX) TO MFSTAT-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFSTAT-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFSTAT-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2530-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STORAGE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2540-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = MFSTAT-STORAGE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = MFSTAT-STORAGE-YTD - (WS-WORK-AMT *
               4)
           COMPUTE MFSTAT-COPY-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2550-DESCRIBE-STATUS.
           EVALUATE MFSTAT-STATUS-CD
               WHEN 'AST'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JXN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KJJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TZL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XZM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE COPY FEE YTD FEE ON THE POSTAGE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2560-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MFSTAT-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO MFSTAT-COPY-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFSTAT-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE MFSTAT-COPY-FEE-YTD ROUNDED =
                       MFSTAT-POSTAGE-YTD * MFSTAT-DISCOUNT / 200
               WHEN MFSTAT-BRANCH-CD = 'AT'
                   MOVE 5.00 TO MFSTAT-COPY-FEE-YTD
               WHEN MFSTAT-BRANCH-CD = 'KR'
                   MOVE 20.00 TO MFSTAT-COPY-FEE-YTD
               WHEN OTHER
                   COMPUTE MFSTAT-COPY-FEE-YTD ROUNDED =
                       MFSTAT-POSTAGE-YTD * MFSTAT-DISCOUNT / 100
           END-EVALUATE
           IF MFSTAT-COPY-FEE-YTD > 0 AND MFSTAT-COPY-FEE-YTD < 5.00
               MOVE 5.00 TO MFSTAT-COPY-FEE-YTD
           END-IF
           IF MFSTAT-COPY-FEE-YTD > 100.00
               MOVE 100.00 TO MFSTAT-COPY-FEE-YTD
           END-IF
           ADD MFSTAT-COPY-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2570-DESCRIBE-CHANNEL.
           EVALUATE MFSTAT-CHANNEL-CD
               WHEN 'FV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ND'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE POSTAGE FEE ON THE STORAGE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2580-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MFSTAT-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO MFSTAT-POSTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFSTAT-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE MFSTAT-POSTAGE-AMT ROUNDED =
                       MFSTAT-STORAGE-AMT * MFSTAT-MARGIN / 200
               WHEN MFSTAT-RETENTION-CD = 'KT'
                   MOVE 5.00 TO MFSTAT-POSTAGE-AMT
               WHEN MFSTAT-RETENTION-CD = 'SJ'
                   MOVE 12.00 TO MFSTAT-POSTAGE-AMT
               WHEN OTHER
                   COMPUTE MFSTAT-POSTAGE-AMT ROUNDED =
                       MFSTAT-STORAGE-AMT * MFSTAT-MARGIN / 100
           END-EVALUATE
           IF MFSTAT-POSTAGE-AMT > 0 AND MFSTAT-POSTAGE-AMT < 5.00
               MOVE 5.00 TO MFSTAT-POSTAGE-AMT
           END-IF
           IF MFSTAT-POSTAGE-AMT > 35.00
               MOVE 35.00 TO MFSTAT-POSTAGE-AMT
           END-IF
           ADD MFSTAT-POSTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2590-COUNT-CYCLE.
           IF MFSTAT-CYCLE-CNT < 9999999
               ADD 1 TO MFSTAT-CYCLE-CNT
           END-IF
           IF MFSTAT-CYCLE-CNT NOT < 5
               MOVE 'Y' TO MFSTAT-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2600-ACCRUE-RESEARCH.
           IF MFSTAT-RESEARCH-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFSTAT-RESEARCH-YTD * MFSTAT-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO MFSTAT-POSTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2610-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF MFSTAT-NEXT-DUE-DATE > 0 AND MFSTAT-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFSTAT-NEXT-DUE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE BAND TIER FOR THE COPY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2620-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-BAND-RATE(1) TO MFSTAT-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF MFSTAT-COPY-FEE-YTD NOT < WS-COPY-FEE-BAND-LOW(WS-TX)
                   MOVE WS-COPY-FEE-BAND-RATE(WS-TX) TO MFSTAT-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFSTAT-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFSTAT-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2630-FLAG-COPY-FEE.
           IF MFSTAT-COPY-FEE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MFSTAT-COPY-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2640-COUNT-CYCLE.
           IF MFSTAT-CYCLE-CNT < 9999999
               ADD 1 TO MFSTAT-CYCLE-CNT
           END-IF
           IF MFSTAT-CYCLE-CNT NOT < 6
               MOVE 'Y' TO MFSTAT-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COPY FEE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2650-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFSTAT-STATUS-CD
                   ADD MFSTAT-COPY-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFSTAT-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFSTAT-COPY-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO MFSTAT-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO MFSTAT-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO MFSTAT-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'MFSTAT EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
