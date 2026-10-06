      *================================================================*
      * PROGRAM  : MFRISK                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : MICROFICHE STATEMENTS RULE - VALIDATION. CALLED BY  *
      *            THE MICROFICHE STATEMENTS BATCH AND ONLINE PROGRAMS.*
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-11-06 GAF  ORIGINAL                                       *
      * 1996-07-09 MKL  PERFORMANCE - POSTAGE READ AHEAD INC7689       *
      * 1998-09-23 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-01-08 GAF  RESEARCH TABLE EXPANDED PRB5765                *
      * 1999-08-17 MKL  NEW POSTAGE REPORT COLUMNS INC5331             *
      * 2000-07-11 JLM  POSTAGE TABLE EXPANDED CHG0525                 *
      * 2000-10-07 EWS  COPY-FEE NOTICE TEXT PER LEGAL REQ5702         *
      * 2001-06-18 GAF  POSTAGE ROUNDING - HALF UP CHG8639             *
      * 2004-09-27 SKN  PERFORMANCE - COPY-FEE READ AHEAD INC7282      *
      * 2006-08-25 MKL  STORAGE STATUS CODES ADDED INC8173             *
      * 2007-08-22 JLM  NEW STORAGE REPORT COLUMNS PRB6134             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MFRISK.
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
                                           VALUE 16.000000.
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
      * WS-STORAGE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STORAGE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000825000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000421000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000886000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002650000000032000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002900000000092000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003150000000095500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003250000000117500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003750000000702500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004250000000321000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009250000000097000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009300000000584000'.
       01  WS-STORAGE-SCHED-TBL REDEFINES WS-STORAGE-SCHED-VALUES.
           05  WS-STORAGE-SCHED-ROW OCCURS 11 TIMES.
               10  WS-STORAGE-SCHED-LOW    PIC 9(09)V99.
               10  WS-STORAGE-SCHED-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY MFRISKP.

       PROCEDURE DIVISION USING MFRISK-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-CHECK-OPEN
           PERFORM 2010-ACCUM-BRANCH
           PERFORM 2020-COUNT-CYCLE
           PERFORM 2030-PRORATE-COPY-FEE
           PERFORM 2040-DESCRIBE-BRANCH
           PERFORM 2050-EDIT-NAME
           PERFORM 2060-EDIT-ADDR
           PERFORM 2070-SPLIT-STORAGE
           PERFORM 2080-PRORATE-COPY-FEE
           PERFORM 2090-EDIT-NAME
           PERFORM 2100-DESCRIBE-CHANNEL
           PERFORM 2110-ACCUM-STATUS
           PERFORM 2120-PRORATE-RESEARCH
           PERFORM 2130-EDIT-ADDR
           PERFORM 2140-AGE-OPEN
           PERFORM 2150-FLAG-STORAGE
           PERFORM 2160-COUNT-CYCLE
           PERFORM 2170-SET-RETENTION
           PERFORM 2180-CHECK-NOTICE
           PERFORM 2190-AGE-LAST-PAY
           PERFORM 2200-ACCUM-RETENTION
           PERFORM 2210-SELECT-FEE-PCT
           PERFORM 2220-SELECT-MARGIN
           PERFORM 2230-EDIT-NAME
           PERFORM 2240-COUNT-NOTICE
           PERFORM 2250-LIMIT-RESEARCH
           PERFORM 2260-ASSESS-AUTO-RENEW
           PERFORM 2270-ACCRUE-STORAGE
           PERFORM 2280-LIMIT-RESEARCH
           PERFORM 2290-SELECT-FEE-PCT
           PERFORM 2300-EDIT-ADDR
           PERFORM 2310-SPLIT-RESEARCH
           PERFORM 2320-PRORATE-RESEARCH
           PERFORM 2330-SELECT-MARGIN
           PERFORM 2340-COUNT-NOTICE
           PERFORM 2350-SET-RETENTION
           PERFORM 2360-DESCRIBE-RETENTION
           PERFORM 2370-AGE-LAST-MAINT
           PERFORM 2380-DESCRIBE-CHANNEL
           PERFORM 2390-AGE-OPEN
           PERFORM 2400-PRORATE-RESEARCH
           PERFORM 2410-PRORATE-RESEARCH
           PERFORM 2420-ACCRUE-STORAGE
           PERFORM 2430-SPLIT-COPY-FEE
           PERFORM 2440-LIMIT-RESEARCH
           PERFORM 2450-EDIT-NAME
           PERFORM 2460-PRORATE-RESEARCH
           PERFORM 2470-ACCRUE-RESEARCH
           PERFORM 2480-ACCUM-CHANNEL
           PERFORM 2490-DESCRIBE-RETENTION
           PERFORM 2500-ASSESS-AUTO-RENEW
           PERFORM 2510-CHECK-LAST-PAY
           PERFORM 2520-SELECT-FEE-PCT
           PERFORM 2530-EDIT-ADDR
           PERFORM 2540-SPLIT-RESEARCH
           PERFORM 2550-SET-STATUS
           PERFORM 2560-SET-STATUS
           PERFORM 2570-LIMIT-COPY-FEE
           PERFORM 2580-LIMIT-STORAGE
           PERFORM 2590-FLAG-RESEARCH
           PERFORM 2600-EDIT-NAME
           PERFORM 2610-SET-BRANCH
           PERFORM 2620-SPLIT-COPY-FEE
           PERFORM 2630-AGE-OPEN
           PERFORM 2640-LIMIT-COPY-FEE
           PERFORM 2650-SPLIT-RESEARCH
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO MFRISK-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2000-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MFRISK-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFRISK-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO MFRISK-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2010-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFRISK-BRANCH-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFRISK-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2020-COUNT-CYCLE.
           IF MFRISK-CYCLE-CNT < 9999999
               ADD 1 TO MFRISK-CYCLE-CNT
           END-IF
           IF MFRISK-CYCLE-CNT NOT < 5
               MOVE 'Y' TO MFRISK-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COPY FEE AMT.
      *----------------------------------------------------------------*
       2030-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFRISK-COPY-FEE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFRISK-COPY-FEE-AMT
           END-IF
           ADD MFRISK-COPY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2040-DESCRIBE-BRANCH.
           EVALUATE MFRISK-BRANCH-CD
               WHEN 'AT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'US'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E187 NAME REQUIRED
      * E944 WORK AMT NEGATIVE
      * E407 WORK AMT OVER 250000
      * E057 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2050-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFRISK-NAME = SPACES
                   MOVE 'E187' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E944' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E407' TO WS-REJECT-CODE
               WHEN MFRISK-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E057' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E026 ADDR REQUIRED
      * E788 COPY FEE YTD NEGATIVE
      * E645 COPY FEE YTD OVER 10000
      * E550 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2060-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFRISK-ADDR = SPACES
                   MOVE 'E026' TO WS-REJECT-CODE
               WHEN MFRISK-COPY-FEE-YTD < 0
                   MOVE 'E788' TO WS-REJECT-CODE
               WHEN MFRISK-COPY-FEE-YTD > 10000.00
                   MOVE 'E645' TO WS-REJECT-CODE
               WHEN MFRISK-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E550' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STORAGE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2070-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = MFRISK-STORAGE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MFRISK-STORAGE-YTD - (WS-WORK-AMT *
               2)
           COMPUTE MFRISK-COPY-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2080-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFRISK-COPY-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E067 NAME REQUIRED
      * E813 STORAGE AMT NEGATIVE
      * E436 STORAGE AMT OVER 5000
      * E203 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2090-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFRISK-NAME = SPACES
                   MOVE 'E067' TO WS-REJECT-CODE
               WHEN MFRISK-STORAGE-AMT < 0
                   MOVE 'E813' TO WS-REJECT-CODE
               WHEN MFRISK-STORAGE-AMT > 5000.00
                   MOVE 'E436' TO WS-REJECT-CODE
               WHEN MFRISK-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E203' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2100-DESCRIBE-CHANNEL.
           EVALUATE MFRISK-CHANNEL-CD
               WHEN 'FV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ND'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2110-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFRISK-STATUS-CD
                   ADD MFRISK-STORAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFRISK-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFRISK-STORAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2120-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MFRISK-RESEARCH-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E633 ADDR REQUIRED
      * E275 STORAGE AMT NEGATIVE
      * E439 STORAGE AMT OVER 25000
      * E350 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2130-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFRISK-ADDR = SPACES
                   MOVE 'E633' TO WS-REJECT-CODE
               WHEN MFRISK-STORAGE-AMT < 0
                   MOVE 'E275' TO WS-REJECT-CODE
               WHEN MFRISK-STORAGE-AMT > 25000.00
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN MFRISK-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E350' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2140-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MFRISK-OPEN-DATE > 0 AND MFRISK-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFRISK-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFRISK-RESEARCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MFRISK-RESEARCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MFRISK-RESEARCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD MFRISK-RESEARCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFRISK-RESEARCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STORAGE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2150-FLAG-STORAGE.
           IF MFRISK-STORAGE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MFRISK-STORAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2160-COUNT-CYCLE.
           IF MFRISK-CYCLE-CNT < 9999999
               ADD 1 TO MFRISK-CYCLE-CNT
           END-IF
           IF MFRISK-CYCLE-CNT NOT < 6
               MOVE 'Y' TO MFRISK-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR RETENTION CD:
      * KT -> SJ WHEN OPEN DATE IS PAST
      * SJ -> WV WHEN OPEN DATE IS PAST
      * WV -> KT WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2170-SET-RETENTION.
           EVALUATE MFRISK-RETENTION-CD
               WHEN 'KT'
                   IF MFRISK-OPEN-DATE > 0 AND MFRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SJ' TO MFRISK-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SJ'
                   IF MFRISK-OPEN-DATE > 0 AND MFRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WV' TO MFRISK-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF MFRISK-OPEN-DATE > 0 AND MFRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KT' TO MFRISK-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2180-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF MFRISK-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFRISK-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO MFRISK-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2190-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MFRISK-LAST-PAY-DATE > 0 AND MFRISK-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFRISK-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE YTD BY RETENTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2200-ACCUM-RETENTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFRISK-RETENTION-CD
                   ADD MFRISK-STORAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFRISK-RETENTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFRISK-STORAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE SCHED TIER FOR THE COPY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2210-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO MFRISK-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF MFRISK-COPY-FEE-YTD NOT < WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO MFRISK-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFRISK-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFRISK-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE SCHED TIER FOR THE RESEARCH YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2220-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO MFRISK-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF MFRISK-RESEARCH-YTD NOT < WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO MFRISK-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFRISK-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFRISK-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E844 NAME REQUIRED
      * E869 COPY FEE AMT NEGATIVE
      * E874 COPY FEE AMT OVER 99999
      * E732 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2230-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFRISK-NAME = SPACES
                   MOVE 'E844' TO WS-REJECT-CODE
               WHEN MFRISK-COPY-FEE-AMT < 0
                   MOVE 'E869' TO WS-REJECT-CODE
               WHEN MFRISK-COPY-FEE-AMT > 99999.00
                   MOVE 'E874' TO WS-REJECT-CODE
               WHEN MFRISK-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E732' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2240-COUNT-NOTICE.
           IF MFRISK-NOTICE-CNT < 9999999
               ADD 1 TO MFRISK-NOTICE-CNT
           END-IF
           IF MFRISK-NOTICE-CNT NOT < 12
               MOVE 'Y' TO MFRISK-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2250-LIMIT-RESEARCH.
           IF MFRISK-RESEARCH-AMT < 50.00
               MOVE 50.00 TO MFRISK-RESEARCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFRISK-RESEARCH-AMT > 10000.00
               MOVE 10000.00 TO MFRISK-RESEARCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH FEE ON THE COPY FEE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2260-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MFRISK-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO MFRISK-RESEARCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFRISK-STOP-FLAG = 'Y'
                   COMPUTE MFRISK-RESEARCH-AMT ROUNDED =
                       MFRISK-COPY-FEE-AMT * MFRISK-FEE-PCT / 200
               WHEN MFRISK-STATUS-CD = 'AST'
                   MOVE 12.00 TO MFRISK-RESEARCH-AMT
               WHEN MFRISK-STATUS-CD = 'JXN'
                   MOVE 7.00 TO MFRISK-RESEARCH-AMT
               WHEN OTHER
                   COMPUTE MFRISK-RESEARCH-AMT ROUNDED =
                       MFRISK-COPY-FEE-AMT * MFRISK-FEE-PCT / 100
           END-EVALUATE
           IF MFRISK-RESEARCH-AMT > 0 AND MFRISK-RESEARCH-AMT < 10.00
               MOVE 10.00 TO MFRISK-RESEARCH-AMT
           END-IF
           IF MFRISK-RESEARCH-AMT > 25.00
               MOVE 25.00 TO MFRISK-RESEARCH-AMT
           END-IF
           ADD MFRISK-RESEARCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2270-ACCRUE-STORAGE.
           IF MFRISK-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFRISK-STORAGE-AMT * MFRISK-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2280-LIMIT-RESEARCH.
           IF MFRISK-RESEARCH-YTD < 25.00
               MOVE 25.00 TO MFRISK-RESEARCH-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFRISK-RESEARCH-YTD > 1000.00
               MOVE 1000.00 TO MFRISK-RESEARCH-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2290-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO MFRISK-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO MFRISK-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFRISK-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFRISK-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E317 ADDR REQUIRED
      * E821 RESEARCH YTD NEGATIVE
      * E515 RESEARCH YTD OVER 99999
      * E819 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2300-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFRISK-ADDR = SPACES
                   MOVE 'E317' TO WS-REJECT-CODE
               WHEN MFRISK-RESEARCH-YTD < 0
                   MOVE 'E821' TO WS-REJECT-CODE
               WHEN MFRISK-RESEARCH-YTD > 99999.00
                   MOVE 'E515' TO WS-REJECT-CODE
               WHEN MFRISK-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E819' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2310-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFRISK-RESEARCH-YTD / 4
           COMPUTE WS-WORK-AMT-2 = MFRISK-RESEARCH-YTD - (WS-WORK-AMT *
               4)
           COMPUTE MFRISK-COPY-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESEARCH YTD.
      *----------------------------------------------------------------*
       2320-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFRISK-RESEARCH-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFRISK-RESEARCH-YTD
           END-IF
           ADD MFRISK-RESEARCH-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE SCHED TIER FOR THE COPY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2330-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO MFRISK-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF MFRISK-COPY-FEE-YTD NOT < WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO MFRISK-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFRISK-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFRISK-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2340-COUNT-NOTICE.
           IF MFRISK-NOTICE-CNT < 9999999
               ADD 1 TO MFRISK-NOTICE-CNT
           END-IF
           IF MFRISK-NOTICE-CNT NOT < 6
               MOVE 'Y' TO MFRISK-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR RETENTION CD:
      * KT -> SJ WHEN LAST MAINT DATE IS PAST
      * SJ -> WV WHEN LAST MAINT DATE IS PAST
      * WV -> KT WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2350-SET-RETENTION.
           EVALUATE MFRISK-RETENTION-CD
               WHEN 'KT'
                   IF MFRISK-LAST-MAINT-DATE > 0 AND
                       MFRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'SJ' TO MFRISK-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SJ'
                   IF MFRISK-LAST-MAINT-DATE > 0 AND
                       MFRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'WV' TO MFRISK-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF MFRISK-LAST-MAINT-DATE > 0 AND
                       MFRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'KT' TO MFRISK-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE RETENTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2360-DESCRIBE-RETENTION.
           EVALUATE MFRISK-RETENTION-CD
               WHEN 'KT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2370-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF MFRISK-LAST-MAINT-DATE > 0 AND MFRISK-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFRISK-LAST-MAINT-DATE TO DTU-DATE-1
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
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2380-DESCRIBE-CHANNEL.
           EVALUATE MFRISK-CHANNEL-CD
               WHEN 'FV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ND'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2390-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MFRISK-OPEN-DATE > 0 AND MFRISK-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFRISK-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFRISK-COPY-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFRISK-COPY-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD MFRISK-COPY-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFRISK-COPY-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFRISK-COPY-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE STORAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESEARCH YTD.
      *----------------------------------------------------------------*
       2400-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFRISK-RESEARCH-YTD ROUNDED =
                   MFRISK-STORAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFRISK-RESEARCH-YTD
           END-IF
           ADD MFRISK-RESEARCH-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESEARCH AMT.
      *----------------------------------------------------------------*
       2410-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFRISK-RESEARCH-AMT ROUNDED =
                   MFRISK-COPY-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFRISK-RESEARCH-AMT
           END-IF
           ADD MFRISK-RESEARCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2420-ACCRUE-STORAGE.
           IF MFRISK-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFRISK-STORAGE-AMT * MFRISK-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COPY FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2430-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = MFRISK-COPY-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = MFRISK-COPY-FEE-AMT - (WS-WORK-AMT *
               3)
           COMPUTE MFRISK-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2440-LIMIT-RESEARCH.
           IF MFRISK-RESEARCH-YTD < 0.00
               MOVE 0.00 TO MFRISK-RESEARCH-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFRISK-RESEARCH-YTD > 5000.00
               MOVE 5000.00 TO MFRISK-RESEARCH-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E030 NAME REQUIRED
      * E755 RESEARCH AMT NEGATIVE
      * E945 RESEARCH AMT OVER 250000
      * E113 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2450-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFRISK-NAME = SPACES
                   MOVE 'E030' TO WS-REJECT-CODE
               WHEN MFRISK-RESEARCH-AMT < 0
                   MOVE 'E755' TO WS-REJECT-CODE
               WHEN MFRISK-RESEARCH-AMT > 250000.00
                   MOVE 'E945' TO WS-REJECT-CODE
               WHEN MFRISK-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E113' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESEARCH AMT.
      *----------------------------------------------------------------*
       2460-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFRISK-RESEARCH-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFRISK-RESEARCH-AMT
           END-IF
           ADD MFRISK-RESEARCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2470-ACCRUE-RESEARCH.
           IF MFRISK-RESEARCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFRISK-RESEARCH-AMT * MFRISK-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO MFRISK-RESEARCH-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2480-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFRISK-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFRISK-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE RETENTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2490-DESCRIBE-RETENTION.
           EVALUATE MFRISK-RETENTION-CD
               WHEN 'KT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE STORAGE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2500-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MFRISK-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFRISK-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = MFRISK-STORAGE-AMT *
                       MFRISK-MARGIN / 200
               WHEN MFRISK-BRANCH-CD = 'AT'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN MFRISK-BRANCH-CD = 'KR'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = MFRISK-STORAGE-AMT *
                       MFRISK-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2510-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MFRISK-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFRISK-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO MFRISK-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE SCHED TIER FOR THE RESEARCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2520-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO MFRISK-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF MFRISK-RESEARCH-AMT NOT < WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO MFRISK-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFRISK-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFRISK-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E027 ADDR REQUIRED
      * E985 STORAGE AMT NEGATIVE
      * E007 STORAGE AMT OVER 5000
      * E850 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2530-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFRISK-ADDR = SPACES
                   MOVE 'E027' TO WS-REJECT-CODE
               WHEN MFRISK-STORAGE-AMT < 0
                   MOVE 'E985' TO WS-REJECT-CODE
               WHEN MFRISK-STORAGE-AMT > 5000.00
                   MOVE 'E007' TO WS-REJECT-CODE
               WHEN MFRISK-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E850' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2540-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFRISK-RESEARCH-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MFRISK-RESEARCH-YTD - (WS-WORK-AMT *
               2)
           COMPUTE MFRISK-COPY-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AST -> JXN WHEN NOTICE DATE IS PAST
      * JXN -> KJJ WHEN NOTICE DATE IS PAST
      * KJJ -> TZL WHEN NOTICE DATE IS PAST
      * TZL -> XZM WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2550-SET-STATUS.
           EVALUATE MFRISK-STATUS-CD
               WHEN 'AST'
                   IF MFRISK-NOTICE-DATE > 0 AND MFRISK-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JXN' TO MFRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JXN'
                   IF MFRISK-NOTICE-DATE > 0 AND MFRISK-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KJJ' TO MFRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KJJ'
                   IF MFRISK-NOTICE-DATE > 0 AND MFRISK-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TZL' TO MFRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TZL'
                   IF MFRISK-NOTICE-DATE > 0 AND MFRISK-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XZM' TO MFRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AST -> JXN WHEN OPEN DATE IS PAST
      * JXN -> KJJ WHEN OPEN DATE IS PAST
      * KJJ -> TZL WHEN OPEN DATE IS PAST
      * TZL -> XZM WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2560-SET-STATUS.
           EVALUATE MFRISK-STATUS-CD
               WHEN 'AST'
                   IF MFRISK-OPEN-DATE > 0 AND MFRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JXN' TO MFRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JXN'
                   IF MFRISK-OPEN-DATE > 0 AND MFRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KJJ' TO MFRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KJJ'
                   IF MFRISK-OPEN-DATE > 0 AND MFRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TZL' TO MFRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TZL'
                   IF MFRISK-OPEN-DATE > 0 AND MFRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XZM' TO MFRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COPY FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2570-LIMIT-COPY-FEE.
           IF MFRISK-COPY-FEE-YTD < 25.00
               MOVE 25.00 TO MFRISK-COPY-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFRISK-COPY-FEE-YTD > 2500.00
               MOVE 2500.00 TO MFRISK-COPY-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2580-LIMIT-STORAGE.
           IF MFRISK-STORAGE-YTD < 50.00
               MOVE 50.00 TO MFRISK-STORAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFRISK-STORAGE-YTD > 500.00
               MOVE 500.00 TO MFRISK-STORAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2590-FLAG-RESEARCH.
           IF MFRISK-RESEARCH-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MFRISK-RESEARCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E827 NAME REQUIRED
      * E043 RESEARCH AMT NEGATIVE
      * E048 RESEARCH AMT OVER 99999
      * E273 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2600-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFRISK-NAME = SPACES
                   MOVE 'E827' TO WS-REJECT-CODE
               WHEN MFRISK-RESEARCH-AMT < 0
                   MOVE 'E043' TO WS-REJECT-CODE
               WHEN MFRISK-RESEARCH-AMT > 99999.00
                   MOVE 'E048' TO WS-REJECT-CODE
               WHEN MFRISK-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E273' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * AT -> KR WHEN OPEN DATE IS PAST
      * KR -> LK WHEN OPEN DATE IS PAST
      * LK -> US WHEN OPEN DATE IS PAST
      * US -> AT WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2610-SET-BRANCH.
           EVALUATE MFRISK-BRANCH-CD
               WHEN 'AT'
                   IF MFRISK-OPEN-DATE > 0 AND MFRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO MFRISK-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF MFRISK-OPEN-DATE > 0 AND MFRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LK' TO MFRISK-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LK'
                   IF MFRISK-OPEN-DATE > 0 AND MFRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'US' TO MFRISK-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF MFRISK-OPEN-DATE > 0 AND MFRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO MFRISK-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2620-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE MFRISK-COPY-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2630-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MFRISK-OPEN-DATE > 0 AND MFRISK-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFRISK-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFRISK-RESEARCH-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MFRISK-RESEARCH-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD MFRISK-RESEARCH-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFRISK-RESEARCH-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFRISK-RESEARCH-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COPY FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2640-LIMIT-COPY-FEE.
           IF MFRISK-COPY-FEE-AMT < 0.00
               MOVE 0.00 TO MFRISK-COPY-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFRISK-COPY-FEE-AMT > 2500.00
               MOVE 2500.00 TO MFRISK-COPY-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2650-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFRISK-RESEARCH-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MFRISK-RESEARCH-YTD - (WS-WORK-AMT *
               2)
           COMPUTE MFRISK-RESEARCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO MFRISK-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO MFRISK-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO MFRISK-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'MFRISK EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
