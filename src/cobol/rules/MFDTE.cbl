      *================================================================*
      * PROGRAM  : MFDTE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : MICROFICHE STATEMENTS RULE - PRICING. CALLED BY THE *
      *            MICROFICHE STATEMENTS BATCH AND ONLINE PROGRAMS.    *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-10-08 MKL  ORIGINAL                                       *
      * 2000-11-15 CRP  NEW POSTAGE REPORT COLUMNS REQ9271             *
      * 2003-11-23 TWH  STORAGE ROUNDING - HALF UP REQ3091             *
      * 2004-07-02 BHS  RESEARCH RERUN PROTECTION INC6878              *
      * 2006-12-15 LMB  PERFORMANCE - RESEARCH READ AHEAD CHG0681      *
      * 2008-04-11 GAF  RESEARCH LIMIT REVISED INC8052                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MFDTE.
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
                                           VALUE 20.000000.
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
      * WS-STORAGE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STORAGE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000682500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000158000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000894000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010750000000789000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013250000000472500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013750000000156000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013800000000667500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013900000000234000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014000000000360500'.
       01  WS-STORAGE-TIER-TBL REDEFINES WS-STORAGE-TIER-VALUES.
           05  WS-STORAGE-TIER-ROW OCCURS 9 TIMES.
               10  WS-STORAGE-TIER-LOW     PIC 9(09)V99.
               10  WS-STORAGE-TIER-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY MFDTEP.

       PROCEDURE DIVISION USING MFDTE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SET-FICHE-TYPE
           PERFORM 2010-SET-CHANNEL
           PERFORM 2020-EDIT-CUST-ID
           PERFORM 2030-CHECK-LAST-MAINT
           PERFORM 2040-SPLIT-RESEARCH
           PERFORM 2050-EDIT-ADDR
           PERFORM 2060-CHECK-EXPIRY
           PERFORM 2070-SPLIT-COPY-FEE
           PERFORM 2080-AGE-EXPIRY
           PERFORM 2090-SET-RETENTION
           PERFORM 2100-PRORATE-RESEARCH
           PERFORM 2110-DESCRIBE-CHANNEL
           PERFORM 2120-AGE-CLOSE
           PERFORM 2130-LIMIT-POSTAGE
           PERFORM 2140-DESCRIBE-CHANNEL
           PERFORM 2150-LIMIT-POSTAGE
           PERFORM 2160-COUNT-NOTICE
           PERFORM 2170-LIMIT-COPY-FEE
           PERFORM 2180-ASSESS-AUTO-RENEW
           PERFORM 2190-LIMIT-STORAGE
           PERFORM 2200-PRORATE-COPY-FEE
           PERFORM 2210-PRORATE-RESEARCH
           PERFORM 2220-SPLIT-RESEARCH
           PERFORM 2230-LIMIT-POSTAGE
           PERFORM 2240-ACCUM-RETENTION
           PERFORM 2250-SET-FICHE-TYPE
           PERFORM 2260-SET-FICHE-TYPE
           PERFORM 2270-LIMIT-RESEARCH
           PERFORM 2280-FLAG-COPY-FEE
           PERFORM 2290-SELECT-MARGIN
           PERFORM 2300-ACCRUE-RESEARCH
           PERFORM 2310-LIMIT-POSTAGE
           PERFORM 2320-CHECK-CLOSE
           PERFORM 2330-ACCRUE-STORAGE
           PERFORM 2340-DESCRIBE-RETENTION
           PERFORM 2350-DESCRIBE-FICHE-TYPE
           PERFORM 2360-COUNT-CYCLE
           PERFORM 2370-CHECK-CLOSE
           PERFORM 2380-EDIT-ADDR
           PERFORM 2390-DESCRIBE-CHANNEL
           PERFORM 2400-LIMIT-STORAGE
           PERFORM 2410-SPLIT-POSTAGE
           PERFORM 2420-AGE-LAST-MAINT
           PERFORM 2430-SELECT-MARGIN
           PERFORM 2440-SPLIT-POSTAGE
           PERFORM 2450-SET-TYPE
           PERFORM 2460-PRORATE-POSTAGE
           PERFORM 2470-SPLIT-POSTAGE
           PERFORM 2480-ACCUM-RETENTION
           PERFORM 2490-SELECT-DISCOUNT
           PERFORM 2500-DESCRIBE-TYPE
           PERFORM 2510-CHECK-CLOSE
           PERFORM 2520-ACCRUE-POSTAGE
           PERFORM 2530-PRORATE-RESEARCH
           PERFORM 2540-COUNT-NOTICE
           PERFORM 2550-PRORATE-POSTAGE
           PERFORM 2560-PRORATE-POSTAGE
           PERFORM 2570-ACCRUE-DISCOUNT
           PERFORM 2580-EDIT-ADDR
           PERFORM 2590-ACCRUE-POSTAGE
           PERFORM 2600-SPLIT-COPY-FEE
           PERFORM 2610-LIMIT-POSTAGE
           PERFORM 2620-COUNT-CYCLE
           PERFORM 2630-ACCUM-FICHE-TYPE
           PERFORM 2640-SELECT-MARGIN
           PERFORM 2650-SPLIT-COPY-FEE
           PERFORM 2660-DESCRIBE-CHANNEL
           PERFORM 2670-COUNT-CYCLE
           PERFORM 2680-COUNT-NOTICE
           PERFORM 2690-PRORATE-COPY-FEE
           PERFORM 2700-COUNT-NOTICE
           PERFORM 2710-SPLIT-COPY-FEE
           PERFORM 2720-ASSESS-AUTO-RENEW
           PERFORM 2730-SET-FICHE-TYPE
           PERFORM 2740-CHECK-LAST-PAY
           PERFORM 2750-CHECK-CLOSE
           PERFORM 2760-PRORATE-POSTAGE
           PERFORM 2770-SPLIT-POSTAGE
           PERFORM 2780-AGE-LAST-MAINT
           PERFORM 2790-DESCRIBE-TYPE
           PERFORM 2800-SPLIT-POSTAGE
           PERFORM 2810-AGE-LAST-MAINT
           PERFORM 2820-SELECT-MARGIN
           PERFORM 2830-CHECK-EXPIRY
           PERFORM 2840-SPLIT-RESEARCH
           PERFORM 2850-CHECK-EXPIRY
           PERFORM 2860-SET-CHANNEL
           PERFORM 2870-SET-CHANNEL
           PERFORM 2880-CHECK-LAST-MAINT
           PERFORM 2890-DESCRIBE-RETENTION
           PERFORM 2900-ACCRUE-RESEARCH
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO MFDTE-RETURN-CODE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR FICHE TYPE CD:
      * AW -> JM WHEN EXPIRY DATE IS PAST
      * JM -> KV WHEN EXPIRY DATE IS PAST
      * KV -> MD WHEN EXPIRY DATE IS PAST
      * MD -> NR WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2000-SET-FICHE-TYPE.
           EVALUATE MFDTE-FICHE-TYPE-CD
               WHEN 'AW'
                   IF MFDTE-EXPIRY-DATE > 0 AND MFDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO MFDTE-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF MFDTE-EXPIRY-DATE > 0 AND MFDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KV' TO MFDTE-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF MFDTE-EXPIRY-DATE > 0 AND MFDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFDTE-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MD'
                   IF MFDTE-EXPIRY-DATE > 0 AND MFDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NR' TO MFDTE-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN LAST MAINT DATE IS PAST
      * GL -> HH WHEN LAST MAINT DATE IS PAST
      * HH -> LM WHEN LAST MAINT DATE IS PAST
      * LM -> MD WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2010-SET-CHANNEL.
           EVALUATE MFDTE-CHANNEL-CD
               WHEN 'FV'
                   IF MFDTE-LAST-MAINT-DATE > 0 AND
                       MFDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFDTE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFDTE-LAST-MAINT-DATE > 0 AND
                       MFDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFDTE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFDTE-LAST-MAINT-DATE > 0 AND
                       MFDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFDTE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFDTE-LAST-MAINT-DATE > 0 AND
                       MFDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFDTE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E010 CUST ID REQUIRED
      * E985 WORK AMT 2 NEGATIVE
      * E746 WORK AMT 2 OVER 250000
      * E517 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2020-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFDTE-CUST-ID = SPACES
                   MOVE 'E010' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E985' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E746' TO WS-REJECT-CODE
               WHEN MFDTE-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E517' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2030-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF MFDTE-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFDTE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO MFDTE-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2040-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFDTE-RESEARCH-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MFDTE-RESEARCH-AMT - (WS-WORK-AMT *
               2)
           COMPUTE MFDTE-RESEARCH-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E037 ADDR REQUIRED
      * E732 STORAGE YTD NEGATIVE
      * E342 STORAGE YTD OVER 5000
      * E194 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2050-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFDTE-ADDR = SPACES
                   MOVE 'E037' TO WS-REJECT-CODE
               WHEN MFDTE-STORAGE-YTD < 0
                   MOVE 'E732' TO WS-REJECT-CODE
               WHEN MFDTE-STORAGE-YTD > 5000.00
                   MOVE 'E342' TO WS-REJECT-CODE
               WHEN MFDTE-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E194' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2060-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MFDTE-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFDTE-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO MFDTE-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COPY FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2070-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = MFDTE-COPY-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = MFDTE-COPY-FEE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE MFDTE-STORAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2080-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MFDTE-EXPIRY-DATE > 0 AND MFDTE-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFDTE-EXPIRY-DATE TO DTU-DATE-1
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
      * STATUS TRANSITIONS FOR RETENTION CD:
      * KT -> SJ WHEN LAST PAY DATE IS PAST
      * SJ -> WV WHEN LAST PAY DATE IS PAST
      * WV -> KT WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2090-SET-RETENTION.
           EVALUATE MFDTE-RETENTION-CD
               WHEN 'KT'
                   IF MFDTE-LAST-PAY-DATE > 0 AND MFDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SJ' TO MFDTE-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SJ'
                   IF MFDTE-LAST-PAY-DATE > 0 AND MFDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WV' TO MFDTE-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF MFDTE-LAST-PAY-DATE > 0 AND MFDTE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KT' TO MFDTE-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESEARCH YTD.
      *----------------------------------------------------------------*
       2100-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFDTE-RESEARCH-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFDTE-RESEARCH-YTD
           END-IF
           ADD MFDTE-RESEARCH-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2110-DESCRIBE-CHANNEL.
           EVALUATE MFDTE-CHANNEL-CD
               WHEN 'FV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ND'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2120-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MFDTE-CLOSE-DATE > 0 AND MFDTE-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFDTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFDTE-RESEARCH-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD MFDTE-RESEARCH-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MFDTE-RESEARCH-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFDTE-RESEARCH-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFDTE-RESEARCH-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * POSTAGE YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2130-LIMIT-POSTAGE.
           IF MFDTE-POSTAGE-YTD < 10.00
               MOVE 10.00 TO MFDTE-POSTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFDTE-POSTAGE-YTD > 1000.00
               MOVE 1000.00 TO MFDTE-POSTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2140-DESCRIBE-CHANNEL.
           EVALUATE MFDTE-CHANNEL-CD
               WHEN 'FV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ND'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * POSTAGE AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2150-LIMIT-POSTAGE.
           IF MFDTE-POSTAGE-AMT < 10.00
               MOVE 10.00 TO MFDTE-POSTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFDTE-POSTAGE-AMT > 10000.00
               MOVE 10000.00 TO MFDTE-POSTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2160-COUNT-NOTICE.
           IF MFDTE-NOTICE-CNT < 9999999
               ADD 1 TO MFDTE-NOTICE-CNT
           END-IF
           IF MFDTE-NOTICE-CNT NOT < 5
               MOVE 'Y' TO MFDTE-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2170-LIMIT-COPY-FEE.
           IF MFDTE-COPY-FEE-YTD < 25.00
               MOVE 25.00 TO MFDTE-COPY-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFDTE-COPY-FEE-YTD > 2500.00
               MOVE 2500.00 TO MFDTE-COPY-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2180-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MFDTE-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO MFDTE-RESEARCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFDTE-EXEMPT-FLAG = 'Y'
                   COMPUTE MFDTE-RESEARCH-AMT ROUNDED = WS-WORK-AMT-2 *
                       MFDTE-MARGIN / 200
               WHEN MFDTE-CHANNEL-CD = 'FV'
                   MOVE 5.00 TO MFDTE-RESEARCH-AMT
               WHEN MFDTE-CHANNEL-CD = 'GL'
                   MOVE 7.00 TO MFDTE-RESEARCH-AMT
               WHEN OTHER
                   COMPUTE MFDTE-RESEARCH-AMT ROUNDED = WS-WORK-AMT-2 *
                       MFDTE-MARGIN / 100
           END-EVALUATE
           IF MFDTE-RESEARCH-AMT > 0 AND MFDTE-RESEARCH-AMT < 1.00
               MOVE 1.00 TO MFDTE-RESEARCH-AMT
           END-IF
           IF MFDTE-RESEARCH-AMT > 250.00
               MOVE 250.00 TO MFDTE-RESEARCH-AMT
           END-IF
           ADD MFDTE-RESEARCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STORAGE YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2190-LIMIT-STORAGE.
           IF MFDTE-STORAGE-YTD < 0.00
               MOVE 0.00 TO MFDTE-STORAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFDTE-STORAGE-YTD > 10000.00
               MOVE 10000.00 TO MFDTE-STORAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2200-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFDTE-COPY-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESEARCH YTD.
      *----------------------------------------------------------------*
       2210-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFDTE-RESEARCH-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFDTE-RESEARCH-YTD
           END-IF
           ADD MFDTE-RESEARCH-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2220-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFDTE-RESEARCH-AMT / 3
           COMPUTE WS-WORK-AMT-2 = MFDTE-RESEARCH-AMT - (WS-WORK-AMT *
               3)
           COMPUTE MFDTE-POSTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2230-LIMIT-POSTAGE.
           IF MFDTE-POSTAGE-YTD < 25.00
               MOVE 25.00 TO MFDTE-POSTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFDTE-POSTAGE-YTD > 500.00
               MOVE 500.00 TO MFDTE-POSTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESEARCH YTD BY RETENTION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2240-ACCUM-RETENTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFDTE-RETENTION-CD
                   ADD MFDTE-RESEARCH-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFDTE-RETENTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFDTE-RESEARCH-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR FICHE TYPE CD:
      * AW -> JM WHEN EXPIRY DATE IS PAST
      * JM -> KV WHEN EXPIRY DATE IS PAST
      * KV -> MD WHEN EXPIRY DATE IS PAST
      * MD -> NR WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2250-SET-FICHE-TYPE.
           EVALUATE MFDTE-FICHE-TYPE-CD
               WHEN 'AW'
                   IF MFDTE-EXPIRY-DATE > 0 AND MFDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO MFDTE-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF MFDTE-EXPIRY-DATE > 0 AND MFDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KV' TO MFDTE-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF MFDTE-EXPIRY-DATE > 0 AND MFDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFDTE-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MD'
                   IF MFDTE-EXPIRY-DATE > 0 AND MFDTE-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NR' TO MFDTE-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR FICHE TYPE CD:
      * AW -> JM WHEN CLOSE DATE IS PAST
      * JM -> KV WHEN CLOSE DATE IS PAST
      * KV -> MD WHEN CLOSE DATE IS PAST
      * MD -> NR WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2260-SET-FICHE-TYPE.
           EVALUATE MFDTE-FICHE-TYPE-CD
               WHEN 'AW'
                   IF MFDTE-CLOSE-DATE > 0 AND MFDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO MFDTE-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF MFDTE-CLOSE-DATE > 0 AND MFDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KV' TO MFDTE-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF MFDTE-CLOSE-DATE > 0 AND MFDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFDTE-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MD'
                   IF MFDTE-CLOSE-DATE > 0 AND MFDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NR' TO MFDTE-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RESEARCH YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2270-LIMIT-RESEARCH.
           IF MFDTE-RESEARCH-YTD < 0.00
               MOVE 0.00 TO MFDTE-RESEARCH-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFDTE-RESEARCH-YTD > 10000.00
               MOVE 10000.00 TO MFDTE-RESEARCH-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2280-FLAG-COPY-FEE.
           IF MFDTE-COPY-FEE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MFDTE-COPY-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2290-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFDTE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFDTE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFDTE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFDTE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2300-ACCRUE-RESEARCH.
           IF MFDTE-RESEARCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFDTE-RESEARCH-AMT * MFDTE-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO MFDTE-POSTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2310-LIMIT-POSTAGE.
           IF MFDTE-POSTAGE-AMT < 0.00
               MOVE 0.00 TO MFDTE-POSTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFDTE-POSTAGE-AMT > 500.00
               MOVE 500.00 TO MFDTE-POSTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2320-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MFDTE-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFDTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO MFDTE-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2330-ACCRUE-STORAGE.
           IF MFDTE-STORAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFDTE-STORAGE-YTD * MFDTE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MFDTE-POSTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE RETENTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2340-DESCRIBE-RETENTION.
           EVALUATE MFDTE-RETENTION-CD
               WHEN 'KT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE FICHE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2350-DESCRIBE-FICHE-TYPE.
           EVALUATE MFDTE-FICHE-TYPE-CD
               WHEN 'AW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2360-COUNT-CYCLE.
           IF MFDTE-CYCLE-CNT < 9999999
               ADD 1 TO MFDTE-CYCLE-CNT
           END-IF
           IF MFDTE-CYCLE-CNT NOT < 12
               MOVE 'Y' TO MFDTE-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2370-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MFDTE-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFDTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO MFDTE-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E688 ADDR REQUIRED
      * E448 COPY FEE YTD NEGATIVE
      * E259 COPY FEE YTD OVER 10000
      * E326 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2380-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFDTE-ADDR = SPACES
                   MOVE 'E688' TO WS-REJECT-CODE
               WHEN MFDTE-COPY-FEE-YTD < 0
                   MOVE 'E448' TO WS-REJECT-CODE
               WHEN MFDTE-COPY-FEE-YTD > 10000.00
                   MOVE 'E259' TO WS-REJECT-CODE
               WHEN MFDTE-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E326' TO WS-REJECT-CODE
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
       2390-DESCRIBE-CHANNEL.
           EVALUATE MFDTE-CHANNEL-CD
               WHEN 'FV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ND'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STORAGE YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2400-LIMIT-STORAGE.
           IF MFDTE-STORAGE-YTD < 0.00
               MOVE 0.00 TO MFDTE-STORAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFDTE-STORAGE-YTD > 500.00
               MOVE 500.00 TO MFDTE-STORAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2410-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE MFDTE-POSTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2420-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF MFDTE-LAST-MAINT-DATE > 0 AND MFDTE-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFDTE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFDTE-STORAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFDTE-STORAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MFDTE-STORAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MFDTE-STORAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFDTE-STORAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE STORAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2430-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFDTE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF MFDTE-STORAGE-YTD NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFDTE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFDTE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFDTE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT POSTAGE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2440-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = MFDTE-POSTAGE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = MFDTE-POSTAGE-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * G -> K WHEN LAST MAINT DATE IS PAST
      * K -> W WHEN LAST MAINT DATE IS PAST
      * W -> Y WHEN LAST MAINT DATE IS PAST
      * Y -> G WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2450-SET-TYPE.
           EVALUATE MFDTE-TYPE-CD
               WHEN 'G'
                   IF MFDTE-LAST-MAINT-DATE > 0 AND
                       MFDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'K' TO MFDTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF MFDTE-LAST-MAINT-DATE > 0 AND
                       MFDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'W' TO MFDTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF MFDTE-LAST-MAINT-DATE > 0 AND
                       MFDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'Y' TO MFDTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF MFDTE-LAST-MAINT-DATE > 0 AND
                       MFDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'G' TO MFDTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO POSTAGE YTD.
      *----------------------------------------------------------------*
       2460-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFDTE-POSTAGE-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFDTE-POSTAGE-YTD
           END-IF
           ADD MFDTE-POSTAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2470-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE MFDTE-POSTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESEARCH YTD BY RETENTION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2480-ACCUM-RETENTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFDTE-RETENTION-CD
                   ADD MFDTE-RESEARCH-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFDTE-RETENTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFDTE-RESEARCH-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE STORAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2490-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFDTE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF MFDTE-STORAGE-YTD NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFDTE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFDTE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFDTE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2500-DESCRIBE-TYPE.
           EVALUATE MFDTE-TYPE-CD
               WHEN 'G'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2510-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MFDTE-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFDTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO MFDTE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON POSTAGE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2520-ACCRUE-POSTAGE.
           IF MFDTE-POSTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFDTE-POSTAGE-AMT * MFDTE-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO MFDTE-COPY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2530-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MFDTE-RESEARCH-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2540-COUNT-NOTICE.
           IF MFDTE-NOTICE-CNT < 9999999
               ADD 1 TO MFDTE-NOTICE-CNT
           END-IF
           IF MFDTE-NOTICE-CNT NOT < 6
               MOVE 'Y' TO MFDTE-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO POSTAGE AMT.
      *----------------------------------------------------------------*
       2550-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFDTE-POSTAGE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFDTE-POSTAGE-AMT
           END-IF
           ADD MFDTE-POSTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE POSTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2560-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MFDTE-POSTAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2570-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * MFDTE-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MFDTE-RESEARCH-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E808 ADDR REQUIRED
      * E218 POSTAGE AMT NEGATIVE
      * E617 POSTAGE AMT OVER 1000000
      * E365 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2580-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFDTE-ADDR = SPACES
                   MOVE 'E808' TO WS-REJECT-CODE
               WHEN MFDTE-POSTAGE-AMT < 0
                   MOVE 'E218' TO WS-REJECT-CODE
               WHEN MFDTE-POSTAGE-AMT > 1000000.00
                   MOVE 'E617' TO WS-REJECT-CODE
               WHEN MFDTE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E365' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON POSTAGE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2590-ACCRUE-POSTAGE.
           IF MFDTE-POSTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFDTE-POSTAGE-AMT * MFDTE-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO MFDTE-RESEARCH-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COPY FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2600-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = MFDTE-COPY-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = MFDTE-COPY-FEE-YTD - (WS-WORK-AMT *
               3)
           COMPUTE MFDTE-POSTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2610-LIMIT-POSTAGE.
           IF MFDTE-POSTAGE-YTD < 10.00
               MOVE 10.00 TO MFDTE-POSTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFDTE-POSTAGE-YTD > 1000.00
               MOVE 1000.00 TO MFDTE-POSTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2620-COUNT-CYCLE.
           IF MFDTE-CYCLE-CNT < 9999999
               ADD 1 TO MFDTE-CYCLE-CNT
           END-IF
           IF MFDTE-CYCLE-CNT NOT < 10
               MOVE 'Y' TO MFDTE-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESEARCH YTD BY FICHE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2630-ACCUM-FICHE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFDTE-FICHE-TYPE-CD
                   ADD MFDTE-RESEARCH-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFDTE-FICHE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFDTE-RESEARCH-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE COPY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2640-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFDTE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF MFDTE-COPY-FEE-YTD NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFDTE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFDTE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFDTE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COPY FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2650-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = MFDTE-COPY-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = MFDTE-COPY-FEE-YTD - (WS-WORK-AMT *
               3)
           COMPUTE MFDTE-POSTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2660-DESCRIBE-CHANNEL.
           EVALUATE MFDTE-CHANNEL-CD
               WHEN 'FV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'LM'
                   MOVE 'REVIEW' TO WS-DESC
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
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2670-COUNT-CYCLE.
           IF MFDTE-CYCLE-CNT < 9999999
               ADD 1 TO MFDTE-CYCLE-CNT
           END-IF
           IF MFDTE-CYCLE-CNT NOT < 5
               MOVE 'Y' TO MFDTE-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2680-COUNT-NOTICE.
           IF MFDTE-NOTICE-CNT < 9999999
               ADD 1 TO MFDTE-NOTICE-CNT
           END-IF
           IF MFDTE-NOTICE-CNT NOT < 12
               MOVE 'Y' TO MFDTE-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COPY FEE YTD.
      *----------------------------------------------------------------*
       2690-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFDTE-COPY-FEE-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFDTE-COPY-FEE-YTD
           END-IF
           ADD MFDTE-COPY-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2700-COUNT-NOTICE.
           IF MFDTE-NOTICE-CNT < 9999999
               ADD 1 TO MFDTE-NOTICE-CNT
           END-IF
           IF MFDTE-NOTICE-CNT NOT < 6
               MOVE 'Y' TO MFDTE-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COPY FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2710-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = MFDTE-COPY-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = MFDTE-COPY-FEE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE POSTAGE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2720-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MFDTE-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFDTE-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = MFDTE-POSTAGE-AMT *
                       MFDTE-MARGIN / 200
               WHEN MFDTE-RETENTION-CD = 'KT'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN MFDTE-RETENTION-CD = 'SJ'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = MFDTE-POSTAGE-AMT *
                       MFDTE-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR FICHE TYPE CD:
      * AW -> JM WHEN LAST MAINT DATE IS PAST
      * JM -> KV WHEN LAST MAINT DATE IS PAST
      * KV -> MD WHEN LAST MAINT DATE IS PAST
      * MD -> NR WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2730-SET-FICHE-TYPE.
           EVALUATE MFDTE-FICHE-TYPE-CD
               WHEN 'AW'
                   IF MFDTE-LAST-MAINT-DATE > 0 AND
                       MFDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'JM' TO MFDTE-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF MFDTE-LAST-MAINT-DATE > 0 AND
                       MFDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'KV' TO MFDTE-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF MFDTE-LAST-MAINT-DATE > 0 AND
                       MFDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFDTE-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MD'
                   IF MFDTE-LAST-MAINT-DATE > 0 AND
                       MFDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'NR' TO MFDTE-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2740-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MFDTE-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFDTE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO MFDTE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2750-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MFDTE-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFDTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO MFDTE-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE POSTAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2760-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MFDTE-POSTAGE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT POSTAGE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2770-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = MFDTE-POSTAGE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = MFDTE-POSTAGE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2780-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF MFDTE-LAST-MAINT-DATE > 0 AND MFDTE-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFDTE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFDTE-POSTAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFDTE-POSTAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MFDTE-POSTAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MFDTE-POSTAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFDTE-POSTAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2790-DESCRIBE-TYPE.
           EVALUATE MFDTE-TYPE-CD
               WHEN 'G'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Y'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2800-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE MFDTE-POSTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2810-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF MFDTE-LAST-MAINT-DATE > 0 AND MFDTE-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFDTE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFDTE-RESEARCH-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MFDTE-RESEARCH-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MFDTE-RESEARCH-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MFDTE-RESEARCH-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFDTE-RESEARCH-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE POSTAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2820-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFDTE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF MFDTE-POSTAGE-AMT NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFDTE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFDTE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFDTE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2830-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MFDTE-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFDTE-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO MFDTE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2840-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFDTE-RESEARCH-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MFDTE-RESEARCH-AMT - (WS-WORK-AMT *
               4)
           COMPUTE MFDTE-COPY-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2850-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MFDTE-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFDTE-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO MFDTE-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN CLOSE DATE IS PAST
      * GL -> HH WHEN CLOSE DATE IS PAST
      * HH -> LM WHEN CLOSE DATE IS PAST
      * LM -> MD WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2860-SET-CHANNEL.
           EVALUATE MFDTE-CHANNEL-CD
               WHEN 'FV'
                   IF MFDTE-CLOSE-DATE > 0 AND MFDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFDTE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFDTE-CLOSE-DATE > 0 AND MFDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFDTE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFDTE-CLOSE-DATE > 0 AND MFDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFDTE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFDTE-CLOSE-DATE > 0 AND MFDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFDTE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN LAST MAINT DATE IS PAST
      * GL -> HH WHEN LAST MAINT DATE IS PAST
      * HH -> LM WHEN LAST MAINT DATE IS PAST
      * LM -> MD WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2870-SET-CHANNEL.
           EVALUATE MFDTE-CHANNEL-CD
               WHEN 'FV'
                   IF MFDTE-LAST-MAINT-DATE > 0 AND
                       MFDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFDTE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFDTE-LAST-MAINT-DATE > 0 AND
                       MFDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFDTE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFDTE-LAST-MAINT-DATE > 0 AND
                       MFDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFDTE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFDTE-LAST-MAINT-DATE > 0 AND
                       MFDTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFDTE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2880-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF MFDTE-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFDTE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO MFDTE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE RETENTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2890-DESCRIBE-RETENTION.
           EVALUATE MFDTE-RETENTION-CD
               WHEN 'KT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2900-ACCRUE-RESEARCH.
           IF MFDTE-RESEARCH-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFDTE-RESEARCH-YTD * MFDTE-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO MFDTE-COPY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO MFDTE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO MFDTE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO MFDTE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'MFDTE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
