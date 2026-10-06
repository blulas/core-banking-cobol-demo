      *================================================================*
      * PROGRAM  : ATTIER                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : ATM NETWORK SETTLEMENT RULE - ELIGIBILITY. CALLED BY*
      *            THE ATM NETWORK SETTLEMENT BATCH AND ONLINE         *
      *            PROGRAMS.                                           *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-04-20 MKL  ORIGINAL                                       *
      * 1994-11-09 BHS  SURCHG EXCEPTIONS TO OPS QUEUE PRB3521         *
      * 1995-12-10 BHS  AUDIT FINDING - SETTLE CONTROL TOTALS PRB3158  *
      * 1996-10-27 BHS  CASSETTE STATUS CODES ADDED INC9815            *
      * 1996-10-07 CRP  ADJUST RERUN PROTECTION REQ7641                *
      * 2000-11-14 RJT  AUDIT FINDING - SURCHG CONTROL TOTALS PRB5406  *
      * 2003-11-21 JLM  AUDIT FINDING - CASSETTE CONTROL TOTALS INC4162*
      * 2004-10-06 TWH  CASSETTE NOTICE TEXT PER LEGAL CHG5165         *
      * 2005-10-15 EWS  ADJUST LIMIT REVISED REQ4192                   *
      * 2010-06-11 LMB  AUDIT FINDING - ADJUST CONTROL TOTALS INC1876  *
      * 2013-01-09 TWH  DISPENSED RERUN PROTECTION INC0328             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATTIER.
       AUTHOR. B H SANDERS.

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
      * WS-SETTLE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SETTLE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000112500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000552500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000200000000079500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001200000000415500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002200000000886000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012200000000394000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012450000000022000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012950000000231000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022950000000498500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023000000000631000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025500000000866000'.
       01  WS-SETTLE-BAND-TBL REDEFINES WS-SETTLE-BAND-VALUES.
           05  WS-SETTLE-BAND-ROW OCCURS 11 TIMES.
               10  WS-SETTLE-BAND-LOW      PIC 9(09)V99.
               10  WS-SETTLE-BAND-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DISPENSE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISPENSE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000665000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000145000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000851000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000136000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000022000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000867500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012100000000390000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022100000000514000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027100000000358000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027150000000873500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032150000000844500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032250000000401000'.
       01  WS-DISPENSE-BAND-TBL REDEFINES WS-DISPENSE-BAND-VALUES.
           05  WS-DISPENSE-BAND-ROW OCCURS 12 TIMES.
               10  WS-DISPENSE-BAND-LOW    PIC 9(09)V99.
               10  WS-DISPENSE-BAND-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY ATTIERP.

       PROCEDURE DIVISION USING ATTIER-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-FLAG-ADJUST
           PERFORM 2010-SET-BRANCH
           PERFORM 2020-ACCRUE-SURCHG
           PERFORM 2030-SELECT-RATE
           PERFORM 2040-DESCRIBE-BRANCH
           PERFORM 2050-FLAG-SURCHG
           PERFORM 2060-PRORATE-SETTLE
           PERFORM 2070-LIMIT-ITEM
           PERFORM 2080-COUNT-RETRY
           PERFORM 2090-DESCRIBE-STATUS
           PERFORM 2100-AGE-NOTICE
           PERFORM 2110-ACCRUE-ADJUST
           PERFORM 2120-CHECK-NEXT-DUE
           PERFORM 2130-SET-BRANCH
           PERFORM 2140-AGE-NEXT-DUE
           PERFORM 2150-DESCRIBE-REGION
           PERFORM 2160-SET-REASON
           PERFORM 2170-SPLIT-CASSETTE
           PERFORM 2180-ACCUM-REGION
           PERFORM 2190-ACCRUE-RATE
           PERFORM 2200-SELECT-RATE
           PERFORM 2210-AGE-EXPIRY
           PERFORM 2220-FLAG-CASSETTE
           PERFORM 2230-SET-REGION
           PERFORM 2240-ACCRUE-INTERCHG
           PERFORM 2250-FLAG-SETTLE
           PERFORM 2260-SPLIT-SURCHG
           PERFORM 2270-FLAG-ADJUST
           PERFORM 2280-ASSESS-EXEMPT
           PERFORM 2290-ACCUM-REASON
           PERFORM 2300-DESCRIBE-BRANCH
           PERFORM 2310-CHECK-NEXT-DUE
           PERFORM 2320-PRORATE-ADJUST
           PERFORM 2330-PRORATE-SURCHG
           PERFORM 2340-EDIT-DESC
           PERFORM 2350-CHECK-NEXT-REVIEW
           PERFORM 2360-EDIT-DESC
           PERFORM 2370-SPLIT-ADJUST
           PERFORM 2380-ACCUM-STATUS
           PERFORM 2390-SELECT-FEE-PCT
           PERFORM 2400-PRORATE-CASSETTE
           PERFORM 2410-ACCRUE-ADJUST
           PERFORM 2420-SELECT-RATE
           PERFORM 2430-CHECK-NEXT-REVIEW
           PERFORM 2440-ACCRUE-INTERCHG
           PERFORM 2450-DESCRIBE-REGION
           PERFORM 2460-COUNT-ITEM
           PERFORM 2470-DESCRIBE-BRANCH
           PERFORM 2480-SELECT-RATE
           PERFORM 2490-SELECT-FEE-PCT
           PERFORM 2500-SELECT-RATE
           PERFORM 2510-PRORATE-SETTLE
           PERFORM 2520-SELECT-RATE
           PERFORM 2530-ACCUM-STATUS
           PERFORM 2540-COUNT-ITEM
           PERFORM 2550-SET-REGION
           PERFORM 2560-CHECK-NEXT-REVIEW
           PERFORM 2570-EDIT-CUST-ID
           PERFORM 2580-EDIT-DESC
           PERFORM 2590-SET-REGION
           PERFORM 2600-SET-STATUS
           PERFORM 2610-LIMIT-INTERCHG
           PERFORM 2620-SPLIT-SETTLE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO ATTIER-RETURN-CODE.

      *----------------------------------------------------------------*
      * ADJUST AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2000-FLAG-ADJUST.
           IF ATTIER-ADJUST-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATTIER-ADJUST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DU -> HX WHEN NOTICE DATE IS PAST
      * HX -> YC WHEN NOTICE DATE IS PAST
      * YC -> ZB WHEN NOTICE DATE IS PAST
      * ZB -> DU WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2010-SET-BRANCH.
           EVALUATE ATTIER-BRANCH-CD
               WHEN 'DU'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATTIER-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO ATTIER-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZB' TO ATTIER-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZB'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO ATTIER-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURCHG AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2020-ACCRUE-SURCHG.
           IF ATTIER-SURCHG-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATTIER-SURCHG-AMT * ATTIER-RATE / 100 / 360
               ADD WS-WORK-AMT TO ATTIER-ADJUST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISPENSE BAND TIER FOR THE INTERCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2030-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISPENSE-BAND-RATE(1) TO ATTIER-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ATTIER-INTERCHG-YTD NOT < WS-DISPENSE-BAND-LOW(WS-TX)
                   MOVE WS-DISPENSE-BAND-RATE(WS-TX) TO ATTIER-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATTIER-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATTIER-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2040-DESCRIBE-BRANCH.
           EVALUATE ATTIER-BRANCH-CD
               WHEN 'DU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SURCHG AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2050-FLAG-SURCHG.
           IF ATTIER-SURCHG-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATTIER-SURCHG-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2060-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATTIER-SETTLE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2070-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2080-COUNT-RETRY.
           IF ATTIER-RETRY-CNT < 9999999
               ADD 1 TO ATTIER-RETRY-CNT
           END-IF
           IF ATTIER-RETRY-CNT NOT < 5
               MOVE 'Y' TO ATTIER-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2090-DESCRIBE-STATUS.
           EVALUATE ATTIER-STATUS-CD
               WHEN 'DF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2100-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATTIER-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATTIER-ADJUST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ATTIER-ADJUST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ATTIER-ADJUST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ATTIER-ADJUST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATTIER-ADJUST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADJUST YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2110-ACCRUE-ADJUST.
           IF ATTIER-ADJUST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATTIER-ADJUST-YTD * ATTIER-RATE / 100 / 360
               ADD WS-WORK-AMT TO ATTIER-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2120-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ATTIER-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATTIER-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO ATTIER-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DU -> HX WHEN NOTICE DATE IS PAST
      * HX -> YC WHEN NOTICE DATE IS PAST
      * YC -> ZB WHEN NOTICE DATE IS PAST
      * ZB -> DU WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2130-SET-BRANCH.
           EVALUATE ATTIER-BRANCH-CD
               WHEN 'DU'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATTIER-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO ATTIER-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZB' TO ATTIER-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZB'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO ATTIER-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2140-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ATTIER-NEXT-DUE-DATE > 0 AND ATTIER-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATTIER-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATTIER-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATTIER-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ATTIER-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATTIER-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATTIER-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2150-DESCRIBE-REGION.
           EVALUATE ATTIER-REGION-CD
               WHEN 'F'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'G'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'H'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AK -> HX WHEN NOTICE DATE IS PAST
      * HX -> PR WHEN NOTICE DATE IS PAST
      * PR -> XD WHEN NOTICE DATE IS PAST
      * XD -> ZJ WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2160-SET-REASON.
           EVALUATE ATTIER-REASON-CD
               WHEN 'AK'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO ATTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO ATTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZJ' TO ATTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2170-SPLIT-CASSETTE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE ATTIER-CASSETTE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTERCHG YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2180-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATTIER-REGION-CD
                   ADD ATTIER-INTERCHG-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATTIER-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATTIER-INTERCHG-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2190-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ATTIER-RATE / 100 / 365
               ADD WS-WORK-AMT TO ATTIER-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2200-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BAND-RATE(1) TO ATTIER-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-SETTLE-BAND-LOW(WS-TX)
                   MOVE WS-SETTLE-BAND-RATE(WS-TX) TO ATTIER-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATTIER-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATTIER-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2210-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ATTIER-EXPIRY-DATE > 0 AND ATTIER-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATTIER-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASSETTE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2220-FLAG-CASSETTE.
           IF ATTIER-CASSETTE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATTIER-CASSETTE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * F -> G WHEN NEXT REVIEW DATE IS PAST
      * G -> H WHEN NEXT REVIEW DATE IS PAST
      * H -> P WHEN NEXT REVIEW DATE IS PAST
      * P -> V WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2230-SET-REGION.
           EVALUATE ATTIER-REGION-CD
               WHEN 'F'
                   IF ATTIER-NEXT-REVIEW-DATE > 0 AND
                       ATTIER-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'G' TO ATTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ATTIER-NEXT-REVIEW-DATE > 0 AND
                       ATTIER-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'H' TO ATTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATTIER-NEXT-REVIEW-DATE > 0 AND
                       ATTIER-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'P' TO ATTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF ATTIER-NEXT-REVIEW-DATE > 0 AND
                       ATTIER-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'V' TO ATTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTERCHG YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2240-ACCRUE-INTERCHG.
           IF ATTIER-INTERCHG-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATTIER-INTERCHG-YTD * ATTIER-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2250-FLAG-SETTLE.
           IF ATTIER-SETTLE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATTIER-SETTLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2260-SPLIT-SURCHG.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE ATTIER-SURCHG-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ADJUST YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2270-FLAG-ADJUST.
           IF ATTIER-ADJUST-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATTIER-ADJUST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURCHG FEE ON THE ADJUST YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2280-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ATTIER-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ATTIER-SURCHG-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATTIER-STOP-FLAG = 'Y'
                   COMPUTE ATTIER-SURCHG-AMT ROUNDED = ATTIER-ADJUST-YTD
                       * ATTIER-FEE-PCT / 200
               WHEN ATTIER-REASON-CD = 'AK'
                   MOVE 12.00 TO ATTIER-SURCHG-AMT
               WHEN ATTIER-REASON-CD = 'HX'
                   MOVE 12.00 TO ATTIER-SURCHG-AMT
               WHEN OTHER
                   COMPUTE ATTIER-SURCHG-AMT ROUNDED = ATTIER-ADJUST-YTD
                       * ATTIER-FEE-PCT / 100
           END-EVALUATE
           IF ATTIER-SURCHG-AMT > 0 AND ATTIER-SURCHG-AMT < 2.00
               MOVE 2.00 TO ATTIER-SURCHG-AMT
           END-IF
           IF ATTIER-SURCHG-AMT > 75.00
               MOVE 75.00 TO ATTIER-SURCHG-AMT
           END-IF
           ADD ATTIER-SURCHG-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE CASSETTE YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2290-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATTIER-REASON-CD
                   ADD ATTIER-CASSETTE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATTIER-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATTIER-CASSETTE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2300-DESCRIBE-BRANCH.
           EVALUATE ATTIER-BRANCH-CD
               WHEN 'DU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2310-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ATTIER-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATTIER-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ATTIER-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTERCHG YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ADJUST AMT.
      *----------------------------------------------------------------*
       2320-PRORATE-ADJUST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATTIER-ADJUST-AMT ROUNDED =
                   ATTIER-INTERCHG-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATTIER-ADJUST-AMT
           END-IF
           ADD ATTIER-ADJUST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE CASSETTE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SURCHG AMT.
      *----------------------------------------------------------------*
       2330-PRORATE-SURCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATTIER-SURCHG-AMT ROUNDED =
                   ATTIER-CASSETTE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATTIER-SURCHG-AMT
           END-IF
           ADD ATTIER-SURCHG-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E426 DESC REQUIRED
      * E270 INTERCHG YTD NEGATIVE
      * E766 INTERCHG YTD OVER 99999
      * E278 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2340-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATTIER-DESC = SPACES
                   MOVE 'E426' TO WS-REJECT-CODE
               WHEN ATTIER-INTERCHG-YTD < 0
                   MOVE 'E270' TO WS-REJECT-CODE
               WHEN ATTIER-INTERCHG-YTD > 99999.00
                   MOVE 'E766' TO WS-REJECT-CODE
               WHEN ATTIER-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E278' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2350-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ATTIER-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATTIER-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ATTIER-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E813 DESC REQUIRED
      * E600 ADJUST YTD NEGATIVE
      * E621 ADJUST YTD OVER 25000
      * E702 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2360-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATTIER-DESC = SPACES
                   MOVE 'E813' TO WS-REJECT-CODE
               WHEN ATTIER-ADJUST-YTD < 0
                   MOVE 'E600' TO WS-REJECT-CODE
               WHEN ATTIER-ADJUST-YTD > 25000.00
                   MOVE 'E621' TO WS-REJECT-CODE
               WHEN ATTIER-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E702' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADJUST AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2370-SPLIT-ADJUST.
           COMPUTE WS-WORK-AMT = ATTIER-ADJUST-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ATTIER-ADJUST-AMT - (WS-WORK-AMT * 4)
           COMPUTE ATTIER-SURCHG-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2380-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATTIER-STATUS-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATTIER-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISPENSE BAND TIER FOR THE SETTLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2390-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISPENSE-BAND-RATE(1) TO ATTIER-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ATTIER-SETTLE-AMT NOT < WS-DISPENSE-BAND-LOW(WS-TX)
                   MOVE WS-DISPENSE-BAND-RATE(WS-TX) TO ATTIER-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATTIER-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATTIER-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTERCHG YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASSETTE YTD.
      *----------------------------------------------------------------*
       2400-PRORATE-CASSETTE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATTIER-CASSETTE-YTD ROUNDED =
                   ATTIER-INTERCHG-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATTIER-CASSETTE-YTD
           END-IF
           ADD ATTIER-CASSETTE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADJUST AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2410-ACCRUE-ADJUST.
           IF ATTIER-ADJUST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATTIER-ADJUST-AMT * ATTIER-RATE / 100 / 360
               ADD WS-WORK-AMT TO ATTIER-CASSETTE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BAND TIER FOR THE ADJUST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2420-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BAND-RATE(1) TO ATTIER-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ATTIER-ADJUST-YTD NOT < WS-SETTLE-BAND-LOW(WS-TX)
                   MOVE WS-SETTLE-BAND-RATE(WS-TX) TO ATTIER-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATTIER-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATTIER-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2430-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ATTIER-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATTIER-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ATTIER-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTERCHG YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2440-ACCRUE-INTERCHG.
           IF ATTIER-INTERCHG-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATTIER-INTERCHG-YTD * ATTIER-RATE / 100 / 360
               ADD WS-WORK-AMT TO ATTIER-SURCHG-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2450-DESCRIBE-REGION.
           EVALUATE ATTIER-REGION-CD
               WHEN 'F'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'G'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'X'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2460-COUNT-ITEM.
           IF ATTIER-ITEM-CNT < 9999999
               ADD 1 TO ATTIER-ITEM-CNT
           END-IF
           IF ATTIER-ITEM-CNT NOT < 12
               MOVE 'Y' TO ATTIER-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2470-DESCRIBE-BRANCH.
           EVALUATE ATTIER-BRANCH-CD
               WHEN 'DU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BAND TIER FOR THE INTERCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2480-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BAND-RATE(1) TO ATTIER-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ATTIER-INTERCHG-YTD NOT < WS-SETTLE-BAND-LOW(WS-TX)
                   MOVE WS-SETTLE-BAND-RATE(WS-TX) TO ATTIER-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATTIER-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATTIER-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISPENSE BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2490-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISPENSE-BAND-RATE(1) TO ATTIER-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-DISPENSE-BAND-LOW(WS-TX)
                   MOVE WS-DISPENSE-BAND-RATE(WS-TX) TO ATTIER-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATTIER-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATTIER-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BAND TIER FOR THE ADJUST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2500-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BAND-RATE(1) TO ATTIER-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ATTIER-ADJUST-AMT NOT < WS-SETTLE-BAND-LOW(WS-TX)
                   MOVE WS-SETTLE-BAND-RATE(WS-TX) TO ATTIER-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATTIER-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATTIER-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       2510-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATTIER-SETTLE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATTIER-SETTLE-AMT
           END-IF
           ADD ATTIER-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BAND TIER FOR THE ADJUST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2520-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BAND-RATE(1) TO ATTIER-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ATTIER-ADJUST-YTD NOT < WS-SETTLE-BAND-LOW(WS-TX)
                   MOVE WS-SETTLE-BAND-RATE(WS-TX) TO ATTIER-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATTIER-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATTIER-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTERCHG YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2530-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATTIER-STATUS-CD
                   ADD ATTIER-INTERCHG-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATTIER-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATTIER-INTERCHG-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2540-COUNT-ITEM.
           IF ATTIER-ITEM-CNT < 9999999
               ADD 1 TO ATTIER-ITEM-CNT
           END-IF
           IF ATTIER-ITEM-CNT NOT < 6
               MOVE 'Y' TO ATTIER-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * F -> G WHEN NEXT REVIEW DATE IS PAST
      * G -> H WHEN NEXT REVIEW DATE IS PAST
      * H -> P WHEN NEXT REVIEW DATE IS PAST
      * P -> V WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2550-SET-REGION.
           EVALUATE ATTIER-REGION-CD
               WHEN 'F'
                   IF ATTIER-NEXT-REVIEW-DATE > 0 AND
                       ATTIER-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'G' TO ATTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ATTIER-NEXT-REVIEW-DATE > 0 AND
                       ATTIER-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'H' TO ATTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATTIER-NEXT-REVIEW-DATE > 0 AND
                       ATTIER-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'P' TO ATTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF ATTIER-NEXT-REVIEW-DATE > 0 AND
                       ATTIER-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'V' TO ATTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2560-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ATTIER-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATTIER-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ATTIER-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E365 CUST ID REQUIRED
      * E496 ADJUST YTD NEGATIVE
      * E133 ADJUST YTD OVER 10000
      * E532 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2570-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATTIER-CUST-ID = SPACES
                   MOVE 'E365' TO WS-REJECT-CODE
               WHEN ATTIER-ADJUST-YTD < 0
                   MOVE 'E496' TO WS-REJECT-CODE
               WHEN ATTIER-ADJUST-YTD > 10000.00
                   MOVE 'E133' TO WS-REJECT-CODE
               WHEN ATTIER-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E532' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E414 DESC REQUIRED
      * E587 INTERCHG YTD NEGATIVE
      * E554 INTERCHG YTD OVER 50000
      * E480 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2580-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATTIER-DESC = SPACES
                   MOVE 'E414' TO WS-REJECT-CODE
               WHEN ATTIER-INTERCHG-YTD < 0
                   MOVE 'E587' TO WS-REJECT-CODE
               WHEN ATTIER-INTERCHG-YTD > 50000.00
                   MOVE 'E554' TO WS-REJECT-CODE
               WHEN ATTIER-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E480' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * F -> G WHEN NOTICE DATE IS PAST
      * G -> H WHEN NOTICE DATE IS PAST
      * H -> P WHEN NOTICE DATE IS PAST
      * P -> V WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2590-SET-REGION.
           EVALUATE ATTIER-REGION-CD
               WHEN 'F'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO ATTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ATTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO ATTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO ATTIER-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DF -> DP WHEN NOTICE DATE IS PAST
      * DP -> FJ WHEN NOTICE DATE IS PAST
      * FJ -> NA WHEN NOTICE DATE IS PAST
      * NA -> DF WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2600-SET-STATUS.
           EVALUATE ATTIER-STATUS-CD
               WHEN 'DF'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO ATTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJ' TO ATTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJ'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NA' TO ATTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NA'
                   IF ATTIER-NOTICE-DATE > 0 AND ATTIER-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO ATTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTERCHG YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2610-LIMIT-INTERCHG.
           IF ATTIER-INTERCHG-YTD < 10.00
               MOVE 10.00 TO ATTIER-INTERCHG-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATTIER-INTERCHG-YTD > 1000.00
               MOVE 1000.00 TO ATTIER-INTERCHG-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2620-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE ATTIER-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO ATTIER-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO ATTIER-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO ATTIER-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'ATTIER EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
