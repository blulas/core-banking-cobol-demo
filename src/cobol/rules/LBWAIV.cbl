      *================================================================*
      * PROGRAM  : LBWAIV                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : LOCKBOX REMITTANCE RULE - TIERING. CALLED BY THE    *
      *            LOCKBOX REMITTANCE BATCH AND ONLINE PROGRAMS.       *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-12-19 BHS  ORIGINAL                                       *
      * 2004-08-08 SKN  FEE CALCULATION CORRECTED PRB3123              *
      * 2004-01-07 LMB  CASH-ON-HAND TABLE EXPANDED REQ1434            *
      * 2008-04-04 BHS  REMIT STATUS CODES ADDED REQ6083               *
      * 2011-03-23 LMB  FEE CUTOFF MOVED CHG0327                       *
      * 2011-01-07 JLM  DEPOSIT RERUN PROTECTION REQ7718               *
      * 2011-09-10 RJT  DEPOSIT CUTOFF MOVED CHG4326                   *
      * 2011-12-08 JLM  CHECK ROUNDING - HALF UP PRB0362               *
      * 2011-08-19 BHS  CHECK TABLE EXPANDED PRB1521                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBWAIV.
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
                                           VALUE 12.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 30.
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
      * WS-EXCEPT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-EXCEPT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000111500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000019000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000091500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005300000000309000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015300000000721500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015800000000683000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025800000000120500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025900000000612500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026900000000353500'.
       01  WS-EXCEPT-BAND-TBL REDEFINES WS-EXCEPT-BAND-VALUES.
           05  WS-EXCEPT-BAND-ROW OCCURS 9 TIMES.
               10  WS-EXCEPT-BAND-LOW      PIC 9(09)V99.
               10  WS-EXCEPT-BAND-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FLOAT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FLOAT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000105000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000475500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000173000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000683000'.
       01  WS-FLOAT-SCHED-TBL REDEFINES WS-FLOAT-SCHED-VALUES.
           05  WS-FLOAT-SCHED-ROW OCCURS 4 TIMES.
               10  WS-FLOAT-SCHED-LOW      PIC 9(09)V99.
               10  WS-FLOAT-SCHED-RATE     PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY LBWAIVP.

       PROCEDURE DIVISION USING LBWAIV-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-DESCRIBE-CHANNEL
           PERFORM 2010-SPLIT-CHECK
           PERFORM 2020-SELECT-PENALTY-PCT
           PERFORM 2030-COUNT-EXCEPT
           PERFORM 2040-AGE-EXPIRY
           PERFORM 2050-ASSESS-STOP
           PERFORM 2060-SPLIT-CHECK
           PERFORM 2070-EDIT-CONTACT
           PERFORM 2080-ASSESS-STOP
           PERFORM 2090-ACCUM-LOCKBOX
           PERFORM 2100-PRORATE-CASH-ON-HAND
           PERFORM 2110-CHECK-MATURITY
           PERFORM 2120-AGE-LAST-PAY
           PERFORM 2130-SPLIT-FLOAT
           PERFORM 2140-SET-LOCKBOX
           PERFORM 2150-AGE-EXPIRY
           PERFORM 2160-PRORATE-REMIT
           PERFORM 2170-DESCRIBE-REGION
           PERFORM 2180-EDIT-CONTACT
           PERFORM 2190-ACCUM-REASON
           PERFORM 2200-COUNT-PAYMENT
           PERFORM 2210-ACCUM-CHANNEL
           PERFORM 2220-SET-REASON
           PERFORM 2230-LIMIT-EXCEPT
           PERFORM 2240-SET-REASON
           PERFORM 2250-CHECK-LAST-PAY
           PERFORM 2260-ASSESS-EXEMPT
           PERFORM 2270-FLAG-FLOAT
           PERFORM 2280-SET-LOCKBOX
           PERFORM 2290-PRORATE-REMIT
           PERFORM 2300-DESCRIBE-REASON
           PERFORM 2310-ACCUM-REASON
           PERFORM 2320-ACCRUE-CHECK
           PERFORM 2330-ACCUM-REASON
           PERFORM 2340-ACCRUE-PENALTY-PCT
           PERFORM 2350-LIMIT-FLOAT
           PERFORM 2360-DESCRIBE-REASON
           PERFORM 2370-AGE-LAST-STMT
           PERFORM 2380-DESCRIBE-LOCKBOX
           PERFORM 2390-COUNT-PAYMENT
           PERFORM 2400-CHECK-EXPIRY
           PERFORM 2410-SET-REGION
           PERFORM 2420-AGE-LAST-STMT
           PERFORM 2430-PRORATE-FLOAT
           PERFORM 2440-COUNT-PAYMENT
           PERFORM 2450-ACCRUE-CASH-ON-HAND
           PERFORM 2460-SET-REASON
           PERFORM 2470-SELECT-PENALTY-PCT
           PERFORM 2480-EDIT-CUST-ID
           PERFORM 2490-CHECK-LAST-PAY
           PERFORM 2500-SELECT-DISCOUNT
           PERFORM 2510-CHECK-LAST-STMT
           PERFORM 2520-EDIT-CONTACT
           PERFORM 2530-AGE-LAST-STMT
           PERFORM 2540-EDIT-CUST-ID
           PERFORM 2550-CHECK-MATURITY
           PERFORM 2560-ACCUM-REGION
           PERFORM 2570-SPLIT-EXCEPT
           PERFORM 2580-FLAG-REMIT
           PERFORM 2590-COUNT-EXCEPT
           PERFORM 2600-LIMIT-CHECK
           PERFORM 2610-AGE-EXPIRY
           PERFORM 2620-AGE-LAST-PAY
           PERFORM 2630-DESCRIBE-REASON
           PERFORM 2640-EDIT-CONTACT
           PERFORM 2650-ACCRUE-PENALTY-PCT
           PERFORM 2660-ACCRUE-CHECK
           PERFORM 2670-PRORATE-CASH-ON-HAND
           PERFORM 2680-LIMIT-FLOAT
           PERFORM 2690-SELECT-PENALTY-PCT
           PERFORM 2700-CHECK-EXPIRY
           PERFORM 2710-LIMIT-FLOAT
           PERFORM 2720-ACCUM-REASON
           PERFORM 2730-DESCRIBE-REASON
           PERFORM 2740-ACCUM-REGION
           PERFORM 2750-COUNT-EXCEPT
           PERFORM 2760-SPLIT-REMIT
           PERFORM 2770-PRORATE-FLOAT
           PERFORM 2780-ACCUM-LOCKBOX
           PERFORM 2790-ASSESS-STOP
           PERFORM 2800-COUNT-EXCEPT
           PERFORM 2810-LIMIT-FLOAT
           PERFORM 2820-SPLIT-ITEM
           PERFORM 2830-FLAG-REMIT
           PERFORM 2840-CHECK-LAST-STMT
           PERFORM 2850-ACCUM-REGION
           PERFORM 2860-SPLIT-EXCEPT
           PERFORM 2870-PRORATE-CASH-ON-HAND
           PERFORM 2880-COUNT-PAYMENT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO LBWAIV-RETURN-CODE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2000-DESCRIBE-CHANNEL.
           EVALUATE LBWAIV-CHANNEL-CD
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CHECK YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2010-SPLIT-CHECK.
           COMPUTE WS-WORK-AMT = LBWAIV-CHECK-YTD / 4
           COMPUTE WS-WORK-AMT-2 = LBWAIV-CHECK-YTD - (WS-WORK-AMT * 4)
           COMPUTE LBWAIV-CASH-ON-HAND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT SCHED TIER FOR THE REMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2020-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-SCHED-RATE(1) TO LBWAIV-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF LBWAIV-REMIT-YTD NOT < WS-FLOAT-SCHED-LOW(WS-TX)
                   MOVE WS-FLOAT-SCHED-RATE(WS-TX) TO LBWAIV-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBWAIV-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBWAIV-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2030-COUNT-EXCEPT.
           IF LBWAIV-EXCEPT-CNT < 9999999
               ADD 1 TO LBWAIV-EXCEPT-CNT
           END-IF
           IF LBWAIV-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO LBWAIV-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2040-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF LBWAIV-EXPIRY-DATE > 0 AND LBWAIV-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBWAIV-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBWAIV-CHECK-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD LBWAIV-CHECK-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD LBWAIV-CHECK-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD LBWAIV-CHECK-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBWAIV-CHECK-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FLOAT FEE ON THE WORK AMT 2.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2050-ASSESS-STOP.
           EVALUATE TRUE
               WHEN LBWAIV-STOP-FLAG = 'Y'
                   MOVE ZERO TO LBWAIV-FLOAT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBWAIV-EXEMPT-FLAG = 'Y'
                   COMPUTE LBWAIV-FLOAT-AMT ROUNDED = WS-WORK-AMT-2 *
                       LBWAIV-DISCOUNT / 200
               WHEN LBWAIV-REASON-CD = 'GF'
                   MOVE 3.00 TO LBWAIV-FLOAT-AMT
               WHEN LBWAIV-REASON-CD = 'UK'
                   MOVE 5.00 TO LBWAIV-FLOAT-AMT
               WHEN OTHER
                   COMPUTE LBWAIV-FLOAT-AMT ROUNDED = WS-WORK-AMT-2 *
                       LBWAIV-DISCOUNT / 100
           END-EVALUATE
           IF LBWAIV-FLOAT-AMT > 0 AND LBWAIV-FLOAT-AMT < 5.00
               MOVE 5.00 TO LBWAIV-FLOAT-AMT
           END-IF
           IF LBWAIV-FLOAT-AMT > 75.00
               MOVE 75.00 TO LBWAIV-FLOAT-AMT
           END-IF
           ADD LBWAIV-FLOAT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2060-SPLIT-CHECK.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE LBWAIV-CHECK-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E701 CONTACT REQUIRED
      * E669 WORK AMT NEGATIVE
      * E890 WORK AMT OVER 250000
      * E708 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2070-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBWAIV-CONTACT = SPACES
                   MOVE 'E701' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E669' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E890' TO WS-REJECT-CODE
               WHEN LBWAIV-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E708' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT FEE ON THE FLOAT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2080-ASSESS-STOP.
           EVALUATE TRUE
               WHEN LBWAIV-STOP-FLAG = 'Y'
                   MOVE ZERO TO LBWAIV-EXCEPT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBWAIV-EXEMPT-FLAG = 'Y'
                   COMPUTE LBWAIV-EXCEPT-AMT ROUNDED = LBWAIV-FLOAT-AMT
                       * LBWAIV-PENALTY-PCT / 200
               WHEN LBWAIV-REGION-CD = 'DVP'
                   MOVE 5.00 TO LBWAIV-EXCEPT-AMT
               WHEN LBWAIV-REGION-CD = 'FMZ'
                   MOVE 7.00 TO LBWAIV-EXCEPT-AMT
               WHEN OTHER
                   COMPUTE LBWAIV-EXCEPT-AMT ROUNDED = LBWAIV-FLOAT-AMT
                       * LBWAIV-PENALTY-PCT / 100
           END-EVALUATE
           IF LBWAIV-EXCEPT-AMT > 0 AND LBWAIV-EXCEPT-AMT < 10.00
               MOVE 10.00 TO LBWAIV-EXCEPT-AMT
           END-IF
           IF LBWAIV-EXCEPT-AMT > 250.00
               MOVE 250.00 TO LBWAIV-EXCEPT-AMT
           END-IF
           ADD LBWAIV-EXCEPT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE FLOAT YTD BY LOCKBOX CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2090-ACCUM-LOCKBOX.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBWAIV-LOCKBOX-CD
                   ADD LBWAIV-FLOAT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBWAIV-LOCKBOX-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBWAIV-FLOAT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH ON HAND YTD.
      *----------------------------------------------------------------*
       2100-PRORATE-CASH-ON-HAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBWAIV-CASH-ON-HAND-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBWAIV-CASH-ON-HAND-YTD
           END-IF
           ADD LBWAIV-CASH-ON-HAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2110-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF LBWAIV-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBWAIV-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO LBWAIV-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2120-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF LBWAIV-LAST-PAY-DATE > 0 AND LBWAIV-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBWAIV-CHECK-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD LBWAIV-CHECK-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD LBWAIV-CHECK-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD LBWAIV-CHECK-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBWAIV-CHECK-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2130-SPLIT-FLOAT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE LBWAIV-FLOAT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOCKBOX CD:
      * DX -> JM WHEN LAST STMT DATE IS PAST
      * JM -> WW WHEN LAST STMT DATE IS PAST
      * WW -> XE WHEN LAST STMT DATE IS PAST
      * XE -> DX WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2140-SET-LOCKBOX.
           EVALUATE LBWAIV-LOCKBOX-CD
               WHEN 'DX'
                   IF LBWAIV-LAST-STMT-DATE > 0 AND
                       LBWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'JM' TO LBWAIV-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF LBWAIV-LAST-STMT-DATE > 0 AND
                       LBWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'WW' TO LBWAIV-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WW'
                   IF LBWAIV-LAST-STMT-DATE > 0 AND
                       LBWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'XE' TO LBWAIV-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XE'
                   IF LBWAIV-LAST-STMT-DATE > 0 AND
                       LBWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'DX' TO LBWAIV-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2150-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF LBWAIV-EXPIRY-DATE > 0 AND LBWAIV-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBWAIV-EXPIRY-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REMIT YTD.
      *----------------------------------------------------------------*
       2160-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBWAIV-REMIT-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBWAIV-REMIT-YTD
           END-IF
           ADD LBWAIV-REMIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2170-DESCRIBE-REGION.
           EVALUATE LBWAIV-REGION-CD
               WHEN 'DVP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FMZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KSL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NYK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E168 CONTACT REQUIRED
      * E342 CHECK YTD NEGATIVE
      * E962 CHECK YTD OVER 250000
      * E015 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2180-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBWAIV-CONTACT = SPACES
                   MOVE 'E168' TO WS-REJECT-CODE
               WHEN LBWAIV-CHECK-YTD < 0
                   MOVE 'E342' TO WS-REJECT-CODE
               WHEN LBWAIV-CHECK-YTD > 250000.00
                   MOVE 'E962' TO WS-REJECT-CODE
               WHEN LBWAIV-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E015' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2190-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBWAIV-REASON-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBWAIV-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2200-COUNT-PAYMENT.
           IF LBWAIV-PAYMENT-CNT < 9999999
               ADD 1 TO LBWAIV-PAYMENT-CNT
           END-IF
           IF LBWAIV-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO LBWAIV-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2210-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBWAIV-CHANNEL-CD
                   ADD LBWAIV-REMIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBWAIV-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBWAIV-REMIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GF -> UK WHEN LAST STMT DATE IS PAST
      * UK -> VR WHEN LAST STMT DATE IS PAST
      * VR -> XG WHEN LAST STMT DATE IS PAST
      * XG -> GF WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2220-SET-REASON.
           EVALUATE LBWAIV-REASON-CD
               WHEN 'GF'
                   IF LBWAIV-LAST-STMT-DATE > 0 AND
                       LBWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'UK' TO LBWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF LBWAIV-LAST-STMT-DATE > 0 AND
                       LBWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'VR' TO LBWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VR'
                   IF LBWAIV-LAST-STMT-DATE > 0 AND
                       LBWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'XG' TO LBWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XG'
                   IF LBWAIV-LAST-STMT-DATE > 0 AND
                       LBWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'GF' TO LBWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2230-LIMIT-EXCEPT.
           IF LBWAIV-EXCEPT-AMT < 100.00
               MOVE 100.00 TO LBWAIV-EXCEPT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBWAIV-EXCEPT-AMT > 10000.00
               MOVE 10000.00 TO LBWAIV-EXCEPT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GF -> UK WHEN EXPIRY DATE IS PAST
      * UK -> VR WHEN EXPIRY DATE IS PAST
      * VR -> XG WHEN EXPIRY DATE IS PAST
      * XG -> GF WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2240-SET-REASON.
           EVALUATE LBWAIV-REASON-CD
               WHEN 'GF'
                   IF LBWAIV-EXPIRY-DATE > 0 AND LBWAIV-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UK' TO LBWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF LBWAIV-EXPIRY-DATE > 0 AND LBWAIV-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VR' TO LBWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VR'
                   IF LBWAIV-EXPIRY-DATE > 0 AND LBWAIV-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XG' TO LBWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XG'
                   IF LBWAIV-EXPIRY-DATE > 0 AND LBWAIV-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GF' TO LBWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2250-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF LBWAIV-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO LBWAIV-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE CHECK YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2260-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN LBWAIV-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBWAIV-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = LBWAIV-CHECK-YTD *
                       LBWAIV-PENALTY-PCT / 200
               WHEN LBWAIV-CHANNEL-CD = 'B'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN LBWAIV-CHANNEL-CD = 'H'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = LBWAIV-CHECK-YTD *
                       LBWAIV-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FLOAT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2270-FLAG-FLOAT.
           IF LBWAIV-FLOAT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBWAIV-FLOAT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOCKBOX CD:
      * DX -> JM WHEN LAST STMT DATE IS PAST
      * JM -> WW WHEN LAST STMT DATE IS PAST
      * WW -> XE WHEN LAST STMT DATE IS PAST
      * XE -> DX WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2280-SET-LOCKBOX.
           EVALUATE LBWAIV-LOCKBOX-CD
               WHEN 'DX'
                   IF LBWAIV-LAST-STMT-DATE > 0 AND
                       LBWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'JM' TO LBWAIV-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF LBWAIV-LAST-STMT-DATE > 0 AND
                       LBWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'WW' TO LBWAIV-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WW'
                   IF LBWAIV-LAST-STMT-DATE > 0 AND
                       LBWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'XE' TO LBWAIV-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XE'
                   IF LBWAIV-LAST-STMT-DATE > 0 AND
                       LBWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'DX' TO LBWAIV-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CASH ON HAND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REMIT YTD.
      *----------------------------------------------------------------*
       2290-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBWAIV-REMIT-YTD ROUNDED =
                   LBWAIV-CASH-ON-HAND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBWAIV-REMIT-YTD
           END-IF
           ADD LBWAIV-REMIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2300-DESCRIBE-REASON.
           EVALUATE LBWAIV-REASON-CD
               WHEN 'GF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ON HAND YTD BY REASON CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2310-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBWAIV-REASON-CD
                   ADD LBWAIV-CASH-ON-HAND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBWAIV-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBWAIV-CASH-ON-HAND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHECK YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2320-ACCRUE-CHECK.
           IF LBWAIV-CHECK-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBWAIV-CHECK-YTD * LBWAIV-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO LBWAIV-FLOAT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2330-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBWAIV-REASON-CD
                   ADD LBWAIV-REMIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBWAIV-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBWAIV-REMIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2340-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * LBWAIV-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO LBWAIV-EXCEPT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2350-LIMIT-FLOAT.
           IF LBWAIV-FLOAT-YTD < 100.00
               MOVE 100.00 TO LBWAIV-FLOAT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBWAIV-FLOAT-YTD > 2500.00
               MOVE 2500.00 TO LBWAIV-FLOAT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2360-DESCRIBE-REASON.
           EVALUATE LBWAIV-REASON-CD
               WHEN 'GF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2370-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF LBWAIV-LAST-STMT-DATE > 0 AND LBWAIV-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBWAIV-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBWAIV-FLOAT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD LBWAIV-FLOAT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD LBWAIV-FLOAT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBWAIV-FLOAT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBWAIV-FLOAT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE LOCKBOX CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2380-DESCRIBE-LOCKBOX.
           EVALUATE LBWAIV-LOCKBOX-CD
               WHEN 'DX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2390-COUNT-PAYMENT.
           IF LBWAIV-PAYMENT-CNT < 9999999
               ADD 1 TO LBWAIV-PAYMENT-CNT
           END-IF
           IF LBWAIV-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO LBWAIV-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2400-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF LBWAIV-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBWAIV-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO LBWAIV-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DVP -> FMZ WHEN LAST STMT DATE IS PAST
      * FMZ -> KSL WHEN LAST STMT DATE IS PAST
      * KSL -> NYK WHEN LAST STMT DATE IS PAST
      * NYK -> DVP WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2410-SET-REGION.
           EVALUATE LBWAIV-REGION-CD
               WHEN 'DVP'
                   IF LBWAIV-LAST-STMT-DATE > 0 AND
                       LBWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'FMZ' TO LBWAIV-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FMZ'
                   IF LBWAIV-LAST-STMT-DATE > 0 AND
                       LBWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'KSL' TO LBWAIV-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KSL'
                   IF LBWAIV-LAST-STMT-DATE > 0 AND
                       LBWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'NYK' TO LBWAIV-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NYK'
                   IF LBWAIV-LAST-STMT-DATE > 0 AND
                       LBWAIV-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'DVP' TO LBWAIV-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2420-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF LBWAIV-LAST-STMT-DATE > 0 AND LBWAIV-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBWAIV-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBWAIV-CHECK-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD LBWAIV-CHECK-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD LBWAIV-CHECK-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD LBWAIV-CHECK-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBWAIV-CHECK-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FLOAT YTD.
      *----------------------------------------------------------------*
       2430-PRORATE-FLOAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBWAIV-FLOAT-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBWAIV-FLOAT-YTD
           END-IF
           ADD LBWAIV-FLOAT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2440-COUNT-PAYMENT.
           IF LBWAIV-PAYMENT-CNT < 9999999
               ADD 1 TO LBWAIV-PAYMENT-CNT
           END-IF
           IF LBWAIV-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO LBWAIV-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH ON HAND YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2450-ACCRUE-CASH-ON-HAND.
           IF LBWAIV-CASH-ON-HAND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBWAIV-CASH-ON-HAND-YTD * LBWAIV-PENALTY-PCT / 100 /
                       360
               ADD WS-WORK-AMT TO LBWAIV-FLOAT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GF -> UK WHEN EXPIRY DATE IS PAST
      * UK -> VR WHEN EXPIRY DATE IS PAST
      * VR -> XG WHEN EXPIRY DATE IS PAST
      * XG -> GF WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2460-SET-REASON.
           EVALUATE LBWAIV-REASON-CD
               WHEN 'GF'
                   IF LBWAIV-EXPIRY-DATE > 0 AND LBWAIV-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UK' TO LBWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF LBWAIV-EXPIRY-DATE > 0 AND LBWAIV-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VR' TO LBWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VR'
                   IF LBWAIV-EXPIRY-DATE > 0 AND LBWAIV-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XG' TO LBWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XG'
                   IF LBWAIV-EXPIRY-DATE > 0 AND LBWAIV-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GF' TO LBWAIV-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2470-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-SCHED-RATE(1) TO LBWAIV-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-FLOAT-SCHED-LOW(WS-TX)
                   MOVE WS-FLOAT-SCHED-RATE(WS-TX) TO LBWAIV-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBWAIV-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBWAIV-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E634 CUST ID REQUIRED
      * E640 REMIT YTD NEGATIVE
      * E081 REMIT YTD OVER 99999
      * E148 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2480-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBWAIV-CUST-ID = SPACES
                   MOVE 'E634' TO WS-REJECT-CODE
               WHEN LBWAIV-REMIT-YTD < 0
                   MOVE 'E640' TO WS-REJECT-CODE
               WHEN LBWAIV-REMIT-YTD > 99999.00
                   MOVE 'E081' TO WS-REJECT-CODE
               WHEN LBWAIV-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E148' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2490-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF LBWAIV-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO LBWAIV-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT BAND TIER FOR THE CASH ON HAND YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2500-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-BAND-RATE(1) TO LBWAIV-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF LBWAIV-CASH-ON-HAND-YTD NOT <
                   WS-EXCEPT-BAND-LOW(WS-TX)
                   MOVE WS-EXCEPT-BAND-RATE(WS-TX) TO LBWAIV-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBWAIV-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBWAIV-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2510-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF LBWAIV-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBWAIV-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO LBWAIV-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E046 CONTACT REQUIRED
      * E299 WORK AMT NEGATIVE
      * E583 WORK AMT OVER 10000
      * E197 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2520-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBWAIV-CONTACT = SPACES
                   MOVE 'E046' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E299' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E583' TO WS-REJECT-CODE
               WHEN LBWAIV-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E197' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2530-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF LBWAIV-LAST-STMT-DATE > 0 AND LBWAIV-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBWAIV-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E701 CUST ID REQUIRED
      * E888 EXCEPT AMT NEGATIVE
      * E434 EXCEPT AMT OVER 25000
      * E447 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2540-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBWAIV-CUST-ID = SPACES
                   MOVE 'E701' TO WS-REJECT-CODE
               WHEN LBWAIV-EXCEPT-AMT < 0
                   MOVE 'E888' TO WS-REJECT-CODE
               WHEN LBWAIV-EXCEPT-AMT > 25000.00
                   MOVE 'E434' TO WS-REJECT-CODE
               WHEN LBWAIV-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2550-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF LBWAIV-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBWAIV-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO LBWAIV-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FLOAT YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2560-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBWAIV-REGION-CD
                   ADD LBWAIV-FLOAT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBWAIV-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBWAIV-FLOAT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2570-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = LBWAIV-EXCEPT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = LBWAIV-EXCEPT-AMT - (WS-WORK-AMT * 3)
           COMPUTE LBWAIV-REMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REMIT YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2580-FLAG-REMIT.
           IF LBWAIV-REMIT-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE LBWAIV-REMIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2590-COUNT-EXCEPT.
           IF LBWAIV-EXCEPT-CNT < 9999999
               ADD 1 TO LBWAIV-EXCEPT-CNT
           END-IF
           IF LBWAIV-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO LBWAIV-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CHECK YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2600-LIMIT-CHECK.
           IF LBWAIV-CHECK-YTD < 0.00
               MOVE 0.00 TO LBWAIV-CHECK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBWAIV-CHECK-YTD > 50000.00
               MOVE 50000.00 TO LBWAIV-CHECK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2610-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF LBWAIV-EXPIRY-DATE > 0 AND LBWAIV-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBWAIV-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBWAIV-FLOAT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD LBWAIV-FLOAT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD LBWAIV-FLOAT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBWAIV-FLOAT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBWAIV-FLOAT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2620-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF LBWAIV-LAST-PAY-DATE > 0 AND LBWAIV-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBWAIV-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBWAIV-FLOAT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD LBWAIV-FLOAT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD LBWAIV-FLOAT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD LBWAIV-FLOAT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBWAIV-FLOAT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2630-DESCRIBE-REASON.
           EVALUATE LBWAIV-REASON-CD
               WHEN 'GF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E737 CONTACT REQUIRED
      * E526 WORK AMT NEGATIVE
      * E577 WORK AMT OVER 5000
      * E670 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2640-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBWAIV-CONTACT = SPACES
                   MOVE 'E737' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E526' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E577' TO WS-REJECT-CODE
               WHEN LBWAIV-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E670' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2650-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * LBWAIV-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO LBWAIV-FLOAT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHECK YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2660-ACCRUE-CHECK.
           IF LBWAIV-CHECK-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBWAIV-CHECK-YTD * LBWAIV-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO LBWAIV-FLOAT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH ON HAND YTD.
      *----------------------------------------------------------------*
       2670-PRORATE-CASH-ON-HAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBWAIV-CASH-ON-HAND-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBWAIV-CASH-ON-HAND-YTD
           END-IF
           ADD LBWAIV-CASH-ON-HAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FLOAT AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2680-LIMIT-FLOAT.
           IF LBWAIV-FLOAT-AMT < 25.00
               MOVE 25.00 TO LBWAIV-FLOAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBWAIV-FLOAT-AMT > 500.00
               MOVE 500.00 TO LBWAIV-FLOAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT SCHED TIER FOR THE CASH ON HAND YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2690-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-SCHED-RATE(1) TO LBWAIV-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF LBWAIV-CASH-ON-HAND-YTD NOT <
                   WS-FLOAT-SCHED-LOW(WS-TX)
                   MOVE WS-FLOAT-SCHED-RATE(WS-TX) TO LBWAIV-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBWAIV-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBWAIV-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2700-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF LBWAIV-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBWAIV-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO LBWAIV-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2710-LIMIT-FLOAT.
           IF LBWAIV-FLOAT-YTD < 100.00
               MOVE 100.00 TO LBWAIV-FLOAT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBWAIV-FLOAT-YTD > 10000.00
               MOVE 10000.00 TO LBWAIV-FLOAT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FLOAT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2720-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBWAIV-REASON-CD
                   ADD LBWAIV-FLOAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBWAIV-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBWAIV-FLOAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2730-DESCRIBE-REASON.
           EVALUATE LBWAIV-REASON-CD
               WHEN 'GF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ON HAND YTD BY REGION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2740-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBWAIV-REGION-CD
                   ADD LBWAIV-CASH-ON-HAND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBWAIV-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBWAIV-CASH-ON-HAND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2750-COUNT-EXCEPT.
           IF LBWAIV-EXCEPT-CNT < 9999999
               ADD 1 TO LBWAIV-EXCEPT-CNT
           END-IF
           IF LBWAIV-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO LBWAIV-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REMIT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2760-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = LBWAIV-REMIT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = LBWAIV-REMIT-YTD - (WS-WORK-AMT * 3)
           COMPUTE LBWAIV-EXCEPT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FLOAT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2770-PRORATE-FLOAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBWAIV-FLOAT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE FLOAT AMT BY LOCKBOX CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2780-ACCUM-LOCKBOX.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBWAIV-LOCKBOX-CD
                   ADD LBWAIV-FLOAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBWAIV-LOCKBOX-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBWAIV-FLOAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ON HAND YTD FEE ON THE FLOAT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2790-ASSESS-STOP.
           EVALUATE TRUE
               WHEN LBWAIV-STOP-FLAG = 'Y'
                   MOVE ZERO TO LBWAIV-CASH-ON-HAND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBWAIV-PREFERRED-FLAG = 'Y'
                   COMPUTE LBWAIV-CASH-ON-HAND-YTD ROUNDED =
                       LBWAIV-FLOAT-AMT * LBWAIV-PENALTY-PCT / 200
               WHEN LBWAIV-CHANNEL-CD = 'B'
                   MOVE 5.00 TO LBWAIV-CASH-ON-HAND-YTD
               WHEN LBWAIV-CHANNEL-CD = 'H'
                   MOVE 12.00 TO LBWAIV-CASH-ON-HAND-YTD
               WHEN OTHER
                   COMPUTE LBWAIV-CASH-ON-HAND-YTD ROUNDED =
                       LBWAIV-FLOAT-AMT * LBWAIV-PENALTY-PCT / 100
           END-EVALUATE
           IF LBWAIV-CASH-ON-HAND-YTD > 0 AND LBWAIV-CASH-ON-HAND-YTD <
               5.00
               MOVE 5.00 TO LBWAIV-CASH-ON-HAND-YTD
           END-IF
           IF LBWAIV-CASH-ON-HAND-YTD > 75.00
               MOVE 75.00 TO LBWAIV-CASH-ON-HAND-YTD
           END-IF
           ADD LBWAIV-CASH-ON-HAND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2800-COUNT-EXCEPT.
           IF LBWAIV-EXCEPT-CNT < 9999999
               ADD 1 TO LBWAIV-EXCEPT-CNT
           END-IF
           IF LBWAIV-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO LBWAIV-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2810-LIMIT-FLOAT.
           IF LBWAIV-FLOAT-YTD < 10.00
               MOVE 10.00 TO LBWAIV-FLOAT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBWAIV-FLOAT-YTD > 10000.00
               MOVE 10000.00 TO LBWAIV-FLOAT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2820-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REMIT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2830-FLAG-REMIT.
           IF LBWAIV-REMIT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBWAIV-REMIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2840-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF LBWAIV-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBWAIV-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO LBWAIV-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FLOAT AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2850-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBWAIV-REGION-CD
                   ADD LBWAIV-FLOAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBWAIV-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBWAIV-FLOAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2860-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = LBWAIV-EXCEPT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = LBWAIV-EXCEPT-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH ON HAND YTD.
      *----------------------------------------------------------------*
       2870-PRORATE-CASH-ON-HAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBWAIV-CASH-ON-HAND-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBWAIV-CASH-ON-HAND-YTD
           END-IF
           ADD LBWAIV-CASH-ON-HAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2880-COUNT-PAYMENT.
           IF LBWAIV-PAYMENT-CNT < 9999999
               ADD 1 TO LBWAIV-PAYMENT-CNT
           END-IF
           IF LBWAIV-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO LBWAIV-STOP-FLAG
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO LBWAIV-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO LBWAIV-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO LBWAIV-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'LBWAIV EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
