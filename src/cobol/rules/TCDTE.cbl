      *================================================================*
      * PROGRAM  : TCDTE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : TRAVELERS CHEQUES RULE - LIMITS. CALLED BY THE      *
      *            TRAVELERS CHEQUES BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-03-23 CRP  ORIGINAL                                       *
      * 1993-08-21 DPS  COMMISSION RERUN PROTECTION CHG5324            *
      * 1994-06-15 RJT  FACE EXCEPTIONS TO OPS QUEUE PRB9308           *
      * 1997-04-12 RJT  SHORTAGE EXCEPTIONS TO OPS QUEUE INC4405       *
      * 2002-06-22 EWS  FACE LIMIT REVISED REQ0544                     *
      * 2005-01-12 BHS  FACE FEE SCHEDULE UPDATE PRB3790               *
      * 2009-05-03 JLM  COMMISSION STATUS CODES ADDED REQ4909          *
      * 2011-04-13 RJT  SHORTAGE TABLE EXPANDED CHG9650                *
      * 2012-06-20 MKL  NEW FACE REPORT COLUMNS REQ3468                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCDTE.
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
      * WS-COMMISSI-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COMMISSI-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000233000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000392000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000434000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000885000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000498000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025000000000123500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025100000000799000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025200000000319500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025450000000246000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027950000000692000'.
       01  WS-COMMISSI-TIER-TBL REDEFINES WS-COMMISSI-TIER-VALUES.
           05  WS-COMMISSI-TIER-ROW OCCURS 10 TIMES.
               10  WS-COMMISSI-TIER-LOW    PIC 9(09)V99.
               10  WS-COMMISSI-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONSIGN-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONSIGN-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000752000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000106500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000643500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000372000'.
       01  WS-CONSIGN-BAND-TBL REDEFINES WS-CONSIGN-BAND-VALUES.
           05  WS-CONSIGN-BAND-ROW OCCURS 4 TIMES.
               10  WS-CONSIGN-BAND-LOW     PIC 9(09)V99.
               10  WS-CONSIGN-BAND-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY TCDTEP.

       PROCEDURE DIVISION USING TCDTE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-PRORATE-SHORTAGE
           PERFORM 2010-COUNT-EXCEPT
           PERFORM 2020-ACCRUE-SHORTAGE
           PERFORM 2030-PRORATE-SHORTAGE
           PERFORM 2040-ACCUM-BRANCH
           PERFORM 2050-DESCRIBE-CYCLE
           PERFORM 2060-SELECT-RATE
           PERFORM 2070-DESCRIBE-CYCLE
           PERFORM 2080-EDIT-REF-NO
           PERFORM 2090-PRORATE-COMMISSION
           PERFORM 2100-EDIT-REF-NO
           PERFORM 2110-AGE-CLOSE
           PERFORM 2120-AGE-CLOSE
           PERFORM 2130-ACCUM-BRANCH
           PERFORM 2140-DESCRIBE-ISSUER
           PERFORM 2150-SELECT-RATE
           PERFORM 2160-FLAG-ITEM
           PERFORM 2170-SELECT-RATE
           PERFORM 2180-EDIT-REF-NO
           PERFORM 2190-DESCRIBE-CYCLE
           PERFORM 2200-PRORATE-CONSIGN
           PERFORM 2210-ASSESS-STOP
           PERFORM 2220-DESCRIBE-BRANCH
           PERFORM 2230-FLAG-ITEM
           PERFORM 2240-DESCRIBE-STATUS
           PERFORM 2250-COUNT-EXCEPT
           PERFORM 2260-AGE-MATURITY
           PERFORM 2270-PRORATE-REFUND
           PERFORM 2280-ACCRUE-RATE
           PERFORM 2290-ASSESS-REVIEW
           PERFORM 2300-ACCRUE-CONSIGN
           PERFORM 2310-SELECT-RATE
           PERFORM 2320-ACCUM-ISSUER
           PERFORM 2330-CHECK-MATURITY
           PERFORM 2340-SELECT-RATE
           PERFORM 2350-ACCUM-BRANCH
           PERFORM 2360-CHECK-CLOSE
           PERFORM 2370-COUNT-ITEM
           PERFORM 2380-FLAG-COMMISSION
           PERFORM 2390-AGE-MATURITY
           PERFORM 2400-FLAG-REFUND
           PERFORM 2410-COUNT-EXCEPT
           PERFORM 2420-ACCRUE-COMMISSION
           PERFORM 2430-ACCRUE-REFUND
           PERFORM 2440-SELECT-MARGIN
           PERFORM 2450-PRORATE-REFUND
           PERFORM 2460-SPLIT-COMMISSION
           PERFORM 2470-SET-BRANCH
           PERFORM 2480-SELECT-MARGIN
           PERFORM 2490-ASSESS-OVERRIDE
           PERFORM 2500-SET-CYCLE
           PERFORM 2510-ASSESS-REVIEW
           PERFORM 2520-ASSESS-REVIEW
           PERFORM 2530-FLAG-COMMISSION
           PERFORM 2540-AGE-CLOSE
           PERFORM 2550-ACCUM-BRANCH
           PERFORM 2560-LIMIT-ITEM
           PERFORM 2570-CHECK-CLOSE
           PERFORM 2580-FLAG-ITEM
           PERFORM 2590-SPLIT-REFUND
           PERFORM 2600-DESCRIBE-BRANCH
           PERFORM 2610-SPLIT-REFUND
           PERFORM 2620-SELECT-MARGIN
           PERFORM 2630-SET-CYCLE
           PERFORM 2640-SET-BRANCH
           PERFORM 2650-CHECK-LAST-ACT
           PERFORM 2660-DESCRIBE-CYCLE
           PERFORM 2670-CHECK-CLOSE
           PERFORM 2680-ASSESS-REVIEW
           PERFORM 2690-FLAG-SHORTAGE
           PERFORM 2700-FLAG-ITEM
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO TCDTE-RETURN-CODE.

      *----------------------------------------------------------------*
      * PRORATE COMMISSION YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHORTAGE YTD.
      *----------------------------------------------------------------*
       2000-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TCDTE-SHORTAGE-YTD ROUNDED =
                   TCDTE-COMMISSION-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TCDTE-SHORTAGE-YTD
           END-IF
           ADD TCDTE-SHORTAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2010-COUNT-EXCEPT.
           IF TCDTE-EXCEPT-CNT < 9999999
               ADD 1 TO TCDTE-EXCEPT-CNT
           END-IF
           IF TCDTE-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO TCDTE-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2020-ACCRUE-SHORTAGE.
           IF TCDTE-SHORTAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TCDTE-SHORTAGE-YTD * TCDTE-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHORTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2030-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TCDTE-SHORTAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE CONSIGN AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2040-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TCDTE-BRANCH-CD
                   ADD TCDTE-CONSIGN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TCDTE-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TCDTE-CONSIGN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2050-DESCRIBE-CYCLE.
           EVALUATE TCDTE-CYCLE-CD
               WHEN 'EPW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HHK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JCB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UCB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XTY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XUV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XXU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONSIGN BAND TIER FOR THE COMMISSION YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2060-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONSIGN-BAND-RATE(1) TO TCDTE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TCDTE-COMMISSION-YTD NOT < WS-CONSIGN-BAND-LOW(WS-TX)
                   MOVE WS-CONSIGN-BAND-RATE(WS-TX) TO TCDTE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCDTE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCDTE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2070-DESCRIBE-CYCLE.
           EVALUATE TCDTE-CYCLE-CD
               WHEN 'EPW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HHK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JCB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UCB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XTY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XUV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XXU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E293 REF NO REQUIRED
      * E618 CONSIGN AMT NEGATIVE
      * E734 CONSIGN AMT OVER 1000000
      * E156 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2080-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TCDTE-REF-NO = SPACES
                   MOVE 'E293' TO WS-REJECT-CODE
               WHEN TCDTE-CONSIGN-AMT < 0
                   MOVE 'E618' TO WS-REJECT-CODE
               WHEN TCDTE-CONSIGN-AMT > 1000000.00
                   MOVE 'E734' TO WS-REJECT-CODE
               WHEN TCDTE-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E156' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REFUND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COMMISSION YTD.
      *----------------------------------------------------------------*
       2090-PRORATE-COMMISSION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TCDTE-COMMISSION-YTD ROUNDED =
                   TCDTE-REFUND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TCDTE-COMMISSION-YTD
           END-IF
           ADD TCDTE-COMMISSION-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E138 REF NO REQUIRED
      * E893 CONSIGN AMT NEGATIVE
      * E004 CONSIGN AMT OVER 250000
      * E228 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2100-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TCDTE-REF-NO = SPACES
                   MOVE 'E138' TO WS-REJECT-CODE
               WHEN TCDTE-CONSIGN-AMT < 0
                   MOVE 'E893' TO WS-REJECT-CODE
               WHEN TCDTE-CONSIGN-AMT > 250000.00
                   MOVE 'E004' TO WS-REJECT-CODE
               WHEN TCDTE-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E228' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2110-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TCDTE-CLOSE-DATE > 0 AND TCDTE-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCDTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TCDTE-CONSIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TCDTE-CONSIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TCDTE-CONSIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TCDTE-CONSIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TCDTE-CONSIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2120-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TCDTE-CLOSE-DATE > 0 AND TCDTE-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCDTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TCDTE-REFUND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TCDTE-REFUND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TCDTE-REFUND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TCDTE-REFUND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TCDTE-REFUND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SHORTAGE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2130-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TCDTE-BRANCH-CD
                   ADD TCDTE-SHORTAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TCDTE-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TCDTE-SHORTAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2140-DESCRIBE-ISSUER.
           EVALUATE TCDTE-ISSUER-CD
               WHEN 'AF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COMMISSI TIER TIER FOR THE REFUND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2150-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COMMISSI-TIER-RATE(1) TO TCDTE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TCDTE-REFUND-AMT NOT < WS-COMMISSI-TIER-LOW(WS-TX)
                   MOVE WS-COMMISSI-TIER-RATE(WS-TX) TO TCDTE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCDTE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCDTE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2160-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COMMISSI TIER TIER FOR THE SHORTAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2170-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COMMISSI-TIER-RATE(1) TO TCDTE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TCDTE-SHORTAGE-AMT NOT < WS-COMMISSI-TIER-LOW(WS-TX)
                   MOVE WS-COMMISSI-TIER-RATE(WS-TX) TO TCDTE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCDTE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCDTE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E819 REF NO REQUIRED
      * E240 SHORTAGE AMT NEGATIVE
      * E814 SHORTAGE AMT OVER 5000
      * E926 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2180-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TCDTE-REF-NO = SPACES
                   MOVE 'E819' TO WS-REJECT-CODE
               WHEN TCDTE-SHORTAGE-AMT < 0
                   MOVE 'E240' TO WS-REJECT-CODE
               WHEN TCDTE-SHORTAGE-AMT > 5000.00
                   MOVE 'E814' TO WS-REJECT-CODE
               WHEN TCDTE-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E926' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2190-DESCRIBE-CYCLE.
           EVALUATE TCDTE-CYCLE-CD
               WHEN 'EPW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HHK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JCB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UCB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XTY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XUV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XXU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REFUND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONSIGN AMT.
      *----------------------------------------------------------------*
       2200-PRORATE-CONSIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TCDTE-CONSIGN-AMT ROUNDED =
                   TCDTE-REFUND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TCDTE-CONSIGN-AMT
           END-IF
           ADD TCDTE-CONSIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE COMMISSION YTD FEE ON THE SHORTAGE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2210-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TCDTE-STOP-FLAG = 'Y'
                   MOVE ZERO TO TCDTE-COMMISSION-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TCDTE-OVERRIDE-FLAG = 'Y'
                   COMPUTE TCDTE-COMMISSION-YTD ROUNDED =
                       TCDTE-SHORTAGE-YTD * TCDTE-RATE / 200
               WHEN TCDTE-CYCLE-CD = 'EPW'
                   MOVE 3.00 TO TCDTE-COMMISSION-YTD
               WHEN TCDTE-CYCLE-CD = 'HHK'
                   MOVE 15.00 TO TCDTE-COMMISSION-YTD
               WHEN OTHER
                   COMPUTE TCDTE-COMMISSION-YTD ROUNDED =
                       TCDTE-SHORTAGE-YTD * TCDTE-RATE / 100
           END-EVALUATE
           IF TCDTE-COMMISSION-YTD > 0 AND TCDTE-COMMISSION-YTD < 2.00
               MOVE 2.00 TO TCDTE-COMMISSION-YTD
           END-IF
           IF TCDTE-COMMISSION-YTD > 100.00
               MOVE 100.00 TO TCDTE-COMMISSION-YTD
           END-IF
           ADD TCDTE-COMMISSION-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2220-DESCRIBE-BRANCH.
           EVALUATE TCDTE-BRANCH-CD
               WHEN 'EG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2230-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2240-DESCRIBE-STATUS.
           EVALUATE TCDTE-STATUS-CD
               WHEN 'HK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2250-COUNT-EXCEPT.
           IF TCDTE-EXCEPT-CNT < 9999999
               ADD 1 TO TCDTE-EXCEPT-CNT
           END-IF
           IF TCDTE-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO TCDTE-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2260-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TCDTE-MATURITY-DATE > 0 AND TCDTE-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCDTE-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REFUND YTD.
      *----------------------------------------------------------------*
       2270-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TCDTE-REFUND-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TCDTE-REFUND-YTD
           END-IF
           ADD TCDTE-REFUND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2280-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TCDTE-RATE / 100 / 365
               ADD WS-WORK-AMT TO TCDTE-CONSIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2290-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TCDTE-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TCDTE-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       TCDTE-RATE / 200
               WHEN TCDTE-STATUS-CD = 'HK'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN TCDTE-STATUS-CD = 'RU'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       TCDTE-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONSIGN AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2300-ACCRUE-CONSIGN.
           IF TCDTE-CONSIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TCDTE-CONSIGN-AMT * TCDTE-RATE / 100 / 365
               ADD WS-WORK-AMT TO TCDTE-REFUND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COMMISSI TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2310-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COMMISSI-TIER-RATE(1) TO TCDTE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-COMMISSI-TIER-LOW(WS-TX)
                   MOVE WS-COMMISSI-TIER-RATE(WS-TX) TO TCDTE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCDTE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCDTE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND YTD BY ISSUER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2320-ACCUM-ISSUER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TCDTE-ISSUER-CD
                   ADD TCDTE-REFUND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TCDTE-ISSUER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TCDTE-REFUND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2330-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TCDTE-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCDTE-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TCDTE-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONSIGN BAND TIER FOR THE CONSIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2340-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONSIGN-BAND-RATE(1) TO TCDTE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TCDTE-CONSIGN-AMT NOT < WS-CONSIGN-BAND-LOW(WS-TX)
                   MOVE WS-CONSIGN-BAND-RATE(WS-TX) TO TCDTE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCDTE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCDTE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONSIGN AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2350-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TCDTE-BRANCH-CD
                   ADD TCDTE-CONSIGN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TCDTE-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TCDTE-CONSIGN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2360-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TCDTE-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCDTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TCDTE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2370-COUNT-ITEM.
           IF TCDTE-ITEM-CNT < 9999999
               ADD 1 TO TCDTE-ITEM-CNT
           END-IF
           IF TCDTE-ITEM-CNT NOT < 6
               MOVE 'Y' TO TCDTE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * COMMISSION YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2380-FLAG-COMMISSION.
           IF TCDTE-COMMISSION-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TCDTE-COMMISSION-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2390-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TCDTE-MATURITY-DATE > 0 AND TCDTE-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCDTE-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TCDTE-CONSIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TCDTE-CONSIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TCDTE-CONSIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TCDTE-CONSIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TCDTE-CONSIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REFUND YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2400-FLAG-REFUND.
           IF TCDTE-REFUND-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TCDTE-REFUND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2410-COUNT-EXCEPT.
           IF TCDTE-EXCEPT-CNT < 9999999
               ADD 1 TO TCDTE-EXCEPT-CNT
           END-IF
           IF TCDTE-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO TCDTE-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COMMISSION YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2420-ACCRUE-COMMISSION.
           IF TCDTE-COMMISSION-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TCDTE-COMMISSION-YTD * TCDTE-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2430-ACCRUE-REFUND.
           IF TCDTE-REFUND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TCDTE-REFUND-YTD * TCDTE-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TCDTE-CONSIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COMMISSI TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2440-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COMMISSI-TIER-RATE(1) TO TCDTE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-COMMISSI-TIER-LOW(WS-TX)
                   MOVE WS-COMMISSI-TIER-RATE(WS-TX) TO TCDTE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCDTE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCDTE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONSIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REFUND YTD.
      *----------------------------------------------------------------*
       2450-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TCDTE-REFUND-YTD ROUNDED =
                   TCDTE-CONSIGN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TCDTE-REFUND-YTD
           END-IF
           ADD TCDTE-REFUND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2460-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE TCDTE-COMMISSION-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EG -> HZ WHEN LAST ACT DATE IS PAST
      * HZ -> JF WHEN LAST ACT DATE IS PAST
      * JF -> NZ WHEN LAST ACT DATE IS PAST
      * NZ -> EG WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2470-SET-BRANCH.
           EVALUATE TCDTE-BRANCH-CD
               WHEN 'EG'
                   IF TCDTE-LAST-ACT-DATE > 0 AND TCDTE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HZ' TO TCDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HZ'
                   IF TCDTE-LAST-ACT-DATE > 0 AND TCDTE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JF' TO TCDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF TCDTE-LAST-ACT-DATE > 0 AND TCDTE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO TCDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF TCDTE-LAST-ACT-DATE > 0 AND TCDTE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EG' TO TCDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COMMISSI TIER TIER FOR THE CONSIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2480-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COMMISSI-TIER-RATE(1) TO TCDTE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TCDTE-CONSIGN-AMT NOT < WS-COMMISSI-TIER-LOW(WS-TX)
                   MOVE WS-COMMISSI-TIER-RATE(WS-TX) TO TCDTE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCDTE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCDTE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHORTAGE FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2490-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TCDTE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TCDTE-SHORTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TCDTE-REVIEW-FLAG = 'Y'
                   COMPUTE TCDTE-SHORTAGE-AMT ROUNDED = WS-WORK-AMT *
                       TCDTE-RATE / 200
               WHEN TCDTE-ISSUER-CD = 'AF'
                   MOVE 20.00 TO TCDTE-SHORTAGE-AMT
               WHEN TCDTE-ISSUER-CD = 'CJ'
                   MOVE 10.00 TO TCDTE-SHORTAGE-AMT
               WHEN OTHER
                   COMPUTE TCDTE-SHORTAGE-AMT ROUNDED = WS-WORK-AMT *
                       TCDTE-RATE / 100
           END-EVALUATE
           IF TCDTE-SHORTAGE-AMT > 0 AND TCDTE-SHORTAGE-AMT < 5.00
               MOVE 5.00 TO TCDTE-SHORTAGE-AMT
           END-IF
           IF TCDTE-SHORTAGE-AMT > 250.00
               MOVE 250.00 TO TCDTE-SHORTAGE-AMT
           END-IF
           ADD TCDTE-SHORTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EPW -> HHK WHEN MATURITY DATE IS PAST
      * HHK -> JCB WHEN MATURITY DATE IS PAST
      * JCB -> UCB WHEN MATURITY DATE IS PAST
      * UCB -> XTY WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2500-SET-CYCLE.
           EVALUATE TCDTE-CYCLE-CD
               WHEN 'EPW'
                   IF TCDTE-MATURITY-DATE > 0 AND TCDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HHK' TO TCDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HHK'
                   IF TCDTE-MATURITY-DATE > 0 AND TCDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JCB' TO TCDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JCB'
                   IF TCDTE-MATURITY-DATE > 0 AND TCDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UCB' TO TCDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UCB'
                   IF TCDTE-MATURITY-DATE > 0 AND TCDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XTY' TO TCDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SHORTAGE FEE ON THE SHORTAGE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2510-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TCDTE-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TCDTE-SHORTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TCDTE-STOP-FLAG = 'Y'
                   COMPUTE TCDTE-SHORTAGE-AMT ROUNDED =
                       TCDTE-SHORTAGE-YTD * TCDTE-RATE / 200
               WHEN TCDTE-BRANCH-CD = 'EG'
                   MOVE 20.00 TO TCDTE-SHORTAGE-AMT
               WHEN TCDTE-BRANCH-CD = 'HZ'
                   MOVE 12.00 TO TCDTE-SHORTAGE-AMT
               WHEN OTHER
                   COMPUTE TCDTE-SHORTAGE-AMT ROUNDED =
                       TCDTE-SHORTAGE-YTD * TCDTE-RATE / 100
           END-EVALUATE
           IF TCDTE-SHORTAGE-AMT > 0 AND TCDTE-SHORTAGE-AMT < 10.00
               MOVE 10.00 TO TCDTE-SHORTAGE-AMT
           END-IF
           IF TCDTE-SHORTAGE-AMT > 25.00
               MOVE 25.00 TO TCDTE-SHORTAGE-AMT
           END-IF
           ADD TCDTE-SHORTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2520-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TCDTE-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TCDTE-REFUND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TCDTE-STOP-FLAG = 'Y'
                   COMPUTE TCDTE-REFUND-YTD ROUNDED = WS-WORK-AMT-2 *
                       TCDTE-RATE / 200
               WHEN TCDTE-BRANCH-CD = 'EG'
                   MOVE 10.00 TO TCDTE-REFUND-YTD
               WHEN TCDTE-BRANCH-CD = 'HZ'
                   MOVE 3.00 TO TCDTE-REFUND-YTD
               WHEN OTHER
                   COMPUTE TCDTE-REFUND-YTD ROUNDED = WS-WORK-AMT-2 *
                       TCDTE-RATE / 100
           END-EVALUATE
           IF TCDTE-REFUND-YTD > 0 AND TCDTE-REFUND-YTD < 5.00
               MOVE 5.00 TO TCDTE-REFUND-YTD
           END-IF
           IF TCDTE-REFUND-YTD > 75.00
               MOVE 75.00 TO TCDTE-REFUND-YTD
           END-IF
           ADD TCDTE-REFUND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * COMMISSION YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2530-FLAG-COMMISSION.
           IF TCDTE-COMMISSION-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TCDTE-COMMISSION-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2540-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TCDTE-CLOSE-DATE > 0 AND TCDTE-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCDTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2550-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TCDTE-BRANCH-CD
                   ADD TCDTE-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TCDTE-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TCDTE-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2560-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2570-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TCDTE-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCDTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TCDTE-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2580-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2590-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE TCDTE-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2600-DESCRIBE-BRANCH.
           EVALUATE TCDTE-BRANCH-CD
               WHEN 'EG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2610-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE TCDTE-REFUND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONSIGN BAND TIER FOR THE COMMISSION YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2620-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONSIGN-BAND-RATE(1) TO TCDTE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TCDTE-COMMISSION-YTD NOT < WS-CONSIGN-BAND-LOW(WS-TX)
                   MOVE WS-CONSIGN-BAND-RATE(WS-TX) TO TCDTE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCDTE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCDTE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EPW -> HHK WHEN CLOSE DATE IS PAST
      * HHK -> JCB WHEN CLOSE DATE IS PAST
      * JCB -> UCB WHEN CLOSE DATE IS PAST
      * UCB -> XTY WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2630-SET-CYCLE.
           EVALUATE TCDTE-CYCLE-CD
               WHEN 'EPW'
                   IF TCDTE-CLOSE-DATE > 0 AND TCDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HHK' TO TCDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HHK'
                   IF TCDTE-CLOSE-DATE > 0 AND TCDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JCB' TO TCDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JCB'
                   IF TCDTE-CLOSE-DATE > 0 AND TCDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UCB' TO TCDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UCB'
                   IF TCDTE-CLOSE-DATE > 0 AND TCDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XTY' TO TCDTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EG -> HZ WHEN CLOSE DATE IS PAST
      * HZ -> JF WHEN CLOSE DATE IS PAST
      * JF -> NZ WHEN CLOSE DATE IS PAST
      * NZ -> EG WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2640-SET-BRANCH.
           EVALUATE TCDTE-BRANCH-CD
               WHEN 'EG'
                   IF TCDTE-CLOSE-DATE > 0 AND TCDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HZ' TO TCDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HZ'
                   IF TCDTE-CLOSE-DATE > 0 AND TCDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JF' TO TCDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF TCDTE-CLOSE-DATE > 0 AND TCDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO TCDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF TCDTE-CLOSE-DATE > 0 AND TCDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EG' TO TCDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2650-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TCDTE-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCDTE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TCDTE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2660-DESCRIBE-CYCLE.
           EVALUATE TCDTE-CYCLE-CD
               WHEN 'EPW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HHK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JCB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UCB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XTY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XUV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XXU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2670-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TCDTE-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCDTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TCDTE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND YTD FEE ON THE SHORTAGE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2680-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TCDTE-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TCDTE-REFUND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TCDTE-OVERRIDE-FLAG = 'Y'
                   COMPUTE TCDTE-REFUND-YTD ROUNDED = TCDTE-SHORTAGE-YTD
                       * TCDTE-MARGIN / 200
               WHEN TCDTE-CYCLE-CD = 'EPW'
                   MOVE 5.00 TO TCDTE-REFUND-YTD
               WHEN TCDTE-CYCLE-CD = 'HHK'
                   MOVE 15.00 TO TCDTE-REFUND-YTD
               WHEN OTHER
                   COMPUTE TCDTE-REFUND-YTD ROUNDED = TCDTE-SHORTAGE-YTD
                       * TCDTE-MARGIN / 100
           END-EVALUATE
           IF TCDTE-REFUND-YTD > 0 AND TCDTE-REFUND-YTD < 1.00
               MOVE 1.00 TO TCDTE-REFUND-YTD
           END-IF
           IF TCDTE-REFUND-YTD > 75.00
               MOVE 75.00 TO TCDTE-REFUND-YTD
           END-IF
           ADD TCDTE-REFUND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SHORTAGE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2690-FLAG-SHORTAGE.
           IF TCDTE-SHORTAGE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TCDTE-SHORTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2700-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO TCDTE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO TCDTE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO TCDTE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'TCDTE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
