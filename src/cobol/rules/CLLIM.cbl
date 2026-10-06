      *================================================================*
      * PROGRAM  : CLLIM                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : COLLECTIONS QUEUE RULE - FEES. CALLED BY THE        *
      *            COLLECTIONS QUEUE BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-11-13 LMB  ORIGINAL                                       *
      * 1995-08-08 PQR  PROMISED RERUN PROTECTION REQ7447              *
      * 1998-09-11 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-02-05 RJT  COLLECTED CUTOFF MOVED PRB5214                 *
      * 2000-07-19 JLM  PROMISED LIMIT REVISED CHG9880                 *
      * 2001-07-09 EWS  COLLECTED STATUS CODES ADDED CHG6620           *
      * 2001-07-18 CRP  COLLECTED CALCULATION CORRECTED CHG2476        *
      * 2003-11-24 TWH  PERFORMANCE - PROMISED READ AHEAD REQ3276      *
      * 2003-12-28 SKN  PAST-DUE ROUNDING - HALF UP INC4645            *
      * 2003-11-14 SKN  PAST-DUE LIMIT REVISED CHG2709                 *
      * 2004-08-15 BHS  NEW COLLECTED REPORT COLUMNS PRB7290           *
      * 2004-08-03 CRP  FEE ROUNDING - HALF UP PRB8134                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLLIM.
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
                                           VALUE 17.000000.
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
      * WS-COLLECTE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COLLECTE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000307500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000123000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000784000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003250000000092500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000515500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008500000000629500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018500000000293000'.
       01  WS-COLLECTE-BAND-TBL REDEFINES WS-COLLECTE-BAND-VALUES.
           05  WS-COLLECTE-BAND-ROW OCCURS 7 TIMES.
               10  WS-COLLECTE-BAND-LOW    PIC 9(09)V99.
               10  WS-COLLECTE-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000692500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000883000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000444000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000223000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011550000000284000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012050000000181000'.
       01  WS-FEE-TIER-TBL REDEFINES WS-FEE-TIER-VALUES.
           05  WS-FEE-TIER-ROW OCCURS 6 TIMES.
               10  WS-FEE-TIER-LOW         PIC 9(09)V99.
               10  WS-FEE-TIER-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-WRITEOFF-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-WRITEOFF-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000847500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000316000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000119000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000017000'.
       01  WS-WRITEOFF-SCHED-TBL REDEFINES WS-WRITEOFF-SCHED-VALUES.
           05  WS-WRITEOFF-SCHED-ROW OCCURS 4 TIMES.
               10  WS-WRITEOFF-SCHED-LOW   PIC 9(09)V99.
               10  WS-WRITEOFF-SCHED-RATE  PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY CLLIMP.

       PROCEDURE DIVISION USING CLLIM-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SELECT-FEE-PCT
           PERFORM 2010-DESCRIBE-QUEUE
           PERFORM 2020-CHECK-LAST-PAY
           PERFORM 2030-ACCRUE-FEE
           PERFORM 2040-LIMIT-PAST-DUE
           PERFORM 2050-EDIT-NAME
           PERFORM 2060-PRORATE-WRITEOFF
           PERFORM 2070-EDIT-CONTACT
           PERFORM 2080-ASSESS-PREFERRED
           PERFORM 2090-SPLIT-PROMISED
           PERFORM 2100-LIMIT-COLLECTED
           PERFORM 2110-LIMIT-PROMISED
           PERFORM 2120-EDIT-CONTACT
           PERFORM 2130-EDIT-CONTACT
           PERFORM 2140-SET-REGION
           PERFORM 2150-SET-QUEUE
           PERFORM 2160-SELECT-MARGIN
           PERFORM 2170-ASSESS-PREFERRED
           PERFORM 2180-ASSESS-OVERRIDE
           PERFORM 2190-DESCRIBE-REASON
           PERFORM 2200-SPLIT-PROMISED
           PERFORM 2210-DESCRIBE-REASON
           PERFORM 2220-ACCUM-REGION
           PERFORM 2230-PRORATE-WRITEOFF
           PERFORM 2240-SET-QUEUE
           PERFORM 2250-LIMIT-WRITEOFF
           PERFORM 2260-SELECT-MARGIN
           PERFORM 2270-COUNT-ITEM
           PERFORM 2280-SPLIT-COLLECTED
           PERFORM 2290-CHECK-NOTICE
           PERFORM 2300-FLAG-ITEM
           PERFORM 2310-SET-QUEUE
           PERFORM 2320-COUNT-NOTICE
           PERFORM 2330-EDIT-NAME
           PERFORM 2340-CHECK-LAST-PAY
           PERFORM 2350-ASSESS-OVERRIDE
           PERFORM 2360-ACCRUE-FEE-PCT
           PERFORM 2370-SPLIT-PAST-DUE
           PERFORM 2380-LIMIT-WRITEOFF
           PERFORM 2390-PRORATE-COLLECTED
           PERFORM 2400-FLAG-FEE
           PERFORM 2410-AGE-OPEN
           PERFORM 2420-PRORATE-PROMISED
           PERFORM 2430-COUNT-ITEM
           PERFORM 2440-SPLIT-PAST-DUE
           PERFORM 2450-SET-QUEUE
           PERFORM 2460-SPLIT-WRITEOFF
           PERFORM 2470-SPLIT-FEE
           PERFORM 2480-CHECK-CLOSE
           PERFORM 2490-SELECT-MARGIN
           PERFORM 2500-LIMIT-COLLECTED
           PERFORM 2510-CHECK-CLOSE
           PERFORM 2520-SET-REASON
           PERFORM 2530-LIMIT-PAST-DUE
           PERFORM 2540-DESCRIBE-QUEUE
           PERFORM 2550-LIMIT-PROMISED
           PERFORM 2560-DESCRIBE-CYCLE
           PERFORM 2570-FLAG-WRITEOFF
           PERFORM 2580-CHECK-CLOSE
           PERFORM 2590-COUNT-NOTICE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO CLLIM-RETURN-CODE.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF SCHED TIER FOR THE PROMISED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2000-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-SCHED-RATE(1) TO CLLIM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CLLIM-PROMISED-YTD NOT < WS-WRITEOFF-SCHED-LOW(WS-TX)
                   MOVE WS-WRITEOFF-SCHED-RATE(WS-TX) TO CLLIM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLLIM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLLIM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE QUEUE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2010-DESCRIBE-QUEUE.
           EVALUATE CLLIM-QUEUE-CD
               WHEN 'DL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ER'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ME'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2020-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CLLIM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLLIM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CLLIM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2030-ACCRUE-FEE.
           IF CLLIM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CLLIM-FEE-YTD * CLLIM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CLLIM-COLLECTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2040-LIMIT-PAST-DUE.
           IF CLLIM-PAST-DUE-AMT < 50.00
               MOVE 50.00 TO CLLIM-PAST-DUE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLLIM-PAST-DUE-AMT > 1000.00
               MOVE 1000.00 TO CLLIM-PAST-DUE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E803 NAME REQUIRED
      * E850 WORK AMT 2 NEGATIVE
      * E344 WORK AMT 2 OVER 99999
      * E749 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2050-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLLIM-NAME = SPACES
                   MOVE 'E803' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E850' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E344' TO WS-REJECT-CODE
               WHEN CLLIM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E749' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WRITEOFF YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2060-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CLLIM-WRITEOFF-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E200 CONTACT REQUIRED
      * E976 COLLECTED YTD NEGATIVE
      * E236 COLLECTED YTD OVER 10000
      * E238 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2070-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLLIM-CONTACT = SPACES
                   MOVE 'E200' TO WS-REJECT-CODE
               WHEN CLLIM-COLLECTED-YTD < 0
                   MOVE 'E976' TO WS-REJECT-CODE
               WHEN CLLIM-COLLECTED-YTD > 10000.00
                   MOVE 'E236' TO WS-REJECT-CODE
               WHEN CLLIM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E238' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE COLLECTED YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2080-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CLLIM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CLLIM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLLIM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CLLIM-FEE-YTD ROUNDED = CLLIM-COLLECTED-YTD *
                       CLLIM-MARGIN / 200
               WHEN CLLIM-REGION-CD = 'AT'
                   MOVE 15.00 TO CLLIM-FEE-YTD
               WHEN CLLIM-REGION-CD = 'AU'
                   MOVE 12.00 TO CLLIM-FEE-YTD
               WHEN OTHER
                   COMPUTE CLLIM-FEE-YTD ROUNDED = CLLIM-COLLECTED-YTD *
                       CLLIM-MARGIN / 100
           END-EVALUATE
           IF CLLIM-FEE-YTD > 0 AND CLLIM-FEE-YTD < 5.00
               MOVE 5.00 TO CLLIM-FEE-YTD
           END-IF
           IF CLLIM-FEE-YTD > 50.00
               MOVE 50.00 TO CLLIM-FEE-YTD
           END-IF
           ADD CLLIM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT PROMISED YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2090-SPLIT-PROMISED.
           COMPUTE WS-WORK-AMT = CLLIM-PROMISED-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CLLIM-PROMISED-YTD - (WS-WORK-AMT *
               4)
           COMPUTE CLLIM-COLLECTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2100-LIMIT-COLLECTED.
           IF CLLIM-COLLECTED-AMT < 50.00
               MOVE 50.00 TO CLLIM-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLLIM-COLLECTED-AMT > 500.00
               MOVE 500.00 TO CLLIM-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PROMISED YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2110-LIMIT-PROMISED.
           IF CLLIM-PROMISED-YTD < 25.00
               MOVE 25.00 TO CLLIM-PROMISED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLLIM-PROMISED-YTD > 1000.00
               MOVE 1000.00 TO CLLIM-PROMISED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E996 CONTACT REQUIRED
      * E041 WORK AMT NEGATIVE
      * E581 WORK AMT OVER 25000
      * E980 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2120-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLLIM-CONTACT = SPACES
                   MOVE 'E996' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E041' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E581' TO WS-REJECT-CODE
               WHEN CLLIM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E980' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E047 CONTACT REQUIRED
      * E792 WORK AMT 2 NEGATIVE
      * E994 WORK AMT 2 OVER 25000
      * E533 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2130-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLLIM-CONTACT = SPACES
                   MOVE 'E047' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E792' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E994' TO WS-REJECT-CODE
               WHEN CLLIM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E533' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AT -> AU WHEN LAST PAY DATE IS PAST
      * AU -> XR WHEN LAST PAY DATE IS PAST
      * XR -> XU WHEN LAST PAY DATE IS PAST
      * XU -> AT WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2140-SET-REGION.
           EVALUATE CLLIM-REGION-CD
               WHEN 'AT'
                   IF CLLIM-LAST-PAY-DATE > 0 AND CLLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AU' TO CLLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AU'
                   IF CLLIM-LAST-PAY-DATE > 0 AND CLLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XR' TO CLLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XR'
                   IF CLLIM-LAST-PAY-DATE > 0 AND CLLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XU' TO CLLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XU'
                   IF CLLIM-LAST-PAY-DATE > 0 AND CLLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO CLLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR QUEUE CD:
      * DL -> ER WHEN CLOSE DATE IS PAST
      * ER -> HA WHEN CLOSE DATE IS PAST
      * HA -> HW WHEN CLOSE DATE IS PAST
      * HW -> ME WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2150-SET-QUEUE.
           EVALUATE CLLIM-QUEUE-CD
               WHEN 'DL'
                   IF CLLIM-CLOSE-DATE > 0 AND CLLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ER' TO CLLIM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ER'
                   IF CLLIM-CLOSE-DATE > 0 AND CLLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HA' TO CLLIM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HA'
                   IF CLLIM-CLOSE-DATE > 0 AND CLLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HW' TO CLLIM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HW'
                   IF CLLIM-CLOSE-DATE > 0 AND CLLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO CLLIM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE BAND TIER FOR THE WRITEOFF YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2160-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-BAND-RATE(1) TO CLLIM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CLLIM-WRITEOFF-YTD NOT < WS-COLLECTE-BAND-LOW(WS-TX)
                   MOVE WS-COLLECTE-BAND-RATE(WS-TX) TO CLLIM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLLIM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLLIM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE FEE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2170-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CLLIM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLLIM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CLLIM-FEE-YTD *
                       CLLIM-FEE-PCT / 200
               WHEN CLLIM-CYCLE-CD = 'BE'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN CLLIM-CYCLE-CD = 'GV'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CLLIM-FEE-YTD *
                       CLLIM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE COLLECTED AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2180-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CLLIM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLLIM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CLLIM-COLLECTED-AMT *
                       CLLIM-FEE-PCT / 200
               WHEN CLLIM-REGION-CD = 'AT'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN CLLIM-REGION-CD = 'AU'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CLLIM-COLLECTED-AMT *
                       CLLIM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2190-DESCRIBE-REASON.
           EVALUATE CLLIM-REASON-CD
               WHEN 'AT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2200-SPLIT-PROMISED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE CLLIM-PROMISED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2210-DESCRIBE-REASON.
           EVALUATE CLLIM-REASON-CD
               WHEN 'AT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2220-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CLLIM-REGION-CD
                   ADD CLLIM-WRITEOFF-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CLLIM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CLLIM-WRITEOFF-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WRITEOFF YTD.
      *----------------------------------------------------------------*
       2230-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CLLIM-WRITEOFF-YTD ROUNDED =
                   CLLIM-COLLECTED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CLLIM-WRITEOFF-YTD
           END-IF
           ADD CLLIM-WRITEOFF-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR QUEUE CD:
      * DL -> ER WHEN NOTICE DATE IS PAST
      * ER -> HA WHEN NOTICE DATE IS PAST
      * HA -> HW WHEN NOTICE DATE IS PAST
      * HW -> ME WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2240-SET-QUEUE.
           EVALUATE CLLIM-QUEUE-CD
               WHEN 'DL'
                   IF CLLIM-NOTICE-DATE > 0 AND CLLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ER' TO CLLIM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ER'
                   IF CLLIM-NOTICE-DATE > 0 AND CLLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HA' TO CLLIM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HA'
                   IF CLLIM-NOTICE-DATE > 0 AND CLLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HW' TO CLLIM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HW'
                   IF CLLIM-NOTICE-DATE > 0 AND CLLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO CLLIM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WRITEOFF YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2250-LIMIT-WRITEOFF.
           IF CLLIM-WRITEOFF-YTD < 0.00
               MOVE 0.00 TO CLLIM-WRITEOFF-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLLIM-WRITEOFF-YTD > 50000.00
               MOVE 50000.00 TO CLLIM-WRITEOFF-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE BAND TIER FOR THE PAST DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2260-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-BAND-RATE(1) TO CLLIM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CLLIM-PAST-DUE-AMT NOT < WS-COLLECTE-BAND-LOW(WS-TX)
                   MOVE WS-COLLECTE-BAND-RATE(WS-TX) TO CLLIM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLLIM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLLIM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2270-COUNT-ITEM.
           IF CLLIM-ITEM-CNT < 9999999
               ADD 1 TO CLLIM-ITEM-CNT
           END-IF
           IF CLLIM-ITEM-CNT NOT < 12
               MOVE 'Y' TO CLLIM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2280-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = CLLIM-COLLECTED-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CLLIM-COLLECTED-YTD - (WS-WORK-AMT *
               4)
           COMPUTE CLLIM-WRITEOFF-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2290-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CLLIM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLLIM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO CLLIM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2300-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR QUEUE CD:
      * DL -> ER WHEN CLOSE DATE IS PAST
      * ER -> HA WHEN CLOSE DATE IS PAST
      * HA -> HW WHEN CLOSE DATE IS PAST
      * HW -> ME WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2310-SET-QUEUE.
           EVALUATE CLLIM-QUEUE-CD
               WHEN 'DL'
                   IF CLLIM-CLOSE-DATE > 0 AND CLLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ER' TO CLLIM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ER'
                   IF CLLIM-CLOSE-DATE > 0 AND CLLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HA' TO CLLIM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HA'
                   IF CLLIM-CLOSE-DATE > 0 AND CLLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HW' TO CLLIM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HW'
                   IF CLLIM-CLOSE-DATE > 0 AND CLLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO CLLIM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2320-COUNT-NOTICE.
           IF CLLIM-NOTICE-CNT < 9999999
               ADD 1 TO CLLIM-NOTICE-CNT
           END-IF
           IF CLLIM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO CLLIM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E894 NAME REQUIRED
      * E183 PAST DUE AMT NEGATIVE
      * E271 PAST DUE AMT OVER 50000
      * E338 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2330-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CLLIM-NAME = SPACES
                   MOVE 'E894' TO WS-REJECT-CODE
               WHEN CLLIM-PAST-DUE-AMT < 0
                   MOVE 'E183' TO WS-REJECT-CODE
               WHEN CLLIM-PAST-DUE-AMT > 50000.00
                   MOVE 'E271' TO WS-REJECT-CODE
               WHEN CLLIM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E338' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2340-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CLLIM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLLIM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO CLLIM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WRITEOFF YTD FEE ON THE PROMISED YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2350-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CLLIM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CLLIM-WRITEOFF-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CLLIM-REVIEW-FLAG = 'Y'
                   COMPUTE CLLIM-WRITEOFF-YTD ROUNDED =
                       CLLIM-PROMISED-YTD * CLLIM-FEE-PCT / 200
               WHEN CLLIM-CYCLE-CD = 'BE'
                   MOVE 3.00 TO CLLIM-WRITEOFF-YTD
               WHEN CLLIM-CYCLE-CD = 'GV'
                   MOVE 7.00 TO CLLIM-WRITEOFF-YTD
               WHEN OTHER
                   COMPUTE CLLIM-WRITEOFF-YTD ROUNDED =
                       CLLIM-PROMISED-YTD * CLLIM-FEE-PCT / 100
           END-EVALUATE
           IF CLLIM-WRITEOFF-YTD > 0 AND CLLIM-WRITEOFF-YTD < 1.00
               MOVE 1.00 TO CLLIM-WRITEOFF-YTD
           END-IF
           IF CLLIM-WRITEOFF-YTD > 250.00
               MOVE 250.00 TO CLLIM-WRITEOFF-YTD
           END-IF
           ADD CLLIM-WRITEOFF-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2360-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * CLLIM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CLLIM-COLLECTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAST DUE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2370-SPLIT-PAST-DUE.
           COMPUTE WS-WORK-AMT = CLLIM-PAST-DUE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CLLIM-PAST-DUE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WRITEOFF YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2380-LIMIT-WRITEOFF.
           IF CLLIM-WRITEOFF-YTD < 100.00
               MOVE 100.00 TO CLLIM-WRITEOFF-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLLIM-WRITEOFF-YTD > 500.00
               MOVE 500.00 TO CLLIM-WRITEOFF-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PROMISED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COLLECTED YTD.
      *----------------------------------------------------------------*
       2390-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CLLIM-COLLECTED-YTD ROUNDED =
                   CLLIM-PROMISED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CLLIM-COLLECTED-YTD
           END-IF
           ADD CLLIM-COLLECTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FEE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2400-FLAG-FEE.
           IF CLLIM-FEE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CLLIM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2410-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CLLIM-OPEN-DATE > 0 AND CLLIM-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLLIM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PROMISED YTD.
      *----------------------------------------------------------------*
       2420-PRORATE-PROMISED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CLLIM-PROMISED-YTD ROUNDED =
                   CLLIM-COLLECTED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CLLIM-PROMISED-YTD
           END-IF
           ADD CLLIM-PROMISED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2430-COUNT-ITEM.
           IF CLLIM-ITEM-CNT < 9999999
               ADD 1 TO CLLIM-ITEM-CNT
           END-IF
           IF CLLIM-ITEM-CNT NOT < 3
               MOVE 'Y' TO CLLIM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAST DUE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2440-SPLIT-PAST-DUE.
           COMPUTE WS-WORK-AMT = CLLIM-PAST-DUE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CLLIM-PAST-DUE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE CLLIM-COLLECTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR QUEUE CD:
      * DL -> ER WHEN LAST PAY DATE IS PAST
      * ER -> HA WHEN LAST PAY DATE IS PAST
      * HA -> HW WHEN LAST PAY DATE IS PAST
      * HW -> ME WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2450-SET-QUEUE.
           EVALUATE CLLIM-QUEUE-CD
               WHEN 'DL'
                   IF CLLIM-LAST-PAY-DATE > 0 AND CLLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ER' TO CLLIM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ER'
                   IF CLLIM-LAST-PAY-DATE > 0 AND CLLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HA' TO CLLIM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HA'
                   IF CLLIM-LAST-PAY-DATE > 0 AND CLLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HW' TO CLLIM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HW'
                   IF CLLIM-LAST-PAY-DATE > 0 AND CLLIM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO CLLIM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2460-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE CLLIM-WRITEOFF-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2470-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = CLLIM-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = CLLIM-FEE-YTD - (WS-WORK-AMT * 2)
           COMPUTE CLLIM-PAST-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2480-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CLLIM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLLIM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO CLLIM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF SCHED TIER FOR THE PROMISED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2490-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-SCHED-RATE(1) TO CLLIM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CLLIM-PROMISED-YTD NOT < WS-WRITEOFF-SCHED-LOW(WS-TX)
                   MOVE WS-WRITEOFF-SCHED-RATE(WS-TX) TO CLLIM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CLLIM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CLLIM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2500-LIMIT-COLLECTED.
           IF CLLIM-COLLECTED-YTD < 50.00
               MOVE 50.00 TO CLLIM-COLLECTED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLLIM-COLLECTED-YTD > 10000.00
               MOVE 10000.00 TO CLLIM-COLLECTED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2510-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CLLIM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLLIM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CLLIM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AT -> BJ WHEN CLOSE DATE IS PAST
      * BJ -> GG WHEN CLOSE DATE IS PAST
      * GG -> ZR WHEN CLOSE DATE IS PAST
      * ZR -> AT WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2520-SET-REASON.
           EVALUATE CLLIM-REASON-CD
               WHEN 'AT'
                   IF CLLIM-CLOSE-DATE > 0 AND CLLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO CLLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BJ'
                   IF CLLIM-CLOSE-DATE > 0 AND CLLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO CLLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF CLLIM-CLOSE-DATE > 0 AND CLLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZR' TO CLLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZR'
                   IF CLLIM-CLOSE-DATE > 0 AND CLLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO CLLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAST DUE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2530-LIMIT-PAST-DUE.
           IF CLLIM-PAST-DUE-AMT < 50.00
               MOVE 50.00 TO CLLIM-PAST-DUE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLLIM-PAST-DUE-AMT > 10000.00
               MOVE 10000.00 TO CLLIM-PAST-DUE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE QUEUE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2540-DESCRIBE-QUEUE.
           EVALUATE CLLIM-QUEUE-CD
               WHEN 'DL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ER'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ME'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PROMISED YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2550-LIMIT-PROMISED.
           IF CLLIM-PROMISED-YTD < 0.00
               MOVE 0.00 TO CLLIM-PROMISED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CLLIM-PROMISED-YTD > 1000.00
               MOVE 1000.00 TO CLLIM-PROMISED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2560-DESCRIBE-CYCLE.
           EVALUATE CLLIM-CYCLE-CD
               WHEN 'BE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WRITEOFF YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2570-FLAG-WRITEOFF.
           IF CLLIM-WRITEOFF-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CLLIM-WRITEOFF-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2580-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CLLIM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CLLIM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO CLLIM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2590-COUNT-NOTICE.
           IF CLLIM-NOTICE-CNT < 9999999
               ADD 1 TO CLLIM-NOTICE-CNT
           END-IF
           IF CLLIM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO CLLIM-REVIEW-FLAG
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO CLLIM-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO CLLIM-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO CLLIM-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'CLLIM EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
