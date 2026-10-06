      *================================================================*
      * PROGRAM  : CNPEN                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : COIN AND CURRENCY RULE - DATE RULES. CALLED BY THE  *
      *            COIN AND CURRENCY BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-11-20 MKL  ORIGINAL                                       *
      * 1992-02-15 SKN  SHIPMENT NOTICE TEXT PER LEGAL PRB5952         *
      * 1993-02-22 JLM  SHIPMENT NOTICE TEXT PER LEGAL INC5055         *
      * 1993-04-14 RJT  SHIPMENT TABLE EXPANDED INC1572                *
      * 1997-10-01 RJT  SHIPMENT LIMIT REVISED REQ4001                 *
      * 2000-01-14 RJT  COIN CALCULATION CORRECTED INC5596             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CNPEN.
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
      * WS-SHIPMENT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHIPMENT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000019500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000219000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000200000000495000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010200000000616000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010700000000579000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015700000000233000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015750000000293500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015800000000172500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018300000000804000'.
       01  WS-SHIPMENT-BAND-TBL REDEFINES WS-SHIPMENT-BAND-VALUES.
           05  WS-SHIPMENT-BAND-ROW OCCURS 9 TIMES.
               10  WS-SHIPMENT-BAND-LOW    PIC 9(09)V99.
               10  WS-SHIPMENT-BAND-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY CNPENP.

       PROCEDURE DIVISION USING CNPEN-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-CHECK-NOTICE
           PERFORM 2010-ASSESS-HOLD
           PERFORM 2020-EDIT-CONTACT
           PERFORM 2030-DESCRIBE-REASON
           PERFORM 2040-EDIT-CONTACT
           PERFORM 2050-EDIT-CONTACT
           PERFORM 2060-COUNT-RETRY
           PERFORM 2070-FLAG-COIN
           PERFORM 2080-SET-REASON
           PERFORM 2090-PRORATE-STRAP
           PERFORM 2100-SELECT-RATE
           PERFORM 2110-EDIT-REF-NO
           PERFORM 2120-PRORATE-CURRENCY
           PERFORM 2130-ACCUM-REASON
           PERFORM 2140-SPLIT-COIN
           PERFORM 2150-ASSESS-AUTO-RENEW
           PERFORM 2160-ASSESS-HOLD
           PERFORM 2170-FLAG-COIN
           PERFORM 2180-PRORATE-SHIPMENT
           PERFORM 2190-FLAG-ITEM
           PERFORM 2200-EDIT-REF-NO
           PERFORM 2210-ACCRUE-SHIPMENT
           PERFORM 2220-PRORATE-CURRENCY
           PERFORM 2230-COUNT-ITEM
           PERFORM 2240-ASSESS-AUTO-RENEW
           PERFORM 2250-SET-CARRIER
           PERFORM 2260-SPLIT-STRAP
           PERFORM 2270-EDIT-CONTACT
           PERFORM 2280-SELECT-RATE
           PERFORM 2290-LIMIT-ITEM
           PERFORM 2300-AGE-MATURITY
           PERFORM 2310-SET-REASON
           PERFORM 2320-SPLIT-CURRENCY
           PERFORM 2330-COUNT-RETRY
           PERFORM 2340-EDIT-CONTACT
           PERFORM 2350-ACCUM-CHANNEL
           PERFORM 2360-COUNT-ITEM
           PERFORM 2370-SPLIT-COIN
           PERFORM 2380-SPLIT-SHIPMENT
           PERFORM 2390-CHECK-NEXT-REVIEW
           PERFORM 2400-PRORATE-SHIPMENT
           PERFORM 2410-FLAG-FEE
           PERFORM 2420-ACCUM-CHANNEL
           PERFORM 2430-LIMIT-COIN
           PERFORM 2440-EDIT-REF-NO
           PERFORM 2450-SET-CHANNEL
           PERFORM 2460-LIMIT-SHIPMENT
           PERFORM 2470-ASSESS-AUTO-RENEW
           PERFORM 2480-DESCRIBE-SOURCE
           PERFORM 2490-LIMIT-ITEM
           PERFORM 2500-LIMIT-ITEM
           PERFORM 2510-PRORATE-SHIPMENT
           PERFORM 2520-FLAG-STRAP
           PERFORM 2530-SPLIT-ITEM
           PERFORM 2540-EDIT-REF-NO
           PERFORM 2550-SPLIT-ITEM
           PERFORM 2560-SPLIT-FEE
           PERFORM 2570-LIMIT-SHIPMENT
           PERFORM 2580-AGE-NEXT-REVIEW
           PERFORM 2590-LIMIT-CURRENCY
           PERFORM 2600-ACCUM-CHANNEL
           PERFORM 2610-COUNT-RETRY
           PERFORM 2620-LIMIT-STRAP
           PERFORM 2630-PRORATE-SHIPMENT
           PERFORM 2640-SPLIT-STRAP
           PERFORM 2650-SET-CARRIER
           PERFORM 2660-DESCRIBE-REASON
           PERFORM 2670-ACCUM-CHANNEL
           PERFORM 2680-LIMIT-SHIPMENT
           PERFORM 2690-ACCUM-REASON
           PERFORM 2700-CHECK-NOTICE
           PERFORM 2710-PRORATE-SHIPMENT
           PERFORM 2720-PRORATE-CURRENCY
           PERFORM 2730-DESCRIBE-REASON
           PERFORM 2740-ACCUM-CARRIER
           PERFORM 2750-COUNT-ITEM
           PERFORM 2760-ASSESS-AUTO-RENEW
           PERFORM 2770-ASSESS-AUTO-RENEW
           PERFORM 2780-PRORATE-SHIPMENT
           PERFORM 2790-SET-REASON
           PERFORM 2800-ACCRUE-PENALTY-PCT
           PERFORM 2810-LIMIT-CURRENCY
           PERFORM 2820-EDIT-REF-NO
           PERFORM 2830-LIMIT-CURRENCY
           PERFORM 2840-LIMIT-COIN
           PERFORM 2850-EDIT-CONTACT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO CNPEN-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2000-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CNPEN-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNPEN-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO CNPEN-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STRAP YTD FEE ON THE WORK AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2010-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CNPEN-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CNPEN-STRAP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CNPEN-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CNPEN-STRAP-YTD ROUNDED = WS-WORK-AMT *
                       CNPEN-PENALTY-PCT / 200
               WHEN CNPEN-CHANNEL-CD = 'AW'
                   MOVE 10.00 TO CNPEN-STRAP-YTD
               WHEN CNPEN-CHANNEL-CD = 'DW'
                   MOVE 5.00 TO CNPEN-STRAP-YTD
               WHEN OTHER
                   COMPUTE CNPEN-STRAP-YTD ROUNDED = WS-WORK-AMT *
                       CNPEN-PENALTY-PCT / 100
           END-EVALUATE
           IF CNPEN-STRAP-YTD > 0 AND CNPEN-STRAP-YTD < 2.00
               MOVE 2.00 TO CNPEN-STRAP-YTD
           END-IF
           IF CNPEN-STRAP-YTD > 75.00
               MOVE 75.00 TO CNPEN-STRAP-YTD
           END-IF
           ADD CNPEN-STRAP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E484 CONTACT REQUIRED
      * E879 SHIPMENT YTD NEGATIVE
      * E230 SHIPMENT YTD OVER 1000000
      * E586 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2020-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNPEN-CONTACT = SPACES
                   MOVE 'E484' TO WS-REJECT-CODE
               WHEN CNPEN-SHIPMENT-YTD < 0
                   MOVE 'E879' TO WS-REJECT-CODE
               WHEN CNPEN-SHIPMENT-YTD > 1000000.00
                   MOVE 'E230' TO WS-REJECT-CODE
               WHEN CNPEN-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E586' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2030-DESCRIBE-REASON.
           EVALUATE CNPEN-REASON-CD
               WHEN 'JL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E760 CONTACT REQUIRED
      * E978 CURRENCY YTD NEGATIVE
      * E651 CURRENCY YTD OVER 25000
      * E672 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2040-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNPEN-CONTACT = SPACES
                   MOVE 'E760' TO WS-REJECT-CODE
               WHEN CNPEN-CURRENCY-YTD < 0
                   MOVE 'E978' TO WS-REJECT-CODE
               WHEN CNPEN-CURRENCY-YTD > 25000.00
                   MOVE 'E651' TO WS-REJECT-CODE
               WHEN CNPEN-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E672' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E685 CONTACT REQUIRED
      * E816 STRAP YTD NEGATIVE
      * E467 STRAP YTD OVER 50000
      * E762 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2050-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNPEN-CONTACT = SPACES
                   MOVE 'E685' TO WS-REJECT-CODE
               WHEN CNPEN-STRAP-YTD < 0
                   MOVE 'E816' TO WS-REJECT-CODE
               WHEN CNPEN-STRAP-YTD > 50000.00
                   MOVE 'E467' TO WS-REJECT-CODE
               WHEN CNPEN-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E762' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2060-COUNT-RETRY.
           IF CNPEN-RETRY-CNT < 9999999
               ADD 1 TO CNPEN-RETRY-CNT
           END-IF
           IF CNPEN-RETRY-CNT NOT < 12
               MOVE 'Y' TO CNPEN-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * COIN AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2070-FLAG-COIN.
           IF CNPEN-COIN-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CNPEN-COIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * JL -> LW WHEN MATURITY DATE IS PAST
      * LW -> PS WHEN MATURITY DATE IS PAST
      * PS -> JL WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2080-SET-REASON.
           EVALUATE CNPEN-REASON-CD
               WHEN 'JL'
                   IF CNPEN-MATURITY-DATE > 0 AND CNPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LW' TO CNPEN-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LW'
                   IF CNPEN-MATURITY-DATE > 0 AND CNPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO CNPEN-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF CNPEN-MATURITY-DATE > 0 AND CNPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO CNPEN-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE STRAP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2090-PRORATE-STRAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CNPEN-STRAP-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE SHIPMENT BAND TIER FOR THE SHIPMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2100-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPMENT-BAND-RATE(1) TO CNPEN-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CNPEN-SHIPMENT-YTD NOT < WS-SHIPMENT-BAND-LOW(WS-TX)
                   MOVE WS-SHIPMENT-BAND-RATE(WS-TX) TO CNPEN-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNPEN-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNPEN-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E451 REF NO REQUIRED
      * E297 FEE AMT NEGATIVE
      * E425 FEE AMT OVER 1000000
      * E688 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2110-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNPEN-REF-NO = SPACES
                   MOVE 'E451' TO WS-REJECT-CODE
               WHEN CNPEN-FEE-AMT < 0
                   MOVE 'E297' TO WS-REJECT-CODE
               WHEN CNPEN-FEE-AMT > 1000000.00
                   MOVE 'E425' TO WS-REJECT-CODE
               WHEN CNPEN-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E688' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CURRENCY YTD.
      *----------------------------------------------------------------*
       2120-PRORATE-CURRENCY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNPEN-CURRENCY-YTD ROUNDED =
                   CNPEN-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CNPEN-CURRENCY-YTD
           END-IF
           ADD CNPEN-CURRENCY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE COIN AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2130-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNPEN-REASON-CD
                   ADD CNPEN-COIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNPEN-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CNPEN-COIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COIN AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2140-SPLIT-COIN.
           COMPUTE WS-WORK-AMT = CNPEN-COIN-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CNPEN-COIN-AMT - (WS-WORK-AMT * 4)
           COMPUTE CNPEN-SHIPMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE SHIPMENT YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2150-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CNPEN-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CNPEN-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CNPEN-SHIPMENT-YTD *
                       CNPEN-RATE / 200
               WHEN CNPEN-CARRIER-CD = 'J'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN CNPEN-CARRIER-CD = 'L'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CNPEN-SHIPMENT-YTD *
                       CNPEN-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE CURRENCY YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2160-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CNPEN-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CNPEN-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CNPEN-STOP-FLAG = 'Y'
                   COMPUTE CNPEN-FEE-AMT ROUNDED = CNPEN-CURRENCY-YTD *
                       CNPEN-RATE / 200
               WHEN CNPEN-CARRIER-CD = 'J'
                   MOVE 7.00 TO CNPEN-FEE-AMT
               WHEN CNPEN-CARRIER-CD = 'L'
                   MOVE 10.00 TO CNPEN-FEE-AMT
               WHEN OTHER
                   COMPUTE CNPEN-FEE-AMT ROUNDED = CNPEN-CURRENCY-YTD *
                       CNPEN-RATE / 100
           END-EVALUATE
           IF CNPEN-FEE-AMT > 0 AND CNPEN-FEE-AMT < 2.00
               MOVE 2.00 TO CNPEN-FEE-AMT
           END-IF
           IF CNPEN-FEE-AMT > 50.00
               MOVE 50.00 TO CNPEN-FEE-AMT
           END-IF
           ADD CNPEN-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * COIN AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2170-FLAG-COIN.
           IF CNPEN-COIN-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CNPEN-COIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STRAP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHIPMENT AMT.
      *----------------------------------------------------------------*
       2180-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNPEN-SHIPMENT-AMT ROUNDED =
                   CNPEN-STRAP-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CNPEN-SHIPMENT-AMT
           END-IF
           ADD CNPEN-SHIPMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2190-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E876 REF NO REQUIRED
      * E602 FEE AMT NEGATIVE
      * E418 FEE AMT OVER 1000000
      * E617 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2200-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNPEN-REF-NO = SPACES
                   MOVE 'E876' TO WS-REJECT-CODE
               WHEN CNPEN-FEE-AMT < 0
                   MOVE 'E602' TO WS-REJECT-CODE
               WHEN CNPEN-FEE-AMT > 1000000.00
                   MOVE 'E418' TO WS-REJECT-CODE
               WHEN CNPEN-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E617' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPMENT YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2210-ACCRUE-SHIPMENT.
           IF CNPEN-SHIPMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CNPEN-SHIPMENT-YTD * CNPEN-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CURRENCY YTD.
      *----------------------------------------------------------------*
       2220-PRORATE-CURRENCY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNPEN-CURRENCY-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CNPEN-CURRENCY-YTD
           END-IF
           ADD CNPEN-CURRENCY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2230-COUNT-ITEM.
           IF CNPEN-ITEM-CNT < 9999999
               ADD 1 TO CNPEN-ITEM-CNT
           END-IF
           IF CNPEN-ITEM-CNT NOT < 3
               MOVE 'Y' TO CNPEN-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COIN FEE ON THE SHIPMENT AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2240-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CNPEN-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CNPEN-COIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CNPEN-STOP-FLAG = 'Y'
                   COMPUTE CNPEN-COIN-AMT ROUNDED = CNPEN-SHIPMENT-AMT *
                       CNPEN-PENALTY-PCT / 200
               WHEN CNPEN-REASON-CD = 'JL'
                   MOVE 7.00 TO CNPEN-COIN-AMT
               WHEN CNPEN-REASON-CD = 'LW'
                   MOVE 3.00 TO CNPEN-COIN-AMT
               WHEN OTHER
                   COMPUTE CNPEN-COIN-AMT ROUNDED = CNPEN-SHIPMENT-AMT *
                       CNPEN-PENALTY-PCT / 100
           END-EVALUATE
           IF CNPEN-COIN-AMT > 0 AND CNPEN-COIN-AMT < 10.00
               MOVE 10.00 TO CNPEN-COIN-AMT
           END-IF
           IF CNPEN-COIN-AMT > 50.00
               MOVE 50.00 TO CNPEN-COIN-AMT
           END-IF
           ADD CNPEN-COIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARRIER CD:
      * J -> L WHEN MATURITY DATE IS PAST
      * L -> M WHEN MATURITY DATE IS PAST
      * M -> X WHEN MATURITY DATE IS PAST
      * X -> J WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2250-SET-CARRIER.
           EVALUATE CNPEN-CARRIER-CD
               WHEN 'J'
                   IF CNPEN-MATURITY-DATE > 0 AND CNPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO CNPEN-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF CNPEN-MATURITY-DATE > 0 AND CNPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CNPEN-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CNPEN-MATURITY-DATE > 0 AND CNPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO CNPEN-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF CNPEN-MATURITY-DATE > 0 AND CNPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO CNPEN-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT STRAP YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2260-SPLIT-STRAP.
           COMPUTE WS-WORK-AMT = CNPEN-STRAP-YTD / 2
           COMPUTE WS-WORK-AMT-2 = CNPEN-STRAP-YTD - (WS-WORK-AMT * 2)
           COMPUTE CNPEN-CURRENCY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E663 CONTACT REQUIRED
      * E890 SHIPMENT AMT NEGATIVE
      * E598 SHIPMENT AMT OVER 250000
      * E740 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2270-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNPEN-CONTACT = SPACES
                   MOVE 'E663' TO WS-REJECT-CODE
               WHEN CNPEN-SHIPMENT-AMT < 0
                   MOVE 'E890' TO WS-REJECT-CODE
               WHEN CNPEN-SHIPMENT-AMT > 250000.00
                   MOVE 'E598' TO WS-REJECT-CODE
               WHEN CNPEN-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E740' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHIPMENT BAND TIER FOR THE SHIPMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2280-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPMENT-BAND-RATE(1) TO CNPEN-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CNPEN-SHIPMENT-AMT NOT < WS-SHIPMENT-BAND-LOW(WS-TX)
                   MOVE WS-SHIPMENT-BAND-RATE(WS-TX) TO CNPEN-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CNPEN-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CNPEN-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2290-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2300-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CNPEN-MATURITY-DATE > 0 AND CNPEN-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNPEN-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * JL -> LW WHEN MATURITY DATE IS PAST
      * LW -> PS WHEN MATURITY DATE IS PAST
      * PS -> JL WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2310-SET-REASON.
           EVALUATE CNPEN-REASON-CD
               WHEN 'JL'
                   IF CNPEN-MATURITY-DATE > 0 AND CNPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LW' TO CNPEN-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LW'
                   IF CNPEN-MATURITY-DATE > 0 AND CNPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO CNPEN-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF CNPEN-MATURITY-DATE > 0 AND CNPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO CNPEN-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CURRENCY YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2320-SPLIT-CURRENCY.
           COMPUTE WS-WORK-AMT = CNPEN-CURRENCY-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CNPEN-CURRENCY-YTD - (WS-WORK-AMT *
               4)
           COMPUTE CNPEN-COIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2330-COUNT-RETRY.
           IF CNPEN-RETRY-CNT < 9999999
               ADD 1 TO CNPEN-RETRY-CNT
           END-IF
           IF CNPEN-RETRY-CNT NOT < 12
               MOVE 'Y' TO CNPEN-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E241 CONTACT REQUIRED
      * E456 SHIPMENT AMT NEGATIVE
      * E717 SHIPMENT AMT OVER 50000
      * E101 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2340-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNPEN-CONTACT = SPACES
                   MOVE 'E241' TO WS-REJECT-CODE
               WHEN CNPEN-SHIPMENT-AMT < 0
                   MOVE 'E456' TO WS-REJECT-CODE
               WHEN CNPEN-SHIPMENT-AMT > 50000.00
                   MOVE 'E717' TO WS-REJECT-CODE
               WHEN CNPEN-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E101' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2350-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNPEN-CHANNEL-CD
                   ADD CNPEN-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNPEN-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CNPEN-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2360-COUNT-ITEM.
           IF CNPEN-ITEM-CNT < 9999999
               ADD 1 TO CNPEN-ITEM-CNT
           END-IF
           IF CNPEN-ITEM-CNT NOT < 6
               MOVE 'Y' TO CNPEN-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COIN AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2370-SPLIT-COIN.
           COMPUTE WS-WORK-AMT = CNPEN-COIN-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CNPEN-COIN-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHIPMENT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2380-SPLIT-SHIPMENT.
           COMPUTE WS-WORK-AMT = CNPEN-SHIPMENT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = CNPEN-SHIPMENT-YTD - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2390-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CNPEN-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNPEN-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO CNPEN-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHIPMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2400-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CNPEN-SHIPMENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2410-FLAG-FEE.
           IF CNPEN-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CNPEN-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2420-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNPEN-CHANNEL-CD
                   ADD CNPEN-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNPEN-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CNPEN-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * COIN AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2430-LIMIT-COIN.
           IF CNPEN-COIN-AMT < 100.00
               MOVE 100.00 TO CNPEN-COIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNPEN-COIN-AMT > 50000.00
               MOVE 50000.00 TO CNPEN-COIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E776 REF NO REQUIRED
      * E195 SHIPMENT YTD NEGATIVE
      * E255 SHIPMENT YTD OVER 25000
      * E556 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2440-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNPEN-REF-NO = SPACES
                   MOVE 'E776' TO WS-REJECT-CODE
               WHEN CNPEN-SHIPMENT-YTD < 0
                   MOVE 'E195' TO WS-REJECT-CODE
               WHEN CNPEN-SHIPMENT-YTD > 25000.00
                   MOVE 'E255' TO WS-REJECT-CODE
               WHEN CNPEN-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E556' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AW -> DW WHEN MATURITY DATE IS PAST
      * DW -> GN WHEN MATURITY DATE IS PAST
      * GN -> RS WHEN MATURITY DATE IS PAST
      * RS -> UZ WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2450-SET-CHANNEL.
           EVALUATE CNPEN-CHANNEL-CD
               WHEN 'AW'
                   IF CNPEN-MATURITY-DATE > 0 AND CNPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DW' TO CNPEN-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DW'
                   IF CNPEN-MATURITY-DATE > 0 AND CNPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GN' TO CNPEN-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GN'
                   IF CNPEN-MATURITY-DATE > 0 AND CNPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RS' TO CNPEN-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RS'
                   IF CNPEN-MATURITY-DATE > 0 AND CNPEN-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UZ' TO CNPEN-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHIPMENT YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2460-LIMIT-SHIPMENT.
           IF CNPEN-SHIPMENT-YTD < 25.00
               MOVE 25.00 TO CNPEN-SHIPMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNPEN-SHIPMENT-YTD > 50000.00
               MOVE 50000.00 TO CNPEN-SHIPMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPMENT FEE ON THE COIN AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2470-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CNPEN-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CNPEN-SHIPMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CNPEN-STOP-FLAG = 'Y'
                   COMPUTE CNPEN-SHIPMENT-AMT ROUNDED = CNPEN-COIN-AMT *
                       CNPEN-PENALTY-PCT / 200
               WHEN CNPEN-REASON-CD = 'JL'
                   MOVE 7.00 TO CNPEN-SHIPMENT-AMT
               WHEN CNPEN-REASON-CD = 'LW'
                   MOVE 12.00 TO CNPEN-SHIPMENT-AMT
               WHEN OTHER
                   COMPUTE CNPEN-SHIPMENT-AMT ROUNDED = CNPEN-COIN-AMT *
                       CNPEN-PENALTY-PCT / 100
           END-EVALUATE
           IF CNPEN-SHIPMENT-AMT > 0 AND CNPEN-SHIPMENT-AMT < 2.00
               MOVE 2.00 TO CNPEN-SHIPMENT-AMT
           END-IF
           IF CNPEN-SHIPMENT-AMT > 75.00
               MOVE 75.00 TO CNPEN-SHIPMENT-AMT
           END-IF
           ADD CNPEN-SHIPMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2480-DESCRIBE-SOURCE.
           EVALUATE CNPEN-SOURCE-CD
               WHEN 'CFR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DTX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'EEA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'EUA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TPB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WVN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YTT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2490-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2500-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPMENT AMT.
      *----------------------------------------------------------------*
       2510-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNPEN-SHIPMENT-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CNPEN-SHIPMENT-AMT
           END-IF
           ADD CNPEN-SHIPMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STRAP YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2520-FLAG-STRAP.
           IF CNPEN-STRAP-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CNPEN-STRAP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2530-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E892 REF NO REQUIRED
      * E231 CURRENCY YTD NEGATIVE
      * E014 CURRENCY YTD OVER 1000000
      * E539 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2540-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNPEN-REF-NO = SPACES
                   MOVE 'E892' TO WS-REJECT-CODE
               WHEN CNPEN-CURRENCY-YTD < 0
                   MOVE 'E231' TO WS-REJECT-CODE
               WHEN CNPEN-CURRENCY-YTD > 1000000.00
                   MOVE 'E014' TO WS-REJECT-CODE
               WHEN CNPEN-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E539' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2550-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2560-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = CNPEN-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CNPEN-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE CNPEN-SHIPMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHIPMENT AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2570-LIMIT-SHIPMENT.
           IF CNPEN-SHIPMENT-AMT < 25.00
               MOVE 25.00 TO CNPEN-SHIPMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNPEN-SHIPMENT-AMT > 5000.00
               MOVE 5000.00 TO CNPEN-SHIPMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2580-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CNPEN-NEXT-REVIEW-DATE > 0 AND CNPEN-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNPEN-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CNPEN-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CNPEN-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CNPEN-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD CNPEN-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CNPEN-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CURRENCY YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2590-LIMIT-CURRENCY.
           IF CNPEN-CURRENCY-YTD < 0.00
               MOVE 0.00 TO CNPEN-CURRENCY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNPEN-CURRENCY-YTD > 500.00
               MOVE 500.00 TO CNPEN-CURRENCY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHIPMENT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2600-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNPEN-CHANNEL-CD
                   ADD CNPEN-SHIPMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNPEN-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CNPEN-SHIPMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2610-COUNT-RETRY.
           IF CNPEN-RETRY-CNT < 9999999
               ADD 1 TO CNPEN-RETRY-CNT
           END-IF
           IF CNPEN-RETRY-CNT NOT < 5
               MOVE 'Y' TO CNPEN-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STRAP YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2620-LIMIT-STRAP.
           IF CNPEN-STRAP-YTD < 0.00
               MOVE 0.00 TO CNPEN-STRAP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNPEN-STRAP-YTD > 5000.00
               MOVE 5000.00 TO CNPEN-STRAP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPMENT YTD.
      *----------------------------------------------------------------*
       2630-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNPEN-SHIPMENT-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CNPEN-SHIPMENT-YTD
           END-IF
           ADD CNPEN-SHIPMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT STRAP YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2640-SPLIT-STRAP.
           COMPUTE WS-WORK-AMT = CNPEN-STRAP-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CNPEN-STRAP-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARRIER CD:
      * J -> L WHEN NOTICE DATE IS PAST
      * L -> M WHEN NOTICE DATE IS PAST
      * M -> X WHEN NOTICE DATE IS PAST
      * X -> J WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2650-SET-CARRIER.
           EVALUATE CNPEN-CARRIER-CD
               WHEN 'J'
                   IF CNPEN-NOTICE-DATE > 0 AND CNPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO CNPEN-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF CNPEN-NOTICE-DATE > 0 AND CNPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CNPEN-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CNPEN-NOTICE-DATE > 0 AND CNPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO CNPEN-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF CNPEN-NOTICE-DATE > 0 AND CNPEN-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO CNPEN-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2660-DESCRIBE-REASON.
           EVALUATE CNPEN-REASON-CD
               WHEN 'JL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SHIPMENT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2670-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNPEN-CHANNEL-CD
                   ADD CNPEN-SHIPMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNPEN-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CNPEN-SHIPMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SHIPMENT YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2680-LIMIT-SHIPMENT.
           IF CNPEN-SHIPMENT-YTD < 10.00
               MOVE 10.00 TO CNPEN-SHIPMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNPEN-SHIPMENT-YTD > 10000.00
               MOVE 10000.00 TO CNPEN-SHIPMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CURRENCY YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2690-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNPEN-REASON-CD
                   ADD CNPEN-CURRENCY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNPEN-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CNPEN-CURRENCY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2700-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CNPEN-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CNPEN-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CNPEN-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPMENT AMT.
      *----------------------------------------------------------------*
       2710-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNPEN-SHIPMENT-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CNPEN-SHIPMENT-AMT
           END-IF
           ADD CNPEN-SHIPMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CURRENCY YTD.
      *----------------------------------------------------------------*
       2720-PRORATE-CURRENCY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CNPEN-CURRENCY-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CNPEN-CURRENCY-YTD
           END-IF
           ADD CNPEN-CURRENCY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2730-DESCRIBE-REASON.
           EVALUATE CNPEN-REASON-CD
               WHEN 'JL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CARRIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2740-ACCUM-CARRIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CNPEN-CARRIER-CD
                   ADD CNPEN-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CNPEN-CARRIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CNPEN-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2750-COUNT-ITEM.
           IF CNPEN-ITEM-CNT < 9999999
               ADD 1 TO CNPEN-ITEM-CNT
           END-IF
           IF CNPEN-ITEM-CNT NOT < 6
               MOVE 'Y' TO CNPEN-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE COIN AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2760-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CNPEN-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CNPEN-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CNPEN-STOP-FLAG = 'Y'
                   COMPUTE CNPEN-FEE-AMT ROUNDED = CNPEN-COIN-AMT *
                       CNPEN-PENALTY-PCT / 200
               WHEN CNPEN-CARRIER-CD = 'J'
                   MOVE 20.00 TO CNPEN-FEE-AMT
               WHEN CNPEN-CARRIER-CD = 'L'
                   MOVE 15.00 TO CNPEN-FEE-AMT
               WHEN OTHER
                   COMPUTE CNPEN-FEE-AMT ROUNDED = CNPEN-COIN-AMT *
                       CNPEN-PENALTY-PCT / 100
           END-EVALUATE
           IF CNPEN-FEE-AMT > 0 AND CNPEN-FEE-AMT < 1.00
               MOVE 1.00 TO CNPEN-FEE-AMT
           END-IF
           IF CNPEN-FEE-AMT > 35.00
               MOVE 35.00 TO CNPEN-FEE-AMT
           END-IF
           ADD CNPEN-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPMENT FEE ON THE SHIPMENT YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2770-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CNPEN-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CNPEN-SHIPMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CNPEN-STOP-FLAG = 'Y'
                   COMPUTE CNPEN-SHIPMENT-AMT ROUNDED =
                       CNPEN-SHIPMENT-YTD * CNPEN-PENALTY-PCT / 200
               WHEN CNPEN-CARRIER-CD = 'J'
                   MOVE 7.00 TO CNPEN-SHIPMENT-AMT
               WHEN CNPEN-CARRIER-CD = 'L'
                   MOVE 20.00 TO CNPEN-SHIPMENT-AMT
               WHEN OTHER
                   COMPUTE CNPEN-SHIPMENT-AMT ROUNDED =
                       CNPEN-SHIPMENT-YTD * CNPEN-PENALTY-PCT / 100
           END-EVALUATE
           IF CNPEN-SHIPMENT-AMT > 0 AND CNPEN-SHIPMENT-AMT < 1.00
               MOVE 1.00 TO CNPEN-SHIPMENT-AMT
           END-IF
           IF CNPEN-SHIPMENT-AMT > 150.00
               MOVE 150.00 TO CNPEN-SHIPMENT-AMT
           END-IF
           ADD CNPEN-SHIPMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE SHIPMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2780-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CNPEN-SHIPMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * JL -> LW WHEN NEXT REVIEW DATE IS PAST
      * LW -> PS WHEN NEXT REVIEW DATE IS PAST
      * PS -> JL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2790-SET-REASON.
           EVALUATE CNPEN-REASON-CD
               WHEN 'JL'
                   IF CNPEN-NEXT-REVIEW-DATE > 0 AND
                       CNPEN-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'LW' TO CNPEN-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LW'
                   IF CNPEN-NEXT-REVIEW-DATE > 0 AND
                       CNPEN-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PS' TO CNPEN-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF CNPEN-NEXT-REVIEW-DATE > 0 AND
                       CNPEN-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JL' TO CNPEN-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2800-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CNPEN-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CNPEN-COIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CURRENCY YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2810-LIMIT-CURRENCY.
           IF CNPEN-CURRENCY-YTD < 0.00
               MOVE 0.00 TO CNPEN-CURRENCY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNPEN-CURRENCY-YTD > 500.00
               MOVE 500.00 TO CNPEN-CURRENCY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E520 REF NO REQUIRED
      * E763 CURRENCY YTD NEGATIVE
      * E455 CURRENCY YTD OVER 25000
      * E701 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2820-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNPEN-REF-NO = SPACES
                   MOVE 'E520' TO WS-REJECT-CODE
               WHEN CNPEN-CURRENCY-YTD < 0
                   MOVE 'E763' TO WS-REJECT-CODE
               WHEN CNPEN-CURRENCY-YTD > 25000.00
                   MOVE 'E455' TO WS-REJECT-CODE
               WHEN CNPEN-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E701' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CURRENCY YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2830-LIMIT-CURRENCY.
           IF CNPEN-CURRENCY-YTD < 50.00
               MOVE 50.00 TO CNPEN-CURRENCY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNPEN-CURRENCY-YTD > 50000.00
               MOVE 50000.00 TO CNPEN-CURRENCY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COIN AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2840-LIMIT-COIN.
           IF CNPEN-COIN-AMT < 25.00
               MOVE 25.00 TO CNPEN-COIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CNPEN-COIN-AMT > 50000.00
               MOVE 50000.00 TO CNPEN-COIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E052 CONTACT REQUIRED
      * E142 WORK AMT NEGATIVE
      * E461 WORK AMT OVER 1000000
      * E591 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2850-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CNPEN-CONTACT = SPACES
                   MOVE 'E052' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E142' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E461' TO WS-REJECT-CODE
               WHEN CNPEN-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E591' TO WS-REJECT-CODE
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
                   MOVE 08 TO CNPEN-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO CNPEN-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO CNPEN-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'CNPEN EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
