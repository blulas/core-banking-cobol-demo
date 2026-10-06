      *================================================================*
      * PROGRAM  : PYPRC                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : PAYROLL SERVICES RULE - PRICING. CALLED BY THE      *
      *            PAYROLL SERVICES BATCH AND ONLINE PROGRAMS.         *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-08-27 BHS  ORIGINAL                                       *
      * 1996-12-03 EWS  NEW NET REPORT COLUMNS CHG1759                 *
      * 1997-01-02 LMB  GROSS TABLE EXPANDED PRB6518                   *
      * 1998-11-01 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-06-17 LMB  AUDIT FINDING - NET CONTROL TOTALS PRB4299     *
      * 1999-07-15 RJT  AUDIT FINDING - FICA CONTROL TOTALS CHG3775    *
      * 2009-04-22 MKL  DEDUCT NOTICE TEXT PER LEGAL INC0308           *
      * 2010-03-14 BHS  PERFORMANCE - GROSS READ AHEAD INC6725         *
      * 2010-12-05 PQR  GROSS EXCEPTIONS TO OPS QUEUE INC6627          *
      * 2011-02-24 CRP  DEDUCT ROUNDING - HALF UP INC4026              *
      * 2011-03-26 LMB  FED-TAX STATUS CODES ADDED REQ3073             *
      * 2011-12-02 MKL  STATE-TAX CUTOFF MOVED PRB1348                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PYPRC.
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
                                           VALUE 25.000000.
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
      * WS-STATE-TA-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STATE-TA-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000134000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000666000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000614500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007600000000254500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007850000000694000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008850000000812000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008900000000561500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009400000000207500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009900000000877000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019900000000048500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022400000000823500'.
       01  WS-STATE-TA-BAND-TBL REDEFINES WS-STATE-TA-BAND-VALUES.
           05  WS-STATE-TA-BAND-ROW OCCURS 11 TIMES.
               10  WS-STATE-TA-BAND-LOW    PIC 9(09)V99.
               10  WS-STATE-TA-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DEDUCT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEDUCT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000184000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000375000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000284500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002100000000750500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002350000000899500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012350000000370500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012450000000135000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014950000000241000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017450000000755000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017700000000665000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017950000000135000'.
       01  WS-DEDUCT-SCHED-TBL REDEFINES WS-DEDUCT-SCHED-VALUES.
           05  WS-DEDUCT-SCHED-ROW OCCURS 11 TIMES.
               10  WS-DEDUCT-SCHED-LOW     PIC 9(09)V99.
               10  WS-DEDUCT-SCHED-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-NET-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-NET-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000130500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000632000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000735000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000650000000132500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001150000000631000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001650000000028000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002150000000856500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002200000000428000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002450000000746000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000054500'.
       01  WS-NET-SCHED-TBL REDEFINES WS-NET-SCHED-VALUES.
           05  WS-NET-SCHED-ROW OCCURS 10 TIMES.
               10  WS-NET-SCHED-LOW        PIC 9(09)V99.
               10  WS-NET-SCHED-RATE       PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY PYPRCP.

       PROCEDURE DIVISION USING PYPRC-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCUM-REGION
           PERFORM 2010-AGE-OPEN
           PERFORM 2020-LIMIT-FED-TAX
           PERFORM 2030-AGE-EXPIRY
           PERFORM 2040-CHECK-LAST-PAY
           PERFORM 2050-FLAG-NET
           PERFORM 2060-SELECT-FEE-PCT
           PERFORM 2070-EDIT-CUST-ID
           PERFORM 2080-PRORATE-FED-TAX
           PERFORM 2090-COUNT-ITEM
           PERFORM 2100-CHECK-LAST-PAY
           PERFORM 2110-SELECT-FEE-PCT
           PERFORM 2120-DESCRIBE-TAX-STATE
           PERFORM 2130-CHECK-NEXT-DUE
           PERFORM 2140-LIMIT-ITEM
           PERFORM 2150-AGE-EXPIRY
           PERFORM 2160-SET-SOURCE
           PERFORM 2170-ASSESS-STOP
           PERFORM 2180-FLAG-FED-TAX
           PERFORM 2190-SET-SOURCE
           PERFORM 2200-CHECK-OPEN
           PERFORM 2210-LIMIT-STATE-TAX
           PERFORM 2220-SELECT-MARGIN
           PERFORM 2230-EDIT-CONTACT
           PERFORM 2240-ASSESS-STOP
           PERFORM 2250-CHECK-EXPIRY
           PERFORM 2260-AGE-NEXT-DUE
           PERFORM 2270-PRORATE-FED-TAX
           PERFORM 2280-SPLIT-NET
           PERFORM 2290-SET-STATUS
           PERFORM 2300-AGE-LAST-PAY
           PERFORM 2310-ACCRUE-MARGIN
           PERFORM 2320-AGE-LAST-PAY
           PERFORM 2330-FLAG-STATE-TAX
           PERFORM 2340-AGE-EXPIRY
           PERFORM 2350-LIMIT-FICA
           PERFORM 2360-SELECT-FEE-PCT
           PERFORM 2370-EDIT-CONTACT
           PERFORM 2380-ASSESS-PREFERRED
           PERFORM 2390-SET-TAX-STATE
           PERFORM 2400-COUNT-NOTICE
           PERFORM 2410-AGE-EXPIRY
           PERFORM 2420-EDIT-CUST-ID
           PERFORM 2430-FLAG-NET
           PERFORM 2440-PRORATE-FED-TAX
           PERFORM 2450-EDIT-CUST-ID
           PERFORM 2460-SET-TAX-STATE
           PERFORM 2470-FLAG-FED-TAX
           PERFORM 2480-LIMIT-ITEM
           PERFORM 2490-AGE-EXPIRY
           PERFORM 2500-PRORATE-STATE-TAX
           PERFORM 2510-FLAG-STATE-TAX
           PERFORM 2520-EDIT-CONTACT
           PERFORM 2530-CHECK-NEXT-DUE
           PERFORM 2540-PRORATE-STATE-TAX
           PERFORM 2550-CHECK-NEXT-DUE
           PERFORM 2560-CHECK-EXPIRY
           PERFORM 2570-FLAG-ITEM
           PERFORM 2580-ACCRUE-FICA
           PERFORM 2590-ASSESS-STOP
           PERFORM 2600-SPLIT-STATE-TAX
           PERFORM 2610-EDIT-CONTACT
           PERFORM 2620-ACCUM-SOURCE
           PERFORM 2630-ACCUM-REGION
           PERFORM 2640-COUNT-ITEM
           PERFORM 2650-CHECK-LAST-PAY
           PERFORM 2660-FLAG-ITEM
           PERFORM 2670-SPLIT-FICA
           PERFORM 2680-SPLIT-STATE-TAX
           PERFORM 2690-EDIT-CONTACT
           PERFORM 2700-PRORATE-NET
           PERFORM 2710-LIMIT-STATE-TAX
           PERFORM 2720-CHECK-LAST-PAY
           PERFORM 2730-CHECK-OPEN
           PERFORM 2740-CHECK-LAST-PAY
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO PYPRC-RETURN-CODE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2000-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYPRC-REGION-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYPRC-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2010-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-OPEN-DATE > 0 AND PYPRC-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYPRC-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PYPRC-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PYPRC-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PYPRC-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYPRC-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FED TAX AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2020-LIMIT-FED-TAX.
           IF PYPRC-FED-TAX-AMT < 50.00
               MOVE 50.00 TO PYPRC-FED-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYPRC-FED-TAX-AMT > 2500.00
               MOVE 2500.00 TO PYPRC-FED-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2030-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-EXPIRY-DATE > 0 AND PYPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYPRC-FED-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PYPRC-FED-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PYPRC-FED-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PYPRC-FED-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYPRC-FED-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2040-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PYPRC-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NET AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2050-FLAG-NET.
           IF PYPRC-NET-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PYPRC-NET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEDUCT SCHED TIER FOR THE FED TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2060-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEDUCT-SCHED-RATE(1) TO PYPRC-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PYPRC-FED-TAX-YTD NOT < WS-DEDUCT-SCHED-LOW(WS-TX)
                   MOVE WS-DEDUCT-SCHED-RATE(WS-TX) TO PYPRC-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYPRC-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYPRC-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E713 CUST ID REQUIRED
      * E239 STATE TAX AMT NEGATIVE
      * E607 STATE TAX AMT OVER 250000
      * E145 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2070-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYPRC-CUST-ID = SPACES
                   MOVE 'E713' TO WS-REJECT-CODE
               WHEN PYPRC-STATE-TAX-AMT < 0
                   MOVE 'E239' TO WS-REJECT-CODE
               WHEN PYPRC-STATE-TAX-AMT > 250000.00
                   MOVE 'E607' TO WS-REJECT-CODE
               WHEN PYPRC-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E145' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FED TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2080-PRORATE-FED-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYPRC-FED-TAX-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2090-COUNT-ITEM.
           IF PYPRC-ITEM-CNT < 9999999
               ADD 1 TO PYPRC-ITEM-CNT
           END-IF
           IF PYPRC-ITEM-CNT NOT < 5
               MOVE 'Y' TO PYPRC-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2100-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PYPRC-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEDUCT SCHED TIER FOR THE FICA YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2110-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEDUCT-SCHED-RATE(1) TO PYPRC-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PYPRC-FICA-YTD NOT < WS-DEDUCT-SCHED-LOW(WS-TX)
                   MOVE WS-DEDUCT-SCHED-RATE(WS-TX) TO PYPRC-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYPRC-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYPRC-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TAX STATE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2120-DESCRIBE-TAX-STATE.
           EVALUATE PYPRC-TAX-STATE-CD
               WHEN 'CAZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LZU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TUF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WKE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2130-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO PYPRC-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2140-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2150-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-EXPIRY-DATE > 0 AND PYPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * NY -> PL WHEN LAST PAY DATE IS PAST
      * PL -> VK WHEN LAST PAY DATE IS PAST
      * VK -> WB WHEN LAST PAY DATE IS PAST
      * WB -> XL WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2160-SET-SOURCE.
           EVALUATE PYPRC-SOURCE-CD
               WHEN 'NY'
                   IF PYPRC-LAST-PAY-DATE > 0 AND PYPRC-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PL' TO PYPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF PYPRC-LAST-PAY-DATE > 0 AND PYPRC-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VK' TO PYPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF PYPRC-LAST-PAY-DATE > 0 AND PYPRC-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO PYPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF PYPRC-LAST-PAY-DATE > 0 AND PYPRC-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO PYPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE NET FEE ON THE FICA YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2170-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PYPRC-STOP-FLAG = 'Y'
                   MOVE ZERO TO PYPRC-NET-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYPRC-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PYPRC-NET-AMT ROUNDED = PYPRC-FICA-YTD *
                       PYPRC-MARGIN / 200
               WHEN PYPRC-REGION-CD = 'DP'
                   MOVE 10.00 TO PYPRC-NET-AMT
               WHEN PYPRC-REGION-CD = 'TU'
                   MOVE 20.00 TO PYPRC-NET-AMT
               WHEN OTHER
                   COMPUTE PYPRC-NET-AMT ROUNDED = PYPRC-FICA-YTD *
                       PYPRC-MARGIN / 100
           END-EVALUATE
           IF PYPRC-NET-AMT > 0 AND PYPRC-NET-AMT < 1.00
               MOVE 1.00 TO PYPRC-NET-AMT
           END-IF
           IF PYPRC-NET-AMT > 50.00
               MOVE 50.00 TO PYPRC-NET-AMT
           END-IF
           ADD PYPRC-NET-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FED TAX AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2180-FLAG-FED-TAX.
           IF PYPRC-FED-TAX-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PYPRC-FED-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * NY -> PL WHEN EXPIRY DATE IS PAST
      * PL -> VK WHEN EXPIRY DATE IS PAST
      * VK -> WB WHEN EXPIRY DATE IS PAST
      * WB -> XL WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2190-SET-SOURCE.
           EVALUATE PYPRC-SOURCE-CD
               WHEN 'NY'
                   IF PYPRC-EXPIRY-DATE > 0 AND PYPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PL' TO PYPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF PYPRC-EXPIRY-DATE > 0 AND PYPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VK' TO PYPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF PYPRC-EXPIRY-DATE > 0 AND PYPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO PYPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF PYPRC-EXPIRY-DATE > 0 AND PYPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO PYPRC-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2200-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PYPRC-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2210-LIMIT-STATE-TAX.
           IF PYPRC-STATE-TAX-YTD < 50.00
               MOVE 50.00 TO PYPRC-STATE-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYPRC-STATE-TAX-YTD > 2500.00
               MOVE 2500.00 TO PYPRC-STATE-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STATE TA BAND TIER FOR THE STATE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2220-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STATE-TA-BAND-RATE(1) TO PYPRC-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PYPRC-STATE-TAX-AMT NOT < WS-STATE-TA-BAND-LOW(WS-TX)
                   MOVE WS-STATE-TA-BAND-RATE(WS-TX) TO PYPRC-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYPRC-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYPRC-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E823 CONTACT REQUIRED
      * E376 FICA YTD NEGATIVE
      * E313 FICA YTD OVER 1000000
      * E232 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2230-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYPRC-CONTACT = SPACES
                   MOVE 'E823' TO WS-REJECT-CODE
               WHEN PYPRC-FICA-YTD < 0
                   MOVE 'E376' TO WS-REJECT-CODE
               WHEN PYPRC-FICA-YTD > 1000000.00
                   MOVE 'E313' TO WS-REJECT-CODE
               WHEN PYPRC-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E232' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FED TAX YTD FEE ON THE FED TAX AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2240-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PYPRC-STOP-FLAG = 'Y'
                   MOVE ZERO TO PYPRC-FED-TAX-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYPRC-PREFERRED-FLAG = 'Y'
                   COMPUTE PYPRC-FED-TAX-YTD ROUNDED = PYPRC-FED-TAX-AMT
                       * PYPRC-FEE-PCT / 200
               WHEN PYPRC-SOURCE-CD = 'NY'
                   MOVE 12.00 TO PYPRC-FED-TAX-YTD
               WHEN PYPRC-SOURCE-CD = 'PL'
                   MOVE 10.00 TO PYPRC-FED-TAX-YTD
               WHEN OTHER
                   COMPUTE PYPRC-FED-TAX-YTD ROUNDED = PYPRC-FED-TAX-AMT
                       * PYPRC-FEE-PCT / 100
           END-EVALUATE
           IF PYPRC-FED-TAX-YTD > 0 AND PYPRC-FED-TAX-YTD < 5.00
               MOVE 5.00 TO PYPRC-FED-TAX-YTD
           END-IF
           IF PYPRC-FED-TAX-YTD > 35.00
               MOVE 35.00 TO PYPRC-FED-TAX-YTD
           END-IF
           ADD PYPRC-FED-TAX-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2250-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PYPRC-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2260-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-NEXT-DUE-DATE > 0 AND PYPRC-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYPRC-FED-TAX-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PYPRC-FED-TAX-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PYPRC-FED-TAX-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PYPRC-FED-TAX-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYPRC-FED-TAX-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FICA YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FED TAX AMT.
      *----------------------------------------------------------------*
       2270-PRORATE-FED-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PYPRC-FED-TAX-AMT ROUNDED =
                   PYPRC-FICA-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PYPRC-FED-TAX-AMT
           END-IF
           ADD PYPRC-FED-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT NET AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2280-SPLIT-NET.
           COMPUTE WS-WORK-AMT = PYPRC-NET-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PYPRC-NET-AMT - (WS-WORK-AMT * 4)
           COMPUTE PYPRC-FED-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * MV -> RD WHEN OPEN DATE IS PAST
      * RD -> ST WHEN OPEN DATE IS PAST
      * ST -> TJ WHEN OPEN DATE IS PAST
      * TJ -> UC WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2290-SET-STATUS.
           EVALUATE PYPRC-STATUS-CD
               WHEN 'MV'
                   IF PYPRC-OPEN-DATE > 0 AND PYPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO PYPRC-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF PYPRC-OPEN-DATE > 0 AND PYPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ST' TO PYPRC-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF PYPRC-OPEN-DATE > 0 AND PYPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TJ' TO PYPRC-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TJ'
                   IF PYPRC-OPEN-DATE > 0 AND PYPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO PYPRC-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2300-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-LAST-PAY-DATE > 0 AND PYPRC-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2310-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * PYPRC-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PYPRC-STATE-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2320-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-LAST-PAY-DATE > 0 AND PYPRC-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYPRC-FED-TAX-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PYPRC-FED-TAX-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PYPRC-FED-TAX-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PYPRC-FED-TAX-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYPRC-FED-TAX-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATE TAX AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2330-FLAG-STATE-TAX.
           IF PYPRC-STATE-TAX-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PYPRC-STATE-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2340-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-EXPIRY-DATE > 0 AND PYPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYPRC-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PYPRC-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PYPRC-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PYPRC-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYPRC-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FICA YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2350-LIMIT-FICA.
           IF PYPRC-FICA-YTD < 50.00
               MOVE 50.00 TO PYPRC-FICA-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYPRC-FICA-YTD > 5000.00
               MOVE 5000.00 TO PYPRC-FICA-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEDUCT SCHED TIER FOR THE FICA YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2360-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEDUCT-SCHED-RATE(1) TO PYPRC-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PYPRC-FICA-YTD NOT < WS-DEDUCT-SCHED-LOW(WS-TX)
                   MOVE WS-DEDUCT-SCHED-RATE(WS-TX) TO PYPRC-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYPRC-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYPRC-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E358 CONTACT REQUIRED
      * E376 WORK AMT NEGATIVE
      * E677 WORK AMT OVER 10000
      * E987 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2370-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYPRC-CONTACT = SPACES
                   MOVE 'E358' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E376' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E677' TO WS-REJECT-CODE
               WHEN PYPRC-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E987' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE NET AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2380-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN PYPRC-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYPRC-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = PYPRC-NET-AMT *
                       PYPRC-FEE-PCT / 200
               WHEN PYPRC-TAX-STATE-CD = 'CAZ'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN PYPRC-TAX-STATE-CD = 'LZU'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = PYPRC-NET-AMT *
                       PYPRC-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TAX STATE CD:
      * CAZ -> LZU WHEN OPEN DATE IS PAST
      * LZU -> TUF WHEN OPEN DATE IS PAST
      * TUF -> WKE WHEN OPEN DATE IS PAST
      * WKE -> CAZ WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2390-SET-TAX-STATE.
           EVALUATE PYPRC-TAX-STATE-CD
               WHEN 'CAZ'
                   IF PYPRC-OPEN-DATE > 0 AND PYPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LZU' TO PYPRC-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LZU'
                   IF PYPRC-OPEN-DATE > 0 AND PYPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TUF' TO PYPRC-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TUF'
                   IF PYPRC-OPEN-DATE > 0 AND PYPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKE' TO PYPRC-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WKE'
                   IF PYPRC-OPEN-DATE > 0 AND PYPRC-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CAZ' TO PYPRC-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2400-COUNT-NOTICE.
           IF PYPRC-NOTICE-CNT < 9999999
               ADD 1 TO PYPRC-NOTICE-CNT
           END-IF
           IF PYPRC-NOTICE-CNT NOT < 5
               MOVE 'Y' TO PYPRC-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2410-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-EXPIRY-DATE > 0 AND PYPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYPRC-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD PYPRC-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PYPRC-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PYPRC-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYPRC-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E743 CUST ID REQUIRED
      * E772 STATE TAX YTD NEGATIVE
      * E924 STATE TAX YTD OVER 5000
      * E161 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2420-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYPRC-CUST-ID = SPACES
                   MOVE 'E743' TO WS-REJECT-CODE
               WHEN PYPRC-STATE-TAX-YTD < 0
                   MOVE 'E772' TO WS-REJECT-CODE
               WHEN PYPRC-STATE-TAX-YTD > 5000.00
                   MOVE 'E924' TO WS-REJECT-CODE
               WHEN PYPRC-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E161' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NET AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2430-FLAG-NET.
           IF PYPRC-NET-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PYPRC-NET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FED TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2440-PRORATE-FED-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PYPRC-FED-TAX-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E973 CUST ID REQUIRED
      * E849 STATE TAX YTD NEGATIVE
      * E819 STATE TAX YTD OVER 25000
      * E510 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2450-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYPRC-CUST-ID = SPACES
                   MOVE 'E973' TO WS-REJECT-CODE
               WHEN PYPRC-STATE-TAX-YTD < 0
                   MOVE 'E849' TO WS-REJECT-CODE
               WHEN PYPRC-STATE-TAX-YTD > 25000.00
                   MOVE 'E819' TO WS-REJECT-CODE
               WHEN PYPRC-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E510' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TAX STATE CD:
      * CAZ -> LZU WHEN EXPIRY DATE IS PAST
      * LZU -> TUF WHEN EXPIRY DATE IS PAST
      * TUF -> WKE WHEN EXPIRY DATE IS PAST
      * WKE -> CAZ WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2460-SET-TAX-STATE.
           EVALUATE PYPRC-TAX-STATE-CD
               WHEN 'CAZ'
                   IF PYPRC-EXPIRY-DATE > 0 AND PYPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LZU' TO PYPRC-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LZU'
                   IF PYPRC-EXPIRY-DATE > 0 AND PYPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TUF' TO PYPRC-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TUF'
                   IF PYPRC-EXPIRY-DATE > 0 AND PYPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKE' TO PYPRC-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WKE'
                   IF PYPRC-EXPIRY-DATE > 0 AND PYPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CAZ' TO PYPRC-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FED TAX AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2470-FLAG-FED-TAX.
           IF PYPRC-FED-TAX-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PYPRC-FED-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2480-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2490-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-EXPIRY-DATE > 0 AND PYPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYPRC-FICA-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PYPRC-FICA-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PYPRC-FICA-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PYPRC-FICA-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYPRC-FICA-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE STATE TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2500-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PYPRC-STATE-TAX-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATE TAX YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2510-FLAG-STATE-TAX.
           IF PYPRC-STATE-TAX-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PYPRC-STATE-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E764 CONTACT REQUIRED
      * E023 FED TAX AMT NEGATIVE
      * E359 FED TAX AMT OVER 50000
      * E310 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2520-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYPRC-CONTACT = SPACES
                   MOVE 'E764' TO WS-REJECT-CODE
               WHEN PYPRC-FED-TAX-AMT < 0
                   MOVE 'E023' TO WS-REJECT-CODE
               WHEN PYPRC-FED-TAX-AMT > 50000.00
                   MOVE 'E359' TO WS-REJECT-CODE
               WHEN PYPRC-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E310' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2530-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PYPRC-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STATE TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2540-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PYPRC-STATE-TAX-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2550-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PYPRC-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2560-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PYPRC-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2570-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FICA YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2580-ACCRUE-FICA.
           IF PYPRC-FICA-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYPRC-FICA-YTD * PYPRC-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PYPRC-FED-TAX-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FED TAX FEE ON THE WORK AMT 2.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2590-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PYPRC-STOP-FLAG = 'Y'
                   MOVE ZERO TO PYPRC-FED-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYPRC-PREFERRED-FLAG = 'Y'
                   COMPUTE PYPRC-FED-TAX-AMT ROUNDED = WS-WORK-AMT-2 *
                       PYPRC-FEE-PCT / 200
               WHEN PYPRC-STATUS-CD = 'MV'
                   MOVE 7.00 TO PYPRC-FED-TAX-AMT
               WHEN PYPRC-STATUS-CD = 'RD'
                   MOVE 3.00 TO PYPRC-FED-TAX-AMT
               WHEN OTHER
                   COMPUTE PYPRC-FED-TAX-AMT ROUNDED = WS-WORK-AMT-2 *
                       PYPRC-FEE-PCT / 100
           END-EVALUATE
           IF PYPRC-FED-TAX-AMT > 0 AND PYPRC-FED-TAX-AMT < 1.00
               MOVE 1.00 TO PYPRC-FED-TAX-AMT
           END-IF
           IF PYPRC-FED-TAX-AMT > 25.00
               MOVE 25.00 TO PYPRC-FED-TAX-AMT
           END-IF
           ADD PYPRC-FED-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT STATE TAX AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2600-SPLIT-STATE-TAX.
           COMPUTE WS-WORK-AMT = PYPRC-STATE-TAX-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PYPRC-STATE-TAX-AMT - (WS-WORK-AMT *
               4)
           COMPUTE PYPRC-FED-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E908 CONTACT REQUIRED
      * E595 FED TAX YTD NEGATIVE
      * E550 FED TAX YTD OVER 250000
      * E580 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2610-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYPRC-CONTACT = SPACES
                   MOVE 'E908' TO WS-REJECT-CODE
               WHEN PYPRC-FED-TAX-YTD < 0
                   MOVE 'E595' TO WS-REJECT-CODE
               WHEN PYPRC-FED-TAX-YTD > 250000.00
                   MOVE 'E550' TO WS-REJECT-CODE
               WHEN PYPRC-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E580' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NET AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2620-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYPRC-SOURCE-CD
                   ADD PYPRC-NET-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYPRC-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PYPRC-NET-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FICA YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2630-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYPRC-REGION-CD
                   ADD PYPRC-FICA-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYPRC-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PYPRC-FICA-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2640-COUNT-ITEM.
           IF PYPRC-ITEM-CNT < 9999999
               ADD 1 TO PYPRC-ITEM-CNT
           END-IF
           IF PYPRC-ITEM-CNT NOT < 12
               MOVE 'Y' TO PYPRC-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2650-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PYPRC-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2660-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FICA YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2670-SPLIT-FICA.
           COMPUTE WS-WORK-AMT = PYPRC-FICA-YTD / 12
           COMPUTE WS-WORK-AMT-2 = PYPRC-FICA-YTD - (WS-WORK-AMT * 12)
           COMPUTE PYPRC-STATE-TAX-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STATE TAX YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2680-SPLIT-STATE-TAX.
           COMPUTE WS-WORK-AMT = PYPRC-STATE-TAX-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PYPRC-STATE-TAX-YTD - (WS-WORK-AMT *
               2)
           COMPUTE PYPRC-FED-TAX-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E537 CONTACT REQUIRED
      * E494 FED TAX AMT NEGATIVE
      * E511 FED TAX AMT OVER 50000
      * E608 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2690-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYPRC-CONTACT = SPACES
                   MOVE 'E537' TO WS-REJECT-CODE
               WHEN PYPRC-FED-TAX-AMT < 0
                   MOVE 'E494' TO WS-REJECT-CODE
               WHEN PYPRC-FED-TAX-AMT > 50000.00
                   MOVE 'E511' TO WS-REJECT-CODE
               WHEN PYPRC-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E608' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FICA YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NET AMT.
      *----------------------------------------------------------------*
       2700-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PYPRC-NET-AMT ROUNDED =
                   PYPRC-FICA-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PYPRC-NET-AMT
           END-IF
           ADD PYPRC-NET-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATE TAX AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2710-LIMIT-STATE-TAX.
           IF PYPRC-STATE-TAX-AMT < 25.00
               MOVE 25.00 TO PYPRC-STATE-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYPRC-STATE-TAX-AMT > 10000.00
               MOVE 10000.00 TO PYPRC-STATE-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2720-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PYPRC-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2730-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PYPRC-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2740-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PYPRC-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYPRC-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PYPRC-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO PYPRC-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO PYPRC-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO PYPRC-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'PYPRC EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
