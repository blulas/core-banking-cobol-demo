      *================================================================*
      * PROGRAM  : PYTIER                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : PAYROLL SERVICES RULE - TIERING. CALLED BY THE      *
      *            PAYROLL SERVICES BATCH AND ONLINE PROGRAMS.         *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-08-02 DPS  ORIGINAL                                       *
      * 1993-12-16 BHS  NET TABLE EXPANDED CHG3878                     *
      * 1995-02-03 SKN  DEDUCT NOTICE TEXT PER LEGAL REQ9354           *
      * 1998-09-12 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-01-11 PQR  PERFORMANCE - GROSS READ AHEAD PRB4536         *
      * 2003-08-09 DPS  DEDUCT NOTICE TEXT PER LEGAL PRB8508           *
      * 2005-03-04 LMB  STATE-TAX TABLE EXPANDED REQ9058               *
      * 2010-02-10 DPS  NEW DEDUCT REPORT COLUMNS PRB6616              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PYTIER.
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
                                           VALUE 16.000000.
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
      * WS-GROSS-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-GROSS-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000462000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000194500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000711000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002850000000739500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005350000000859000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015350000000788500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016350000000426500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018850000000322500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021350000000219000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031350000000896500'.
       01  WS-GROSS-BAND-TBL REDEFINES WS-GROSS-BAND-VALUES.
           05  WS-GROSS-BAND-ROW OCCURS 10 TIMES.
               10  WS-GROSS-BAND-LOW       PIC 9(09)V99.
               10  WS-GROSS-BAND-RATE      PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY PYTIERP.

       PROCEDURE DIVISION USING PYTIER-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-COUNT-ITEM
           PERFORM 2010-PRORATE-FED-TAX
           PERFORM 2020-ACCRUE-RATE
           PERFORM 2030-EDIT-DESC
           PERFORM 2040-COUNT-PAYMENT
           PERFORM 2050-LIMIT-DEDUCT
           PERFORM 2060-SET-STATUS
           PERFORM 2070-AGE-EXPIRY
           PERFORM 2080-DESCRIBE-TAX-STATE
           PERFORM 2090-AGE-OPEN
           PERFORM 2100-SELECT-RATE
           PERFORM 2110-COUNT-PAYMENT
           PERFORM 2120-ACCRUE-STATE-TAX
           PERFORM 2130-EDIT-CONTACT
           PERFORM 2140-COUNT-PAYMENT
           PERFORM 2150-ACCRUE-RATE
           PERFORM 2160-COUNT-PAYMENT
           PERFORM 2170-SPLIT-NET
           PERFORM 2180-PRORATE-NET
           PERFORM 2190-ACCUM-STATUS
           PERFORM 2200-ACCRUE-FED-TAX
           PERFORM 2210-ACCRUE-STATE-TAX
           PERFORM 2220-ACCUM-PAY-FREQ
           PERFORM 2230-CHECK-CLOSE
           PERFORM 2240-LIMIT-STATE-TAX
           PERFORM 2250-SELECT-RATE
           PERFORM 2260-FLAG-DEDUCT
           PERFORM 2270-FLAG-STATE-TAX
           PERFORM 2280-ASSESS-SUSPEND
           PERFORM 2290-SELECT-FEE-PCT
           PERFORM 2300-ACCUM-STATUS
           PERFORM 2310-EDIT-CONTACT
           PERFORM 2320-COUNT-PAYMENT
           PERFORM 2330-CHECK-EXPIRY
           PERFORM 2340-SET-PAY-FREQ
           PERFORM 2350-CHECK-EXPIRY
           PERFORM 2360-COUNT-PAYMENT
           PERFORM 2370-EDIT-CONTACT
           PERFORM 2380-COUNT-PAYMENT
           PERFORM 2390-ACCUM-REASON
           PERFORM 2400-FLAG-DEDUCT
           PERFORM 2410-PRORATE-FED-TAX
           PERFORM 2420-DESCRIBE-REASON
           PERFORM 2430-AGE-NEXT-DUE
           PERFORM 2440-LIMIT-ITEM
           PERFORM 2450-CHECK-CLOSE
           PERFORM 2460-SET-PAY-FREQ
           PERFORM 2470-SET-STATUS
           PERFORM 2480-EDIT-CONTACT
           PERFORM 2490-PRORATE-STATE-TAX
           PERFORM 2500-ACCUM-REASON
           PERFORM 2510-ACCRUE-NET
           PERFORM 2520-ASSESS-SUSPEND
           PERFORM 2530-SELECT-FEE-PCT
           PERFORM 2540-COUNT-PAYMENT
           PERFORM 2550-SELECT-FEE-PCT
           PERFORM 2560-SET-REASON
           PERFORM 2570-AGE-CLOSE
           PERFORM 2580-FLAG-ITEM
           PERFORM 2590-AGE-CLOSE
           PERFORM 2600-ACCUM-REASON
           PERFORM 2610-SPLIT-STATE-TAX
           PERFORM 2620-PRORATE-ITEM
           PERFORM 2630-ACCRUE-NET
           PERFORM 2640-EDIT-DESC
           PERFORM 2650-FLAG-ITEM
           PERFORM 2660-SET-REASON
           PERFORM 2670-AGE-OPEN
           PERFORM 2680-ACCRUE-RATE
           PERFORM 2690-SPLIT-STATE-TAX
           PERFORM 2700-EDIT-DESC
           PERFORM 2710-FLAG-ITEM
           PERFORM 2720-SET-STATUS
           PERFORM 2730-SELECT-RATE
           PERFORM 2740-EDIT-DESC
           PERFORM 2750-DESCRIBE-STATUS
           PERFORM 2760-ASSESS-PREFERRED
           PERFORM 2770-EDIT-DESC
           PERFORM 2780-DESCRIBE-STATUS
           PERFORM 2790-LIMIT-ITEM
           PERFORM 2800-DESCRIBE-REASON
           PERFORM 2810-SET-TAX-STATE
           PERFORM 2820-ACCRUE-DEDUCT
           PERFORM 2830-LIMIT-NET
           PERFORM 2840-SET-TAX-STATE
           PERFORM 2850-AGE-CLOSE
           PERFORM 2860-ACCUM-PAY-FREQ
           PERFORM 2870-SELECT-FEE-PCT
           PERFORM 2880-CHECK-CLOSE
           PERFORM 2890-CHECK-OPEN
           PERFORM 2900-ACCUM-PAY-FREQ
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO PYTIER-RETURN-CODE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2000-COUNT-ITEM.
           IF PYTIER-ITEM-CNT < 9999999
               ADD 1 TO PYTIER-ITEM-CNT
           END-IF
           IF PYTIER-ITEM-CNT NOT < 6
               MOVE 'Y' TO PYTIER-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FED TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2010-PRORATE-FED-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYTIER-FED-TAX-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2020-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PYTIER-RATE / 100 / 365
               ADD WS-WORK-AMT TO PYTIER-GROSS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E600 DESC REQUIRED
      * E435 NET YTD NEGATIVE
      * E700 NET YTD OVER 10000
      * E480 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2030-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYTIER-DESC = SPACES
                   MOVE 'E600' TO WS-REJECT-CODE
               WHEN PYTIER-NET-YTD < 0
                   MOVE 'E435' TO WS-REJECT-CODE
               WHEN PYTIER-NET-YTD > 10000.00
                   MOVE 'E700' TO WS-REJECT-CODE
               WHEN PYTIER-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E480' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2040-COUNT-PAYMENT.
           IF PYTIER-PAYMENT-CNT < 9999999
               ADD 1 TO PYTIER-PAYMENT-CNT
           END-IF
           IF PYTIER-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PYTIER-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DEDUCT AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2050-LIMIT-DEDUCT.
           IF PYTIER-DEDUCT-AMT < 25.00
               MOVE 25.00 TO PYTIER-DEDUCT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYTIER-DEDUCT-AMT > 50000.00
               MOVE 50000.00 TO PYTIER-DEDUCT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * MV -> RD WHEN EXPIRY DATE IS PAST
      * RD -> ST WHEN EXPIRY DATE IS PAST
      * ST -> TJ WHEN EXPIRY DATE IS PAST
      * TJ -> UC WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2060-SET-STATUS.
           EVALUATE PYTIER-STATUS-CD
               WHEN 'MV'
                   IF PYTIER-EXPIRY-DATE > 0 AND PYTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO PYTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF PYTIER-EXPIRY-DATE > 0 AND PYTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ST' TO PYTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF PYTIER-EXPIRY-DATE > 0 AND PYTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TJ' TO PYTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TJ'
                   IF PYTIER-EXPIRY-DATE > 0 AND PYTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO PYTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2070-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PYTIER-EXPIRY-DATE > 0 AND PYTIER-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYTIER-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYTIER-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PYTIER-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PYTIER-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PYTIER-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYTIER-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TAX STATE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2080-DESCRIBE-TAX-STATE.
           EVALUATE PYTIER-TAX-STATE-CD
               WHEN 'CAZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LZU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TUF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WKE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2090-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PYTIER-OPEN-DATE > 0 AND PYTIER-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYTIER-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYTIER-DEDUCT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PYTIER-DEDUCT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PYTIER-DEDUCT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PYTIER-DEDUCT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYTIER-DEDUCT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE GROSS BAND TIER FOR THE NET YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2100-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-GROSS-BAND-RATE(1) TO PYTIER-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PYTIER-NET-YTD NOT < WS-GROSS-BAND-LOW(WS-TX)
                   MOVE WS-GROSS-BAND-RATE(WS-TX) TO PYTIER-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYTIER-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYTIER-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2110-COUNT-PAYMENT.
           IF PYTIER-PAYMENT-CNT < 9999999
               ADD 1 TO PYTIER-PAYMENT-CNT
           END-IF
           IF PYTIER-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO PYTIER-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STATE TAX AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2120-ACCRUE-STATE-TAX.
           IF PYTIER-STATE-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYTIER-STATE-TAX-AMT * PYTIER-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO PYTIER-FED-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E414 CONTACT REQUIRED
      * E027 FED TAX AMT NEGATIVE
      * E104 FED TAX AMT OVER 1000000
      * E281 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2130-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYTIER-CONTACT = SPACES
                   MOVE 'E414' TO WS-REJECT-CODE
               WHEN PYTIER-FED-TAX-AMT < 0
                   MOVE 'E027' TO WS-REJECT-CODE
               WHEN PYTIER-FED-TAX-AMT > 1000000.00
                   MOVE 'E104' TO WS-REJECT-CODE
               WHEN PYTIER-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E281' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2140-COUNT-PAYMENT.
           IF PYTIER-PAYMENT-CNT < 9999999
               ADD 1 TO PYTIER-PAYMENT-CNT
           END-IF
           IF PYTIER-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PYTIER-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2150-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PYTIER-RATE / 100 / 365
               ADD WS-WORK-AMT TO PYTIER-GROSS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2160-COUNT-PAYMENT.
           IF PYTIER-PAYMENT-CNT < 9999999
               ADD 1 TO PYTIER-PAYMENT-CNT
           END-IF
           IF PYTIER-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO PYTIER-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT NET AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2170-SPLIT-NET.
           COMPUTE WS-WORK-AMT = PYTIER-NET-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PYTIER-NET-AMT - (WS-WORK-AMT * 12)
           COMPUTE PYTIER-DEDUCT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE NET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2180-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PYTIER-NET-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE GROSS AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2190-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYTIER-STATUS-CD
                   ADD PYTIER-GROSS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYTIER-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PYTIER-GROSS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FED TAX AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2200-ACCRUE-FED-TAX.
           IF PYTIER-FED-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYTIER-FED-TAX-AMT * PYTIER-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STATE TAX AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2210-ACCRUE-STATE-TAX.
           IF PYTIER-STATE-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYTIER-STATE-TAX-AMT * PYTIER-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NET AMT BY PAY FREQ CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2220-ACCUM-PAY-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYTIER-PAY-FREQ-CD
                   ADD PYTIER-NET-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYTIER-PAY-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PYTIER-NET-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2230-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PYTIER-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYTIER-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO PYTIER-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2240-LIMIT-STATE-TAX.
           IF PYTIER-STATE-TAX-AMT < 100.00
               MOVE 100.00 TO PYTIER-STATE-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYTIER-STATE-TAX-AMT > 50000.00
               MOVE 50000.00 TO PYTIER-STATE-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GROSS BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2250-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-GROSS-BAND-RATE(1) TO PYTIER-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-GROSS-BAND-LOW(WS-TX)
                   MOVE WS-GROSS-BAND-RATE(WS-TX) TO PYTIER-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYTIER-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYTIER-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEDUCT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2260-FLAG-DEDUCT.
           IF PYTIER-DEDUCT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PYTIER-DEDUCT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2270-FLAG-STATE-TAX.
           IF PYTIER-STATE-TAX-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PYTIER-STATE-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NET FEE ON THE STATE TAX AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2280-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PYTIER-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PYTIER-NET-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYTIER-PREFERRED-FLAG = 'Y'
                   COMPUTE PYTIER-NET-AMT ROUNDED = PYTIER-STATE-TAX-AMT
                       * PYTIER-RATE / 200
               WHEN PYTIER-REASON-CD = 'B'
                   MOVE 7.00 TO PYTIER-NET-AMT
               WHEN PYTIER-REASON-CD = 'C'
                   MOVE 5.00 TO PYTIER-NET-AMT
               WHEN OTHER
                   COMPUTE PYTIER-NET-AMT ROUNDED = PYTIER-STATE-TAX-AMT
                       * PYTIER-RATE / 100
           END-EVALUATE
           IF PYTIER-NET-AMT > 0 AND PYTIER-NET-AMT < 2.00
               MOVE 2.00 TO PYTIER-NET-AMT
           END-IF
           IF PYTIER-NET-AMT > 100.00
               MOVE 100.00 TO PYTIER-NET-AMT
           END-IF
           ADD PYTIER-NET-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE GROSS BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2290-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GROSS-BAND-RATE(1) TO PYTIER-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-GROSS-BAND-LOW(WS-TX)
                   MOVE WS-GROSS-BAND-RATE(WS-TX) TO PYTIER-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYTIER-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYTIER-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NET YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2300-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYTIER-STATUS-CD
                   ADD PYTIER-NET-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYTIER-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PYTIER-NET-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E027 CONTACT REQUIRED
      * E261 STATE TAX AMT NEGATIVE
      * E830 STATE TAX AMT OVER 50000
      * E595 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2310-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYTIER-CONTACT = SPACES
                   MOVE 'E027' TO WS-REJECT-CODE
               WHEN PYTIER-STATE-TAX-AMT < 0
                   MOVE 'E261' TO WS-REJECT-CODE
               WHEN PYTIER-STATE-TAX-AMT > 50000.00
                   MOVE 'E830' TO WS-REJECT-CODE
               WHEN PYTIER-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E595' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2320-COUNT-PAYMENT.
           IF PYTIER-PAYMENT-CNT < 9999999
               ADD 1 TO PYTIER-PAYMENT-CNT
           END-IF
           IF PYTIER-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO PYTIER-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2330-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PYTIER-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYTIER-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO PYTIER-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAY FREQ CD:
      * JT -> XM WHEN OPEN DATE IS PAST
      * XM -> ZT WHEN OPEN DATE IS PAST
      * ZT -> JT WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2340-SET-PAY-FREQ.
           EVALUATE PYTIER-PAY-FREQ-CD
               WHEN 'JT'
                   IF PYTIER-OPEN-DATE > 0 AND PYTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO PYTIER-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF PYTIER-OPEN-DATE > 0 AND PYTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZT' TO PYTIER-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZT'
                   IF PYTIER-OPEN-DATE > 0 AND PYTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO PYTIER-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2350-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PYTIER-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYTIER-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO PYTIER-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2360-COUNT-PAYMENT.
           IF PYTIER-PAYMENT-CNT < 9999999
               ADD 1 TO PYTIER-PAYMENT-CNT
           END-IF
           IF PYTIER-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PYTIER-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E736 CONTACT REQUIRED
      * E855 DEDUCT AMT NEGATIVE
      * E762 DEDUCT AMT OVER 5000
      * E184 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2370-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYTIER-CONTACT = SPACES
                   MOVE 'E736' TO WS-REJECT-CODE
               WHEN PYTIER-DEDUCT-AMT < 0
                   MOVE 'E855' TO WS-REJECT-CODE
               WHEN PYTIER-DEDUCT-AMT > 5000.00
                   MOVE 'E762' TO WS-REJECT-CODE
               WHEN PYTIER-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E184' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2380-COUNT-PAYMENT.
           IF PYTIER-PAYMENT-CNT < 9999999
               ADD 1 TO PYTIER-PAYMENT-CNT
           END-IF
           IF PYTIER-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO PYTIER-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NET AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2390-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYTIER-REASON-CD
                   ADD PYTIER-NET-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYTIER-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PYTIER-NET-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DEDUCT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2400-FLAG-DEDUCT.
           IF PYTIER-DEDUCT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PYTIER-DEDUCT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FED TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2410-PRORATE-FED-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PYTIER-FED-TAX-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2420-DESCRIBE-REASON.
           EVALUATE PYTIER-REASON-CD
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'C'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'G'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'L'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2430-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PYTIER-NEXT-DUE-DATE > 0 AND PYTIER-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYTIER-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2440-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2450-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PYTIER-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYTIER-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO PYTIER-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAY FREQ CD:
      * JT -> XM WHEN EXPIRY DATE IS PAST
      * XM -> ZT WHEN EXPIRY DATE IS PAST
      * ZT -> JT WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2460-SET-PAY-FREQ.
           EVALUATE PYTIER-PAY-FREQ-CD
               WHEN 'JT'
                   IF PYTIER-EXPIRY-DATE > 0 AND PYTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO PYTIER-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF PYTIER-EXPIRY-DATE > 0 AND PYTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZT' TO PYTIER-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZT'
                   IF PYTIER-EXPIRY-DATE > 0 AND PYTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO PYTIER-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * MV -> RD WHEN OPEN DATE IS PAST
      * RD -> ST WHEN OPEN DATE IS PAST
      * ST -> TJ WHEN OPEN DATE IS PAST
      * TJ -> UC WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2470-SET-STATUS.
           EVALUATE PYTIER-STATUS-CD
               WHEN 'MV'
                   IF PYTIER-OPEN-DATE > 0 AND PYTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO PYTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF PYTIER-OPEN-DATE > 0 AND PYTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ST' TO PYTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF PYTIER-OPEN-DATE > 0 AND PYTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TJ' TO PYTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TJ'
                   IF PYTIER-OPEN-DATE > 0 AND PYTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO PYTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E148 CONTACT REQUIRED
      * E273 NET YTD NEGATIVE
      * E553 NET YTD OVER 50000
      * E057 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2480-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYTIER-CONTACT = SPACES
                   MOVE 'E148' TO WS-REJECT-CODE
               WHEN PYTIER-NET-YTD < 0
                   MOVE 'E273' TO WS-REJECT-CODE
               WHEN PYTIER-NET-YTD > 50000.00
                   MOVE 'E553' TO WS-REJECT-CODE
               WHEN PYTIER-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E057' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STATE TAX AMT.
      *----------------------------------------------------------------*
       2490-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PYTIER-STATE-TAX-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PYTIER-STATE-TAX-AMT
           END-IF
           ADD PYTIER-STATE-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2500-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYTIER-REASON-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYTIER-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NET AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2510-ACCRUE-NET.
           IF PYTIER-NET-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYTIER-NET-AMT * PYTIER-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO PYTIER-DEDUCT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NET FEE ON THE NET YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2520-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PYTIER-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PYTIER-NET-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYTIER-HOLD-FLAG = 'Y'
                   COMPUTE PYTIER-NET-AMT ROUNDED = PYTIER-NET-YTD *
                       PYTIER-FEE-PCT / 200
               WHEN PYTIER-STATUS-CD = 'MV'
                   MOVE 5.00 TO PYTIER-NET-AMT
               WHEN PYTIER-STATUS-CD = 'RD'
                   MOVE 5.00 TO PYTIER-NET-AMT
               WHEN OTHER
                   COMPUTE PYTIER-NET-AMT ROUNDED = PYTIER-NET-YTD *
                       PYTIER-FEE-PCT / 100
           END-EVALUATE
           IF PYTIER-NET-AMT > 0 AND PYTIER-NET-AMT < 2.00
               MOVE 2.00 TO PYTIER-NET-AMT
           END-IF
           IF PYTIER-NET-AMT > 100.00
               MOVE 100.00 TO PYTIER-NET-AMT
           END-IF
           ADD PYTIER-NET-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE GROSS BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2530-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GROSS-BAND-RATE(1) TO PYTIER-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-GROSS-BAND-LOW(WS-TX)
                   MOVE WS-GROSS-BAND-RATE(WS-TX) TO PYTIER-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYTIER-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYTIER-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2540-COUNT-PAYMENT.
           IF PYTIER-PAYMENT-CNT < 9999999
               ADD 1 TO PYTIER-PAYMENT-CNT
           END-IF
           IF PYTIER-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PYTIER-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GROSS BAND TIER FOR THE STATE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2550-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GROSS-BAND-RATE(1) TO PYTIER-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PYTIER-STATE-TAX-AMT NOT < WS-GROSS-BAND-LOW(WS-TX)
                   MOVE WS-GROSS-BAND-RATE(WS-TX) TO PYTIER-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYTIER-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYTIER-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> C WHEN EXPIRY DATE IS PAST
      * C -> G WHEN EXPIRY DATE IS PAST
      * G -> H WHEN EXPIRY DATE IS PAST
      * H -> L WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2560-SET-REASON.
           EVALUATE PYTIER-REASON-CD
               WHEN 'B'
                   IF PYTIER-EXPIRY-DATE > 0 AND PYTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO PYTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PYTIER-EXPIRY-DATE > 0 AND PYTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO PYTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF PYTIER-EXPIRY-DATE > 0 AND PYTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PYTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PYTIER-EXPIRY-DATE > 0 AND PYTIER-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO PYTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2570-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PYTIER-CLOSE-DATE > 0 AND PYTIER-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYTIER-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYTIER-DEDUCT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PYTIER-DEDUCT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PYTIER-DEDUCT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PYTIER-DEDUCT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYTIER-DEDUCT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2580-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2590-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PYTIER-CLOSE-DATE > 0 AND PYTIER-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYTIER-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYTIER-STATE-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PYTIER-STATE-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PYTIER-STATE-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PYTIER-STATE-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYTIER-STATE-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2600-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYTIER-REASON-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYTIER-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STATE TAX AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2610-SPLIT-STATE-TAX.
           COMPUTE WS-WORK-AMT = PYTIER-STATE-TAX-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PYTIER-STATE-TAX-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2620-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NET AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2630-ACCRUE-NET.
           IF PYTIER-NET-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYTIER-NET-AMT * PYTIER-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO PYTIER-FED-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E387 DESC REQUIRED
      * E446 WORK AMT 2 NEGATIVE
      * E740 WORK AMT 2 OVER 5000
      * E581 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2640-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYTIER-DESC = SPACES
                   MOVE 'E387' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E446' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E740' TO WS-REJECT-CODE
               WHEN PYTIER-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E581' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2650-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> C WHEN OPEN DATE IS PAST
      * C -> G WHEN OPEN DATE IS PAST
      * G -> H WHEN OPEN DATE IS PAST
      * H -> L WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2660-SET-REASON.
           EVALUATE PYTIER-REASON-CD
               WHEN 'B'
                   IF PYTIER-OPEN-DATE > 0 AND PYTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO PYTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PYTIER-OPEN-DATE > 0 AND PYTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO PYTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF PYTIER-OPEN-DATE > 0 AND PYTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PYTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PYTIER-OPEN-DATE > 0 AND PYTIER-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO PYTIER-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2670-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PYTIER-OPEN-DATE > 0 AND PYTIER-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYTIER-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PYTIER-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PYTIER-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PYTIER-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PYTIER-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PYTIER-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2680-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PYTIER-RATE / 100 / 365
               ADD WS-WORK-AMT TO PYTIER-NET-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2690-SPLIT-STATE-TAX.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE PYTIER-STATE-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E351 DESC REQUIRED
      * E143 NET YTD NEGATIVE
      * E667 NET YTD OVER 99999
      * E706 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2700-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYTIER-DESC = SPACES
                   MOVE 'E351' TO WS-REJECT-CODE
               WHEN PYTIER-NET-YTD < 0
                   MOVE 'E143' TO WS-REJECT-CODE
               WHEN PYTIER-NET-YTD > 99999.00
                   MOVE 'E667' TO WS-REJECT-CODE
               WHEN PYTIER-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E706' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2710-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * MV -> RD WHEN CLOSE DATE IS PAST
      * RD -> ST WHEN CLOSE DATE IS PAST
      * ST -> TJ WHEN CLOSE DATE IS PAST
      * TJ -> UC WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2720-SET-STATUS.
           EVALUATE PYTIER-STATUS-CD
               WHEN 'MV'
                   IF PYTIER-CLOSE-DATE > 0 AND PYTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO PYTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF PYTIER-CLOSE-DATE > 0 AND PYTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ST' TO PYTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF PYTIER-CLOSE-DATE > 0 AND PYTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TJ' TO PYTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TJ'
                   IF PYTIER-CLOSE-DATE > 0 AND PYTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO PYTIER-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE GROSS BAND TIER FOR THE STATE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2730-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-GROSS-BAND-RATE(1) TO PYTIER-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PYTIER-STATE-TAX-AMT NOT < WS-GROSS-BAND-LOW(WS-TX)
                   MOVE WS-GROSS-BAND-RATE(WS-TX) TO PYTIER-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYTIER-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYTIER-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E496 DESC REQUIRED
      * E764 FED TAX AMT NEGATIVE
      * E180 FED TAX AMT OVER 5000
      * E539 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2740-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYTIER-DESC = SPACES
                   MOVE 'E496' TO WS-REJECT-CODE
               WHEN PYTIER-FED-TAX-AMT < 0
                   MOVE 'E764' TO WS-REJECT-CODE
               WHEN PYTIER-FED-TAX-AMT > 5000.00
                   MOVE 'E180' TO WS-REJECT-CODE
               WHEN PYTIER-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E539' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2750-DESCRIBE-STATUS.
           EVALUATE PYTIER-STATUS-CD
               WHEN 'MV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ST'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DEDUCT FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2760-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN PYTIER-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO PYTIER-DEDUCT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PYTIER-SUSPEND-FLAG = 'Y'
                   COMPUTE PYTIER-DEDUCT-AMT ROUNDED = WS-WORK-AMT *
                       PYTIER-RATE / 200
               WHEN PYTIER-PAY-FREQ-CD = 'JT'
                   MOVE 5.00 TO PYTIER-DEDUCT-AMT
               WHEN PYTIER-PAY-FREQ-CD = 'XM'
                   MOVE 5.00 TO PYTIER-DEDUCT-AMT
               WHEN OTHER
                   COMPUTE PYTIER-DEDUCT-AMT ROUNDED = WS-WORK-AMT *
                       PYTIER-RATE / 100
           END-EVALUATE
           IF PYTIER-DEDUCT-AMT > 0 AND PYTIER-DEDUCT-AMT < 5.00
               MOVE 5.00 TO PYTIER-DEDUCT-AMT
           END-IF
           IF PYTIER-DEDUCT-AMT > 100.00
               MOVE 100.00 TO PYTIER-DEDUCT-AMT
           END-IF
           ADD PYTIER-DEDUCT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E025 DESC REQUIRED
      * E392 FED TAX AMT NEGATIVE
      * E325 FED TAX AMT OVER 1000000
      * E025 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2770-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PYTIER-DESC = SPACES
                   MOVE 'E025' TO WS-REJECT-CODE
               WHEN PYTIER-FED-TAX-AMT < 0
                   MOVE 'E392' TO WS-REJECT-CODE
               WHEN PYTIER-FED-TAX-AMT > 1000000.00
                   MOVE 'E325' TO WS-REJECT-CODE
               WHEN PYTIER-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E025' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2780-DESCRIBE-STATUS.
           EVALUATE PYTIER-STATUS-CD
               WHEN 'MV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ST'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2790-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2800-DESCRIBE-REASON.
           EVALUATE PYTIER-REASON-CD
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'C'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'G'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'L'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TAX STATE CD:
      * CAZ -> LZU WHEN NEXT DUE DATE IS PAST
      * LZU -> TUF WHEN NEXT DUE DATE IS PAST
      * TUF -> WKE WHEN NEXT DUE DATE IS PAST
      * WKE -> CAZ WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2810-SET-TAX-STATE.
           EVALUATE PYTIER-TAX-STATE-CD
               WHEN 'CAZ'
                   IF PYTIER-NEXT-DUE-DATE > 0 AND PYTIER-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LZU' TO PYTIER-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LZU'
                   IF PYTIER-NEXT-DUE-DATE > 0 AND PYTIER-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TUF' TO PYTIER-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TUF'
                   IF PYTIER-NEXT-DUE-DATE > 0 AND PYTIER-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WKE' TO PYTIER-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WKE'
                   IF PYTIER-NEXT-DUE-DATE > 0 AND PYTIER-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'CAZ' TO PYTIER-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEDUCT AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2820-ACCRUE-DEDUCT.
           IF PYTIER-DEDUCT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PYTIER-DEDUCT-AMT * PYTIER-RATE / 100 / 360
               ADD WS-WORK-AMT TO PYTIER-GROSS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NET YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2830-LIMIT-NET.
           IF PYTIER-NET-YTD < 100.00
               MOVE 100.00 TO PYTIER-NET-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PYTIER-NET-YTD > 50000.00
               MOVE 50000.00 TO PYTIER-NET-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TAX STATE CD:
      * CAZ -> LZU WHEN CLOSE DATE IS PAST
      * LZU -> TUF WHEN CLOSE DATE IS PAST
      * TUF -> WKE WHEN CLOSE DATE IS PAST
      * WKE -> CAZ WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2840-SET-TAX-STATE.
           EVALUATE PYTIER-TAX-STATE-CD
               WHEN 'CAZ'
                   IF PYTIER-CLOSE-DATE > 0 AND PYTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LZU' TO PYTIER-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LZU'
                   IF PYTIER-CLOSE-DATE > 0 AND PYTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TUF' TO PYTIER-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TUF'
                   IF PYTIER-CLOSE-DATE > 0 AND PYTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKE' TO PYTIER-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WKE'
                   IF PYTIER-CLOSE-DATE > 0 AND PYTIER-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CAZ' TO PYTIER-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2850-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PYTIER-CLOSE-DATE > 0 AND PYTIER-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYTIER-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FED TAX AMT BY PAY FREQ CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2860-ACCUM-PAY-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYTIER-PAY-FREQ-CD
                   ADD PYTIER-FED-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYTIER-PAY-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PYTIER-FED-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GROSS BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2870-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GROSS-BAND-RATE(1) TO PYTIER-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-GROSS-BAND-LOW(WS-TX)
                   MOVE WS-GROSS-BAND-RATE(WS-TX) TO PYTIER-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PYTIER-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PYTIER-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2880-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PYTIER-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYTIER-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PYTIER-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2890-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PYTIER-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PYTIER-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PYTIER-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY PAY FREQ CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2900-ACCUM-PAY-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PYTIER-PAY-FREQ-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PYTIER-PAY-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO PYTIER-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO PYTIER-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO PYTIER-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'PYTIER EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
