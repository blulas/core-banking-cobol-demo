      *================================================================*
      * PROGRAM  : BPELG                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : BILL PAYMENT SERVICE RULE - STATUS. CALLED BY THE   *
      *            BILL PAYMENT SERVICE BATCH AND ONLINE PROGRAMS.     *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-01-15 CRP  ORIGINAL                                       *
      * 1996-01-24 BHS  REFUND EXCEPTIONS TO OPS QUEUE REQ9931         *
      * 1998-02-28 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-11-27 BHS  FEE ROUNDING - HALF UP INC4999                 *
      * 2004-08-23 SKN  AUDIT FINDING - FEE CONTROL TOTALS CHG4149     *
      * 2008-11-22 LMB  PAYMENT STATUS CODES ADDED CHG2929             *
      * 2008-06-10 MKL  REFUND NOTICE TEXT PER LEGAL INC1973           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BPELG.
       AUTHOR. R J TRAVERS.

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
      * WS-REFUND-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REFUND-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000744500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000143000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000565000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000551500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001600000000592000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000896000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003600000000155000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003700000000324000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003800000000680500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004050000000561000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000162500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015050000000128000'.
       01  WS-REFUND-SCHED-TBL REDEFINES WS-REFUND-SCHED-VALUES.
           05  WS-REFUND-SCHED-ROW OCCURS 12 TIMES.
               10  WS-REFUND-SCHED-LOW     PIC 9(09)V99.
               10  WS-REFUND-SCHED-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000356500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000147000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000220000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021000000000571500'.
       01  WS-FEE-GRID-TBL REDEFINES WS-FEE-GRID-VALUES.
           05  WS-FEE-GRID-ROW OCCURS 4 TIMES.
               10  WS-FEE-GRID-LOW         PIC 9(09)V99.
               10  WS-FEE-GRID-RATE        PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY BPELGP.

       PROCEDURE DIVISION USING BPELG-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCUM-PAYEE-TYPE
           PERFORM 2010-PRORATE-FEE
           PERFORM 2020-AGE-LAST-STMT
           PERFORM 2030-AGE-LAST-STMT
           PERFORM 2040-PRORATE-REFUND
           PERFORM 2050-EDIT-ADDR
           PERFORM 2060-AGE-LAST-STMT
           PERFORM 2070-LIMIT-PAYMENT
           PERFORM 2080-PRORATE-PAYMENT
           PERFORM 2090-DESCRIBE-STATUS
           PERFORM 2100-ASSESS-HOLD
           PERFORM 2110-ASSESS-HOLD
           PERFORM 2120-ACCUM-CYCLE
           PERFORM 2130-SPLIT-REFUND
           PERFORM 2140-LIMIT-PAYMENT
           PERFORM 2150-SELECT-MARGIN
           PERFORM 2160-SPLIT-REFUND
           PERFORM 2170-ACCUM-REMIT-METHOD
           PERFORM 2180-DESCRIBE-PAYEE-TYPE
           PERFORM 2190-PRORATE-FEE
           PERFORM 2200-AGE-CLOSE
           PERFORM 2210-FLAG-ITEM
           PERFORM 2220-DESCRIBE-CYCLE
           PERFORM 2230-CHECK-CLOSE
           PERFORM 2240-ACCRUE-MARGIN
           PERFORM 2250-COUNT-EXCEPT
           PERFORM 2260-AGE-NEXT-DUE
           PERFORM 2270-COUNT-EXCEPT
           PERFORM 2280-ACCRUE-RATE
           PERFORM 2290-COUNT-CYCLE
           PERFORM 2300-FLAG-REFUND
           PERFORM 2310-FLAG-FEE
           PERFORM 2320-SET-STATUS
           PERFORM 2330-LIMIT-ITEM
           PERFORM 2340-COUNT-EXCEPT
           PERFORM 2350-LIMIT-FEE
           PERFORM 2360-FLAG-FEE
           PERFORM 2370-ASSESS-SUSPEND
           PERFORM 2380-CHECK-NEXT-DUE
           PERFORM 2390-CHECK-NEXT-DUE
           PERFORM 2400-SPLIT-PAYMENT
           PERFORM 2410-SET-STATUS
           PERFORM 2420-SPLIT-PAYMENT
           PERFORM 2430-SPLIT-FEE
           PERFORM 2440-EDIT-ADDR
           PERFORM 2450-DESCRIBE-CYCLE
           PERFORM 2460-COUNT-CYCLE
           PERFORM 2470-DESCRIBE-REMIT-METHOD
           PERFORM 2480-PRORATE-REFUND
           PERFORM 2490-SET-PAYEE-TYPE
           PERFORM 2500-ASSESS-HOLD
           PERFORM 2510-EDIT-REF-NO
           PERFORM 2520-FLAG-REFUND
           PERFORM 2530-ACCRUE-MARGIN
           PERFORM 2540-ACCUM-REMIT-METHOD
           PERFORM 2550-DESCRIBE-REMIT-METHOD
           PERFORM 2560-COUNT-EXCEPT
           PERFORM 2570-FLAG-REFUND
           PERFORM 2580-AGE-CLOSE
           PERFORM 2590-EDIT-REF-NO
           PERFORM 2600-AGE-LAST-STMT
           PERFORM 2610-ACCUM-REMIT-METHOD
           PERFORM 2620-LIMIT-ITEM
           PERFORM 2630-SET-PAYEE-TYPE
           PERFORM 2640-PRORATE-FEE
           PERFORM 2650-ASSESS-OVERRIDE
           PERFORM 2660-SPLIT-FEE
           PERFORM 2670-CHECK-CLOSE
           PERFORM 2680-ASSESS-HOLD
           PERFORM 2690-ACCUM-STATUS
           PERFORM 2700-ACCRUE-FEE
           PERFORM 2710-ASSESS-OVERRIDE
           PERFORM 2720-ACCUM-CYCLE
           PERFORM 2730-AGE-EXPIRY
           PERFORM 2740-FLAG-REFUND
           PERFORM 2750-ACCRUE-FEE
           PERFORM 2760-SELECT-MARGIN
           PERFORM 2770-SET-CYCLE
           PERFORM 2780-EDIT-REF-NO
           PERFORM 2790-EDIT-ADDR
           PERFORM 2800-COUNT-EXCEPT
           PERFORM 2810-AGE-EXPIRY
           PERFORM 2820-ACCUM-PAYEE-TYPE
           PERFORM 2830-SELECT-RATE
           PERFORM 2840-CHECK-EXPIRY
           PERFORM 2850-ACCUM-CYCLE
           PERFORM 2860-CHECK-EXPIRY
           PERFORM 2870-AGE-EXPIRY
           PERFORM 2880-ASSESS-SUSPEND
           PERFORM 2890-ACCUM-REMIT-METHOD
           PERFORM 2900-SPLIT-PAYMENT
           PERFORM 2910-EDIT-REF-NO
           PERFORM 2920-SPLIT-REFUND
           PERFORM 2930-LIMIT-ITEM
           PERFORM 2940-DESCRIBE-CYCLE
           PERFORM 2950-FLAG-ITEM
           PERFORM 2960-ACCRUE-FEE
           PERFORM 2970-ASSESS-HOLD
           PERFORM 2980-FLAG-ITEM
           PERFORM 2990-PRORATE-FEE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO BPELG-RETURN-CODE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY PAYEE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2000-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPELG-PAYEE-TYPE-CD
                   ADD BPELG-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPELG-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPELG-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2010-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   BPELG-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2020-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BPELG-LAST-STMT-DATE > 0 AND BPELG-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPELG-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPELG-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD BPELG-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BPELG-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPELG-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPELG-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2030-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BPELG-LAST-STMT-DATE > 0 AND BPELG-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPELG-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPELG-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BPELG-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BPELG-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BPELG-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPELG-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REFUND YTD.
      *----------------------------------------------------------------*
       2040-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPELG-REFUND-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPELG-REFUND-YTD
           END-IF
           ADD BPELG-REFUND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E306 ADDR REQUIRED
      * E462 PAYMENT AMT NEGATIVE
      * E865 PAYMENT AMT OVER 10000
      * E521 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2050-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPELG-ADDR = SPACES
                   MOVE 'E306' TO WS-REJECT-CODE
               WHEN BPELG-PAYMENT-AMT < 0
                   MOVE 'E462' TO WS-REJECT-CODE
               WHEN BPELG-PAYMENT-AMT > 10000.00
                   MOVE 'E865' TO WS-REJECT-CODE
               WHEN BPELG-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E521' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2060-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BPELG-LAST-STMT-DATE > 0 AND BPELG-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPELG-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2070-LIMIT-PAYMENT.
           IF BPELG-PAYMENT-YTD < 25.00
               MOVE 25.00 TO BPELG-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPELG-PAYMENT-YTD > 1000.00
               MOVE 1000.00 TO BPELG-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       2080-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPELG-PAYMENT-YTD ROUNDED =
                   BPELG-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPELG-PAYMENT-YTD
           END-IF
           ADD BPELG-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2090-DESCRIBE-STATUS.
           EVALUATE BPELG-STATUS-CD
               WHEN 'JX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND YTD FEE ON THE FEE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2100-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BPELG-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BPELG-REFUND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPELG-OVERRIDE-FLAG = 'Y'
                   COMPUTE BPELG-REFUND-YTD ROUNDED = BPELG-FEE-YTD *
                       BPELG-MARGIN / 200
               WHEN BPELG-PAYEE-TYPE-CD = 'BT'
                   MOVE 12.00 TO BPELG-REFUND-YTD
               WHEN BPELG-PAYEE-TYPE-CD = 'FR'
                   MOVE 20.00 TO BPELG-REFUND-YTD
               WHEN OTHER
                   COMPUTE BPELG-REFUND-YTD ROUNDED = BPELG-FEE-YTD *
                       BPELG-MARGIN / 100
           END-EVALUATE
           IF BPELG-REFUND-YTD > 0 AND BPELG-REFUND-YTD < 1.00
               MOVE 1.00 TO BPELG-REFUND-YTD
           END-IF
           IF BPELG-REFUND-YTD > 150.00
               MOVE 150.00 TO BPELG-REFUND-YTD
           END-IF
           ADD BPELG-REFUND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE FEE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2110-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BPELG-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BPELG-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPELG-SUSPEND-FLAG = 'Y'
                   COMPUTE BPELG-PAYMENT-YTD ROUNDED = BPELG-FEE-AMT *
                       BPELG-RATE / 200
               WHEN BPELG-CYCLE-CD = 'DA'
                   MOVE 3.00 TO BPELG-PAYMENT-YTD
               WHEN BPELG-CYCLE-CD = 'FP'
                   MOVE 3.00 TO BPELG-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE BPELG-PAYMENT-YTD ROUNDED = BPELG-FEE-AMT *
                       BPELG-RATE / 100
           END-EVALUATE
           IF BPELG-PAYMENT-YTD > 0 AND BPELG-PAYMENT-YTD < 10.00
               MOVE 10.00 TO BPELG-PAYMENT-YTD
           END-IF
           IF BPELG-PAYMENT-YTD > 50.00
               MOVE 50.00 TO BPELG-PAYMENT-YTD
           END-IF
           ADD BPELG-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2120-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPELG-CYCLE-CD
                   ADD BPELG-REFUND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPELG-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPELG-REFUND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REFUND YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2130-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = BPELG-REFUND-YTD / 3
           COMPUTE WS-WORK-AMT-2 = BPELG-REFUND-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2140-LIMIT-PAYMENT.
           IF BPELG-PAYMENT-YTD < 50.00
               MOVE 50.00 TO BPELG-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPELG-PAYMENT-YTD > 50000.00
               MOVE 50000.00 TO BPELG-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND SCHED TIER FOR THE REFUND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2150-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-SCHED-RATE(1) TO BPELG-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BPELG-REFUND-AMT NOT < WS-REFUND-SCHED-LOW(WS-TX)
                   MOVE WS-REFUND-SCHED-RATE(WS-TX) TO BPELG-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPELG-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPELG-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REFUND YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2160-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = BPELG-REFUND-YTD / 3
           COMPUTE WS-WORK-AMT-2 = BPELG-REFUND-YTD - (WS-WORK-AMT * 3)
           COMPUTE BPELG-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY REMIT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2170-ACCUM-REMIT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPELG-REMIT-METHOD-CD
                   ADD BPELG-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPELG-REMIT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPELG-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2180-DESCRIBE-PAYEE-TYPE.
           EVALUATE BPELG-PAYEE-TYPE-CD
               WHEN 'BT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'US'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       2190-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPELG-FEE-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPELG-FEE-YTD
           END-IF
           ADD BPELG-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2200-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BPELG-CLOSE-DATE > 0 AND BPELG-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPELG-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPELG-REFUND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BPELG-REFUND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BPELG-REFUND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPELG-REFUND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPELG-REFUND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2210-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2220-DESCRIBE-CYCLE.
           EVALUATE BPELG-CYCLE-CD
               WHEN 'DA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2230-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BPELG-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPELG-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BPELG-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2240-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * BPELG-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BPELG-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2250-COUNT-EXCEPT.
           IF BPELG-EXCEPT-CNT < 9999999
               ADD 1 TO BPELG-EXCEPT-CNT
           END-IF
           IF BPELG-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO BPELG-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2260-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BPELG-NEXT-DUE-DATE > 0 AND BPELG-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPELG-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPELG-REFUND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BPELG-REFUND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BPELG-REFUND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BPELG-REFUND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPELG-REFUND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2270-COUNT-EXCEPT.
           IF BPELG-EXCEPT-CNT < 9999999
               ADD 1 TO BPELG-EXCEPT-CNT
           END-IF
           IF BPELG-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO BPELG-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2280-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * BPELG-RATE / 100 / 360
               ADD WS-WORK-AMT TO BPELG-REFUND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2290-COUNT-CYCLE.
           IF BPELG-CYCLE-CNT < 9999999
               ADD 1 TO BPELG-CYCLE-CNT
           END-IF
           IF BPELG-CYCLE-CNT NOT < 10
               MOVE 'Y' TO BPELG-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2300-FLAG-REFUND.
           IF BPELG-REFUND-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPELG-REFUND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2310-FLAG-FEE.
           IF BPELG-FEE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPELG-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * JX -> LR WHEN CLOSE DATE IS PAST
      * LR -> NZ WHEN CLOSE DATE IS PAST
      * NZ -> RV WHEN CLOSE DATE IS PAST
      * RV -> TM WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2320-SET-STATUS.
           EVALUATE BPELG-STATUS-CD
               WHEN 'JX'
                   IF BPELG-CLOSE-DATE > 0 AND BPELG-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LR' TO BPELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LR'
                   IF BPELG-CLOSE-DATE > 0 AND BPELG-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO BPELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF BPELG-CLOSE-DATE > 0 AND BPELG-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO BPELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF BPELG-CLOSE-DATE > 0 AND BPELG-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO BPELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2330-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2340-COUNT-EXCEPT.
           IF BPELG-EXCEPT-CNT < 9999999
               ADD 1 TO BPELG-EXCEPT-CNT
           END-IF
           IF BPELG-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO BPELG-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2350-LIMIT-FEE.
           IF BPELG-FEE-YTD < 10.00
               MOVE 10.00 TO BPELG-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPELG-FEE-YTD > 50000.00
               MOVE 50000.00 TO BPELG-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2360-FLAG-FEE.
           IF BPELG-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPELG-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND YTD FEE ON THE REFUND AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2370-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BPELG-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BPELG-REFUND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPELG-HOLD-FLAG = 'Y'
                   COMPUTE BPELG-REFUND-YTD ROUNDED = BPELG-REFUND-AMT *
                       BPELG-MARGIN / 200
               WHEN BPELG-CYCLE-CD = 'DA'
                   MOVE 15.00 TO BPELG-REFUND-YTD
               WHEN BPELG-CYCLE-CD = 'FP'
                   MOVE 15.00 TO BPELG-REFUND-YTD
               WHEN OTHER
                   COMPUTE BPELG-REFUND-YTD ROUNDED = BPELG-REFUND-AMT *
                       BPELG-MARGIN / 100
           END-EVALUATE
           IF BPELG-REFUND-YTD > 0 AND BPELG-REFUND-YTD < 2.00
               MOVE 2.00 TO BPELG-REFUND-YTD
           END-IF
           IF BPELG-REFUND-YTD > 35.00
               MOVE 35.00 TO BPELG-REFUND-YTD
           END-IF
           ADD BPELG-REFUND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2380-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BPELG-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPELG-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BPELG-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2390-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BPELG-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPELG-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO BPELG-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2400-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = BPELG-PAYMENT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BPELG-PAYMENT-AMT - (WS-WORK-AMT * 3)
           COMPUTE BPELG-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * JX -> LR WHEN LAST STMT DATE IS PAST
      * LR -> NZ WHEN LAST STMT DATE IS PAST
      * NZ -> RV WHEN LAST STMT DATE IS PAST
      * RV -> TM WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2410-SET-STATUS.
           EVALUATE BPELG-STATUS-CD
               WHEN 'JX'
                   IF BPELG-LAST-STMT-DATE > 0 AND BPELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LR' TO BPELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LR'
                   IF BPELG-LAST-STMT-DATE > 0 AND BPELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'NZ' TO BPELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF BPELG-LAST-STMT-DATE > 0 AND BPELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RV' TO BPELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF BPELG-LAST-STMT-DATE > 0 AND BPELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TM' TO BPELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2420-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = BPELG-PAYMENT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = BPELG-PAYMENT-YTD - (WS-WORK-AMT * 2)
           COMPUTE BPELG-REFUND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2430-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPELG-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = BPELG-FEE-YTD - (WS-WORK-AMT * 3)
           COMPUTE BPELG-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E244 ADDR REQUIRED
      * E956 WORK AMT 2 NEGATIVE
      * E997 WORK AMT 2 OVER 99999
      * E881 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2440-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPELG-ADDR = SPACES
                   MOVE 'E244' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E956' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E997' TO WS-REJECT-CODE
               WHEN BPELG-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E881' TO WS-REJECT-CODE
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
       2450-DESCRIBE-CYCLE.
           EVALUATE BPELG-CYCLE-CD
               WHEN 'DA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'YR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2460-COUNT-CYCLE.
           IF BPELG-CYCLE-CNT < 9999999
               ADD 1 TO BPELG-CYCLE-CNT
           END-IF
           IF BPELG-CYCLE-CNT NOT < 12
               MOVE 'Y' TO BPELG-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REMIT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2470-DESCRIBE-REMIT-METHOD.
           EVALUATE BPELG-REMIT-METHOD-CD
               WHEN 'CGV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FNS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NMS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TAF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VNK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WUA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XCT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REFUND AMT.
      *----------------------------------------------------------------*
       2480-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPELG-REFUND-AMT ROUNDED =
                   BPELG-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPELG-REFUND-AMT
           END-IF
           ADD BPELG-REFUND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * BT -> FR WHEN LAST STMT DATE IS PAST
      * FR -> LL WHEN LAST STMT DATE IS PAST
      * LL -> US WHEN LAST STMT DATE IS PAST
      * US -> XM WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2490-SET-PAYEE-TYPE.
           EVALUATE BPELG-PAYEE-TYPE-CD
               WHEN 'BT'
                   IF BPELG-LAST-STMT-DATE > 0 AND BPELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FR' TO BPELG-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FR'
                   IF BPELG-LAST-STMT-DATE > 0 AND BPELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LL' TO BPELG-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LL'
                   IF BPELG-LAST-STMT-DATE > 0 AND BPELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'US' TO BPELG-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF BPELG-LAST-STMT-DATE > 0 AND BPELG-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XM' TO BPELG-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE PAYMENT YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2500-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BPELG-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BPELG-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPELG-SUSPEND-FLAG = 'Y'
                   COMPUTE BPELG-PAYMENT-AMT ROUNDED = BPELG-PAYMENT-YTD
                       * BPELG-RATE / 200
               WHEN BPELG-STATUS-CD = 'JX'
                   MOVE 20.00 TO BPELG-PAYMENT-AMT
               WHEN BPELG-STATUS-CD = 'LR'
                   MOVE 15.00 TO BPELG-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE BPELG-PAYMENT-AMT ROUNDED = BPELG-PAYMENT-YTD
                       * BPELG-RATE / 100
           END-EVALUATE
           IF BPELG-PAYMENT-AMT > 0 AND BPELG-PAYMENT-AMT < 2.00
               MOVE 2.00 TO BPELG-PAYMENT-AMT
           END-IF
           IF BPELG-PAYMENT-AMT > 35.00
               MOVE 35.00 TO BPELG-PAYMENT-AMT
           END-IF
           ADD BPELG-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E114 REF NO REQUIRED
      * E418 REFUND YTD NEGATIVE
      * E820 REFUND YTD OVER 1000000
      * E843 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2510-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPELG-REF-NO = SPACES
                   MOVE 'E114' TO WS-REJECT-CODE
               WHEN BPELG-REFUND-YTD < 0
                   MOVE 'E418' TO WS-REJECT-CODE
               WHEN BPELG-REFUND-YTD > 1000000.00
                   MOVE 'E820' TO WS-REJECT-CODE
               WHEN BPELG-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E843' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REFUND YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2520-FLAG-REFUND.
           IF BPELG-REFUND-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPELG-REFUND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2530-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * BPELG-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BPELG-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND YTD BY REMIT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2540-ACCUM-REMIT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPELG-REMIT-METHOD-CD
                   ADD BPELG-REFUND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPELG-REMIT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPELG-REFUND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REMIT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2550-DESCRIBE-REMIT-METHOD.
           EVALUATE BPELG-REMIT-METHOD-CD
               WHEN 'CGV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FNS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NMS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TAF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VNK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WUA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XCT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2560-COUNT-EXCEPT.
           IF BPELG-EXCEPT-CNT < 9999999
               ADD 1 TO BPELG-EXCEPT-CNT
           END-IF
           IF BPELG-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO BPELG-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REFUND YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2570-FLAG-REFUND.
           IF BPELG-REFUND-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPELG-REFUND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2580-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BPELG-CLOSE-DATE > 0 AND BPELG-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPELG-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPELG-REFUND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPELG-REFUND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD BPELG-REFUND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD BPELG-REFUND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPELG-REFUND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E008 REF NO REQUIRED
      * E217 WORK AMT NEGATIVE
      * E226 WORK AMT OVER 250000
      * E244 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2590-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPELG-REF-NO = SPACES
                   MOVE 'E008' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E217' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E226' TO WS-REJECT-CODE
               WHEN BPELG-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E244' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       2600-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BPELG-LAST-STMT-DATE > 0 AND BPELG-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPELG-LAST-STMT-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY REMIT METHOD CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2610-ACCUM-REMIT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPELG-REMIT-METHOD-CD
                   ADD BPELG-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPELG-REMIT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPELG-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2620-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * BT -> FR WHEN NEXT DUE DATE IS PAST
      * FR -> LL WHEN NEXT DUE DATE IS PAST
      * LL -> US WHEN NEXT DUE DATE IS PAST
      * US -> XM WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2630-SET-PAYEE-TYPE.
           EVALUATE BPELG-PAYEE-TYPE-CD
               WHEN 'BT'
                   IF BPELG-NEXT-DUE-DATE > 0 AND BPELG-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FR' TO BPELG-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FR'
                   IF BPELG-NEXT-DUE-DATE > 0 AND BPELG-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LL' TO BPELG-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LL'
                   IF BPELG-NEXT-DUE-DATE > 0 AND BPELG-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'US' TO BPELG-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF BPELG-NEXT-DUE-DATE > 0 AND BPELG-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO BPELG-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       2640-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPELG-FEE-AMT ROUNDED =
                   BPELG-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPELG-FEE-AMT
           END-IF
           ADD BPELG-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2650-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BPELG-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BPELG-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPELG-SUSPEND-FLAG = 'Y'
                   COMPUTE BPELG-PAYMENT-YTD ROUNDED = WS-WORK-AMT-2 *
                       BPELG-RATE / 200
               WHEN BPELG-STATUS-CD = 'JX'
                   MOVE 20.00 TO BPELG-PAYMENT-YTD
               WHEN BPELG-STATUS-CD = 'LR'
                   MOVE 7.00 TO BPELG-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE BPELG-PAYMENT-YTD ROUNDED = WS-WORK-AMT-2 *
                       BPELG-RATE / 100
           END-EVALUATE
           IF BPELG-PAYMENT-YTD > 0 AND BPELG-PAYMENT-YTD < 5.00
               MOVE 5.00 TO BPELG-PAYMENT-YTD
           END-IF
           IF BPELG-PAYMENT-YTD > 250.00
               MOVE 250.00 TO BPELG-PAYMENT-YTD
           END-IF
           ADD BPELG-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2660-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPELG-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BPELG-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2670-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BPELG-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPELG-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BPELG-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE WORK AMT 2.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2680-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BPELG-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BPELG-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPELG-OVERRIDE-FLAG = 'Y'
                   COMPUTE BPELG-PAYMENT-AMT ROUNDED = WS-WORK-AMT-2 *
                       BPELG-MARGIN / 200
               WHEN BPELG-REMIT-METHOD-CD = 'CGV'
                   MOVE 10.00 TO BPELG-PAYMENT-AMT
               WHEN BPELG-REMIT-METHOD-CD = 'FNS'
                   MOVE 3.00 TO BPELG-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE BPELG-PAYMENT-AMT ROUNDED = WS-WORK-AMT-2 *
                       BPELG-MARGIN / 100
           END-EVALUATE
           IF BPELG-PAYMENT-AMT > 0 AND BPELG-PAYMENT-AMT < 2.00
               MOVE 2.00 TO BPELG-PAYMENT-AMT
           END-IF
           IF BPELG-PAYMENT-AMT > 150.00
               MOVE 150.00 TO BPELG-PAYMENT-AMT
           END-IF
           ADD BPELG-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2690-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPELG-STATUS-CD
                   ADD BPELG-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPELG-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPELG-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2700-ACCRUE-FEE.
           IF BPELG-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPELG-FEE-YTD * BPELG-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE PAYMENT YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2710-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BPELG-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPELG-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = BPELG-PAYMENT-YTD *
                       BPELG-RATE / 200
               WHEN BPELG-CYCLE-CD = 'DA'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN BPELG-CYCLE-CD = 'FP'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = BPELG-PAYMENT-YTD *
                       BPELG-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2720-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPELG-CYCLE-CD
                   ADD BPELG-REFUND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPELG-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPELG-REFUND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2730-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BPELG-EXPIRY-DATE > 0 AND BPELG-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPELG-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPELG-REFUND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPELG-REFUND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD BPELG-REFUND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPELG-REFUND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPELG-REFUND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REFUND AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2740-FLAG-REFUND.
           IF BPELG-REFUND-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPELG-REFUND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2750-ACCRUE-FEE.
           IF BPELG-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPELG-FEE-YTD * BPELG-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BPELG-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2760-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO BPELG-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO BPELG-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPELG-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPELG-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DA -> FP WHEN EXPIRY DATE IS PAST
      * FP -> JH WHEN EXPIRY DATE IS PAST
      * JH -> JV WHEN EXPIRY DATE IS PAST
      * JV -> RM WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2770-SET-CYCLE.
           EVALUATE BPELG-CYCLE-CD
               WHEN 'DA'
                   IF BPELG-EXPIRY-DATE > 0 AND BPELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FP' TO BPELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FP'
                   IF BPELG-EXPIRY-DATE > 0 AND BPELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JH' TO BPELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JH'
                   IF BPELG-EXPIRY-DATE > 0 AND BPELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JV' TO BPELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JV'
                   IF BPELG-EXPIRY-DATE > 0 AND BPELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO BPELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E171 REF NO REQUIRED
      * E413 FEE YTD NEGATIVE
      * E994 FEE YTD OVER 10000
      * E006 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2780-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPELG-REF-NO = SPACES
                   MOVE 'E171' TO WS-REJECT-CODE
               WHEN BPELG-FEE-YTD < 0
                   MOVE 'E413' TO WS-REJECT-CODE
               WHEN BPELG-FEE-YTD > 10000.00
                   MOVE 'E994' TO WS-REJECT-CODE
               WHEN BPELG-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E006' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E890 ADDR REQUIRED
      * E354 PAYMENT AMT NEGATIVE
      * E496 PAYMENT AMT OVER 99999
      * E228 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2790-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPELG-ADDR = SPACES
                   MOVE 'E890' TO WS-REJECT-CODE
               WHEN BPELG-PAYMENT-AMT < 0
                   MOVE 'E354' TO WS-REJECT-CODE
               WHEN BPELG-PAYMENT-AMT > 99999.00
                   MOVE 'E496' TO WS-REJECT-CODE
               WHEN BPELG-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E228' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2800-COUNT-EXCEPT.
           IF BPELG-EXCEPT-CNT < 9999999
               ADD 1 TO BPELG-EXCEPT-CNT
           END-IF
           IF BPELG-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO BPELG-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2810-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BPELG-EXPIRY-DATE > 0 AND BPELG-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPELG-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPELG-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD BPELG-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BPELG-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPELG-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPELG-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY PAYEE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2820-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPELG-PAYEE-TYPE-CD
                   ADD BPELG-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPELG-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPELG-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND SCHED TIER FOR THE REFUND YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2830-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-SCHED-RATE(1) TO BPELG-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BPELG-REFUND-YTD NOT < WS-REFUND-SCHED-LOW(WS-TX)
                   MOVE WS-REFUND-SCHED-RATE(WS-TX) TO BPELG-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPELG-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPELG-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2840-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BPELG-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPELG-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BPELG-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2850-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPELG-CYCLE-CD
                   ADD BPELG-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPELG-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPELG-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2860-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BPELG-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPELG-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BPELG-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2870-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BPELG-EXPIRY-DATE > 0 AND BPELG-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPELG-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPELG-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPELG-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BPELG-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPELG-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPELG-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE REFUND AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2880-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BPELG-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BPELG-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPELG-HOLD-FLAG = 'Y'
                   COMPUTE BPELG-PAYMENT-YTD ROUNDED = BPELG-REFUND-AMT
                       * BPELG-MARGIN / 200
               WHEN BPELG-STATUS-CD = 'JX'
                   MOVE 7.00 TO BPELG-PAYMENT-YTD
               WHEN BPELG-STATUS-CD = 'LR'
                   MOVE 15.00 TO BPELG-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE BPELG-PAYMENT-YTD ROUNDED = BPELG-REFUND-AMT
                       * BPELG-MARGIN / 100
           END-EVALUATE
           IF BPELG-PAYMENT-YTD > 0 AND BPELG-PAYMENT-YTD < 1.00
               MOVE 1.00 TO BPELG-PAYMENT-YTD
           END-IF
           IF BPELG-PAYMENT-YTD > 35.00
               MOVE 35.00 TO BPELG-PAYMENT-YTD
           END-IF
           ADD BPELG-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REMIT METHOD CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2890-ACCUM-REMIT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPELG-REMIT-METHOD-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPELG-REMIT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2900-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = BPELG-PAYMENT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = BPELG-PAYMENT-YTD - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E043 REF NO REQUIRED
      * E143 FEE AMT NEGATIVE
      * E219 FEE AMT OVER 50000
      * E879 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2910-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPELG-REF-NO = SPACES
                   MOVE 'E043' TO WS-REJECT-CODE
               WHEN BPELG-FEE-AMT < 0
                   MOVE 'E143' TO WS-REJECT-CODE
               WHEN BPELG-FEE-AMT > 50000.00
                   MOVE 'E219' TO WS-REJECT-CODE
               WHEN BPELG-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E879' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REFUND YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2920-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = BPELG-REFUND-YTD / 12
           COMPUTE WS-WORK-AMT-2 = BPELG-REFUND-YTD - (WS-WORK-AMT * 12)
           COMPUTE BPELG-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2930-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2940-DESCRIBE-CYCLE.
           EVALUATE BPELG-CYCLE-CD
               WHEN 'DA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2950-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2960-ACCRUE-FEE.
           IF BPELG-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPELG-FEE-YTD * BPELG-RATE / 100 / 360
               ADD WS-WORK-AMT TO BPELG-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE FEE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2970-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BPELG-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BPELG-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPELG-OVERRIDE-FLAG = 'Y'
                   COMPUTE BPELG-PAYMENT-AMT ROUNDED = BPELG-FEE-AMT *
                       BPELG-MARGIN / 200
               WHEN BPELG-REMIT-METHOD-CD = 'CGV'
                   MOVE 20.00 TO BPELG-PAYMENT-AMT
               WHEN BPELG-REMIT-METHOD-CD = 'FNS'
                   MOVE 10.00 TO BPELG-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE BPELG-PAYMENT-AMT ROUNDED = BPELG-FEE-AMT *
                       BPELG-MARGIN / 100
           END-EVALUATE
           IF BPELG-PAYMENT-AMT > 0 AND BPELG-PAYMENT-AMT < 2.00
               MOVE 2.00 TO BPELG-PAYMENT-AMT
           END-IF
           IF BPELG-PAYMENT-AMT > 100.00
               MOVE 100.00 TO BPELG-PAYMENT-AMT
           END-IF
           ADD BPELG-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2980-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2990-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   BPELG-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO BPELG-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO BPELG-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO BPELG-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'BPELG EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
