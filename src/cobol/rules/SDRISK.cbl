      *================================================================*
      * PROGRAM  : SDRISK                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : SAFE DEPOSIT BOXES RULE - LIMITS. CALLED BY THE SAFE*
      *            DEPOSIT BOXES BATCH AND ONLINE PROGRAMS.            *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-02-22 DPS  ORIGINAL                                       *
      * 1996-04-04 GAF  PREPAID LIMIT REVISED CHG2144                  *
      * 1996-02-13 EWS  DEPOSIT EXCEPTIONS TO OPS QUEUE INC1223        *
      * 1997-03-05 PQR  RENT CALCULATION CORRECTED INC3662             *
      * 1999-07-02 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-07-14 RJT  NEW PREPAID REPORT COLUMNS CHG4389             *
      * 2002-03-10 DPS  DRILL FEE SCHEDULE UPDATE PRB8250              *
      * 2004-01-16 LMB  DRILL CALCULATION CORRECTED PRB1890            *
      * 2005-12-23 LMB  LATE-FEE EXCEPTIONS TO OPS QUEUE CHG4234       *
      * 2006-01-17 CRP  AUDIT FINDING - PREPAID CONTROL TOTALS INC2542 *
      * 2007-09-01 PQR  PREPAID CALCULATION CORRECTED CHG4884          *
      * 2007-11-01 LMB  LATE-FEE LIMIT REVISED REQ5814                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SDRISK.
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
                                           VALUE 19.000000.
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
      * WS-PREPAID-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PREPAID-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000733500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000591000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000176500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007750000000440000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017750000000899000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018750000000632000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018850000000502500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028850000000683500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029850000000826000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00034850000000626500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00039850000000686000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00039900000000509500'.
       01  WS-PREPAID-SCHED-TBL REDEFINES WS-PREPAID-SCHED-VALUES.
           05  WS-PREPAID-SCHED-ROW OCCURS 12 TIMES.
               10  WS-PREPAID-SCHED-LOW    PIC 9(09)V99.
               10  WS-PREPAID-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DEPOSIT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEPOSIT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000374500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000726000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000475500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005350000000144500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007850000000319500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017850000000364000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020350000000709500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020400000000102500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030400000000486000'.
       01  WS-DEPOSIT-SCHED-TBL REDEFINES WS-DEPOSIT-SCHED-VALUES.
           05  WS-DEPOSIT-SCHED-ROW OCCURS 9 TIMES.
               10  WS-DEPOSIT-SCHED-LOW    PIC 9(09)V99.
               10  WS-DEPOSIT-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PREPAID-SCHEDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PREPAID-SCHEDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000825500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000188500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000638000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000448000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026000000000608000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026500000000661500'.
       01  WS-PREPAID-SCHEDX-TBL REDEFINES WS-PREPAID-SCHEDX-VALUES.
           05  WS-PREPAID-SCHEDX-ROW OCCURS 6 TIMES.
               10  WS-PREPAID-SCHEDX-LOW   PIC 9(09)V99.
               10  WS-PREPAID-SCHEDX-RATE  PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY SDRISKP.

       PROCEDURE DIVISION USING SDRISK-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCUM-CYCLE
           PERFORM 2010-ACCRUE-FEE-PCT
           PERFORM 2020-CHECK-NEXT-REVIEW
           PERFORM 2030-COUNT-PAYMENT
           PERFORM 2040-ACCUM-VAULT
           PERFORM 2050-COUNT-PAYMENT
           PERFORM 2060-COUNT-PAYMENT
           PERFORM 2070-AGE-NEXT-REVIEW
           PERFORM 2080-CHECK-OPEN
           PERFORM 2090-SELECT-PENALTY-PCT
           PERFORM 2100-CHECK-LAST-MAINT
           PERFORM 2110-ACCUM-REASON
           PERFORM 2120-DESCRIBE-VAULT
           PERFORM 2130-CHECK-NEXT-REVIEW
           PERFORM 2140-LIMIT-ITEM
           PERFORM 2150-COUNT-PAYMENT
           PERFORM 2160-ASSESS-AUTO-RENEW
           PERFORM 2170-FLAG-LATE-FEE
           PERFORM 2180-DESCRIBE-VAULT
           PERFORM 2190-EDIT-CUST-ID
           PERFORM 2200-DESCRIBE-BOX-SIZE
           PERFORM 2210-SPLIT-LATE-FEE
           PERFORM 2220-ACCUM-BOX-SIZE
           PERFORM 2230-FLAG-DEPOSIT
           PERFORM 2240-ASSESS-OVERRIDE
           PERFORM 2250-PRORATE-KEY-FEE
           PERFORM 2260-EDIT-DESC
           PERFORM 2270-SELECT-PENALTY-PCT
           PERFORM 2280-AGE-NEXT-DUE
           PERFORM 2290-DESCRIBE-CYCLE
           PERFORM 2300-CHECK-NEXT-DUE
           PERFORM 2310-ASSESS-AUTO-RENEW
           PERFORM 2320-LIMIT-LATE-FEE
           PERFORM 2330-FLAG-ITEM
           PERFORM 2340-ACCRUE-KEY-FEE
           PERFORM 2350-SET-VAULT
           PERFORM 2360-AGE-NEXT-DUE
           PERFORM 2370-SET-VAULT
           PERFORM 2380-COUNT-ITEM
           PERFORM 2390-EDIT-CUST-ID
           PERFORM 2400-ACCUM-VAULT
           PERFORM 2410-SPLIT-ITEM
           PERFORM 2420-SELECT-FEE-PCT
           PERFORM 2430-CHECK-LAST-MAINT
           PERFORM 2440-ACCUM-CYCLE
           PERFORM 2450-SELECT-PENALTY-PCT
           PERFORM 2460-ASSESS-PREFERRED
           PERFORM 2470-DESCRIBE-CYCLE
           PERFORM 2480-ASSESS-OVERRIDE
           PERFORM 2490-LIMIT-KEY-FEE
           PERFORM 2500-ACCRUE-FEE-PCT
           PERFORM 2510-COUNT-PAYMENT
           PERFORM 2520-ACCUM-CYCLE
           PERFORM 2530-SET-CYCLE
           PERFORM 2540-AGE-NEXT-DUE
           PERFORM 2550-ASSESS-PREFERRED
           PERFORM 2560-FLAG-LATE-FEE
           PERFORM 2570-COUNT-PAYMENT
           PERFORM 2580-COUNT-PAYMENT
           PERFORM 2590-ASSESS-AUTO-RENEW
           PERFORM 2600-ACCRUE-LATE-FEE
           PERFORM 2610-ASSESS-OVERRIDE
           PERFORM 2620-AGE-NEXT-DUE
           PERFORM 2630-DESCRIBE-VAULT
           PERFORM 2640-PRORATE-LATE-FEE
           PERFORM 2650-SPLIT-LATE-FEE
           PERFORM 2660-SELECT-PENALTY-PCT
           PERFORM 2670-SET-BOX-SIZE
           PERFORM 2680-PRORATE-LATE-FEE
           PERFORM 2690-SELECT-FEE-PCT
           PERFORM 2700-SET-VAULT
           PERFORM 2710-AGE-NEXT-DUE
           PERFORM 2720-AGE-LAST-MAINT
           PERFORM 2730-EDIT-DESC
           PERFORM 2740-ASSESS-PREFERRED
           PERFORM 2750-FLAG-LATE-FEE
           PERFORM 2760-PRORATE-LATE-FEE
           PERFORM 2770-PRORATE-DEPOSIT
           PERFORM 2780-LIMIT-DEPOSIT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO SDRISK-RETURN-CODE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2000-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDRISK-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDRISK-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2010-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * SDRISK-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDRISK-LATE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2020-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF SDRISK-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDRISK-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO SDRISK-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2030-COUNT-PAYMENT.
           IF SDRISK-PAYMENT-CNT < 9999999
               ADD 1 TO SDRISK-PAYMENT-CNT
           END-IF
           IF SDRISK-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO SDRISK-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY VAULT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2040-ACCUM-VAULT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDRISK-VAULT-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDRISK-VAULT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2050-COUNT-PAYMENT.
           IF SDRISK-PAYMENT-CNT < 9999999
               ADD 1 TO SDRISK-PAYMENT-CNT
           END-IF
           IF SDRISK-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO SDRISK-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2060-COUNT-PAYMENT.
           IF SDRISK-PAYMENT-CNT < 9999999
               ADD 1 TO SDRISK-PAYMENT-CNT
           END-IF
           IF SDRISK-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO SDRISK-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2070-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF SDRISK-NEXT-REVIEW-DATE > 0 AND SDRISK-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDRISK-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDRISK-KEY-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDRISK-KEY-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SDRISK-KEY-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDRISK-KEY-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDRISK-KEY-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2080-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SDRISK-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDRISK-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SDRISK-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PREPAID SCHED TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2090-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PREPAID-SCHED-RATE(1) TO SDRISK-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SDRISK-DEPOSIT-AMT NOT < WS-PREPAID-SCHED-LOW(WS-TX)
                   MOVE WS-PREPAID-SCHED-RATE(WS-TX) TO
                       SDRISK-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDRISK-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDRISK-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2100-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SDRISK-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDRISK-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SDRISK-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2110-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDRISK-REASON-CD
                   ADD SDRISK-LATE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDRISK-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDRISK-LATE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE VAULT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2120-DESCRIBE-VAULT.
           EVALUATE SDRISK-VAULT-CD
               WHEN 'BU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ET'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2130-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF SDRISK-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDRISK-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO SDRISK-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2140-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2150-COUNT-PAYMENT.
           IF SDRISK-PAYMENT-CNT < 9999999
               ADD 1 TO SDRISK-PAYMENT-CNT
           END-IF
           IF SDRISK-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO SDRISK-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2160-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDRISK-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDRISK-DEPOSIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDRISK-PREFERRED-FLAG = 'Y'
                   COMPUTE SDRISK-DEPOSIT-AMT ROUNDED = WS-WORK-AMT-2 *
                       SDRISK-FEE-PCT / 200
               WHEN SDRISK-REASON-CD = 'J'
                   MOVE 15.00 TO SDRISK-DEPOSIT-AMT
               WHEN SDRISK-REASON-CD = 'K'
                   MOVE 15.00 TO SDRISK-DEPOSIT-AMT
               WHEN OTHER
                   COMPUTE SDRISK-DEPOSIT-AMT ROUNDED = WS-WORK-AMT-2 *
                       SDRISK-FEE-PCT / 100
           END-EVALUATE
           IF SDRISK-DEPOSIT-AMT > 0 AND SDRISK-DEPOSIT-AMT < 2.00
               MOVE 2.00 TO SDRISK-DEPOSIT-AMT
           END-IF
           IF SDRISK-DEPOSIT-AMT > 25.00
               MOVE 25.00 TO SDRISK-DEPOSIT-AMT
           END-IF
           ADD SDRISK-DEPOSIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * LATE FEE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2170-FLAG-LATE-FEE.
           IF SDRISK-LATE-FEE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDRISK-LATE-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE VAULT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2180-DESCRIBE-VAULT.
           EVALUATE SDRISK-VAULT-CD
               WHEN 'BU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ET'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E074 CUST ID REQUIRED
      * E398 LATE FEE YTD NEGATIVE
      * E597 LATE FEE YTD OVER 1000000
      * E032 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2190-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDRISK-CUST-ID = SPACES
                   MOVE 'E074' TO WS-REJECT-CODE
               WHEN SDRISK-LATE-FEE-YTD < 0
                   MOVE 'E398' TO WS-REJECT-CODE
               WHEN SDRISK-LATE-FEE-YTD > 1000000.00
                   MOVE 'E597' TO WS-REJECT-CODE
               WHEN SDRISK-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E032' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2200-DESCRIBE-BOX-SIZE.
           EVALUATE SDRISK-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2210-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = SDRISK-LATE-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SDRISK-LATE-FEE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE SDRISK-DEPOSIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY BOX SIZE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2220-ACCUM-BOX-SIZE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDRISK-BOX-SIZE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDRISK-BOX-SIZE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2230-FLAG-DEPOSIT.
           IF SDRISK-DEPOSIT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDRISK-DEPOSIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT FEE ON THE KEY FEE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2240-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SDRISK-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SDRISK-DEPOSIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDRISK-PREFERRED-FLAG = 'Y'
                   COMPUTE SDRISK-DEPOSIT-AMT ROUNDED =
                       SDRISK-KEY-FEE-AMT * SDRISK-PENALTY-PCT / 200
               WHEN SDRISK-VAULT-CD = 'BU'
                   MOVE 15.00 TO SDRISK-DEPOSIT-AMT
               WHEN SDRISK-VAULT-CD = 'ET'
                   MOVE 5.00 TO SDRISK-DEPOSIT-AMT
               WHEN OTHER
                   COMPUTE SDRISK-DEPOSIT-AMT ROUNDED =
                       SDRISK-KEY-FEE-AMT * SDRISK-PENALTY-PCT / 100
           END-EVALUATE
           IF SDRISK-DEPOSIT-AMT > 0 AND SDRISK-DEPOSIT-AMT < 5.00
               MOVE 5.00 TO SDRISK-DEPOSIT-AMT
           END-IF
           IF SDRISK-DEPOSIT-AMT > 250.00
               MOVE 250.00 TO SDRISK-DEPOSIT-AMT
           END-IF
           ADD SDRISK-DEPOSIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE KEY FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2250-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDRISK-KEY-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E549 DESC REQUIRED
      * E395 DEPOSIT AMT NEGATIVE
      * E317 DEPOSIT AMT OVER 99999
      * E401 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2260-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDRISK-DESC = SPACES
                   MOVE 'E549' TO WS-REJECT-CODE
               WHEN SDRISK-DEPOSIT-AMT < 0
                   MOVE 'E395' TO WS-REJECT-CODE
               WHEN SDRISK-DEPOSIT-AMT > 99999.00
                   MOVE 'E317' TO WS-REJECT-CODE
               WHEN SDRISK-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E401' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT SCHED TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2270-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-SCHED-RATE(1) TO SDRISK-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SDRISK-DEPOSIT-AMT NOT < WS-DEPOSIT-SCHED-LOW(WS-TX)
                   MOVE WS-DEPOSIT-SCHED-RATE(WS-TX) TO
                       SDRISK-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDRISK-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDRISK-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2280-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDRISK-NEXT-DUE-DATE > 0 AND SDRISK-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDRISK-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2290-DESCRIBE-CYCLE.
           EVALUATE SDRISK-CYCLE-CD
               WHEN 'AP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2300-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDRISK-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDRISK-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SDRISK-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE KEY FEE YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2310-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDRISK-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDRISK-KEY-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDRISK-PREFERRED-FLAG = 'Y'
                   COMPUTE SDRISK-KEY-FEE-YTD ROUNDED = WS-WORK-AMT-2 *
                       SDRISK-FEE-PCT / 200
               WHEN SDRISK-CYCLE-CD = 'AP'
                   MOVE 15.00 TO SDRISK-KEY-FEE-YTD
               WHEN SDRISK-CYCLE-CD = 'DA'
                   MOVE 20.00 TO SDRISK-KEY-FEE-YTD
               WHEN OTHER
                   COMPUTE SDRISK-KEY-FEE-YTD ROUNDED = WS-WORK-AMT-2 *
                       SDRISK-FEE-PCT / 100
           END-EVALUATE
           IF SDRISK-KEY-FEE-YTD > 0 AND SDRISK-KEY-FEE-YTD < 2.00
               MOVE 2.00 TO SDRISK-KEY-FEE-YTD
           END-IF
           IF SDRISK-KEY-FEE-YTD > 250.00
               MOVE 250.00 TO SDRISK-KEY-FEE-YTD
           END-IF
           ADD SDRISK-KEY-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * LATE FEE YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2320-LIMIT-LATE-FEE.
           IF SDRISK-LATE-FEE-YTD < 50.00
               MOVE 50.00 TO SDRISK-LATE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDRISK-LATE-FEE-YTD > 2500.00
               MOVE 2500.00 TO SDRISK-LATE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2330-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON KEY FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2340-ACCRUE-KEY-FEE.
           IF SDRISK-KEY-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDRISK-KEY-FEE-AMT * SDRISK-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDRISK-LATE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR VAULT CD:
      * BU -> ET WHEN LAST MAINT DATE IS PAST
      * ET -> FF WHEN LAST MAINT DATE IS PAST
      * FF -> NG WHEN LAST MAINT DATE IS PAST
      * NG -> NH WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2350-SET-VAULT.
           EVALUATE SDRISK-VAULT-CD
               WHEN 'BU'
                   IF SDRISK-LAST-MAINT-DATE > 0 AND
                       SDRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'ET' TO SDRISK-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ET'
                   IF SDRISK-LAST-MAINT-DATE > 0 AND
                       SDRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'FF' TO SDRISK-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FF'
                   IF SDRISK-LAST-MAINT-DATE > 0 AND
                       SDRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'NG' TO SDRISK-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NG'
                   IF SDRISK-LAST-MAINT-DATE > 0 AND
                       SDRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'NH' TO SDRISK-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2360-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDRISK-NEXT-DUE-DATE > 0 AND SDRISK-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDRISK-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR VAULT CD:
      * BU -> ET WHEN NEXT REVIEW DATE IS PAST
      * ET -> FF WHEN NEXT REVIEW DATE IS PAST
      * FF -> NG WHEN NEXT REVIEW DATE IS PAST
      * NG -> NH WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2370-SET-VAULT.
           EVALUATE SDRISK-VAULT-CD
               WHEN 'BU'
                   IF SDRISK-NEXT-REVIEW-DATE > 0 AND
                       SDRISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ET' TO SDRISK-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ET'
                   IF SDRISK-NEXT-REVIEW-DATE > 0 AND
                       SDRISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'FF' TO SDRISK-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FF'
                   IF SDRISK-NEXT-REVIEW-DATE > 0 AND
                       SDRISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'NG' TO SDRISK-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NG'
                   IF SDRISK-NEXT-REVIEW-DATE > 0 AND
                       SDRISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'NH' TO SDRISK-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2380-COUNT-ITEM.
           IF SDRISK-ITEM-CNT < 9999999
               ADD 1 TO SDRISK-ITEM-CNT
           END-IF
           IF SDRISK-ITEM-CNT NOT < 5
               MOVE 'Y' TO SDRISK-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E974 CUST ID REQUIRED
      * E525 KEY FEE AMT NEGATIVE
      * E773 KEY FEE AMT OVER 5000
      * E024 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2390-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDRISK-CUST-ID = SPACES
                   MOVE 'E974' TO WS-REJECT-CODE
               WHEN SDRISK-KEY-FEE-AMT < 0
                   MOVE 'E525' TO WS-REJECT-CODE
               WHEN SDRISK-KEY-FEE-AMT > 5000.00
                   MOVE 'E773' TO WS-REJECT-CODE
               WHEN SDRISK-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E024' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE KEY FEE AMT BY VAULT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2400-ACCUM-VAULT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDRISK-VAULT-CD
                   ADD SDRISK-KEY-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDRISK-VAULT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDRISK-KEY-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2410-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PREPAID SCHED TIER FOR THE LATE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2420-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PREPAID-SCHED-RATE(1) TO SDRISK-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SDRISK-LATE-FEE-YTD NOT < WS-PREPAID-SCHED-LOW(WS-TX)
                   MOVE WS-PREPAID-SCHED-RATE(WS-TX) TO SDRISK-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDRISK-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDRISK-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2430-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SDRISK-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDRISK-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO SDRISK-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2440-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDRISK-CYCLE-CD
                   ADD SDRISK-DEPOSIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDRISK-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDRISK-DEPOSIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PREPAID SCHED TIER FOR THE KEY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2450-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PREPAID-SCHED-RATE(1) TO SDRISK-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SDRISK-KEY-FEE-YTD NOT < WS-PREPAID-SCHED-LOW(WS-TX)
                   MOVE WS-PREPAID-SCHED-RATE(WS-TX) TO
                       SDRISK-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDRISK-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDRISK-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LATE FEE YTD FEE ON THE KEY FEE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2460-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SDRISK-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SDRISK-LATE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDRISK-OVERRIDE-FLAG = 'Y'
                   COMPUTE SDRISK-LATE-FEE-YTD ROUNDED =
                       SDRISK-KEY-FEE-AMT * SDRISK-PENALTY-PCT / 200
               WHEN SDRISK-BOX-SIZE-CD = 'CU'
                   MOVE 10.00 TO SDRISK-LATE-FEE-YTD
               WHEN SDRISK-BOX-SIZE-CD = 'MG'
                   MOVE 12.00 TO SDRISK-LATE-FEE-YTD
               WHEN OTHER
                   COMPUTE SDRISK-LATE-FEE-YTD ROUNDED =
                       SDRISK-KEY-FEE-AMT * SDRISK-PENALTY-PCT / 100
           END-EVALUATE
           IF SDRISK-LATE-FEE-YTD > 0 AND SDRISK-LATE-FEE-YTD < 2.00
               MOVE 2.00 TO SDRISK-LATE-FEE-YTD
           END-IF
           IF SDRISK-LATE-FEE-YTD > 75.00
               MOVE 75.00 TO SDRISK-LATE-FEE-YTD
           END-IF
           ADD SDRISK-LATE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2470-DESCRIBE-CYCLE.
           EVALUATE SDRISK-CYCLE-CD
               WHEN 'AP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'DA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE LATE FEE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2480-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SDRISK-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDRISK-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SDRISK-LATE-FEE-YTD *
                       SDRISK-PENALTY-PCT / 200
               WHEN SDRISK-REASON-CD = 'J'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN SDRISK-REASON-CD = 'K'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SDRISK-LATE-FEE-YTD *
                       SDRISK-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * KEY FEE YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2490-LIMIT-KEY-FEE.
           IF SDRISK-KEY-FEE-YTD < 100.00
               MOVE 100.00 TO SDRISK-KEY-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDRISK-KEY-FEE-YTD > 50000.00
               MOVE 50000.00 TO SDRISK-KEY-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2500-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * SDRISK-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDRISK-LATE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2510-COUNT-PAYMENT.
           IF SDRISK-PAYMENT-CNT < 9999999
               ADD 1 TO SDRISK-PAYMENT-CNT
           END-IF
           IF SDRISK-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO SDRISK-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2520-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDRISK-CYCLE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDRISK-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * AP -> DA WHEN NEXT REVIEW DATE IS PAST
      * DA -> HV WHEN NEXT REVIEW DATE IS PAST
      * HV -> SE WHEN NEXT REVIEW DATE IS PAST
      * SE -> AP WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2530-SET-CYCLE.
           EVALUATE SDRISK-CYCLE-CD
               WHEN 'AP'
                   IF SDRISK-NEXT-REVIEW-DATE > 0 AND
                       SDRISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DA' TO SDRISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DA'
                   IF SDRISK-NEXT-REVIEW-DATE > 0 AND
                       SDRISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HV' TO SDRISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HV'
                   IF SDRISK-NEXT-REVIEW-DATE > 0 AND
                       SDRISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'SE' TO SDRISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SE'
                   IF SDRISK-NEXT-REVIEW-DATE > 0 AND
                       SDRISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'AP' TO SDRISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2540-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDRISK-NEXT-DUE-DATE > 0 AND SDRISK-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDRISK-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2550-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SDRISK-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDRISK-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       SDRISK-PENALTY-PCT / 200
               WHEN SDRISK-BOX-SIZE-CD = 'CU'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN SDRISK-BOX-SIZE-CD = 'MG'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       SDRISK-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * LATE FEE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2560-FLAG-LATE-FEE.
           IF SDRISK-LATE-FEE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDRISK-LATE-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2570-COUNT-PAYMENT.
           IF SDRISK-PAYMENT-CNT < 9999999
               ADD 1 TO SDRISK-PAYMENT-CNT
           END-IF
           IF SDRISK-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO SDRISK-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2580-COUNT-PAYMENT.
           IF SDRISK-PAYMENT-CNT < 9999999
               ADD 1 TO SDRISK-PAYMENT-CNT
           END-IF
           IF SDRISK-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO SDRISK-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LATE FEE FEE ON THE LATE FEE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2590-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDRISK-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDRISK-LATE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDRISK-OVERRIDE-FLAG = 'Y'
                   COMPUTE SDRISK-LATE-FEE-AMT ROUNDED =
                       SDRISK-LATE-FEE-YTD * SDRISK-PENALTY-PCT / 200
               WHEN SDRISK-BOX-SIZE-CD = 'CU'
                   MOVE 12.00 TO SDRISK-LATE-FEE-AMT
               WHEN SDRISK-BOX-SIZE-CD = 'MG'
                   MOVE 15.00 TO SDRISK-LATE-FEE-AMT
               WHEN OTHER
                   COMPUTE SDRISK-LATE-FEE-AMT ROUNDED =
                       SDRISK-LATE-FEE-YTD * SDRISK-PENALTY-PCT / 100
           END-EVALUATE
           IF SDRISK-LATE-FEE-AMT > 0 AND SDRISK-LATE-FEE-AMT < 5.00
               MOVE 5.00 TO SDRISK-LATE-FEE-AMT
           END-IF
           IF SDRISK-LATE-FEE-AMT > 75.00
               MOVE 75.00 TO SDRISK-LATE-FEE-AMT
           END-IF
           ADD SDRISK-LATE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2600-ACCRUE-LATE-FEE.
           IF SDRISK-LATE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDRISK-LATE-FEE-AMT * SDRISK-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDRISK-LATE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE KEY FEE FEE ON THE DEPOSIT YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2610-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SDRISK-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SDRISK-KEY-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDRISK-PREFERRED-FLAG = 'Y'
                   COMPUTE SDRISK-KEY-FEE-AMT ROUNDED =
                       SDRISK-DEPOSIT-YTD * SDRISK-FEE-PCT / 200
               WHEN SDRISK-REASON-CD = 'J'
                   MOVE 10.00 TO SDRISK-KEY-FEE-AMT
               WHEN SDRISK-REASON-CD = 'K'
                   MOVE 5.00 TO SDRISK-KEY-FEE-AMT
               WHEN OTHER
                   COMPUTE SDRISK-KEY-FEE-AMT ROUNDED =
                       SDRISK-DEPOSIT-YTD * SDRISK-FEE-PCT / 100
           END-EVALUATE
           IF SDRISK-KEY-FEE-AMT > 0 AND SDRISK-KEY-FEE-AMT < 10.00
               MOVE 10.00 TO SDRISK-KEY-FEE-AMT
           END-IF
           IF SDRISK-KEY-FEE-AMT > 250.00
               MOVE 250.00 TO SDRISK-KEY-FEE-AMT
           END-IF
           ADD SDRISK-KEY-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2620-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDRISK-NEXT-DUE-DATE > 0 AND SDRISK-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDRISK-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDRISK-LATE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDRISK-LATE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDRISK-LATE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SDRISK-LATE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDRISK-LATE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE VAULT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2630-DESCRIBE-VAULT.
           EVALUATE SDRISK-VAULT-CD
               WHEN 'BU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ET'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'NH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE KEY FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LATE FEE AMT.
      *----------------------------------------------------------------*
       2640-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDRISK-LATE-FEE-AMT ROUNDED =
                   SDRISK-KEY-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDRISK-LATE-FEE-AMT
           END-IF
           ADD SDRISK-LATE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2650-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = SDRISK-LATE-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SDRISK-LATE-FEE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PREPAID SCHED TIER FOR THE LATE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2660-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PREPAID-SCHED-RATE(1) TO SDRISK-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SDRISK-LATE-FEE-YTD NOT < WS-PREPAID-SCHED-LOW(WS-TX)
                   MOVE WS-PREPAID-SCHED-RATE(WS-TX) TO
                       SDRISK-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDRISK-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDRISK-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOX SIZE CD:
      * CU -> MG WHEN LAST MAINT DATE IS PAST
      * MG -> VG WHEN LAST MAINT DATE IS PAST
      * VG -> VN WHEN LAST MAINT DATE IS PAST
      * VN -> XJ WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2670-SET-BOX-SIZE.
           EVALUATE SDRISK-BOX-SIZE-CD
               WHEN 'CU'
                   IF SDRISK-LAST-MAINT-DATE > 0 AND
                       SDRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'MG' TO SDRISK-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MG'
                   IF SDRISK-LAST-MAINT-DATE > 0 AND
                       SDRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'VG' TO SDRISK-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VG'
                   IF SDRISK-LAST-MAINT-DATE > 0 AND
                       SDRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'VN' TO SDRISK-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VN'
                   IF SDRISK-LAST-MAINT-DATE > 0 AND
                       SDRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'XJ' TO SDRISK-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LATE FEE YTD.
      *----------------------------------------------------------------*
       2680-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDRISK-LATE-FEE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDRISK-LATE-FEE-YTD
           END-IF
           ADD SDRISK-LATE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PREPAID SCHED TIER FOR THE LATE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2690-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PREPAID-SCHED-RATE(1) TO SDRISK-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SDRISK-LATE-FEE-AMT NOT < WS-PREPAID-SCHED-LOW(WS-TX)
                   MOVE WS-PREPAID-SCHED-RATE(WS-TX) TO SDRISK-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDRISK-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDRISK-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR VAULT CD:
      * BU -> ET WHEN NEXT REVIEW DATE IS PAST
      * ET -> FF WHEN NEXT REVIEW DATE IS PAST
      * FF -> NG WHEN NEXT REVIEW DATE IS PAST
      * NG -> NH WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2700-SET-VAULT.
           EVALUATE SDRISK-VAULT-CD
               WHEN 'BU'
                   IF SDRISK-NEXT-REVIEW-DATE > 0 AND
                       SDRISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ET' TO SDRISK-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ET'
                   IF SDRISK-NEXT-REVIEW-DATE > 0 AND
                       SDRISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'FF' TO SDRISK-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FF'
                   IF SDRISK-NEXT-REVIEW-DATE > 0 AND
                       SDRISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'NG' TO SDRISK-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NG'
                   IF SDRISK-NEXT-REVIEW-DATE > 0 AND
                       SDRISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'NH' TO SDRISK-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2710-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDRISK-NEXT-DUE-DATE > 0 AND SDRISK-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDRISK-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDRISK-LATE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDRISK-LATE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDRISK-LATE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDRISK-LATE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDRISK-LATE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2720-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SDRISK-LAST-MAINT-DATE > 0 AND SDRISK-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDRISK-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDRISK-DEPOSIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E377 DESC REQUIRED
      * E732 LATE FEE AMT NEGATIVE
      * E361 LATE FEE AMT OVER 10000
      * E819 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2730-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDRISK-DESC = SPACES
                   MOVE 'E377' TO WS-REJECT-CODE
               WHEN SDRISK-LATE-FEE-AMT < 0
                   MOVE 'E732' TO WS-REJECT-CODE
               WHEN SDRISK-LATE-FEE-AMT > 10000.00
                   MOVE 'E361' TO WS-REJECT-CODE
               WHEN SDRISK-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E819' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LATE FEE FEE ON THE KEY FEE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2740-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SDRISK-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SDRISK-LATE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDRISK-OVERRIDE-FLAG = 'Y'
                   COMPUTE SDRISK-LATE-FEE-AMT ROUNDED =
                       SDRISK-KEY-FEE-YTD * SDRISK-FEE-PCT / 200
               WHEN SDRISK-VAULT-CD = 'BU'
                   MOVE 10.00 TO SDRISK-LATE-FEE-AMT
               WHEN SDRISK-VAULT-CD = 'ET'
                   MOVE 12.00 TO SDRISK-LATE-FEE-AMT
               WHEN OTHER
                   COMPUTE SDRISK-LATE-FEE-AMT ROUNDED =
                       SDRISK-KEY-FEE-YTD * SDRISK-FEE-PCT / 100
           END-EVALUATE
           IF SDRISK-LATE-FEE-AMT > 0 AND SDRISK-LATE-FEE-AMT < 1.00
               MOVE 1.00 TO SDRISK-LATE-FEE-AMT
           END-IF
           IF SDRISK-LATE-FEE-AMT > 150.00
               MOVE 150.00 TO SDRISK-LATE-FEE-AMT
           END-IF
           ADD SDRISK-LATE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * LATE FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2750-FLAG-LATE-FEE.
           IF SDRISK-LATE-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDRISK-LATE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE KEY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LATE FEE YTD.
      *----------------------------------------------------------------*
       2760-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDRISK-LATE-FEE-YTD ROUNDED =
                   SDRISK-KEY-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDRISK-LATE-FEE-YTD
           END-IF
           ADD SDRISK-LATE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2770-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDRISK-DEPOSIT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DEPOSIT AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2780-LIMIT-DEPOSIT.
           IF SDRISK-DEPOSIT-AMT < 25.00
               MOVE 25.00 TO SDRISK-DEPOSIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDRISK-DEPOSIT-AMT > 5000.00
               MOVE 5000.00 TO SDRISK-DEPOSIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO SDRISK-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO SDRISK-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO SDRISK-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'SDRISK EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
