      *================================================================*
      * PROGRAM  : TCPRC                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : TRAVELERS CHEQUES RULE - VALIDATION. CALLED BY THE  *
      *            TRAVELERS CHEQUES BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-04-22 PQR  ORIGINAL                                       *
      * 1992-07-03 RJT  AUDIT FINDING - COMMISSION CONTROL TOTALS REQ49*
      * 1993-10-24 GAF  REMIT TABLE EXPANDED INC9904                   *
      * 1996-11-22 GAF  REFUND CUTOFF MOVED REQ5517                    *
      * 1996-09-09 JLM  REFUND CALCULATION CORRECTED REQ6567           *
      * 2004-04-22 BHS  REMIT NOTICE TEXT PER LEGAL INC6630            *
      * 2005-08-16 PQR  REMIT NOTICE TEXT PER LEGAL PRB8991            *
      * 2006-04-07 TWH  CONSIGN ROUNDING - HALF UP CHG0579             *
      * 2007-11-21 JLM  AUDIT FINDING - FACE CONTROL TOTALS CHG9982    *
      * 2008-11-17 BHS  COMMISSION EXCEPTIONS TO OPS QUEUE REQ3635     *
      * 2009-03-25 JLM  PERFORMANCE - REFUND READ AHEAD PRB4011        *
      * 2012-01-12 SKN  REMIT EXCEPTIONS TO OPS QUEUE CHG0380          *
      * 2014-12-04 DPS  REFUND RERUN PROTECTION INC0393                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCPRC.
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
                                           VALUE 9.000000.
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
      * WS-REMIT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REMIT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000759000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000799500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000446500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017500000000625500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018500000000779500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028500000000466500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028750000000465500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029000000000709000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00039000000000469000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00039100000000345000'.
       01  WS-REMIT-TIER-TBL REDEFINES WS-REMIT-TIER-VALUES.
           05  WS-REMIT-TIER-ROW OCCURS 10 TIMES.
               10  WS-REMIT-TIER-LOW       PIC 9(09)V99.
               10  WS-REMIT-TIER-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FACE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FACE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000052500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000485000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000358500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000133500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000211000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000800500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001350000000581000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006350000000121500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011350000000323000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021350000000424500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021600000000519500'.
       01  WS-FACE-TIER-TBL REDEFINES WS-FACE-TIER-VALUES.
           05  WS-FACE-TIER-ROW OCCURS 11 TIMES.
               10  WS-FACE-TIER-LOW        PIC 9(09)V99.
               10  WS-FACE-TIER-RATE       PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY TCPRCP.

       PROCEDURE DIVISION USING TCPRC-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SELECT-FEE-PCT
           PERFORM 2010-ACCRUE-CONSIGN
           PERFORM 2020-ACCUM-CYCLE
           PERFORM 2030-LIMIT-SHORTAGE
           PERFORM 2040-DESCRIBE-CYCLE
           PERFORM 2050-SET-ISSUER
           PERFORM 2060-AGE-CLOSE
           PERFORM 2070-ACCUM-CYCLE
           PERFORM 2080-ACCUM-STATUS
           PERFORM 2090-LIMIT-SHORTAGE
           PERFORM 2100-ASSESS-SUSPEND
           PERFORM 2110-CHECK-CLOSE
           PERFORM 2120-FLAG-SHORTAGE
           PERFORM 2130-ASSESS-REVIEW
           PERFORM 2140-FLAG-CONSIGN
           PERFORM 2150-CHECK-NEXT-REVIEW
           PERFORM 2160-ACCUM-CYCLE
           PERFORM 2170-LIMIT-REFUND
           PERFORM 2180-SPLIT-CONSIGN
           PERFORM 2190-CHECK-EXPIRY
           PERFORM 2200-SELECT-MARGIN
           PERFORM 2210-DESCRIBE-ISSUER
           PERFORM 2220-SELECT-MARGIN
           PERFORM 2230-AGE-CLOSE
           PERFORM 2240-SPLIT-CONSIGN
           PERFORM 2250-DESCRIBE-CYCLE
           PERFORM 2260-ASSESS-SUSPEND
           PERFORM 2270-SELECT-MARGIN
           PERFORM 2280-PRORATE-CONSIGN
           PERFORM 2290-CHECK-CLOSE
           PERFORM 2300-SET-ISSUER
           PERFORM 2310-AGE-CLOSE
           PERFORM 2320-AGE-CLOSE
           PERFORM 2330-EDIT-CONTACT
           PERFORM 2340-COUNT-EXCEPT
           PERFORM 2350-CHECK-EXPIRY
           PERFORM 2360-SELECT-MARGIN
           PERFORM 2370-DESCRIBE-BRANCH
           PERFORM 2380-DESCRIBE-STATUS
           PERFORM 2390-ASSESS-SUSPEND
           PERFORM 2400-AGE-EXPIRY
           PERFORM 2410-COUNT-CYCLE
           PERFORM 2420-SPLIT-REMIT
           PERFORM 2430-AGE-MATURITY
           PERFORM 2440-FLAG-SHORTAGE
           PERFORM 2450-ACCUM-CYCLE
           PERFORM 2460-ACCRUE-REMIT
           PERFORM 2470-EDIT-NAME
           PERFORM 2480-DESCRIBE-ISSUER
           PERFORM 2490-ACCUM-STATUS
           PERFORM 2500-ACCRUE-REMIT
           PERFORM 2510-ACCUM-BRANCH
           PERFORM 2520-AGE-MATURITY
           PERFORM 2530-SELECT-MARGIN
           PERFORM 2540-AGE-CLOSE
           PERFORM 2550-LIMIT-ITEM
           PERFORM 2560-ACCRUE-CONSIGN
           PERFORM 2570-LIMIT-CONSIGN
           PERFORM 2580-FLAG-REMIT
           PERFORM 2590-COUNT-CYCLE
           PERFORM 2600-CHECK-CLOSE
           PERFORM 2610-SELECT-FEE-PCT
           PERFORM 2620-ACCUM-CYCLE
           PERFORM 2630-AGE-MATURITY
           PERFORM 2640-COUNT-EXCEPT
           PERFORM 2650-SPLIT-REFUND
           PERFORM 2660-SELECT-MARGIN
           PERFORM 2670-SELECT-MARGIN
           PERFORM 2680-CHECK-MATURITY
           PERFORM 2690-EDIT-NAME
           PERFORM 2700-FLAG-SHORTAGE
           PERFORM 2710-SET-CYCLE
           PERFORM 2720-PRORATE-CONSIGN
           PERFORM 2730-SPLIT-CONSIGN
           PERFORM 2740-ASSESS-NOTICE-SENT
           PERFORM 2750-EDIT-CONTACT
           PERFORM 2760-ACCRUE-MARGIN
           PERFORM 2770-ACCRUE-FACE
           PERFORM 2780-COUNT-EXCEPT
           PERFORM 2790-SELECT-FEE-PCT
           PERFORM 2800-AGE-MATURITY
           PERFORM 2810-DESCRIBE-STATUS
           PERFORM 2820-SET-STATUS
           PERFORM 2830-AGE-MATURITY
           PERFORM 2840-SET-CYCLE
           PERFORM 2850-CHECK-CLOSE
           PERFORM 2860-CHECK-EXPIRY
           PERFORM 2870-ACCRUE-MARGIN
           PERFORM 2880-SELECT-FEE-PCT
           PERFORM 2890-COUNT-CYCLE
           PERFORM 2900-SPLIT-REMIT
           PERFORM 2910-DESCRIBE-STATUS
           PERFORM 2920-DESCRIBE-CYCLE
           PERFORM 2930-SET-STATUS
           PERFORM 2940-SELECT-MARGIN
           PERFORM 2950-COUNT-EXCEPT
           PERFORM 2960-PRORATE-REMIT
           PERFORM 2970-CHECK-EXPIRY
           PERFORM 2980-PRORATE-CONSIGN
           PERFORM 2990-PRORATE-SHORTAGE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO TCPRC-RETURN-CODE.

      *----------------------------------------------------------------*
      * SELECT THE FACE TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2000-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-TIER-RATE(1) TO TCPRC-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-FACE-TIER-LOW(WS-TX)
                   MOVE WS-FACE-TIER-RATE(WS-TX) TO TCPRC-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCPRC-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCPRC-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONSIGN AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2010-ACCRUE-CONSIGN.
           IF TCPRC-CONSIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TCPRC-CONSIGN-AMT * TCPRC-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2020-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TCPRC-CYCLE-CD
                   ADD TCPRC-FACE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TCPRC-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TCPRC-FACE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2030-LIMIT-SHORTAGE.
           IF TCPRC-SHORTAGE-YTD < 100.00
               MOVE 100.00 TO TCPRC-SHORTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TCPRC-SHORTAGE-YTD > 2500.00
               MOVE 2500.00 TO TCPRC-SHORTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2040-DESCRIBE-CYCLE.
           EVALUATE TCPRC-CYCLE-CD
               WHEN 'EPW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HHK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JCB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UCB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XTY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XUV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XXU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ISSUER CD:
      * AF -> CJ WHEN NEXT REVIEW DATE IS PAST
      * CJ -> DU WHEN NEXT REVIEW DATE IS PAST
      * DU -> JY WHEN NEXT REVIEW DATE IS PAST
      * JY -> RR WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2050-SET-ISSUER.
           EVALUATE TCPRC-ISSUER-CD
               WHEN 'AF'
                   IF TCPRC-NEXT-REVIEW-DATE > 0 AND
                       TCPRC-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'CJ' TO TCPRC-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJ'
                   IF TCPRC-NEXT-REVIEW-DATE > 0 AND
                       TCPRC-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DU' TO TCPRC-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DU'
                   IF TCPRC-NEXT-REVIEW-DATE > 0 AND
                       TCPRC-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JY' TO TCPRC-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JY'
                   IF TCPRC-NEXT-REVIEW-DATE > 0 AND
                       TCPRC-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'RR' TO TCPRC-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2060-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-CLOSE-DATE > 0 AND TCPRC-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-CLOSE-DATE TO DTU-DATE-1
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
      * ACCUMULATE WORK AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2070-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TCPRC-CYCLE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TCPRC-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2080-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TCPRC-STATUS-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TCPRC-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2090-LIMIT-SHORTAGE.
           IF TCPRC-SHORTAGE-YTD < 50.00
               MOVE 50.00 TO TCPRC-SHORTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TCPRC-SHORTAGE-YTD > 500.00
               MOVE 500.00 TO TCPRC-SHORTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FACE YTD FEE ON THE CONSIGN YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2100-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TCPRC-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TCPRC-FACE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TCPRC-REVIEW-FLAG = 'Y'
                   COMPUTE TCPRC-FACE-YTD ROUNDED = TCPRC-CONSIGN-YTD *
                       TCPRC-MARGIN / 200
               WHEN TCPRC-STATUS-CD = 'HK'
                   MOVE 7.00 TO TCPRC-FACE-YTD
               WHEN TCPRC-STATUS-CD = 'RU'
                   MOVE 3.00 TO TCPRC-FACE-YTD
               WHEN OTHER
                   COMPUTE TCPRC-FACE-YTD ROUNDED = TCPRC-CONSIGN-YTD *
                       TCPRC-MARGIN / 100
           END-EVALUATE
           IF TCPRC-FACE-YTD > 0 AND TCPRC-FACE-YTD < 5.00
               MOVE 5.00 TO TCPRC-FACE-YTD
           END-IF
           IF TCPRC-FACE-YTD > 50.00
               MOVE 50.00 TO TCPRC-FACE-YTD
           END-IF
           ADD TCPRC-FACE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2110-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TCPRC-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2120-FLAG-SHORTAGE.
           IF TCPRC-SHORTAGE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TCPRC-SHORTAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FACE YTD FEE ON THE SHORTAGE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2130-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TCPRC-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TCPRC-FACE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TCPRC-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TCPRC-FACE-YTD ROUNDED = TCPRC-SHORTAGE-YTD *
                       TCPRC-FEE-PCT / 200
               WHEN TCPRC-ISSUER-CD = 'AF'
                   MOVE 12.00 TO TCPRC-FACE-YTD
               WHEN TCPRC-ISSUER-CD = 'CJ'
                   MOVE 20.00 TO TCPRC-FACE-YTD
               WHEN OTHER
                   COMPUTE TCPRC-FACE-YTD ROUNDED = TCPRC-SHORTAGE-YTD *
                       TCPRC-FEE-PCT / 100
           END-EVALUATE
           IF TCPRC-FACE-YTD > 0 AND TCPRC-FACE-YTD < 5.00
               MOVE 5.00 TO TCPRC-FACE-YTD
           END-IF
           IF TCPRC-FACE-YTD > 35.00
               MOVE 35.00 TO TCPRC-FACE-YTD
           END-IF
           ADD TCPRC-FACE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CONSIGN YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2140-FLAG-CONSIGN.
           IF TCPRC-CONSIGN-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TCPRC-CONSIGN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2150-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TCPRC-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2160-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TCPRC-CYCLE-CD
                   ADD TCPRC-FACE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TCPRC-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TCPRC-FACE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * REFUND YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2170-LIMIT-REFUND.
           IF TCPRC-REFUND-YTD < 10.00
               MOVE 10.00 TO TCPRC-REFUND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TCPRC-REFUND-YTD > 1000.00
               MOVE 1000.00 TO TCPRC-REFUND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONSIGN YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2180-SPLIT-CONSIGN.
           COMPUTE WS-WORK-AMT = TCPRC-CONSIGN-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TCPRC-CONSIGN-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2190-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TCPRC-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REMIT TIER TIER FOR THE REFUND YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2200-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-TIER-RATE(1) TO TCPRC-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TCPRC-REFUND-YTD NOT < WS-REMIT-TIER-LOW(WS-TX)
                   MOVE WS-REMIT-TIER-RATE(WS-TX) TO TCPRC-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCPRC-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCPRC-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2210-DESCRIBE-ISSUER.
           EVALUATE TCPRC-ISSUER-CD
               WHEN 'AF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE REMIT TIER TIER FOR THE CONSIGN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2220-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-TIER-RATE(1) TO TCPRC-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TCPRC-CONSIGN-YTD NOT < WS-REMIT-TIER-LOW(WS-TX)
                   MOVE WS-REMIT-TIER-RATE(WS-TX) TO TCPRC-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCPRC-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCPRC-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2230-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-CLOSE-DATE > 0 AND TCPRC-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TCPRC-REMIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TCPRC-REMIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TCPRC-REMIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TCPRC-REMIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TCPRC-REMIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CONSIGN AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2240-SPLIT-CONSIGN.
           COMPUTE WS-WORK-AMT = TCPRC-CONSIGN-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TCPRC-CONSIGN-AMT - (WS-WORK-AMT * 4)
           COMPUTE TCPRC-SHORTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2250-DESCRIBE-CYCLE.
           EVALUATE TCPRC-CYCLE-CD
               WHEN 'EPW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HHK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JCB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UCB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XTY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XUV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XXU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CONSIGN AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2260-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TCPRC-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TCPRC-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TCPRC-CONSIGN-AMT *
                       TCPRC-FEE-PCT / 200
               WHEN TCPRC-BRANCH-CD = 'EG'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN TCPRC-BRANCH-CD = 'HZ'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TCPRC-CONSIGN-AMT *
                       TCPRC-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE FACE TIER TIER FOR THE REFUND YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2270-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-TIER-RATE(1) TO TCPRC-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TCPRC-REFUND-YTD NOT < WS-FACE-TIER-LOW(WS-TX)
                   MOVE WS-FACE-TIER-RATE(WS-TX) TO TCPRC-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCPRC-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCPRC-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONSIGN AMT.
      *----------------------------------------------------------------*
       2280-PRORATE-CONSIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TCPRC-CONSIGN-AMT ROUNDED =
                   TCPRC-REMIT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TCPRC-CONSIGN-AMT
           END-IF
           ADD TCPRC-CONSIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2290-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TCPRC-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ISSUER CD:
      * AF -> CJ WHEN MATURITY DATE IS PAST
      * CJ -> DU WHEN MATURITY DATE IS PAST
      * DU -> JY WHEN MATURITY DATE IS PAST
      * JY -> RR WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2300-SET-ISSUER.
           EVALUATE TCPRC-ISSUER-CD
               WHEN 'AF'
                   IF TCPRC-MATURITY-DATE > 0 AND TCPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CJ' TO TCPRC-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJ'
                   IF TCPRC-MATURITY-DATE > 0 AND TCPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO TCPRC-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DU'
                   IF TCPRC-MATURITY-DATE > 0 AND TCPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JY' TO TCPRC-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JY'
                   IF TCPRC-MATURITY-DATE > 0 AND TCPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RR' TO TCPRC-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2310-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-CLOSE-DATE > 0 AND TCPRC-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-CLOSE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2320-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-CLOSE-DATE > 0 AND TCPRC-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TCPRC-CONSIGN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TCPRC-CONSIGN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TCPRC-CONSIGN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TCPRC-CONSIGN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TCPRC-CONSIGN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E055 CONTACT REQUIRED
      * E155 WORK AMT 2 NEGATIVE
      * E983 WORK AMT 2 OVER 250000
      * E323 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2330-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TCPRC-CONTACT = SPACES
                   MOVE 'E055' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E155' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E983' TO WS-REJECT-CODE
               WHEN TCPRC-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E323' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2340-COUNT-EXCEPT.
           IF TCPRC-EXCEPT-CNT < 9999999
               ADD 1 TO TCPRC-EXCEPT-CNT
           END-IF
           IF TCPRC-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO TCPRC-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2350-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TCPRC-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REMIT TIER TIER FOR THE FACE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2360-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-TIER-RATE(1) TO TCPRC-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TCPRC-FACE-YTD NOT < WS-REMIT-TIER-LOW(WS-TX)
                   MOVE WS-REMIT-TIER-RATE(WS-TX) TO TCPRC-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCPRC-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCPRC-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2370-DESCRIBE-BRANCH.
           EVALUATE TCPRC-BRANCH-CD
               WHEN 'EG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2380-DESCRIBE-STATUS.
           EVALUATE TCPRC-STATUS-CD
               WHEN 'HK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE CONSIGN YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2390-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TCPRC-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TCPRC-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TCPRC-CONSIGN-YTD *
                       TCPRC-MARGIN / 200
               WHEN TCPRC-STATUS-CD = 'HK'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN TCPRC-STATUS-CD = 'RU'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TCPRC-CONSIGN-YTD *
                       TCPRC-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2400-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-EXPIRY-DATE > 0 AND TCPRC-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TCPRC-CONSIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TCPRC-CONSIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TCPRC-CONSIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TCPRC-CONSIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TCPRC-CONSIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2410-COUNT-CYCLE.
           IF TCPRC-CYCLE-CNT < 9999999
               ADD 1 TO TCPRC-CYCLE-CNT
           END-IF
           IF TCPRC-CYCLE-CNT NOT < 6
               MOVE 'Y' TO TCPRC-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REMIT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2420-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = TCPRC-REMIT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TCPRC-REMIT-YTD - (WS-WORK-AMT * 3)
           COMPUTE TCPRC-REFUND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2430-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-MATURITY-DATE > 0 AND TCPRC-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TCPRC-REFUND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD TCPRC-REFUND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TCPRC-REFUND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TCPRC-REFUND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TCPRC-REFUND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHORTAGE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2440-FLAG-SHORTAGE.
           IF TCPRC-SHORTAGE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TCPRC-SHORTAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2450-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TCPRC-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TCPRC-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REMIT YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2460-ACCRUE-REMIT.
           IF TCPRC-REMIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TCPRC-REMIT-YTD * TCPRC-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TCPRC-SHORTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E715 NAME REQUIRED
      * E998 SHORTAGE YTD NEGATIVE
      * E857 SHORTAGE YTD OVER 50000
      * E956 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2470-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TCPRC-NAME = SPACES
                   MOVE 'E715' TO WS-REJECT-CODE
               WHEN TCPRC-SHORTAGE-YTD < 0
                   MOVE 'E998' TO WS-REJECT-CODE
               WHEN TCPRC-SHORTAGE-YTD > 50000.00
                   MOVE 'E857' TO WS-REJECT-CODE
               WHEN TCPRC-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E956' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2480-DESCRIBE-ISSUER.
           EVALUATE TCPRC-ISSUER-CD
               WHEN 'AF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'CJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2490-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TCPRC-STATUS-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TCPRC-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REMIT YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2500-ACCRUE-REMIT.
           IF TCPRC-REMIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TCPRC-REMIT-YTD * TCPRC-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TCPRC-CONSIGN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2510-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TCPRC-BRANCH-CD
                   ADD TCPRC-REFUND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TCPRC-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TCPRC-REFUND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2520-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-MATURITY-DATE > 0 AND TCPRC-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TCPRC-FACE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD TCPRC-FACE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TCPRC-FACE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TCPRC-FACE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TCPRC-FACE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FACE TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2530-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-TIER-RATE(1) TO TCPRC-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-FACE-TIER-LOW(WS-TX)
                   MOVE WS-FACE-TIER-RATE(WS-TX) TO TCPRC-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCPRC-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCPRC-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2540-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-CLOSE-DATE > 0 AND TCPRC-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TCPRC-CONSIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TCPRC-CONSIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TCPRC-CONSIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TCPRC-CONSIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TCPRC-CONSIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2550-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONSIGN YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2560-ACCRUE-CONSIGN.
           IF TCPRC-CONSIGN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TCPRC-CONSIGN-YTD * TCPRC-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TCPRC-CONSIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONSIGN YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2570-LIMIT-CONSIGN.
           IF TCPRC-CONSIGN-YTD < 0.00
               MOVE 0.00 TO TCPRC-CONSIGN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TCPRC-CONSIGN-YTD > 5000.00
               MOVE 5000.00 TO TCPRC-CONSIGN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REMIT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2580-FLAG-REMIT.
           IF TCPRC-REMIT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TCPRC-REMIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2590-COUNT-CYCLE.
           IF TCPRC-CYCLE-CNT < 9999999
               ADD 1 TO TCPRC-CYCLE-CNT
           END-IF
           IF TCPRC-CYCLE-CNT NOT < 3
               MOVE 'Y' TO TCPRC-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2600-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TCPRC-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REMIT TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2610-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-TIER-RATE(1) TO TCPRC-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-REMIT-TIER-LOW(WS-TX)
                   MOVE WS-REMIT-TIER-RATE(WS-TX) TO TCPRC-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCPRC-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCPRC-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2620-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TCPRC-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TCPRC-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2630-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-MATURITY-DATE > 0 AND TCPRC-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TCPRC-CONSIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TCPRC-CONSIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TCPRC-CONSIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TCPRC-CONSIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TCPRC-CONSIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2640-COUNT-EXCEPT.
           IF TCPRC-EXCEPT-CNT < 9999999
               ADD 1 TO TCPRC-EXCEPT-CNT
           END-IF
           IF TCPRC-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO TCPRC-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REFUND YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2650-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = TCPRC-REFUND-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TCPRC-REFUND-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FACE TIER TIER FOR THE SHORTAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2660-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-TIER-RATE(1) TO TCPRC-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TCPRC-SHORTAGE-YTD NOT < WS-FACE-TIER-LOW(WS-TX)
                   MOVE WS-FACE-TIER-RATE(WS-TX) TO TCPRC-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCPRC-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCPRC-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FACE TIER TIER FOR THE SHORTAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2670-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-TIER-RATE(1) TO TCPRC-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TCPRC-SHORTAGE-YTD NOT < WS-FACE-TIER-LOW(WS-TX)
                   MOVE WS-FACE-TIER-RATE(WS-TX) TO TCPRC-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCPRC-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCPRC-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2680-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TCPRC-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E713 NAME REQUIRED
      * E478 CONSIGN YTD NEGATIVE
      * E487 CONSIGN YTD OVER 50000
      * E574 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2690-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TCPRC-NAME = SPACES
                   MOVE 'E713' TO WS-REJECT-CODE
               WHEN TCPRC-CONSIGN-YTD < 0
                   MOVE 'E478' TO WS-REJECT-CODE
               WHEN TCPRC-CONSIGN-YTD > 50000.00
                   MOVE 'E487' TO WS-REJECT-CODE
               WHEN TCPRC-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E574' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2700-FLAG-SHORTAGE.
           IF TCPRC-SHORTAGE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TCPRC-SHORTAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EPW -> HHK WHEN EXPIRY DATE IS PAST
      * HHK -> JCB WHEN EXPIRY DATE IS PAST
      * JCB -> UCB WHEN EXPIRY DATE IS PAST
      * UCB -> XTY WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2710-SET-CYCLE.
           EVALUATE TCPRC-CYCLE-CD
               WHEN 'EPW'
                   IF TCPRC-EXPIRY-DATE > 0 AND TCPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HHK' TO TCPRC-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HHK'
                   IF TCPRC-EXPIRY-DATE > 0 AND TCPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JCB' TO TCPRC-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JCB'
                   IF TCPRC-EXPIRY-DATE > 0 AND TCPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UCB' TO TCPRC-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UCB'
                   IF TCPRC-EXPIRY-DATE > 0 AND TCPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XTY' TO TCPRC-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONSIGN YTD.
      *----------------------------------------------------------------*
       2720-PRORATE-CONSIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TCPRC-CONSIGN-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TCPRC-CONSIGN-YTD
           END-IF
           ADD TCPRC-CONSIGN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CONSIGN YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2730-SPLIT-CONSIGN.
           COMPUTE WS-WORK-AMT = TCPRC-CONSIGN-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TCPRC-CONSIGN-YTD - (WS-WORK-AMT *
               12)
           COMPUTE TCPRC-CONSIGN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REMIT YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2740-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TCPRC-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TCPRC-REMIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TCPRC-REVIEW-FLAG = 'Y'
                   COMPUTE TCPRC-REMIT-YTD ROUNDED = WS-WORK-AMT-2 *
                       TCPRC-FEE-PCT / 200
               WHEN TCPRC-BRANCH-CD = 'EG'
                   MOVE 7.00 TO TCPRC-REMIT-YTD
               WHEN TCPRC-BRANCH-CD = 'HZ'
                   MOVE 12.00 TO TCPRC-REMIT-YTD
               WHEN OTHER
                   COMPUTE TCPRC-REMIT-YTD ROUNDED = WS-WORK-AMT-2 *
                       TCPRC-FEE-PCT / 100
           END-EVALUATE
           IF TCPRC-REMIT-YTD > 0 AND TCPRC-REMIT-YTD < 1.00
               MOVE 1.00 TO TCPRC-REMIT-YTD
           END-IF
           IF TCPRC-REMIT-YTD > 75.00
               MOVE 75.00 TO TCPRC-REMIT-YTD
           END-IF
           ADD TCPRC-REMIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E735 CONTACT REQUIRED
      * E299 FACE YTD NEGATIVE
      * E156 FACE YTD OVER 10000
      * E888 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2750-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TCPRC-CONTACT = SPACES
                   MOVE 'E735' TO WS-REJECT-CODE
               WHEN TCPRC-FACE-YTD < 0
                   MOVE 'E299' TO WS-REJECT-CODE
               WHEN TCPRC-FACE-YTD > 10000.00
                   MOVE 'E156' TO WS-REJECT-CODE
               WHEN TCPRC-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E888' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2760-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TCPRC-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TCPRC-CONSIGN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2770-ACCRUE-FACE.
           IF TCPRC-FACE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TCPRC-FACE-YTD * TCPRC-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TCPRC-REMIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2780-COUNT-EXCEPT.
           IF TCPRC-EXCEPT-CNT < 9999999
               ADD 1 TO TCPRC-EXCEPT-CNT
           END-IF
           IF TCPRC-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO TCPRC-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REMIT TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2790-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-TIER-RATE(1) TO TCPRC-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-REMIT-TIER-LOW(WS-TX)
                   MOVE WS-REMIT-TIER-RATE(WS-TX) TO TCPRC-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCPRC-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCPRC-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2800-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-MATURITY-DATE > 0 AND TCPRC-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TCPRC-REFUND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TCPRC-REFUND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TCPRC-REFUND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TCPRC-REFUND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TCPRC-REFUND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2810-DESCRIBE-STATUS.
           EVALUATE TCPRC-STATUS-CD
               WHEN 'HK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * HK -> RU WHEN MATURITY DATE IS PAST
      * RU -> WA WHEN MATURITY DATE IS PAST
      * WA -> HK WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2820-SET-STATUS.
           EVALUATE TCPRC-STATUS-CD
               WHEN 'HK'
                   IF TCPRC-MATURITY-DATE > 0 AND TCPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RU' TO TCPRC-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF TCPRC-MATURITY-DATE > 0 AND TCPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WA' TO TCPRC-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WA'
                   IF TCPRC-MATURITY-DATE > 0 AND TCPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HK' TO TCPRC-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2830-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-MATURITY-DATE > 0 AND TCPRC-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TCPRC-CONSIGN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TCPRC-CONSIGN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TCPRC-CONSIGN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TCPRC-CONSIGN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TCPRC-CONSIGN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EPW -> HHK WHEN EXPIRY DATE IS PAST
      * HHK -> JCB WHEN EXPIRY DATE IS PAST
      * JCB -> UCB WHEN EXPIRY DATE IS PAST
      * UCB -> XTY WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2840-SET-CYCLE.
           EVALUATE TCPRC-CYCLE-CD
               WHEN 'EPW'
                   IF TCPRC-EXPIRY-DATE > 0 AND TCPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HHK' TO TCPRC-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HHK'
                   IF TCPRC-EXPIRY-DATE > 0 AND TCPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JCB' TO TCPRC-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JCB'
                   IF TCPRC-EXPIRY-DATE > 0 AND TCPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UCB' TO TCPRC-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UCB'
                   IF TCPRC-EXPIRY-DATE > 0 AND TCPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XTY' TO TCPRC-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2850-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TCPRC-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2860-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TCPRC-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2870-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TCPRC-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TCPRC-SHORTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FACE TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2880-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-TIER-RATE(1) TO TCPRC-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-FACE-TIER-LOW(WS-TX)
                   MOVE WS-FACE-TIER-RATE(WS-TX) TO TCPRC-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCPRC-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCPRC-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2890-COUNT-CYCLE.
           IF TCPRC-CYCLE-CNT < 9999999
               ADD 1 TO TCPRC-CYCLE-CNT
           END-IF
           IF TCPRC-CYCLE-CNT NOT < 10
               MOVE 'Y' TO TCPRC-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2900-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE TCPRC-REMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2910-DESCRIBE-STATUS.
           EVALUATE TCPRC-STATUS-CD
               WHEN 'HK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2920-DESCRIBE-CYCLE.
           EVALUATE TCPRC-CYCLE-CD
               WHEN 'EPW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HHK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JCB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UCB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XTY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XUV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XXU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * HK -> RU WHEN EXPIRY DATE IS PAST
      * RU -> WA WHEN EXPIRY DATE IS PAST
      * WA -> HK WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2930-SET-STATUS.
           EVALUATE TCPRC-STATUS-CD
               WHEN 'HK'
                   IF TCPRC-EXPIRY-DATE > 0 AND TCPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RU' TO TCPRC-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF TCPRC-EXPIRY-DATE > 0 AND TCPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WA' TO TCPRC-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WA'
                   IF TCPRC-EXPIRY-DATE > 0 AND TCPRC-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HK' TO TCPRC-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FACE TIER TIER FOR THE REMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2940-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-TIER-RATE(1) TO TCPRC-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TCPRC-REMIT-YTD NOT < WS-FACE-TIER-LOW(WS-TX)
                   MOVE WS-FACE-TIER-RATE(WS-TX) TO TCPRC-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TCPRC-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TCPRC-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2950-COUNT-EXCEPT.
           IF TCPRC-EXCEPT-CNT < 9999999
               ADD 1 TO TCPRC-EXCEPT-CNT
           END-IF
           IF TCPRC-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO TCPRC-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHORTAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REMIT YTD.
      *----------------------------------------------------------------*
       2960-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TCPRC-REMIT-YTD ROUNDED =
                   TCPRC-SHORTAGE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TCPRC-REMIT-YTD
           END-IF
           ADD TCPRC-REMIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2970-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TCPRC-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TCPRC-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TCPRC-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONSIGN AMT.
      *----------------------------------------------------------------*
       2980-PRORATE-CONSIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TCPRC-CONSIGN-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TCPRC-CONSIGN-AMT
           END-IF
           ADD TCPRC-CONSIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHORTAGE YTD.
      *----------------------------------------------------------------*
       2990-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TCPRC-SHORTAGE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TCPRC-SHORTAGE-YTD
           END-IF
           ADD TCPRC-SHORTAGE-YTD TO WS-TOTAL-PRORATED.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO TCPRC-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO TCPRC-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO TCPRC-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'TCPRC EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
