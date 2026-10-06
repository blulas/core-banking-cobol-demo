      *================================================================*
      * PROGRAM  : MODTE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : MONEY ORDERS RULE - VALIDATION. CALLED BY THE MONEY *
      *            ORDERS BATCH AND ONLINE PROGRAMS.                   *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-27 PQR  ORIGINAL                                       *
      * 1991-05-02 PQR  VOIDED LIMIT REVISED PRB1934                   *
      * 1997-10-09 MKL  PERFORMANCE - PAID READ AHEAD PRB8097          *
      * 2003-12-25 PQR  OUTSTAND EXCEPTIONS TO OPS QUEUE CHG9985       *
      * 2004-01-15 DPS  PAID CALCULATION CORRECTED REQ8629             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MODTE.
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
                                           VALUE 10.000000.
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
      * WS-FACE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FACE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000811000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000042500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000848500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000595000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011150000000661000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011650000000032500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011750000000435500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012250000000735500'.
       01  WS-FACE-GRID-TBL REDEFINES WS-FACE-GRID-VALUES.
           05  WS-FACE-GRID-ROW OCCURS 8 TIMES.
               10  WS-FACE-GRID-LOW        PIC 9(09)V99.
               10  WS-FACE-GRID-RATE       PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY MODTEP.

       PROCEDURE DIVISION USING MODTE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SET-ITEM-TYPE
           PERFORM 2010-SET-TYPE
           PERFORM 2020-CHECK-CLOSE
           PERFORM 2030-DESCRIBE-REASON
           PERFORM 2040-ASSESS-EXEMPT
           PERFORM 2050-AGE-LAST-MAINT
           PERFORM 2060-SPLIT-ESCHEAT
           PERFORM 2070-AGE-OPEN
           PERFORM 2080-SET-REASON
           PERFORM 2090-SELECT-FEE-PCT
           PERFORM 2100-ACCRUE-VOIDED
           PERFORM 2110-AGE-LAST-MAINT
           PERFORM 2120-DESCRIBE-TYPE
           PERFORM 2130-SELECT-DISCOUNT
           PERFORM 2140-PRORATE-PAID
           PERFORM 2150-AGE-EXPIRY
           PERFORM 2160-SPLIT-FEE
           PERFORM 2170-ACCUM-REASON
           PERFORM 2180-ACCRUE-FEE-PCT
           PERFORM 2190-LIMIT-ITEM
           PERFORM 2200-ASSESS-STOP
           PERFORM 2210-FLAG-ITEM
           PERFORM 2220-DESCRIBE-CYCLE
           PERFORM 2230-DESCRIBE-REASON
           PERFORM 2240-COUNT-PAYMENT
           PERFORM 2250-DESCRIBE-REASON
           PERFORM 2260-DESCRIBE-CYCLE
           PERFORM 2270-ACCRUE-FEE
           PERFORM 2280-SPLIT-VOIDED
           PERFORM 2290-LIMIT-FACE
           PERFORM 2300-ACCUM-TYPE
           PERFORM 2310-CHECK-OPEN
           PERFORM 2320-CHECK-EXPIRY
           PERFORM 2330-COUNT-PAYMENT
           PERFORM 2340-LIMIT-VOIDED
           PERFORM 2350-ACCUM-ITEM-TYPE
           PERFORM 2360-LIMIT-FEE
           PERFORM 2370-SELECT-FEE-PCT
           PERFORM 2380-ASSESS-STOP
           PERFORM 2390-EDIT-NAME
           PERFORM 2400-CHECK-LAST-MAINT
           PERFORM 2410-CHECK-CLOSE
           PERFORM 2420-DESCRIBE-CYCLE
           PERFORM 2430-ACCUM-CYCLE
           PERFORM 2440-ASSESS-STOP
           PERFORM 2450-ASSESS-EXEMPT
           PERFORM 2460-ACCRUE-FEE-PCT
           PERFORM 2470-SELECT-FEE-PCT
           PERFORM 2480-AGE-EXPIRY
           PERFORM 2490-ACCUM-CYCLE
           PERFORM 2500-ACCRUE-DISCOUNT
           PERFORM 2510-ACCUM-CYCLE
           PERFORM 2520-FLAG-FEE
           PERFORM 2530-SELECT-DISCOUNT
           PERFORM 2540-AGE-OPEN
           PERFORM 2550-EDIT-REF-NO
           PERFORM 2560-AGE-CLOSE
           PERFORM 2570-LIMIT-PAID
           PERFORM 2580-ACCUM-CYCLE
           PERFORM 2590-ACCRUE-FEE
           PERFORM 2600-ACCRUE-PAID
           PERFORM 2610-ASSESS-AUTO-RENEW
           PERFORM 2620-LIMIT-ITEM
           PERFORM 2630-ACCRUE-FEE
           PERFORM 2640-LIMIT-ESCHEAT
           PERFORM 2650-PRORATE-ESCHEAT
           PERFORM 2660-SET-CYCLE
           PERFORM 2670-SPLIT-ESCHEAT
           PERFORM 2680-SET-CYCLE
           PERFORM 2690-SPLIT-FEE
           PERFORM 2700-ASSESS-AUTO-RENEW
           PERFORM 2710-LIMIT-FEE
           PERFORM 2720-ACCRUE-ESCHEAT
           PERFORM 2730-FLAG-ITEM
           PERFORM 2740-SPLIT-PAID
           PERFORM 2750-PRORATE-FACE
           PERFORM 2760-PRORATE-VOIDED
           PERFORM 2770-SET-ITEM-TYPE
           PERFORM 2780-AGE-OPEN
           PERFORM 2790-EDIT-NAME
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO MODTE-RETURN-CODE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * B -> H WHEN OPEN DATE IS PAST
      * H -> S WHEN OPEN DATE IS PAST
      * S -> T WHEN OPEN DATE IS PAST
      * T -> B WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2000-SET-ITEM-TYPE.
           EVALUATE MODTE-ITEM-TYPE-CD
               WHEN 'B'
                   IF MODTE-OPEN-DATE > 0 AND MODTE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO MODTE-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF MODTE-OPEN-DATE > 0 AND MODTE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MODTE-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MODTE-OPEN-DATE > 0 AND MODTE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MODTE-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF MODTE-OPEN-DATE > 0 AND MODTE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO MODTE-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGE -> JDX WHEN CLOSE DATE IS PAST
      * JDX -> LDX WHEN CLOSE DATE IS PAST
      * LDX -> RHT WHEN CLOSE DATE IS PAST
      * RHT -> AGE WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2010-SET-TYPE.
           EVALUATE MODTE-TYPE-CD
               WHEN 'AGE'
                   IF MODTE-CLOSE-DATE > 0 AND MODTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JDX' TO MODTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JDX'
                   IF MODTE-CLOSE-DATE > 0 AND MODTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LDX' TO MODTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LDX'
                   IF MODTE-CLOSE-DATE > 0 AND MODTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RHT' TO MODTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RHT'
                   IF MODTE-CLOSE-DATE > 0 AND MODTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AGE' TO MODTE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2020-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MODTE-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MODTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO MODTE-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2030-DESCRIBE-REASON.
           EVALUATE MODTE-REASON-CD
               WHEN 'B'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'P'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'S'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VOIDED FEE ON THE FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2040-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MODTE-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MODTE-VOIDED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MODTE-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MODTE-VOIDED-AMT ROUNDED = MODTE-FEE-AMT *
                       MODTE-FEE-PCT / 200
               WHEN MODTE-CYCLE-CD = 'DGU'
                   MOVE 7.00 TO MODTE-VOIDED-AMT
               WHEN MODTE-CYCLE-CD = 'GEJ'
                   MOVE 15.00 TO MODTE-VOIDED-AMT
               WHEN OTHER
                   COMPUTE MODTE-VOIDED-AMT ROUNDED = MODTE-FEE-AMT *
                       MODTE-FEE-PCT / 100
           END-EVALUATE
           IF MODTE-VOIDED-AMT > 0 AND MODTE-VOIDED-AMT < 2.00
               MOVE 2.00 TO MODTE-VOIDED-AMT
           END-IF
           IF MODTE-VOIDED-AMT > 25.00
               MOVE 25.00 TO MODTE-VOIDED-AMT
           END-IF
           ADD MODTE-VOIDED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2050-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF MODTE-LAST-MAINT-DATE > 0 AND MODTE-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MODTE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MODTE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MODTE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MODTE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MODTE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MODTE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ESCHEAT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2060-SPLIT-ESCHEAT.
           COMPUTE WS-WORK-AMT = MODTE-ESCHEAT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MODTE-ESCHEAT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE MODTE-VOIDED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2070-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MODTE-OPEN-DATE > 0 AND MODTE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MODTE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MODTE-FACE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MODTE-FACE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MODTE-FACE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD MODTE-FACE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MODTE-FACE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> L WHEN LAST MAINT DATE IS PAST
      * L -> P WHEN LAST MAINT DATE IS PAST
      * P -> S WHEN LAST MAINT DATE IS PAST
      * S -> T WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2080-SET-REASON.
           EVALUATE MODTE-REASON-CD
               WHEN 'B'
                   IF MODTE-LAST-MAINT-DATE > 0 AND
                       MODTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'L' TO MODTE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF MODTE-LAST-MAINT-DATE > 0 AND
                       MODTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'P' TO MODTE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF MODTE-LAST-MAINT-DATE > 0 AND
                       MODTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'S' TO MODTE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MODTE-LAST-MAINT-DATE > 0 AND
                       MODTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'T' TO MODTE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FACE GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2090-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-GRID-RATE(1) TO MODTE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT-2 NOT < WS-FACE-GRID-LOW(WS-TX)
                   MOVE WS-FACE-GRID-RATE(WS-TX) TO MODTE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MODTE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MODTE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOIDED AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2100-ACCRUE-VOIDED.
           IF MODTE-VOIDED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MODTE-VOIDED-AMT * MODTE-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MODTE-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2110-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF MODTE-LAST-MAINT-DATE > 0 AND MODTE-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MODTE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MODTE-FACE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MODTE-FACE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MODTE-FACE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MODTE-FACE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MODTE-FACE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2120-DESCRIBE-TYPE.
           EVALUATE MODTE-TYPE-CD
               WHEN 'AGE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JDX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LDX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RHT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FACE GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2130-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-GRID-RATE(1) TO MODTE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-FACE-GRID-LOW(WS-TX)
                   MOVE WS-FACE-GRID-RATE(WS-TX) TO MODTE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MODTE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MODTE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2140-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MODTE-PAID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2150-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MODTE-EXPIRY-DATE > 0 AND MODTE-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MODTE-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2160-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MODTE-FEE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = MODTE-FEE-YTD - (WS-WORK-AMT * 4)
           COMPUTE MODTE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ESCHEAT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2170-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MODTE-REASON-CD
                   ADD MODTE-ESCHEAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MODTE-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MODTE-ESCHEAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2180-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * MODTE-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO MODTE-VOIDED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2190-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ESCHEAT FEE ON THE VOIDED AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2200-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MODTE-STOP-FLAG = 'Y'
                   MOVE ZERO TO MODTE-ESCHEAT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MODTE-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MODTE-ESCHEAT-AMT ROUNDED = MODTE-VOIDED-AMT
                       * MODTE-DISCOUNT / 200
               WHEN MODTE-TYPE-CD = 'AGE'
                   MOVE 7.00 TO MODTE-ESCHEAT-AMT
               WHEN MODTE-TYPE-CD = 'JDX'
                   MOVE 15.00 TO MODTE-ESCHEAT-AMT
               WHEN OTHER
                   COMPUTE MODTE-ESCHEAT-AMT ROUNDED = MODTE-VOIDED-AMT
                       * MODTE-DISCOUNT / 100
           END-EVALUATE
           IF MODTE-ESCHEAT-AMT > 0 AND MODTE-ESCHEAT-AMT < 5.00
               MOVE 5.00 TO MODTE-ESCHEAT-AMT
           END-IF
           IF MODTE-ESCHEAT-AMT > 25.00
               MOVE 25.00 TO MODTE-ESCHEAT-AMT
           END-IF
           ADD MODTE-ESCHEAT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2210-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2220-DESCRIBE-CYCLE.
           EVALUATE MODTE-CYCLE-CD
               WHEN 'DGU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GEJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JBE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2230-DESCRIBE-REASON.
           EVALUATE MODTE-REASON-CD
               WHEN 'B'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'P'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'S'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Z'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2240-COUNT-PAYMENT.
           IF MODTE-PAYMENT-CNT < 9999999
               ADD 1 TO MODTE-PAYMENT-CNT
           END-IF
           IF MODTE-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO MODTE-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2250-DESCRIBE-REASON.
           EVALUATE MODTE-REASON-CD
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'S'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2260-DESCRIBE-CYCLE.
           EVALUATE MODTE-CYCLE-CD
               WHEN 'DGU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GEJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JBE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2270-ACCRUE-FEE.
           IF MODTE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MODTE-FEE-YTD * MODTE-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOIDED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2280-SPLIT-VOIDED.
           COMPUTE WS-WORK-AMT = MODTE-VOIDED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = MODTE-VOIDED-AMT - (WS-WORK-AMT * 3)
           COMPUTE MODTE-FACE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2290-LIMIT-FACE.
           IF MODTE-FACE-AMT < 0.00
               MOVE 0.00 TO MODTE-FACE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MODTE-FACE-AMT > 2500.00
               MOVE 2500.00 TO MODTE-FACE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2300-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MODTE-TYPE-CD
                   ADD MODTE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MODTE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MODTE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2310-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MODTE-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MODTE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO MODTE-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2320-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MODTE-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MODTE-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO MODTE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2330-COUNT-PAYMENT.
           IF MODTE-PAYMENT-CNT < 9999999
               ADD 1 TO MODTE-PAYMENT-CNT
           END-IF
           IF MODTE-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO MODTE-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VOIDED AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2340-LIMIT-VOIDED.
           IF MODTE-VOIDED-AMT < 25.00
               MOVE 25.00 TO MODTE-VOIDED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MODTE-VOIDED-AMT > 500.00
               MOVE 500.00 TO MODTE-VOIDED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE AMT BY ITEM TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2350-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MODTE-ITEM-TYPE-CD
                   ADD MODTE-FACE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MODTE-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MODTE-FACE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2360-LIMIT-FEE.
           IF MODTE-FEE-AMT < 100.00
               MOVE 100.00 TO MODTE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MODTE-FEE-AMT > 500.00
               MOVE 500.00 TO MODTE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FACE GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2370-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-GRID-RATE(1) TO MODTE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT-2 NOT < WS-FACE-GRID-LOW(WS-TX)
                   MOVE WS-FACE-GRID-RATE(WS-TX) TO MODTE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MODTE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MODTE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE VOIDED AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2380-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MODTE-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MODTE-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = MODTE-VOIDED-AMT *
                       MODTE-FEE-PCT / 200
               WHEN MODTE-TYPE-CD = 'AGE'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN MODTE-TYPE-CD = 'JDX'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = MODTE-VOIDED-AMT *
                       MODTE-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E741 NAME REQUIRED
      * E789 ESCHEAT AMT NEGATIVE
      * E133 ESCHEAT AMT OVER 250000
      * E294 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2390-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MODTE-NAME = SPACES
                   MOVE 'E741' TO WS-REJECT-CODE
               WHEN MODTE-ESCHEAT-AMT < 0
                   MOVE 'E789' TO WS-REJECT-CODE
               WHEN MODTE-ESCHEAT-AMT > 250000.00
                   MOVE 'E133' TO WS-REJECT-CODE
               WHEN MODTE-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E294' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2400-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF MODTE-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MODTE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO MODTE-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2410-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MODTE-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MODTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO MODTE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2420-DESCRIBE-CYCLE.
           EVALUATE MODTE-CYCLE-CD
               WHEN 'DGU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GEJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JBE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2430-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MODTE-CYCLE-CD
                   ADD MODTE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MODTE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MODTE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOIDED FEE ON THE FEE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2440-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MODTE-STOP-FLAG = 'Y'
                   MOVE ZERO TO MODTE-VOIDED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MODTE-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MODTE-VOIDED-AMT ROUNDED = MODTE-FEE-YTD *
                       MODTE-DISCOUNT / 200
               WHEN MODTE-ITEM-TYPE-CD = 'B'
                   MOVE 5.00 TO MODTE-VOIDED-AMT
               WHEN MODTE-ITEM-TYPE-CD = 'H'
                   MOVE 15.00 TO MODTE-VOIDED-AMT
               WHEN OTHER
                   COMPUTE MODTE-VOIDED-AMT ROUNDED = MODTE-FEE-YTD *
                       MODTE-DISCOUNT / 100
           END-EVALUATE
           IF MODTE-VOIDED-AMT > 0 AND MODTE-VOIDED-AMT < 2.00
               MOVE 2.00 TO MODTE-VOIDED-AMT
           END-IF
           IF MODTE-VOIDED-AMT > 50.00
               MOVE 50.00 TO MODTE-VOIDED-AMT
           END-IF
           ADD MODTE-VOIDED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE FACE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2450-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MODTE-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MODTE-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MODTE-STOP-FLAG = 'Y'
                   COMPUTE MODTE-PAID-AMT ROUNDED = MODTE-FACE-AMT *
                       MODTE-DISCOUNT / 200
               WHEN MODTE-ITEM-TYPE-CD = 'B'
                   MOVE 3.00 TO MODTE-PAID-AMT
               WHEN MODTE-ITEM-TYPE-CD = 'H'
                   MOVE 5.00 TO MODTE-PAID-AMT
               WHEN OTHER
                   COMPUTE MODTE-PAID-AMT ROUNDED = MODTE-FACE-AMT *
                       MODTE-DISCOUNT / 100
           END-EVALUATE
           IF MODTE-PAID-AMT > 0 AND MODTE-PAID-AMT < 2.00
               MOVE 2.00 TO MODTE-PAID-AMT
           END-IF
           IF MODTE-PAID-AMT > 75.00
               MOVE 75.00 TO MODTE-PAID-AMT
           END-IF
           ADD MODTE-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2460-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * MODTE-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MODTE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FACE GRID TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2470-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-GRID-RATE(1) TO MODTE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF MODTE-FEE-YTD NOT < WS-FACE-GRID-LOW(WS-TX)
                   MOVE WS-FACE-GRID-RATE(WS-TX) TO MODTE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MODTE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MODTE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2480-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MODTE-EXPIRY-DATE > 0 AND MODTE-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MODTE-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MODTE-FACE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MODTE-FACE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MODTE-FACE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MODTE-FACE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MODTE-FACE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2490-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MODTE-CYCLE-CD
                   ADD MODTE-PAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MODTE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MODTE-PAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2500-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * MODTE-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MODTE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2510-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MODTE-CYCLE-CD
                   ADD MODTE-PAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MODTE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MODTE-PAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2520-FLAG-FEE.
           IF MODTE-FEE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MODTE-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FACE GRID TIER FOR THE FACE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2530-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-GRID-RATE(1) TO MODTE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF MODTE-FACE-AMT NOT < WS-FACE-GRID-LOW(WS-TX)
                   MOVE WS-FACE-GRID-RATE(WS-TX) TO MODTE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MODTE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MODTE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2540-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MODTE-OPEN-DATE > 0 AND MODTE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MODTE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MODTE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MODTE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MODTE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MODTE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MODTE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E028 REF NO REQUIRED
      * E812 WORK AMT NEGATIVE
      * E317 WORK AMT OVER 5000
      * E373 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2550-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MODTE-REF-NO = SPACES
                   MOVE 'E028' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E812' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E317' TO WS-REJECT-CODE
               WHEN MODTE-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E373' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2560-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MODTE-CLOSE-DATE > 0 AND MODTE-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MODTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MODTE-ESCHEAT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MODTE-ESCHEAT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD MODTE-ESCHEAT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MODTE-ESCHEAT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MODTE-ESCHEAT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2570-LIMIT-PAID.
           IF MODTE-PAID-AMT < 100.00
               MOVE 100.00 TO MODTE-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MODTE-PAID-AMT > 500.00
               MOVE 500.00 TO MODTE-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2580-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MODTE-CYCLE-CD
                   ADD MODTE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MODTE-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MODTE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2590-ACCRUE-FEE.
           IF MODTE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MODTE-FEE-YTD * MODTE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MODTE-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2600-ACCRUE-PAID.
           IF MODTE-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MODTE-PAID-AMT * MODTE-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE FEE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2610-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MODTE-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MODTE-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = MODTE-FEE-YTD *
                       MODTE-DISCOUNT / 200
               WHEN MODTE-ITEM-TYPE-CD = 'B'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN MODTE-ITEM-TYPE-CD = 'H'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = MODTE-FEE-YTD *
                       MODTE-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2620-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2630-ACCRUE-FEE.
           IF MODTE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MODTE-FEE-YTD * MODTE-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO MODTE-ESCHEAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2640-LIMIT-ESCHEAT.
           IF MODTE-ESCHEAT-AMT < 50.00
               MOVE 50.00 TO MODTE-ESCHEAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MODTE-ESCHEAT-AMT > 1000.00
               MOVE 1000.00 TO MODTE-ESCHEAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ESCHEAT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2650-PRORATE-ESCHEAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MODTE-ESCHEAT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DGU -> GEJ WHEN LAST MAINT DATE IS PAST
      * GEJ -> JBE WHEN LAST MAINT DATE IS PAST
      * JBE -> DGU WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2660-SET-CYCLE.
           EVALUATE MODTE-CYCLE-CD
               WHEN 'DGU'
                   IF MODTE-LAST-MAINT-DATE > 0 AND
                       MODTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'GEJ' TO MODTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GEJ'
                   IF MODTE-LAST-MAINT-DATE > 0 AND
                       MODTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'JBE' TO MODTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBE'
                   IF MODTE-LAST-MAINT-DATE > 0 AND
                       MODTE-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'DGU' TO MODTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ESCHEAT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2670-SPLIT-ESCHEAT.
           COMPUTE WS-WORK-AMT = MODTE-ESCHEAT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MODTE-ESCHEAT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE MODTE-PAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DGU -> GEJ WHEN OPEN DATE IS PAST
      * GEJ -> JBE WHEN OPEN DATE IS PAST
      * JBE -> DGU WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2680-SET-CYCLE.
           EVALUATE MODTE-CYCLE-CD
               WHEN 'DGU'
                   IF MODTE-OPEN-DATE > 0 AND MODTE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GEJ' TO MODTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GEJ'
                   IF MODTE-OPEN-DATE > 0 AND MODTE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBE' TO MODTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBE'
                   IF MODTE-OPEN-DATE > 0 AND MODTE-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO MODTE-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2690-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MODTE-FEE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = MODTE-FEE-YTD - (WS-WORK-AMT * 4)
           COMPUTE MODTE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE FEE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2700-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MODTE-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO MODTE-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MODTE-STOP-FLAG = 'Y'
                   COMPUTE MODTE-PAID-AMT ROUNDED = MODTE-FEE-YTD *
                       MODTE-FEE-PCT / 200
               WHEN MODTE-REASON-CD = 'B'
                   MOVE 20.00 TO MODTE-PAID-AMT
               WHEN MODTE-REASON-CD = 'L'
                   MOVE 20.00 TO MODTE-PAID-AMT
               WHEN OTHER
                   COMPUTE MODTE-PAID-AMT ROUNDED = MODTE-FEE-YTD *
                       MODTE-FEE-PCT / 100
           END-EVALUATE
           IF MODTE-PAID-AMT > 0 AND MODTE-PAID-AMT < 1.00
               MOVE 1.00 TO MODTE-PAID-AMT
           END-IF
           IF MODTE-PAID-AMT > 35.00
               MOVE 35.00 TO MODTE-PAID-AMT
           END-IF
           ADD MODTE-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2710-LIMIT-FEE.
           IF MODTE-FEE-AMT < 0.00
               MOVE 0.00 TO MODTE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MODTE-FEE-AMT > 500.00
               MOVE 500.00 TO MODTE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ESCHEAT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2720-ACCRUE-ESCHEAT.
           IF MODTE-ESCHEAT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MODTE-ESCHEAT-AMT * MODTE-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MODTE-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2730-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2740-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = MODTE-PAID-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MODTE-PAID-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FACE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2750-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MODTE-FACE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE VOIDED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2760-PRORATE-VOIDED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MODTE-VOIDED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * B -> H WHEN CLOSE DATE IS PAST
      * H -> S WHEN CLOSE DATE IS PAST
      * S -> T WHEN CLOSE DATE IS PAST
      * T -> B WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2770-SET-ITEM-TYPE.
           EVALUATE MODTE-ITEM-TYPE-CD
               WHEN 'B'
                   IF MODTE-CLOSE-DATE > 0 AND MODTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO MODTE-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF MODTE-CLOSE-DATE > 0 AND MODTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MODTE-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MODTE-CLOSE-DATE > 0 AND MODTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MODTE-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF MODTE-CLOSE-DATE > 0 AND MODTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO MODTE-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2780-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MODTE-OPEN-DATE > 0 AND MODTE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MODTE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MODTE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MODTE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MODTE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MODTE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MODTE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E707 NAME REQUIRED
      * E548 VOIDED AMT NEGATIVE
      * E563 VOIDED AMT OVER 10000
      * E979 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2790-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MODTE-NAME = SPACES
                   MOVE 'E707' TO WS-REJECT-CODE
               WHEN MODTE-VOIDED-AMT < 0
                   MOVE 'E548' TO WS-REJECT-CODE
               WHEN MODTE-VOIDED-AMT > 10000.00
                   MOVE 'E563' TO WS-REJECT-CODE
               WHEN MODTE-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E979' TO WS-REJECT-CODE
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
                   MOVE 08 TO MODTE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO MODTE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO MODTE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'MODTE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
