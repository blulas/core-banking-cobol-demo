      *================================================================*
      * PROGRAM  : ESFEE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : MORTGAGE ESCROW RULE - TIERING. CALLED BY THE       *
      *            MORTGAGE ESCROW BATCH AND ONLINE PROGRAMS.          *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-09-27 PQR  ORIGINAL                                       *
      * 1990-08-07 MKL  AUDIT FINDING - DISBURSE CONTROL TOTALS PRB7208*
      * 1992-09-28 BHS  INSURANCE CALCULATION CORRECTED INC1695        *
      * 1993-03-20 JLM  CUSHION NOTICE TEXT PER LEGAL REQ9210          *
      * 1999-04-09 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-07-17 LMB  AUDIT FINDING - SURPLUS CONTROL TOTALS CHG3756 *
      * 2002-06-06 CRP  SHORTAGE CUTOFF MOVED REQ8544                  *
      * 2008-09-03 JLM  PERFORMANCE - INSURANCE READ AHEAD REQ4277     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESFEE.
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
                                           VALUE 13.000000.
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
      * WS-DISBURSE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISBURSE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000075500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000555500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000552000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000164000'.
       01  WS-DISBURSE-BAND-TBL REDEFINES WS-DISBURSE-BAND-VALUES.
           05  WS-DISBURSE-BAND-ROW OCCURS 4 TIMES.
               10  WS-DISBURSE-BAND-LOW    PIC 9(09)V99.
               10  WS-DISBURSE-BAND-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY ESFEEP.

       PROCEDURE DIVISION USING ESFEE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-CHECK-NEXT-REVIEW
           PERFORM 2010-EDIT-CONTACT
           PERFORM 2020-ACCUM-TYPE
           PERFORM 2030-SPLIT-INSURANCE
           PERFORM 2040-ACCRUE-DISBURSE
           PERFORM 2050-AGE-LAST-STMT
           PERFORM 2060-AGE-LAST-STMT
           PERFORM 2070-ASSESS-PREFERRED
           PERFORM 2080-LIMIT-SURPLUS
           PERFORM 2090-ACCRUE-DISBURSE
           PERFORM 2100-CHECK-CLOSE
           PERFORM 2110-DESCRIBE-REGION
           PERFORM 2120-ASSESS-OVERRIDE
           PERFORM 2130-FLAG-INSURANCE
           PERFORM 2140-PRORATE-CUSHION
           PERFORM 2150-ACCUM-TYPE
           PERFORM 2160-EDIT-CONTACT
           PERFORM 2170-SET-TYPE
           PERFORM 2180-ASSESS-OVERRIDE
           PERFORM 2190-DESCRIBE-ANALYSIS
           PERFORM 2200-FLAG-DISBURSE
           PERFORM 2210-SET-REASON
           PERFORM 2220-DESCRIBE-ANALYSIS
           PERFORM 2230-CHECK-NEXT-REVIEW
           PERFORM 2240-ASSESS-PREFERRED
           PERFORM 2250-SPLIT-TAX
           PERFORM 2260-SELECT-DISCOUNT
           PERFORM 2270-SET-REGION
           PERFORM 2280-SPLIT-SURPLUS
           PERFORM 2290-AGE-CLOSE
           PERFORM 2300-SET-TYPE
           PERFORM 2310-CHECK-CLOSE
           PERFORM 2320-EDIT-CONTACT
           PERFORM 2330-FLAG-SURPLUS
           PERFORM 2340-ACCUM-TYPE
           PERFORM 2350-SPLIT-CUSHION
           PERFORM 2360-CHECK-EFFECTIVE
           PERFORM 2370-SELECT-DISCOUNT
           PERFORM 2380-FLAG-SURPLUS
           PERFORM 2390-ACCRUE-CUSHION
           PERFORM 2400-DESCRIBE-ANALYSIS
           PERFORM 2410-EDIT-CONTACT
           PERFORM 2420-PRORATE-DISBURSE
           PERFORM 2430-PRORATE-INSURANCE
           PERFORM 2440-CHECK-EFFECTIVE
           PERFORM 2450-DESCRIBE-REGION
           PERFORM 2460-SPLIT-TAX
           PERFORM 2470-COUNT-EXCEPT
           PERFORM 2480-SELECT-DISCOUNT
           PERFORM 2490-LIMIT-TAX
           PERFORM 2500-AGE-EFFECTIVE
           PERFORM 2510-EDIT-CONTACT
           PERFORM 2520-SPLIT-DISBURSE
           PERFORM 2530-SET-TYPE
           PERFORM 2540-SPLIT-TAX
           PERFORM 2550-ASSESS-SUSPEND
           PERFORM 2560-DESCRIBE-REASON
           PERFORM 2570-PRORATE-INSURANCE
           PERFORM 2580-DESCRIBE-REASON
           PERFORM 2590-LIMIT-ITEM
           PERFORM 2600-EDIT-ADDR
           PERFORM 2610-DESCRIBE-REGION
           PERFORM 2620-COUNT-EXCEPT
           PERFORM 2630-ASSESS-SUSPEND
           PERFORM 2640-ACCUM-ANALYSIS
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO ESFEE-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2000-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ESFEE-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ESFEE-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E607 CONTACT REQUIRED
      * E740 WORK AMT NEGATIVE
      * E615 WORK AMT OVER 10000
      * E703 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2010-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESFEE-CONTACT = SPACES
                   MOVE 'E607' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E740' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E615' TO WS-REJECT-CODE
               WHEN ESFEE-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E703' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SURPLUS YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2020-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESFEE-TYPE-CD
                   ADD ESFEE-SURPLUS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESFEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESFEE-SURPLUS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INSURANCE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2030-SPLIT-INSURANCE.
           COMPUTE WS-WORK-AMT = ESFEE-INSURANCE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ESFEE-INSURANCE-AMT - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2040-ACCRUE-DISBURSE.
           IF ESFEE-DISBURSE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESFEE-DISBURSE-AMT * ESFEE-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ESFEE-INSURANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2050-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ESFEE-LAST-STMT-DATE > 0 AND ESFEE-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESFEE-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESFEE-TAX-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ESFEE-TAX-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ESFEE-TAX-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESFEE-TAX-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESFEE-TAX-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2060-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ESFEE-LAST-STMT-DATE > 0 AND ESFEE-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESFEE-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESFEE-DISBURSE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESFEE-DISBURSE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ESFEE-DISBURSE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESFEE-DISBURSE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESFEE-DISBURSE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2070-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ESFEE-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESFEE-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       ESFEE-DISCOUNT / 200
               WHEN ESFEE-TYPE-CD = 'AKX'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN ESFEE-TYPE-CD = 'REP'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       ESFEE-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SURPLUS YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2080-LIMIT-SURPLUS.
           IF ESFEE-SURPLUS-YTD < 50.00
               MOVE 50.00 TO ESFEE-SURPLUS-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESFEE-SURPLUS-YTD > 5000.00
               MOVE 5000.00 TO ESFEE-SURPLUS-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2090-ACCRUE-DISBURSE.
           IF ESFEE-DISBURSE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESFEE-DISBURSE-AMT * ESFEE-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ESFEE-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2100-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ESFEE-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESFEE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ESFEE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2110-DESCRIBE-REGION.
           EVALUATE ESFEE-REGION-CD
               WHEN 'B'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'G'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE TAX AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2120-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ESFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESFEE-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ESFEE-TAX-AMT *
                       ESFEE-DISCOUNT / 200
               WHEN ESFEE-TYPE-CD = 'AKX'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN ESFEE-TYPE-CD = 'REP'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ESFEE-TAX-AMT *
                       ESFEE-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * INSURANCE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2130-FLAG-INSURANCE.
           IF ESFEE-INSURANCE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESFEE-INSURANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SURPLUS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CUSHION YTD.
      *----------------------------------------------------------------*
       2140-PRORATE-CUSHION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESFEE-CUSHION-YTD ROUNDED =
                   ESFEE-SURPLUS-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESFEE-CUSHION-YTD
           END-IF
           ADD ESFEE-CUSHION-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE SURPLUS YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2150-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESFEE-TYPE-CD
                   ADD ESFEE-SURPLUS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESFEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESFEE-SURPLUS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E225 CONTACT REQUIRED
      * E701 WORK AMT NEGATIVE
      * E044 WORK AMT OVER 10000
      * E575 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2160-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESFEE-CONTACT = SPACES
                   MOVE 'E225' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E701' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E044' TO WS-REJECT-CODE
               WHEN ESFEE-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E575' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AKX -> REP WHEN EFFECTIVE DATE IS PAST
      * REP -> SMX WHEN EFFECTIVE DATE IS PAST
      * SMX -> VJL WHEN EFFECTIVE DATE IS PAST
      * VJL -> WKN WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2170-SET-TYPE.
           EVALUATE ESFEE-TYPE-CD
               WHEN 'AKX'
                   IF ESFEE-EFFECTIVE-DATE > 0 AND ESFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'REP' TO ESFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'REP'
                   IF ESFEE-EFFECTIVE-DATE > 0 AND ESFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'SMX' TO ESFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMX'
                   IF ESFEE-EFFECTIVE-DATE > 0 AND ESFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VJL' TO ESFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJL'
                   IF ESFEE-EFFECTIVE-DATE > 0 AND ESFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WKN' TO ESFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SURPLUS YTD FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2180-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ESFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ESFEE-SURPLUS-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESFEE-SUSPEND-FLAG = 'Y'
                   COMPUTE ESFEE-SURPLUS-YTD ROUNDED = WS-WORK-AMT *
                       ESFEE-FEE-PCT / 200
               WHEN ESFEE-REGION-CD = 'B'
                   MOVE 20.00 TO ESFEE-SURPLUS-YTD
               WHEN ESFEE-REGION-CD = 'G'
                   MOVE 20.00 TO ESFEE-SURPLUS-YTD
               WHEN OTHER
                   COMPUTE ESFEE-SURPLUS-YTD ROUNDED = WS-WORK-AMT *
                       ESFEE-FEE-PCT / 100
           END-EVALUATE
           IF ESFEE-SURPLUS-YTD > 0 AND ESFEE-SURPLUS-YTD < 10.00
               MOVE 10.00 TO ESFEE-SURPLUS-YTD
           END-IF
           IF ESFEE-SURPLUS-YTD > 25.00
               MOVE 25.00 TO ESFEE-SURPLUS-YTD
           END-IF
           ADD ESFEE-SURPLUS-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE ANALYSIS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2190-DESCRIBE-ANALYSIS.
           EVALUATE ESFEE-ANALYSIS-CD
               WHEN 'GD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DISBURSE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2200-FLAG-DISBURSE.
           IF ESFEE-DISBURSE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESFEE-DISBURSE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * C -> F WHEN LAST STMT DATE IS PAST
      * F -> K WHEN LAST STMT DATE IS PAST
      * K -> L WHEN LAST STMT DATE IS PAST
      * L -> S WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2210-SET-REASON.
           EVALUATE ESFEE-REASON-CD
               WHEN 'C'
                   IF ESFEE-LAST-STMT-DATE > 0 AND ESFEE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'F' TO ESFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF ESFEE-LAST-STMT-DATE > 0 AND ESFEE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'K' TO ESFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF ESFEE-LAST-STMT-DATE > 0 AND ESFEE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'L' TO ESFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF ESFEE-LAST-STMT-DATE > 0 AND ESFEE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'S' TO ESFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ANALYSIS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2220-DESCRIBE-ANALYSIS.
           EVALUATE ESFEE-ANALYSIS-CD
               WHEN 'GD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2230-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ESFEE-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESFEE-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ESFEE-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISBURSE FEE ON THE CUSHION YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2240-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ESFEE-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ESFEE-DISBURSE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESFEE-SUSPEND-FLAG = 'Y'
                   COMPUTE ESFEE-DISBURSE-AMT ROUNDED =
                       ESFEE-CUSHION-YTD * ESFEE-DISCOUNT / 200
               WHEN ESFEE-REASON-CD = 'C'
                   MOVE 7.00 TO ESFEE-DISBURSE-AMT
               WHEN ESFEE-REASON-CD = 'F'
                   MOVE 20.00 TO ESFEE-DISBURSE-AMT
               WHEN OTHER
                   COMPUTE ESFEE-DISBURSE-AMT ROUNDED =
                       ESFEE-CUSHION-YTD * ESFEE-DISCOUNT / 100
           END-EVALUATE
           IF ESFEE-DISBURSE-AMT > 0 AND ESFEE-DISBURSE-AMT < 10.00
               MOVE 10.00 TO ESFEE-DISBURSE-AMT
           END-IF
           IF ESFEE-DISBURSE-AMT > 100.00
               MOVE 100.00 TO ESFEE-DISBURSE-AMT
           END-IF
           ADD ESFEE-DISBURSE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT TAX AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2250-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = ESFEE-TAX-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ESFEE-TAX-AMT - (WS-WORK-AMT * 4)
           COMPUTE ESFEE-TAX-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE BAND TIER FOR THE CUSHION YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2260-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-BAND-RATE(1) TO ESFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ESFEE-CUSHION-YTD NOT < WS-DISBURSE-BAND-LOW(WS-TX)
                   MOVE WS-DISBURSE-BAND-RATE(WS-TX) TO ESFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * B -> G WHEN EFFECTIVE DATE IS PAST
      * G -> Y WHEN EFFECTIVE DATE IS PAST
      * Y -> B WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2270-SET-REGION.
           EVALUATE ESFEE-REGION-CD
               WHEN 'B'
                   IF ESFEE-EFFECTIVE-DATE > 0 AND ESFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'G' TO ESFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ESFEE-EFFECTIVE-DATE > 0 AND ESFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'Y' TO ESFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF ESFEE-EFFECTIVE-DATE > 0 AND ESFEE-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'B' TO ESFEE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SURPLUS YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2280-SPLIT-SURPLUS.
           COMPUTE WS-WORK-AMT = ESFEE-SURPLUS-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ESFEE-SURPLUS-YTD - (WS-WORK-AMT * 4)
           COMPUTE ESFEE-CUSHION-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2290-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ESFEE-CLOSE-DATE > 0 AND ESFEE-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESFEE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESFEE-CUSHION-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESFEE-CUSHION-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ESFEE-CUSHION-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESFEE-CUSHION-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESFEE-CUSHION-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AKX -> REP WHEN NEXT REVIEW DATE IS PAST
      * REP -> SMX WHEN NEXT REVIEW DATE IS PAST
      * SMX -> VJL WHEN NEXT REVIEW DATE IS PAST
      * VJL -> WKN WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2300-SET-TYPE.
           EVALUATE ESFEE-TYPE-CD
               WHEN 'AKX'
                   IF ESFEE-NEXT-REVIEW-DATE > 0 AND
                       ESFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'REP' TO ESFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'REP'
                   IF ESFEE-NEXT-REVIEW-DATE > 0 AND
                       ESFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'SMX' TO ESFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMX'
                   IF ESFEE-NEXT-REVIEW-DATE > 0 AND
                       ESFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VJL' TO ESFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJL'
                   IF ESFEE-NEXT-REVIEW-DATE > 0 AND
                       ESFEE-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'WKN' TO ESFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2310-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ESFEE-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESFEE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ESFEE-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E447 CONTACT REQUIRED
      * E964 WORK AMT 2 NEGATIVE
      * E178 WORK AMT 2 OVER 5000
      * E526 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2320-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESFEE-CONTACT = SPACES
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E964' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E178' TO WS-REJECT-CODE
               WHEN ESFEE-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E526' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SURPLUS YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2330-FLAG-SURPLUS.
           IF ESFEE-SURPLUS-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESFEE-SURPLUS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2340-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESFEE-TYPE-CD
                   ADD ESFEE-DISBURSE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESFEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESFEE-DISBURSE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CUSHION YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2350-SPLIT-CUSHION.
           COMPUTE WS-WORK-AMT = ESFEE-CUSHION-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ESFEE-CUSHION-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2360-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ESFEE-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESFEE-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ESFEE-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE BAND TIER FOR THE INSURANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2370-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-BAND-RATE(1) TO ESFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ESFEE-INSURANCE-AMT NOT < WS-DISBURSE-BAND-LOW(WS-TX)
                   MOVE WS-DISBURSE-BAND-RATE(WS-TX) TO ESFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SURPLUS YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2380-FLAG-SURPLUS.
           IF ESFEE-SURPLUS-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESFEE-SURPLUS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CUSHION YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2390-ACCRUE-CUSHION.
           IF ESFEE-CUSHION-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESFEE-CUSHION-YTD * ESFEE-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ANALYSIS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2400-DESCRIBE-ANALYSIS.
           EVALUATE ESFEE-ANALYSIS-CD
               WHEN 'GD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E173 CONTACT REQUIRED
      * E448 SURPLUS YTD NEGATIVE
      * E297 SURPLUS YTD OVER 99999
      * E858 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2410-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESFEE-CONTACT = SPACES
                   MOVE 'E173' TO WS-REJECT-CODE
               WHEN ESFEE-SURPLUS-YTD < 0
                   MOVE 'E448' TO WS-REJECT-CODE
               WHEN ESFEE-SURPLUS-YTD > 99999.00
                   MOVE 'E297' TO WS-REJECT-CODE
               WHEN ESFEE-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E858' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISBURSE AMT.
      *----------------------------------------------------------------*
       2420-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESFEE-DISBURSE-AMT ROUNDED =
                   ESFEE-TAX-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESFEE-DISBURSE-AMT
           END-IF
           ADD ESFEE-DISBURSE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE INSURANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2430-PRORATE-INSURANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ESFEE-INSURANCE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2440-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ESFEE-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESFEE-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO ESFEE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2450-DESCRIBE-REGION.
           EVALUATE ESFEE-REGION-CD
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'G'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT TAX YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2460-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = ESFEE-TAX-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ESFEE-TAX-YTD - (WS-WORK-AMT * 12)
           COMPUTE ESFEE-DISBURSE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2470-COUNT-EXCEPT.
           IF ESFEE-EXCEPT-CNT < 9999999
               ADD 1 TO ESFEE-EXCEPT-CNT
           END-IF
           IF ESFEE-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO ESFEE-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2480-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-BAND-RATE(1) TO ESFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-DISBURSE-BAND-LOW(WS-TX)
                   MOVE WS-DISBURSE-BAND-RATE(WS-TX) TO ESFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAX AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2490-LIMIT-TAX.
           IF ESFEE-TAX-AMT < 25.00
               MOVE 25.00 TO ESFEE-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESFEE-TAX-AMT > 5000.00
               MOVE 5000.00 TO ESFEE-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2500-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ESFEE-EFFECTIVE-DATE > 0 AND ESFEE-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESFEE-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESFEE-CUSHION-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESFEE-CUSHION-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESFEE-CUSHION-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ESFEE-CUSHION-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESFEE-CUSHION-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E793 CONTACT REQUIRED
      * E830 CUSHION YTD NEGATIVE
      * E834 CUSHION YTD OVER 1000000
      * E872 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2510-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESFEE-CONTACT = SPACES
                   MOVE 'E793' TO WS-REJECT-CODE
               WHEN ESFEE-CUSHION-YTD < 0
                   MOVE 'E830' TO WS-REJECT-CODE
               WHEN ESFEE-CUSHION-YTD > 1000000.00
                   MOVE 'E834' TO WS-REJECT-CODE
               WHEN ESFEE-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E872' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2520-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = ESFEE-DISBURSE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ESFEE-DISBURSE-AMT - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AKX -> REP WHEN LAST STMT DATE IS PAST
      * REP -> SMX WHEN LAST STMT DATE IS PAST
      * SMX -> VJL WHEN LAST STMT DATE IS PAST
      * VJL -> WKN WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2530-SET-TYPE.
           EVALUATE ESFEE-TYPE-CD
               WHEN 'AKX'
                   IF ESFEE-LAST-STMT-DATE > 0 AND ESFEE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'REP' TO ESFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'REP'
                   IF ESFEE-LAST-STMT-DATE > 0 AND ESFEE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'SMX' TO ESFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMX'
                   IF ESFEE-LAST-STMT-DATE > 0 AND ESFEE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VJL' TO ESFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJL'
                   IF ESFEE-LAST-STMT-DATE > 0 AND ESFEE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WKN' TO ESFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2540-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE ESFEE-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURPLUS YTD FEE ON THE TAX AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2550-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ESFEE-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ESFEE-SURPLUS-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESFEE-PREFERRED-FLAG = 'Y'
                   COMPUTE ESFEE-SURPLUS-YTD ROUNDED = ESFEE-TAX-AMT *
                       ESFEE-FEE-PCT / 200
               WHEN ESFEE-REGION-CD = 'B'
                   MOVE 3.00 TO ESFEE-SURPLUS-YTD
               WHEN ESFEE-REGION-CD = 'G'
                   MOVE 5.00 TO ESFEE-SURPLUS-YTD
               WHEN OTHER
                   COMPUTE ESFEE-SURPLUS-YTD ROUNDED = ESFEE-TAX-AMT *
                       ESFEE-FEE-PCT / 100
           END-EVALUATE
           IF ESFEE-SURPLUS-YTD > 0 AND ESFEE-SURPLUS-YTD < 5.00
               MOVE 5.00 TO ESFEE-SURPLUS-YTD
           END-IF
           IF ESFEE-SURPLUS-YTD > 75.00
               MOVE 75.00 TO ESFEE-SURPLUS-YTD
           END-IF
           ADD ESFEE-SURPLUS-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2560-DESCRIBE-REASON.
           EVALUATE ESFEE-REASON-CD
               WHEN 'C'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'X'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INSURANCE AMT.
      *----------------------------------------------------------------*
       2570-PRORATE-INSURANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESFEE-INSURANCE-AMT ROUNDED =
                   ESFEE-TAX-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESFEE-INSURANCE-AMT
           END-IF
           ADD ESFEE-INSURANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2580-DESCRIBE-REASON.
           EVALUATE ESFEE-REASON-CD
               WHEN 'C'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'K'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'X'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2590-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E482 ADDR REQUIRED
      * E573 TAX YTD NEGATIVE
      * E910 TAX YTD OVER 5000
      * E745 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2600-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESFEE-ADDR = SPACES
                   MOVE 'E482' TO WS-REJECT-CODE
               WHEN ESFEE-TAX-YTD < 0
                   MOVE 'E573' TO WS-REJECT-CODE
               WHEN ESFEE-TAX-YTD > 5000.00
                   MOVE 'E910' TO WS-REJECT-CODE
               WHEN ESFEE-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E745' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2610-DESCRIBE-REGION.
           EVALUATE ESFEE-REGION-CD
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'G'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2620-COUNT-EXCEPT.
           IF ESFEE-EXCEPT-CNT < 9999999
               ADD 1 TO ESFEE-EXCEPT-CNT
           END-IF
           IF ESFEE-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO ESFEE-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CUSHION YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2630-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ESFEE-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESFEE-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = ESFEE-CUSHION-YTD *
                       ESFEE-FEE-PCT / 200
               WHEN ESFEE-ANALYSIS-CD = 'GD'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN ESFEE-ANALYSIS-CD = 'LY'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = ESFEE-CUSHION-YTD *
                       ESFEE-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY ANALYSIS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2640-ACCUM-ANALYSIS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESFEE-ANALYSIS-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESFEE-ANALYSIS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO ESFEE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO ESFEE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO ESFEE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'ESFEE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
