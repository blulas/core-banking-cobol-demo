      *================================================================*
      * PROGRAM  : SLLIM                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : STUDENT LOANS RULE - DATE RULES. CALLED BY THE      *
      *            STUDENT LOANS BATCH AND ONLINE PROGRAMS.            *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1987-01-19 LMB  ORIGINAL                                       *
      * 1989-03-04 TWH  PAYMENT ROUNDING - HALF UP CHG7501             *
      * 1989-11-05 PQR  AUDIT FINDING - DISBURSE CONTROL TOTALS PRB6213*
      * 1992-06-07 TWH  SUBSIDY ROUNDING - HALF UP REQ3738             *
      * 1993-10-05 BHS  PERFORMANCE - PRINCIPAL READ AHEAD CHG8504     *
      * 1994-05-21 BHS  PRINCIPAL EXCEPTIONS TO OPS QUEUE CHG6167      *
      * 2000-06-18 CRP  DISBURSE RERUN PROTECTION INC9775              *
      * 2001-02-16 DPS  PERFORMANCE - PAYMENT READ AHEAD INC5992       *
      * 2001-03-08 GAF  NEW PRINCIPAL REPORT COLUMNS INC0579           *
      * 2003-08-23 SKN  PERFORMANCE - DISBURSE READ AHEAD PRB6650      *
      * 2004-10-19 TWH  PRINCIPAL TABLE EXPANDED REQ1749               *
      * 2004-07-12 CRP  SUBSIDY NOTICE TEXT PER LEGAL REQ5955          *
      * 2005-03-24 GAF  DEFERINT FEE SCHEDULE UPDATE PRB4523           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SLLIM.
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
                                           VALUE 21.000000.
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
      * WS-SUBSIDY-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SUBSIDY-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000377500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000343500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000513500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000186500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000685000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000101000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006150000000205500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016150000000479500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016650000000728000'.
       01  WS-SUBSIDY-TIER-TBL REDEFINES WS-SUBSIDY-TIER-VALUES.
           05  WS-SUBSIDY-TIER-ROW OCCURS 9 TIMES.
               10  WS-SUBSIDY-TIER-LOW     PIC 9(09)V99.
               10  WS-SUBSIDY-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DISBURSE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISBURSE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000031000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000289000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000125000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000601500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026000000000566000'.
       01  WS-DISBURSE-BAND-TBL REDEFINES WS-DISBURSE-BAND-VALUES.
           05  WS-DISBURSE-BAND-ROW OCCURS 5 TIMES.
               10  WS-DISBURSE-BAND-LOW    PIC 9(09)V99.
               10  WS-DISBURSE-BAND-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY SLLIMP.

       PROCEDURE DIVISION USING SLLIM-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SET-SOURCE
           PERFORM 2010-EDIT-CONTACT
           PERFORM 2020-PRORATE-ITEM
           PERFORM 2030-PRORATE-PRINCIPAL
           PERFORM 2040-ACCRUE-PAYMENT
           PERFORM 2050-SPLIT-DISBURSE
           PERFORM 2060-AGE-CLOSE
           PERFORM 2070-DESCRIBE-LOAN-TYPE
           PERFORM 2080-FLAG-PAYMENT
           PERFORM 2090-ASSESS-REVIEW
           PERFORM 2100-FLAG-PRINCIPAL
           PERFORM 2110-LIMIT-SUBSIDY
           PERFORM 2120-COUNT-PAYMENT
           PERFORM 2130-LIMIT-SUBSIDY
           PERFORM 2140-EDIT-NAME
           PERFORM 2150-LIMIT-ITEM
           PERFORM 2160-SET-REGION
           PERFORM 2170-SPLIT-PRINCIPAL
           PERFORM 2180-ACCUM-LOAN-TYPE
           PERFORM 2190-LIMIT-DISBURSE
           PERFORM 2200-EDIT-NAME
           PERFORM 2210-PRORATE-PAYMENT
           PERFORM 2220-SET-SOURCE
           PERFORM 2230-EDIT-CONTACT
           PERFORM 2240-ASSESS-REVIEW
           PERFORM 2250-ACCUM-SOURCE
           PERFORM 2260-ACCRUE-PAYMENT
           PERFORM 2270-LIMIT-ITEM
           PERFORM 2280-PRORATE-PRINCIPAL
           PERFORM 2290-CHECK-LAST-STMT
           PERFORM 2300-SET-CYCLE
           PERFORM 2310-ASSESS-REVIEW
           PERFORM 2320-COUNT-ITEM
           PERFORM 2330-SELECT-FEE-PCT
           PERFORM 2340-SELECT-FEE-PCT
           PERFORM 2350-ASSESS-NOTICE-SENT
           PERFORM 2360-EDIT-CONTACT
           PERFORM 2370-ACCRUE-DISBURSE
           PERFORM 2380-ASSESS-NOTICE-SENT
           PERFORM 2390-ASSESS-NOTICE-SENT
           PERFORM 2400-ACCUM-REGION
           PERFORM 2410-SPLIT-DISBURSE
           PERFORM 2420-ASSESS-REVIEW
           PERFORM 2430-ACCRUE-DISBURSE
           PERFORM 2440-ACCUM-REGION
           PERFORM 2450-CHECK-EXPIRY
           PERFORM 2460-CHECK-LAST-STMT
           PERFORM 2470-ASSESS-REVIEW
           PERFORM 2480-LIMIT-PAYMENT
           PERFORM 2490-DESCRIBE-LOAN-TYPE
           PERFORM 2500-SET-LOAN-TYPE
           PERFORM 2510-ACCRUE-DISCOUNT
           PERFORM 2520-ASSESS-NOTICE-SENT
           PERFORM 2530-SPLIT-SUBSIDY
           PERFORM 2540-AGE-LAST-STMT
           PERFORM 2550-SPLIT-DISBURSE
           PERFORM 2560-CHECK-EXPIRY
           PERFORM 2570-PRORATE-SUBSIDY
           PERFORM 2580-DESCRIBE-REGION
           PERFORM 2590-FLAG-PRINCIPAL
           PERFORM 2600-ACCUM-REGION
           PERFORM 2610-PRORATE-PRINCIPAL
           PERFORM 2620-AGE-CLOSE
           PERFORM 2630-SPLIT-PRINCIPAL
           PERFORM 2640-SELECT-DISCOUNT
           PERFORM 2650-ASSESS-NOTICE-SENT
           PERFORM 2660-COUNT-ITEM
           PERFORM 2670-FLAG-PRINCIPAL
           PERFORM 2680-ACCUM-CYCLE
           PERFORM 2690-FLAG-PRINCIPAL
           PERFORM 2700-SPLIT-PAYMENT
           PERFORM 2710-LIMIT-SUBSIDY
           PERFORM 2720-ACCRUE-SUBSIDY
           PERFORM 2730-ACCRUE-PRINCIPAL
           PERFORM 2740-CHECK-CLOSE
           PERFORM 2750-AGE-EXPIRY
           PERFORM 2760-LIMIT-PRINCIPAL
           PERFORM 2770-ASSESS-NOTICE-SENT
           PERFORM 2780-SELECT-DISCOUNT
           PERFORM 2790-DESCRIBE-LOAN-TYPE
           PERFORM 2800-ASSESS-NOTICE-SENT
           PERFORM 2810-AGE-MATURITY
           PERFORM 2820-PRORATE-PRINCIPAL
           PERFORM 2830-PRORATE-DISBURSE
           PERFORM 2840-AGE-MATURITY
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO SLLIM-RETURN-CODE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AST -> BCK WHEN LAST STMT DATE IS PAST
      * BCK -> CJG WHEN LAST STMT DATE IS PAST
      * CJG -> DSJ WHEN LAST STMT DATE IS PAST
      * DSJ -> JVY WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2000-SET-SOURCE.
           EVALUATE SLLIM-SOURCE-CD
               WHEN 'AST'
                   IF SLLIM-LAST-STMT-DATE > 0 AND SLLIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BCK' TO SLLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BCK'
                   IF SLLIM-LAST-STMT-DATE > 0 AND SLLIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'CJG' TO SLLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJG'
                   IF SLLIM-LAST-STMT-DATE > 0 AND SLLIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DSJ' TO SLLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DSJ'
                   IF SLLIM-LAST-STMT-DATE > 0 AND SLLIM-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JVY' TO SLLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E382 CONTACT REQUIRED
      * E173 PAYMENT YTD NEGATIVE
      * E525 PAYMENT YTD OVER 50000
      * E054 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2010-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLLIM-CONTACT = SPACES
                   MOVE 'E382' TO WS-REJECT-CODE
               WHEN SLLIM-PAYMENT-YTD < 0
                   MOVE 'E173' TO WS-REJECT-CODE
               WHEN SLLIM-PAYMENT-YTD > 50000.00
                   MOVE 'E525' TO WS-REJECT-CODE
               WHEN SLLIM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E054' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2020-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       2030-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SLLIM-PRINCIPAL-AMT ROUNDED =
                   SLLIM-PAYMENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SLLIM-PRINCIPAL-AMT
           END-IF
           ADD SLLIM-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2040-ACCRUE-PAYMENT.
           IF SLLIM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SLLIM-PAYMENT-YTD * SLLIM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2050-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE SLLIM-DISBURSE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2060-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF SLLIM-CLOSE-DATE > 0 AND SLLIM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLLIM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SLLIM-PRINCIPAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SLLIM-PRINCIPAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD SLLIM-PRINCIPAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD SLLIM-PRINCIPAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SLLIM-PRINCIPAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE LOAN TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2070-DESCRIBE-LOAN-TYPE.
           EVALUATE SLLIM-LOAN-TYPE-CD
               WHEN 'CS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2080-FLAG-PAYMENT.
           IF SLLIM-PAYMENT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SLLIM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE SUBSIDY YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2090-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SLLIM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SLLIM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLLIM-HOLD-FLAG = 'Y'
                   COMPUTE SLLIM-PRINCIPAL-YTD ROUNDED =
                       SLLIM-SUBSIDY-YTD * SLLIM-FEE-PCT / 200
               WHEN SLLIM-LOAN-TYPE-CD = 'CS'
                   MOVE 5.00 TO SLLIM-PRINCIPAL-YTD
               WHEN SLLIM-LOAN-TYPE-CD = 'DV'
                   MOVE 12.00 TO SLLIM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE SLLIM-PRINCIPAL-YTD ROUNDED =
                       SLLIM-SUBSIDY-YTD * SLLIM-FEE-PCT / 100
           END-EVALUATE
           IF SLLIM-PRINCIPAL-YTD > 0 AND SLLIM-PRINCIPAL-YTD < 1.00
               MOVE 1.00 TO SLLIM-PRINCIPAL-YTD
           END-IF
           IF SLLIM-PRINCIPAL-YTD > 25.00
               MOVE 25.00 TO SLLIM-PRINCIPAL-YTD
           END-IF
           ADD SLLIM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2100-FLAG-PRINCIPAL.
           IF SLLIM-PRINCIPAL-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SLLIM-PRINCIPAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SUBSIDY YTD IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2110-LIMIT-SUBSIDY.
           IF SLLIM-SUBSIDY-YTD < 10.00
               MOVE 10.00 TO SLLIM-SUBSIDY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SLLIM-SUBSIDY-YTD > 500.00
               MOVE 500.00 TO SLLIM-SUBSIDY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2120-COUNT-PAYMENT.
           IF SLLIM-PAYMENT-CNT < 9999999
               ADD 1 TO SLLIM-PAYMENT-CNT
           END-IF
           IF SLLIM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO SLLIM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SUBSIDY YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2130-LIMIT-SUBSIDY.
           IF SLLIM-SUBSIDY-YTD < 100.00
               MOVE 100.00 TO SLLIM-SUBSIDY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SLLIM-SUBSIDY-YTD > 1000.00
               MOVE 1000.00 TO SLLIM-SUBSIDY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E888 NAME REQUIRED
      * E988 PAYMENT YTD NEGATIVE
      * E382 PAYMENT YTD OVER 5000
      * E151 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2140-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLLIM-NAME = SPACES
                   MOVE 'E888' TO WS-REJECT-CODE
               WHEN SLLIM-PAYMENT-YTD < 0
                   MOVE 'E988' TO WS-REJECT-CODE
               WHEN SLLIM-PAYMENT-YTD > 5000.00
                   MOVE 'E382' TO WS-REJECT-CODE
               WHEN SLLIM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E151' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2150-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> D WHEN EXPIRY DATE IS PAST
      * D -> T WHEN EXPIRY DATE IS PAST
      * T -> U WHEN EXPIRY DATE IS PAST
      * U -> W WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2160-SET-REGION.
           EVALUATE SLLIM-REGION-CD
               WHEN 'C'
                   IF SLLIM-EXPIRY-DATE > 0 AND SLLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO SLLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF SLLIM-EXPIRY-DATE > 0 AND SLLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO SLLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF SLLIM-EXPIRY-DATE > 0 AND SLLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO SLLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF SLLIM-EXPIRY-DATE > 0 AND SLLIM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO SLLIM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2170-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE SLLIM-PRINCIPAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE YTD BY LOAN TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2180-ACCUM-LOAN-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLLIM-LOAN-TYPE-CD
                   ADD SLLIM-DISBURSE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLLIM-LOAN-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SLLIM-DISBURSE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2190-LIMIT-DISBURSE.
           IF SLLIM-DISBURSE-YTD < 25.00
               MOVE 25.00 TO SLLIM-DISBURSE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SLLIM-DISBURSE-YTD > 1000.00
               MOVE 1000.00 TO SLLIM-DISBURSE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E846 NAME REQUIRED
      * E723 WORK AMT NEGATIVE
      * E492 WORK AMT OVER 250000
      * E068 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2200-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLLIM-NAME = SPACES
                   MOVE 'E846' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E723' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E492' TO WS-REJECT-CODE
               WHEN SLLIM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E068' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2210-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   SLLIM-PAYMENT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AST -> BCK WHEN MATURITY DATE IS PAST
      * BCK -> CJG WHEN MATURITY DATE IS PAST
      * CJG -> DSJ WHEN MATURITY DATE IS PAST
      * DSJ -> JVY WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2220-SET-SOURCE.
           EVALUATE SLLIM-SOURCE-CD
               WHEN 'AST'
                   IF SLLIM-MATURITY-DATE > 0 AND SLLIM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BCK' TO SLLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BCK'
                   IF SLLIM-MATURITY-DATE > 0 AND SLLIM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CJG' TO SLLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJG'
                   IF SLLIM-MATURITY-DATE > 0 AND SLLIM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DSJ' TO SLLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DSJ'
                   IF SLLIM-MATURITY-DATE > 0 AND SLLIM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JVY' TO SLLIM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E198 CONTACT REQUIRED
      * E742 SUBSIDY YTD NEGATIVE
      * E420 SUBSIDY YTD OVER 99999
      * E616 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2230-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLLIM-CONTACT = SPACES
                   MOVE 'E198' TO WS-REJECT-CODE
               WHEN SLLIM-SUBSIDY-YTD < 0
                   MOVE 'E742' TO WS-REJECT-CODE
               WHEN SLLIM-SUBSIDY-YTD > 99999.00
                   MOVE 'E420' TO WS-REJECT-CODE
               WHEN SLLIM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E616' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE SUBSIDY YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2240-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SLLIM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLLIM-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SLLIM-SUBSIDY-YTD *
                       SLLIM-DISCOUNT / 200
               WHEN SLLIM-SOURCE-CD = 'AST'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN SLLIM-SOURCE-CD = 'BCK'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SLLIM-SUBSIDY-YTD *
                       SLLIM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2250-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLLIM-SOURCE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLLIM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2260-ACCRUE-PAYMENT.
           IF SLLIM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SLLIM-PAYMENT-YTD * SLLIM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SLLIM-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2270-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       2280-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SLLIM-PRINCIPAL-AMT ROUNDED =
                   SLLIM-DISBURSE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SLLIM-PRINCIPAL-AMT
           END-IF
           ADD SLLIM-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2290-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF SLLIM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLLIM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO SLLIM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CFJ -> KHC WHEN MATURITY DATE IS PAST
      * KHC -> SEU WHEN MATURITY DATE IS PAST
      * SEU -> XKA WHEN MATURITY DATE IS PAST
      * XKA -> CFJ WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2300-SET-CYCLE.
           EVALUATE SLLIM-CYCLE-CD
               WHEN 'CFJ'
                   IF SLLIM-MATURITY-DATE > 0 AND SLLIM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHC' TO SLLIM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KHC'
                   IF SLLIM-MATURITY-DATE > 0 AND SLLIM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SEU' TO SLLIM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SEU'
                   IF SLLIM-MATURITY-DATE > 0 AND SLLIM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XKA' TO SLLIM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XKA'
                   IF SLLIM-MATURITY-DATE > 0 AND SLLIM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CFJ' TO SLLIM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2310-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SLLIM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SLLIM-CAPINT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLLIM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE SLLIM-CAPINT-AMT ROUNDED =
                       SLLIM-PRINCIPAL-AMT * SLLIM-DISCOUNT / 200
               WHEN SLLIM-LOAN-TYPE-CD = 'CS'
                   MOVE 7.00 TO SLLIM-CAPINT-AMT
               WHEN SLLIM-LOAN-TYPE-CD = 'DV'
                   MOVE 5.00 TO SLLIM-CAPINT-AMT
               WHEN OTHER
                   COMPUTE SLLIM-CAPINT-AMT ROUNDED =
                       SLLIM-PRINCIPAL-AMT * SLLIM-DISCOUNT / 100
           END-EVALUATE
           IF SLLIM-CAPINT-AMT > 0 AND SLLIM-CAPINT-AMT < 2.00
               MOVE 2.00 TO SLLIM-CAPINT-AMT
           END-IF
           IF SLLIM-CAPINT-AMT > 25.00
               MOVE 25.00 TO SLLIM-CAPINT-AMT
           END-IF
           ADD SLLIM-CAPINT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2320-COUNT-ITEM.
           IF SLLIM-ITEM-CNT < 9999999
               ADD 1 TO SLLIM-ITEM-CNT
           END-IF
           IF SLLIM-ITEM-CNT NOT < 5
               MOVE 'Y' TO SLLIM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SUBSIDY TIER TIER FOR THE CAPINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2330-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SUBSIDY-TIER-RATE(1) TO SLLIM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SLLIM-CAPINT-AMT NOT < WS-SUBSIDY-TIER-LOW(WS-TX)
                   MOVE WS-SUBSIDY-TIER-RATE(WS-TX) TO SLLIM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SLLIM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SLLIM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SUBSIDY TIER TIER FOR THE SUBSIDY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2340-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SUBSIDY-TIER-RATE(1) TO SLLIM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SLLIM-SUBSIDY-YTD NOT < WS-SUBSIDY-TIER-LOW(WS-TX)
                   MOVE WS-SUBSIDY-TIER-RATE(WS-TX) TO SLLIM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SLLIM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SLLIM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE DISBURSE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2350-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SLLIM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO SLLIM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLLIM-HOLD-FLAG = 'Y'
                   COMPUTE SLLIM-PAYMENT-YTD ROUNDED =
                       SLLIM-DISBURSE-YTD * SLLIM-DISCOUNT / 200
               WHEN SLLIM-LOAN-TYPE-CD = 'CS'
                   MOVE 12.00 TO SLLIM-PAYMENT-YTD
               WHEN SLLIM-LOAN-TYPE-CD = 'DV'
                   MOVE 5.00 TO SLLIM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE SLLIM-PAYMENT-YTD ROUNDED =
                       SLLIM-DISBURSE-YTD * SLLIM-DISCOUNT / 100
           END-EVALUATE
           IF SLLIM-PAYMENT-YTD > 0 AND SLLIM-PAYMENT-YTD < 2.00
               MOVE 2.00 TO SLLIM-PAYMENT-YTD
           END-IF
           IF SLLIM-PAYMENT-YTD > 250.00
               MOVE 250.00 TO SLLIM-PAYMENT-YTD
           END-IF
           ADD SLLIM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E633 CONTACT REQUIRED
      * E337 DISBURSE YTD NEGATIVE
      * E601 DISBURSE YTD OVER 10000
      * E565 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2360-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLLIM-CONTACT = SPACES
                   MOVE 'E633' TO WS-REJECT-CODE
               WHEN SLLIM-DISBURSE-YTD < 0
                   MOVE 'E337' TO WS-REJECT-CODE
               WHEN SLLIM-DISBURSE-YTD > 10000.00
                   MOVE 'E601' TO WS-REJECT-CODE
               WHEN SLLIM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E565' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2370-ACCRUE-DISBURSE.
           IF SLLIM-DISBURSE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SLLIM-DISBURSE-YTD * SLLIM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE PAYMENT YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2380-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SLLIM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO SLLIM-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLLIM-HOLD-FLAG = 'Y'
                   COMPUTE SLLIM-PRINCIPAL-AMT ROUNDED =
                       SLLIM-PAYMENT-YTD * SLLIM-FEE-PCT / 200
               WHEN SLLIM-CYCLE-CD = 'CFJ'
                   MOVE 10.00 TO SLLIM-PRINCIPAL-AMT
               WHEN SLLIM-CYCLE-CD = 'KHC'
                   MOVE 15.00 TO SLLIM-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE SLLIM-PRINCIPAL-AMT ROUNDED =
                       SLLIM-PAYMENT-YTD * SLLIM-FEE-PCT / 100
           END-EVALUATE
           IF SLLIM-PRINCIPAL-AMT > 0 AND SLLIM-PRINCIPAL-AMT < 10.00
               MOVE 10.00 TO SLLIM-PRINCIPAL-AMT
           END-IF
           IF SLLIM-PRINCIPAL-AMT > 250.00
               MOVE 250.00 TO SLLIM-PRINCIPAL-AMT
           END-IF
           ADD SLLIM-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE PAYMENT YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2390-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SLLIM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO SLLIM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLLIM-HOLD-FLAG = 'Y'
                   COMPUTE SLLIM-PRINCIPAL-YTD ROUNDED =
                       SLLIM-PAYMENT-YTD * SLLIM-DISCOUNT / 200
               WHEN SLLIM-CYCLE-CD = 'CFJ'
                   MOVE 5.00 TO SLLIM-PRINCIPAL-YTD
               WHEN SLLIM-CYCLE-CD = 'KHC'
                   MOVE 7.00 TO SLLIM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE SLLIM-PRINCIPAL-YTD ROUNDED =
                       SLLIM-PAYMENT-YTD * SLLIM-DISCOUNT / 100
           END-EVALUATE
           IF SLLIM-PRINCIPAL-YTD > 0 AND SLLIM-PRINCIPAL-YTD < 10.00
               MOVE 10.00 TO SLLIM-PRINCIPAL-YTD
           END-IF
           IF SLLIM-PRINCIPAL-YTD > 150.00
               MOVE 150.00 TO SLLIM-PRINCIPAL-YTD
           END-IF
           ADD SLLIM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE SUBSIDY YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2400-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLLIM-REGION-CD
                   ADD SLLIM-SUBSIDY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLLIM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SLLIM-SUBSIDY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2410-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE SLLIM-DISBURSE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE SUBSIDY YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2420-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SLLIM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLLIM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = SLLIM-SUBSIDY-YTD *
                       SLLIM-FEE-PCT / 200
               WHEN SLLIM-REGION-CD = 'C'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN SLLIM-REGION-CD = 'D'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = SLLIM-SUBSIDY-YTD *
                       SLLIM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2430-ACCRUE-DISBURSE.
           IF SLLIM-DISBURSE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SLLIM-DISBURSE-YTD * SLLIM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SLLIM-PAYMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2440-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLLIM-REGION-CD
                   ADD SLLIM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLLIM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SLLIM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2450-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF SLLIM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLLIM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO SLLIM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2460-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF SLLIM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLLIM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO SLLIM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2470-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SLLIM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SLLIM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLLIM-HOLD-FLAG = 'Y'
                   COMPUTE SLLIM-PRINCIPAL-YTD ROUNDED = WS-WORK-AMT-2 *
                       SLLIM-DISCOUNT / 200
               WHEN SLLIM-LOAN-TYPE-CD = 'CS'
                   MOVE 15.00 TO SLLIM-PRINCIPAL-YTD
               WHEN SLLIM-LOAN-TYPE-CD = 'DV'
                   MOVE 20.00 TO SLLIM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE SLLIM-PRINCIPAL-YTD ROUNDED = WS-WORK-AMT-2 *
                       SLLIM-DISCOUNT / 100
           END-EVALUATE
           IF SLLIM-PRINCIPAL-YTD > 0 AND SLLIM-PRINCIPAL-YTD < 1.00
               MOVE 1.00 TO SLLIM-PRINCIPAL-YTD
           END-IF
           IF SLLIM-PRINCIPAL-YTD > 250.00
               MOVE 250.00 TO SLLIM-PRINCIPAL-YTD
           END-IF
           ADD SLLIM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2480-LIMIT-PAYMENT.
           IF SLLIM-PAYMENT-YTD < 25.00
               MOVE 25.00 TO SLLIM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SLLIM-PAYMENT-YTD > 1000.00
               MOVE 1000.00 TO SLLIM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LOAN TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2490-DESCRIBE-LOAN-TYPE.
           EVALUATE SLLIM-LOAN-TYPE-CD
               WHEN 'CS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOAN TYPE CD:
      * CS -> DV WHEN CLOSE DATE IS PAST
      * DV -> JL WHEN CLOSE DATE IS PAST
      * JL -> NE WHEN CLOSE DATE IS PAST
      * NE -> SH WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2500-SET-LOAN-TYPE.
           EVALUATE SLLIM-LOAN-TYPE-CD
               WHEN 'CS'
                   IF SLLIM-CLOSE-DATE > 0 AND SLLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DV' TO SLLIM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DV'
                   IF SLLIM-CLOSE-DATE > 0 AND SLLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO SLLIM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JL'
                   IF SLLIM-CLOSE-DATE > 0 AND SLLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NE' TO SLLIM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NE'
                   IF SLLIM-CLOSE-DATE > 0 AND SLLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO SLLIM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2510-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * SLLIM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO SLLIM-PAYMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2520-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SLLIM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLLIM-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = SLLIM-PRINCIPAL-AMT *
                       SLLIM-DISCOUNT / 200
               WHEN SLLIM-REGION-CD = 'C'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN SLLIM-REGION-CD = 'D'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = SLLIM-PRINCIPAL-AMT *
                       SLLIM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2530-SPLIT-SUBSIDY.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE SLLIM-SUBSIDY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2540-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF SLLIM-LAST-STMT-DATE > 0 AND SLLIM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLLIM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SLLIM-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SLLIM-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SLLIM-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SLLIM-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SLLIM-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2550-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = SLLIM-DISBURSE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SLLIM-DISBURSE-YTD - (WS-WORK-AMT *
               4)
           COMPUTE SLLIM-CAPINT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2560-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF SLLIM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLLIM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO SLLIM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SUBSIDY YTD.
      *----------------------------------------------------------------*
       2570-PRORATE-SUBSIDY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SLLIM-SUBSIDY-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SLLIM-SUBSIDY-YTD
           END-IF
           ADD SLLIM-SUBSIDY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2580-DESCRIBE-REGION.
           EVALUATE SLLIM-REGION-CD
               WHEN 'C'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'D'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'W'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2590-FLAG-PRINCIPAL.
           IF SLLIM-PRINCIPAL-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SLLIM-PRINCIPAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2600-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLLIM-REGION-CD
                   ADD SLLIM-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLLIM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SLLIM-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2610-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SLLIM-PRINCIPAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2620-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF SLLIM-CLOSE-DATE > 0 AND SLLIM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLLIM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SLLIM-SUBSIDY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SLLIM-SUBSIDY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD SLLIM-SUBSIDY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD SLLIM-SUBSIDY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SLLIM-SUBSIDY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2630-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = SLLIM-PRINCIPAL-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SLLIM-PRINCIPAL-YTD - (WS-WORK-AMT *
               4)
           COMPUTE SLLIM-DISBURSE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE BAND TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2640-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-BAND-RATE(1) TO SLLIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SLLIM-PRINCIPAL-AMT NOT < WS-DISBURSE-BAND-LOW(WS-TX)
                   MOVE WS-DISBURSE-BAND-RATE(WS-TX) TO SLLIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SLLIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SLLIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE CAPINT AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2650-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SLLIM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO SLLIM-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLLIM-HOLD-FLAG = 'Y'
                   COMPUTE SLLIM-PRINCIPAL-AMT ROUNDED =
                       SLLIM-CAPINT-AMT * SLLIM-DISCOUNT / 200
               WHEN SLLIM-CYCLE-CD = 'CFJ'
                   MOVE 12.00 TO SLLIM-PRINCIPAL-AMT
               WHEN SLLIM-CYCLE-CD = 'KHC'
                   MOVE 3.00 TO SLLIM-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE SLLIM-PRINCIPAL-AMT ROUNDED =
                       SLLIM-CAPINT-AMT * SLLIM-DISCOUNT / 100
           END-EVALUATE
           IF SLLIM-PRINCIPAL-AMT > 0 AND SLLIM-PRINCIPAL-AMT < 5.00
               MOVE 5.00 TO SLLIM-PRINCIPAL-AMT
           END-IF
           IF SLLIM-PRINCIPAL-AMT > 50.00
               MOVE 50.00 TO SLLIM-PRINCIPAL-AMT
           END-IF
           ADD SLLIM-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2660-COUNT-ITEM.
           IF SLLIM-ITEM-CNT < 9999999
               ADD 1 TO SLLIM-ITEM-CNT
           END-IF
           IF SLLIM-ITEM-CNT NOT < 10
               MOVE 'Y' TO SLLIM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2670-FLAG-PRINCIPAL.
           IF SLLIM-PRINCIPAL-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SLLIM-PRINCIPAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2680-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLLIM-CYCLE-CD
                   ADD SLLIM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLLIM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SLLIM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2690-FLAG-PRINCIPAL.
           IF SLLIM-PRINCIPAL-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SLLIM-PRINCIPAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2700-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = SLLIM-PAYMENT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = SLLIM-PAYMENT-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SUBSIDY YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2710-LIMIT-SUBSIDY.
           IF SLLIM-SUBSIDY-YTD < 0.00
               MOVE 0.00 TO SLLIM-SUBSIDY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SLLIM-SUBSIDY-YTD > 5000.00
               MOVE 5000.00 TO SLLIM-SUBSIDY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SUBSIDY YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2720-ACCRUE-SUBSIDY.
           IF SLLIM-SUBSIDY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SLLIM-SUBSIDY-YTD * SLLIM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SLLIM-DISBURSE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2730-ACCRUE-PRINCIPAL.
           IF SLLIM-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SLLIM-PRINCIPAL-AMT * SLLIM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2740-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF SLLIM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLLIM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO SLLIM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2750-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF SLLIM-EXPIRY-DATE > 0 AND SLLIM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLLIM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SLLIM-CAPINT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SLLIM-CAPINT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SLLIM-CAPINT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SLLIM-CAPINT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SLLIM-CAPINT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2760-LIMIT-PRINCIPAL.
           IF SLLIM-PRINCIPAL-YTD < 50.00
               MOVE 50.00 TO SLLIM-PRINCIPAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SLLIM-PRINCIPAL-YTD > 1000.00
               MOVE 1000.00 TO SLLIM-PRINCIPAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CAPINT AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2770-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SLLIM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLLIM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = SLLIM-CAPINT-AMT *
                       SLLIM-FEE-PCT / 200
               WHEN SLLIM-SOURCE-CD = 'AST'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN SLLIM-SOURCE-CD = 'BCK'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = SLLIM-CAPINT-AMT *
                       SLLIM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2780-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-BAND-RATE(1) TO SLLIM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT NOT < WS-DISBURSE-BAND-LOW(WS-TX)
                   MOVE WS-DISBURSE-BAND-RATE(WS-TX) TO SLLIM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SLLIM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SLLIM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LOAN TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2790-DESCRIBE-LOAN-TYPE.
           EVALUATE SLLIM-LOAN-TYPE-CD
               WHEN 'CS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SUBSIDY YTD FEE ON THE PAYMENT YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2800-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SLLIM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO SLLIM-SUBSIDY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLLIM-REVIEW-FLAG = 'Y'
                   COMPUTE SLLIM-SUBSIDY-YTD ROUNDED = SLLIM-PAYMENT-YTD
                       * SLLIM-FEE-PCT / 200
               WHEN SLLIM-REGION-CD = 'C'
                   MOVE 12.00 TO SLLIM-SUBSIDY-YTD
               WHEN SLLIM-REGION-CD = 'D'
                   MOVE 15.00 TO SLLIM-SUBSIDY-YTD
               WHEN OTHER
                   COMPUTE SLLIM-SUBSIDY-YTD ROUNDED = SLLIM-PAYMENT-YTD
                       * SLLIM-FEE-PCT / 100
           END-EVALUATE
           IF SLLIM-SUBSIDY-YTD > 0 AND SLLIM-SUBSIDY-YTD < 2.00
               MOVE 2.00 TO SLLIM-SUBSIDY-YTD
           END-IF
           IF SLLIM-SUBSIDY-YTD > 50.00
               MOVE 50.00 TO SLLIM-SUBSIDY-YTD
           END-IF
           ADD SLLIM-SUBSIDY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2810-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SLLIM-MATURITY-DATE > 0 AND SLLIM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLLIM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SLLIM-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SLLIM-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SLLIM-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SLLIM-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SLLIM-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       2820-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SLLIM-PRINCIPAL-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SLLIM-PRINCIPAL-AMT
           END-IF
           ADD SLLIM-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE SUBSIDY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISBURSE YTD.
      *----------------------------------------------------------------*
       2830-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SLLIM-DISBURSE-YTD ROUNDED =
                   SLLIM-SUBSIDY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SLLIM-DISBURSE-YTD
           END-IF
           ADD SLLIM-DISBURSE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2840-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SLLIM-MATURITY-DATE > 0 AND SLLIM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLLIM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SLLIM-DISBURSE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SLLIM-DISBURSE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SLLIM-DISBURSE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SLLIM-DISBURSE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SLLIM-DISBURSE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO SLLIM-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO SLLIM-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO SLLIM-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'SLLIM EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
