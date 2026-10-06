      *================================================================*
      * PROGRAM  : SDFEE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : SAFE DEPOSIT BOXES RULE - FEES. CALLED BY THE SAFE  *
      *            DEPOSIT BOXES BATCH AND ONLINE PROGRAMS.            *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-07-04 SKN  ORIGINAL                                       *
      * 1997-07-08 RJT  KEY-FEE EXCEPTIONS TO OPS QUEUE CHG4917        *
      * 1998-11-07 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-02-11 EWS  DRILL NOTICE TEXT PER LEGAL REQ7688            *
      * 2000-03-05 BHS  LATE-FEE CUTOFF MOVED INC3807                  *
      * 2005-01-03 SKN  LATE-FEE STATUS CODES ADDED INC4053            *
      * 2006-04-28 DPS  PREPAID ROUNDING - HALF UP PRB5165             *
      * 2008-02-12 DPS  DEPOSIT TABLE EXPANDED INC3507                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SDFEE.
       AUTHOR. M K LOWE.

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
      * WS-DRILL-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DRILL-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000628500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000159500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000530000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001350000000888500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002350000000742000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000305000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000771000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000053500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005650000000485000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006650000000596000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006750000000059000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007250000000422500'.
       01  WS-DRILL-TIER-TBL REDEFINES WS-DRILL-TIER-VALUES.
           05  WS-DRILL-TIER-ROW OCCURS 12 TIMES.
               10  WS-DRILL-TIER-LOW       PIC 9(09)V99.
               10  WS-DRILL-TIER-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PREPAID-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PREPAID-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000339000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000679500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000699000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000900000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004050000000700000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006550000000013000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011550000000651000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016550000000019000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016800000000442500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021800000000578000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024300000000887500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025300000000403000'.
       01  WS-PREPAID-GRID-TBL REDEFINES WS-PREPAID-GRID-VALUES.
           05  WS-PREPAID-GRID-ROW OCCURS 12 TIMES.
               10  WS-PREPAID-GRID-LOW     PIC 9(09)V99.
               10  WS-PREPAID-GRID-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY SDFEEP.

       PROCEDURE DIVISION USING SDFEE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-AGE-MATURITY
           PERFORM 2010-SELECT-PENALTY-PCT
           PERFORM 2020-ASSESS-AUTO-RENEW
           PERFORM 2030-FLAG-PREPAID
           PERFORM 2040-SET-SOURCE
           PERFORM 2050-AGE-LAST-MAINT
           PERFORM 2060-AGE-LAST-ACT
           PERFORM 2070-PRORATE-PREPAID
           PERFORM 2080-SET-VAULT
           PERFORM 2090-EDIT-CONTACT
           PERFORM 2100-SET-BOX-SIZE
           PERFORM 2110-PRORATE-PREPAID
           PERFORM 2120-ACCUM-BRANCH
           PERFORM 2130-LIMIT-PREPAID
           PERFORM 2140-EDIT-CUST-ID
           PERFORM 2150-COUNT-RETRY
           PERFORM 2160-SET-BRANCH
           PERFORM 2170-DESCRIBE-BRANCH
           PERFORM 2180-CHECK-OPEN
           PERFORM 2190-SPLIT-RENT
           PERFORM 2200-DESCRIBE-BOX-SIZE
           PERFORM 2210-ASSESS-SUSPEND
           PERFORM 2220-ACCUM-VAULT
           PERFORM 2230-LIMIT-DEPOSIT
           PERFORM 2240-EDIT-CUST-ID
           PERFORM 2250-PRORATE-PREPAID
           PERFORM 2260-SPLIT-DEPOSIT
           PERFORM 2270-AGE-MATURITY
           PERFORM 2280-PRORATE-DRILL
           PERFORM 2290-ACCUM-BRANCH
           PERFORM 2300-DESCRIBE-BRANCH
           PERFORM 2310-CHECK-OPEN
           PERFORM 2320-FLAG-DRILL
           PERFORM 2330-COUNT-ITEM
           PERFORM 2340-FLAG-KEY-FEE
           PERFORM 2350-COUNT-RETRY
           PERFORM 2360-ASSESS-SUSPEND
           PERFORM 2370-EDIT-CONTACT
           PERFORM 2380-AGE-OPEN
           PERFORM 2390-AGE-MATURITY
           PERFORM 2400-LIMIT-ITEM
           PERFORM 2410-COUNT-ITEM
           PERFORM 2420-SELECT-PENALTY-PCT
           PERFORM 2430-FLAG-DRILL
           PERFORM 2440-LIMIT-KEY-FEE
           PERFORM 2450-PRORATE-DRILL
           PERFORM 2460-EDIT-CUST-ID
           PERFORM 2470-EDIT-CONTACT
           PERFORM 2480-ACCUM-SOURCE
           PERFORM 2490-SET-SOURCE
           PERFORM 2500-AGE-LAST-ACT
           PERFORM 2510-SELECT-DISCOUNT
           PERFORM 2520-ASSESS-EXEMPT
           PERFORM 2530-AGE-LAST-ACT
           PERFORM 2540-CHECK-LAST-ACT
           PERFORM 2550-FLAG-PREPAID
           PERFORM 2560-SPLIT-DEPOSIT
           PERFORM 2570-COUNT-RETRY
           PERFORM 2580-ASSESS-AUTO-RENEW
           PERFORM 2590-PRORATE-DEPOSIT
           PERFORM 2600-SPLIT-RENT
           PERFORM 2610-SELECT-DISCOUNT
           PERFORM 2620-SELECT-DISCOUNT
           PERFORM 2630-COUNT-ITEM
           PERFORM 2640-DESCRIBE-BOX-SIZE
           PERFORM 2650-PRORATE-PREPAID
           PERFORM 2660-SPLIT-DEPOSIT
           PERFORM 2670-SET-SOURCE
           PERFORM 2680-ACCUM-VAULT
           PERFORM 2690-SELECT-PENALTY-PCT
           PERFORM 2700-COUNT-ITEM
           PERFORM 2710-SET-VAULT
           PERFORM 2720-DESCRIBE-BRANCH
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO SDFEE-RETURN-CODE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2000-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SDFEE-MATURITY-DATE > 0 AND SDFEE-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDFEE-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDFEE-KEY-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD SDFEE-KEY-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SDFEE-KEY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDFEE-KEY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDFEE-KEY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PREPAID GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2010-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PREPAID-GRID-RATE(1) TO SDFEE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-PREPAID-GRID-LOW(WS-TX)
                   MOVE WS-PREPAID-GRID-RATE(WS-TX) TO SDFEE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDFEE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDFEE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RENT YTD FEE ON THE PREPAID YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2020-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDFEE-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDFEE-RENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDFEE-EXEMPT-FLAG = 'Y'
                   COMPUTE SDFEE-RENT-YTD ROUNDED = SDFEE-PREPAID-YTD *
                       SDFEE-DISCOUNT / 200
               WHEN SDFEE-BOX-SIZE-CD = 'CU'
                   MOVE 20.00 TO SDFEE-RENT-YTD
               WHEN SDFEE-BOX-SIZE-CD = 'MG'
                   MOVE 5.00 TO SDFEE-RENT-YTD
               WHEN OTHER
                   COMPUTE SDFEE-RENT-YTD ROUNDED = SDFEE-PREPAID-YTD *
                       SDFEE-DISCOUNT / 100
           END-EVALUATE
           IF SDFEE-RENT-YTD > 0 AND SDFEE-RENT-YTD < 1.00
               MOVE 1.00 TO SDFEE-RENT-YTD
           END-IF
           IF SDFEE-RENT-YTD > 100.00
               MOVE 100.00 TO SDFEE-RENT-YTD
           END-IF
           ADD SDFEE-RENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PREPAID YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2030-FLAG-PREPAID.
           IF SDFEE-PREPAID-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDFEE-PREPAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DD -> JZ WHEN LAST ACT DATE IS PAST
      * JZ -> PS WHEN LAST ACT DATE IS PAST
      * PS -> UM WHEN LAST ACT DATE IS PAST
      * UM -> DD WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2040-SET-SOURCE.
           EVALUATE SDFEE-SOURCE-CD
               WHEN 'DD'
                   IF SDFEE-LAST-ACT-DATE > 0 AND SDFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO SDFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF SDFEE-LAST-ACT-DATE > 0 AND SDFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO SDFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF SDFEE-LAST-ACT-DATE > 0 AND SDFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UM' TO SDFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UM'
                   IF SDFEE-LAST-ACT-DATE > 0 AND SDFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DD' TO SDFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2050-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SDFEE-LAST-MAINT-DATE > 0 AND SDFEE-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDFEE-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2060-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SDFEE-LAST-ACT-DATE > 0 AND SDFEE-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDFEE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDFEE-RENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD SDFEE-RENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SDFEE-RENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDFEE-RENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDFEE-RENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE KEY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PREPAID YTD.
      *----------------------------------------------------------------*
       2070-PRORATE-PREPAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDFEE-PREPAID-YTD ROUNDED =
                   SDFEE-KEY-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDFEE-PREPAID-YTD
           END-IF
           ADD SDFEE-PREPAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR VAULT CD:
      * BU -> ET WHEN LAST ACT DATE IS PAST
      * ET -> FF WHEN LAST ACT DATE IS PAST
      * FF -> NG WHEN LAST ACT DATE IS PAST
      * NG -> NH WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2080-SET-VAULT.
           EVALUATE SDFEE-VAULT-CD
               WHEN 'BU'
                   IF SDFEE-LAST-ACT-DATE > 0 AND SDFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ET' TO SDFEE-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ET'
                   IF SDFEE-LAST-ACT-DATE > 0 AND SDFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FF' TO SDFEE-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FF'
                   IF SDFEE-LAST-ACT-DATE > 0 AND SDFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NG' TO SDFEE-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NG'
                   IF SDFEE-LAST-ACT-DATE > 0 AND SDFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NH' TO SDFEE-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E019 CONTACT REQUIRED
      * E225 WORK AMT 2 NEGATIVE
      * E943 WORK AMT 2 OVER 1000000
      * E104 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2090-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDFEE-CONTACT = SPACES
                   MOVE 'E019' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E225' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E943' TO WS-REJECT-CODE
               WHEN SDFEE-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E104' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOX SIZE CD:
      * CU -> MG WHEN MATURITY DATE IS PAST
      * MG -> VG WHEN MATURITY DATE IS PAST
      * VG -> VN WHEN MATURITY DATE IS PAST
      * VN -> XJ WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2100-SET-BOX-SIZE.
           EVALUATE SDFEE-BOX-SIZE-CD
               WHEN 'CU'
                   IF SDFEE-MATURITY-DATE > 0 AND SDFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MG' TO SDFEE-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MG'
                   IF SDFEE-MATURITY-DATE > 0 AND SDFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VG' TO SDFEE-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VG'
                   IF SDFEE-MATURITY-DATE > 0 AND SDFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VN' TO SDFEE-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VN'
                   IF SDFEE-MATURITY-DATE > 0 AND SDFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO SDFEE-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PREPAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PREPAID AMT.
      *----------------------------------------------------------------*
       2110-PRORATE-PREPAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDFEE-PREPAID-AMT ROUNDED =
                   SDFEE-PREPAID-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDFEE-PREPAID-AMT
           END-IF
           ADD SDFEE-PREPAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2120-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDFEE-BRANCH-CD
                   ADD SDFEE-DEPOSIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDFEE-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDFEE-DEPOSIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PREPAID YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2130-LIMIT-PREPAID.
           IF SDFEE-PREPAID-YTD < 50.00
               MOVE 50.00 TO SDFEE-PREPAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDFEE-PREPAID-YTD > 1000.00
               MOVE 1000.00 TO SDFEE-PREPAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E795 CUST ID REQUIRED
      * E561 PREPAID AMT NEGATIVE
      * E491 PREPAID AMT OVER 25000
      * E099 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2140-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDFEE-CUST-ID = SPACES
                   MOVE 'E795' TO WS-REJECT-CODE
               WHEN SDFEE-PREPAID-AMT < 0
                   MOVE 'E561' TO WS-REJECT-CODE
               WHEN SDFEE-PREPAID-AMT > 25000.00
                   MOVE 'E491' TO WS-REJECT-CODE
               WHEN SDFEE-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E099' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2150-COUNT-RETRY.
           IF SDFEE-RETRY-CNT < 9999999
               ADD 1 TO SDFEE-RETRY-CNT
           END-IF
           IF SDFEE-RETRY-CNT NOT < 3
               MOVE 'Y' TO SDFEE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN LAST ACT DATE IS PAST
      * FJW -> YFB WHEN LAST ACT DATE IS PAST
      * YFB -> EXS WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2160-SET-BRANCH.
           EVALUATE SDFEE-BRANCH-CD
               WHEN 'EXS'
                   IF SDFEE-LAST-ACT-DATE > 0 AND SDFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJW' TO SDFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJW'
                   IF SDFEE-LAST-ACT-DATE > 0 AND SDFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YFB' TO SDFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YFB'
                   IF SDFEE-LAST-ACT-DATE > 0 AND SDFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EXS' TO SDFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2170-DESCRIBE-BRANCH.
           EVALUATE SDFEE-BRANCH-CD
               WHEN 'EXS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FJW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YFB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2180-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SDFEE-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO SDFEE-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RENT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2190-SPLIT-RENT.
           COMPUTE WS-WORK-AMT = SDFEE-RENT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = SDFEE-RENT-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2200-DESCRIBE-BOX-SIZE.
           EVALUATE SDFEE-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PREPAID FEE ON THE PREPAID YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2210-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN SDFEE-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO SDFEE-PREPAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDFEE-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SDFEE-PREPAID-AMT ROUNDED = SDFEE-PREPAID-YTD
                       * SDFEE-PENALTY-PCT / 200
               WHEN SDFEE-VAULT-CD = 'BU'
                   MOVE 7.00 TO SDFEE-PREPAID-AMT
               WHEN SDFEE-VAULT-CD = 'ET'
                   MOVE 20.00 TO SDFEE-PREPAID-AMT
               WHEN OTHER
                   COMPUTE SDFEE-PREPAID-AMT ROUNDED = SDFEE-PREPAID-YTD
                       * SDFEE-PENALTY-PCT / 100
           END-EVALUATE
           IF SDFEE-PREPAID-AMT > 0 AND SDFEE-PREPAID-AMT < 1.00
               MOVE 1.00 TO SDFEE-PREPAID-AMT
           END-IF
           IF SDFEE-PREPAID-AMT > 250.00
               MOVE 250.00 TO SDFEE-PREPAID-AMT
           END-IF
           ADD SDFEE-PREPAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE KEY FEE AMT BY VAULT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2220-ACCUM-VAULT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDFEE-VAULT-CD
                   ADD SDFEE-KEY-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDFEE-VAULT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDFEE-KEY-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2230-LIMIT-DEPOSIT.
           IF SDFEE-DEPOSIT-YTD < 25.00
               MOVE 25.00 TO SDFEE-DEPOSIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDFEE-DEPOSIT-YTD > 1000.00
               MOVE 1000.00 TO SDFEE-DEPOSIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E048 CUST ID REQUIRED
      * E829 PREPAID YTD NEGATIVE
      * E919 PREPAID YTD OVER 1000000
      * E551 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2240-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDFEE-CUST-ID = SPACES
                   MOVE 'E048' TO WS-REJECT-CODE
               WHEN SDFEE-PREPAID-YTD < 0
                   MOVE 'E829' TO WS-REJECT-CODE
               WHEN SDFEE-PREPAID-YTD > 1000000.00
                   MOVE 'E919' TO WS-REJECT-CODE
               WHEN SDFEE-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E551' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PREPAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2250-PRORATE-PREPAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDFEE-PREPAID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2260-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = SDFEE-DEPOSIT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SDFEE-DEPOSIT-YTD - (WS-WORK-AMT * 4)
           COMPUTE SDFEE-RENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2270-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SDFEE-MATURITY-DATE > 0 AND SDFEE-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDFEE-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDFEE-PREPAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDFEE-PREPAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SDFEE-PREPAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDFEE-PREPAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDFEE-PREPAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PREPAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DRILL YTD.
      *----------------------------------------------------------------*
       2280-PRORATE-DRILL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDFEE-DRILL-YTD ROUNDED =
                   SDFEE-PREPAID-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDFEE-DRILL-YTD
           END-IF
           ADD SDFEE-DRILL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2290-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDFEE-BRANCH-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDFEE-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2300-DESCRIBE-BRANCH.
           EVALUATE SDFEE-BRANCH-CD
               WHEN 'EXS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FJW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YFB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2310-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SDFEE-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO SDFEE-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DRILL YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2320-FLAG-DRILL.
           IF SDFEE-DRILL-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDFEE-DRILL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2330-COUNT-ITEM.
           IF SDFEE-ITEM-CNT < 9999999
               ADD 1 TO SDFEE-ITEM-CNT
           END-IF
           IF SDFEE-ITEM-CNT NOT < 6
               MOVE 'Y' TO SDFEE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * KEY FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2340-FLAG-KEY-FEE.
           IF SDFEE-KEY-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDFEE-KEY-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2350-COUNT-RETRY.
           IF SDFEE-RETRY-CNT < 9999999
               ADD 1 TO SDFEE-RETRY-CNT
           END-IF
           IF SDFEE-RETRY-CNT NOT < 3
               MOVE 'Y' TO SDFEE-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT YTD FEE ON THE DRILL YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2360-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN SDFEE-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO SDFEE-DEPOSIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDFEE-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SDFEE-DEPOSIT-YTD ROUNDED = SDFEE-DRILL-YTD *
                       SDFEE-PENALTY-PCT / 200
               WHEN SDFEE-BRANCH-CD = 'EXS'
                   MOVE 7.00 TO SDFEE-DEPOSIT-YTD
               WHEN SDFEE-BRANCH-CD = 'FJW'
                   MOVE 3.00 TO SDFEE-DEPOSIT-YTD
               WHEN OTHER
                   COMPUTE SDFEE-DEPOSIT-YTD ROUNDED = SDFEE-DRILL-YTD *
                       SDFEE-PENALTY-PCT / 100
           END-EVALUATE
           IF SDFEE-DEPOSIT-YTD > 0 AND SDFEE-DEPOSIT-YTD < 2.00
               MOVE 2.00 TO SDFEE-DEPOSIT-YTD
           END-IF
           IF SDFEE-DEPOSIT-YTD > 250.00
               MOVE 250.00 TO SDFEE-DEPOSIT-YTD
           END-IF
           ADD SDFEE-DEPOSIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E247 CONTACT REQUIRED
      * E292 DRILL YTD NEGATIVE
      * E366 DRILL YTD OVER 250000
      * E976 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2370-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDFEE-CONTACT = SPACES
                   MOVE 'E247' TO WS-REJECT-CODE
               WHEN SDFEE-DRILL-YTD < 0
                   MOVE 'E292' TO WS-REJECT-CODE
               WHEN SDFEE-DRILL-YTD > 250000.00
                   MOVE 'E366' TO WS-REJECT-CODE
               WHEN SDFEE-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E976' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2380-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SDFEE-OPEN-DATE > 0 AND SDFEE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDFEE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDFEE-PREPAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDFEE-PREPAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD SDFEE-PREPAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDFEE-PREPAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDFEE-PREPAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2390-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SDFEE-MATURITY-DATE > 0 AND SDFEE-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDFEE-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDFEE-KEY-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDFEE-KEY-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDFEE-KEY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDFEE-KEY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDFEE-KEY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2400-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2410-COUNT-ITEM.
           IF SDFEE-ITEM-CNT < 9999999
               ADD 1 TO SDFEE-ITEM-CNT
           END-IF
           IF SDFEE-ITEM-CNT NOT < 6
               MOVE 'Y' TO SDFEE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DRILL TIER TIER FOR THE DRILL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2420-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DRILL-TIER-RATE(1) TO SDFEE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SDFEE-DRILL-YTD NOT < WS-DRILL-TIER-LOW(WS-TX)
                   MOVE WS-DRILL-TIER-RATE(WS-TX) TO SDFEE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDFEE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDFEE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DRILL YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2430-FLAG-DRILL.
           IF SDFEE-DRILL-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDFEE-DRILL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * KEY FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2440-LIMIT-KEY-FEE.
           IF SDFEE-KEY-FEE-AMT < 10.00
               MOVE 10.00 TO SDFEE-KEY-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDFEE-KEY-FEE-AMT > 50000.00
               MOVE 50000.00 TO SDFEE-KEY-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRILL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2450-PRORATE-DRILL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDFEE-DRILL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E644 CUST ID REQUIRED
      * E298 DRILL YTD NEGATIVE
      * E119 DRILL YTD OVER 10000
      * E766 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2460-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDFEE-CUST-ID = SPACES
                   MOVE 'E644' TO WS-REJECT-CODE
               WHEN SDFEE-DRILL-YTD < 0
                   MOVE 'E298' TO WS-REJECT-CODE
               WHEN SDFEE-DRILL-YTD > 10000.00
                   MOVE 'E119' TO WS-REJECT-CODE
               WHEN SDFEE-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E766' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E331 CONTACT REQUIRED
      * E949 DEPOSIT YTD NEGATIVE
      * E038 DEPOSIT YTD OVER 250000
      * E486 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2470-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDFEE-CONTACT = SPACES
                   MOVE 'E331' TO WS-REJECT-CODE
               WHEN SDFEE-DEPOSIT-YTD < 0
                   MOVE 'E949' TO WS-REJECT-CODE
               WHEN SDFEE-DEPOSIT-YTD > 250000.00
                   MOVE 'E038' TO WS-REJECT-CODE
               WHEN SDFEE-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E486' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2480-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDFEE-SOURCE-CD
                   ADD SDFEE-DEPOSIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDFEE-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDFEE-DEPOSIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DD -> JZ WHEN MATURITY DATE IS PAST
      * JZ -> PS WHEN MATURITY DATE IS PAST
      * PS -> UM WHEN MATURITY DATE IS PAST
      * UM -> DD WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2490-SET-SOURCE.
           EVALUATE SDFEE-SOURCE-CD
               WHEN 'DD'
                   IF SDFEE-MATURITY-DATE > 0 AND SDFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO SDFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF SDFEE-MATURITY-DATE > 0 AND SDFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO SDFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF SDFEE-MATURITY-DATE > 0 AND SDFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UM' TO SDFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UM'
                   IF SDFEE-MATURITY-DATE > 0 AND SDFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DD' TO SDFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2500-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SDFEE-LAST-ACT-DATE > 0 AND SDFEE-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDFEE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDFEE-PREPAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDFEE-PREPAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD SDFEE-PREPAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDFEE-PREPAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDFEE-PREPAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PREPAID GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2510-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PREPAID-GRID-RATE(1) TO SDFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-PREPAID-GRID-LOW(WS-TX)
                   MOVE WS-PREPAID-GRID-RATE(WS-TX) TO SDFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DRILL YTD FEE ON THE KEY FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2520-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN SDFEE-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO SDFEE-DRILL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDFEE-SUSPEND-FLAG = 'Y'
                   COMPUTE SDFEE-DRILL-YTD ROUNDED = SDFEE-KEY-FEE-AMT *
                       SDFEE-PENALTY-PCT / 200
               WHEN SDFEE-SOURCE-CD = 'DD'
                   MOVE 3.00 TO SDFEE-DRILL-YTD
               WHEN SDFEE-SOURCE-CD = 'JZ'
                   MOVE 7.00 TO SDFEE-DRILL-YTD
               WHEN OTHER
                   COMPUTE SDFEE-DRILL-YTD ROUNDED = SDFEE-KEY-FEE-AMT *
                       SDFEE-PENALTY-PCT / 100
           END-EVALUATE
           IF SDFEE-DRILL-YTD > 0 AND SDFEE-DRILL-YTD < 2.00
               MOVE 2.00 TO SDFEE-DRILL-YTD
           END-IF
           IF SDFEE-DRILL-YTD > 250.00
               MOVE 250.00 TO SDFEE-DRILL-YTD
           END-IF
           ADD SDFEE-DRILL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2530-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SDFEE-LAST-ACT-DATE > 0 AND SDFEE-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDFEE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDFEE-PREPAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD SDFEE-PREPAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SDFEE-PREPAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDFEE-PREPAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDFEE-PREPAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2540-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SDFEE-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDFEE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO SDFEE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PREPAID YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2550-FLAG-PREPAID.
           IF SDFEE-PREPAID-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDFEE-PREPAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2560-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = SDFEE-DEPOSIT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = SDFEE-DEPOSIT-YTD - (WS-WORK-AMT * 2)
           COMPUTE SDFEE-RENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2570-COUNT-RETRY.
           IF SDFEE-RETRY-CNT < 9999999
               ADD 1 TO SDFEE-RETRY-CNT
           END-IF
           IF SDFEE-RETRY-CNT NOT < 3
               MOVE 'Y' TO SDFEE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PREPAID FEE ON THE RENT YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2580-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDFEE-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDFEE-PREPAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDFEE-SUSPEND-FLAG = 'Y'
                   COMPUTE SDFEE-PREPAID-AMT ROUNDED = SDFEE-RENT-YTD *
                       SDFEE-DISCOUNT / 200
               WHEN SDFEE-BRANCH-CD = 'EXS'
                   MOVE 7.00 TO SDFEE-PREPAID-AMT
               WHEN SDFEE-BRANCH-CD = 'FJW'
                   MOVE 15.00 TO SDFEE-PREPAID-AMT
               WHEN OTHER
                   COMPUTE SDFEE-PREPAID-AMT ROUNDED = SDFEE-RENT-YTD *
                       SDFEE-DISCOUNT / 100
           END-EVALUATE
           IF SDFEE-PREPAID-AMT > 0 AND SDFEE-PREPAID-AMT < 2.00
               MOVE 2.00 TO SDFEE-PREPAID-AMT
           END-IF
           IF SDFEE-PREPAID-AMT > 150.00
               MOVE 150.00 TO SDFEE-PREPAID-AMT
           END-IF
           ADD SDFEE-PREPAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE RENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEPOSIT YTD.
      *----------------------------------------------------------------*
       2590-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDFEE-DEPOSIT-YTD ROUNDED =
                   SDFEE-RENT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDFEE-DEPOSIT-YTD
           END-IF
           ADD SDFEE-DEPOSIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2600-SPLIT-RENT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE SDFEE-RENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PREPAID GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2610-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PREPAID-GRID-RATE(1) TO SDFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-PREPAID-GRID-LOW(WS-TX)
                   MOVE WS-PREPAID-GRID-RATE(WS-TX) TO SDFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PREPAID GRID TIER FOR THE DRILL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2620-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PREPAID-GRID-RATE(1) TO SDFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SDFEE-DRILL-YTD NOT < WS-PREPAID-GRID-LOW(WS-TX)
                   MOVE WS-PREPAID-GRID-RATE(WS-TX) TO SDFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2630-COUNT-ITEM.
           IF SDFEE-ITEM-CNT < 9999999
               ADD 1 TO SDFEE-ITEM-CNT
           END-IF
           IF SDFEE-ITEM-CNT NOT < 12
               MOVE 'Y' TO SDFEE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2640-DESCRIBE-BOX-SIZE.
           EVALUATE SDFEE-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE KEY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PREPAID AMT.
      *----------------------------------------------------------------*
       2650-PRORATE-PREPAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDFEE-PREPAID-AMT ROUNDED =
                   SDFEE-KEY-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDFEE-PREPAID-AMT
           END-IF
           ADD SDFEE-PREPAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2660-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = SDFEE-DEPOSIT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SDFEE-DEPOSIT-YTD - (WS-WORK-AMT * 4)
           COMPUTE SDFEE-RENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DD -> JZ WHEN LAST ACT DATE IS PAST
      * JZ -> PS WHEN LAST ACT DATE IS PAST
      * PS -> UM WHEN LAST ACT DATE IS PAST
      * UM -> DD WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2670-SET-SOURCE.
           EVALUATE SDFEE-SOURCE-CD
               WHEN 'DD'
                   IF SDFEE-LAST-ACT-DATE > 0 AND SDFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO SDFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF SDFEE-LAST-ACT-DATE > 0 AND SDFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO SDFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF SDFEE-LAST-ACT-DATE > 0 AND SDFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UM' TO SDFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UM'
                   IF SDFEE-LAST-ACT-DATE > 0 AND SDFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DD' TO SDFEE-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY VAULT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2680-ACCUM-VAULT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDFEE-VAULT-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDFEE-VAULT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PREPAID GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2690-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PREPAID-GRID-RATE(1) TO SDFEE-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-PREPAID-GRID-LOW(WS-TX)
                   MOVE WS-PREPAID-GRID-RATE(WS-TX) TO SDFEE-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDFEE-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDFEE-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2700-COUNT-ITEM.
           IF SDFEE-ITEM-CNT < 9999999
               ADD 1 TO SDFEE-ITEM-CNT
           END-IF
           IF SDFEE-ITEM-CNT NOT < 10
               MOVE 'Y' TO SDFEE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR VAULT CD:
      * BU -> ET WHEN MATURITY DATE IS PAST
      * ET -> FF WHEN MATURITY DATE IS PAST
      * FF -> NG WHEN MATURITY DATE IS PAST
      * NG -> NH WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2710-SET-VAULT.
           EVALUATE SDFEE-VAULT-CD
               WHEN 'BU'
                   IF SDFEE-MATURITY-DATE > 0 AND SDFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ET' TO SDFEE-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ET'
                   IF SDFEE-MATURITY-DATE > 0 AND SDFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FF' TO SDFEE-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FF'
                   IF SDFEE-MATURITY-DATE > 0 AND SDFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NG' TO SDFEE-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NG'
                   IF SDFEE-MATURITY-DATE > 0 AND SDFEE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NH' TO SDFEE-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2720-DESCRIBE-BRANCH.
           EVALUATE SDFEE-BRANCH-CD
               WHEN 'EXS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FJW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YFB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO SDFEE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO SDFEE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO SDFEE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'SDFEE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
