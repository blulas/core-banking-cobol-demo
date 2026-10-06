      *================================================================*
      * PROGRAM  : PBDTE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : PASSBOOK PRINTING RULE - STATUS. CALLED BY THE      *
      *            PASSBOOK PRINTING BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-06-17 JLM  ORIGINAL                                       *
      * 1997-10-08 JLM  AUDIT FINDING - PRINTED CONTROL TOTALS PRB5992 *
      * 1998-06-12 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-11-04 EWS  INTEREST FEE SCHEDULE UPDATE INC8095           *
      * 2002-05-03 GAF  UNPOSTED CUTOFF MOVED CHG9293                  *
      * 2002-07-01 SKN  INTEREST ROUNDING - HALF UP CHG8325            *
      * 2002-12-06 RJT  UNPOSTED EXCEPTIONS TO OPS QUEUE PRB4997       *
      * 2007-11-01 MKL  INTEREST RERUN PROTECTION CHG0516              *
      * 2010-08-23 CRP  CARRYFWD FEE SCHEDULE UPDATE CHG8388           *
      * 2011-12-07 DPS  PRINTED EXCEPTIONS TO OPS QUEUE CHG4323        *
      * 2011-03-01 DPS  PRINTED TABLE EXPANDED INC4366                 *
      * 2013-05-20 PQR  CARRYFWD CALCULATION CORRECTED PRB5919         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBDTE.
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
                                           VALUE 24.000000.
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
      * WS-LINE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-LINE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000214000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000713000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000292000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003250000000545000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003350000000641000'.
       01  WS-LINE-BAND-TBL REDEFINES WS-LINE-BAND-VALUES.
           05  WS-LINE-BAND-ROW OCCURS 5 TIMES.
               10  WS-LINE-BAND-LOW        PIC 9(09)V99.
               10  WS-LINE-BAND-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INTEREST-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000130500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000176500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000796500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000479000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001300000000757000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001350000000703500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002350000000228500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012350000000272500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017350000000216500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017850000000714500'.
       01  WS-INTEREST-GRID-TBL REDEFINES WS-INTEREST-GRID-VALUES.
           05  WS-INTEREST-GRID-ROW OCCURS 10 TIMES.
               10  WS-INTEREST-GRID-LOW    PIC 9(09)V99.
               10  WS-INTEREST-GRID-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY PBDTEP.

       PROCEDURE DIVISION USING PBDTE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-FLAG-PRINTED
           PERFORM 2010-SET-REGION
           PERFORM 2020-ACCUM-SOURCE
           PERFORM 2030-EDIT-CUST-ID
           PERFORM 2040-AGE-NEXT-DUE
           PERFORM 2050-SET-BRANCH
           PERFORM 2060-PRORATE-UNPOSTED
           PERFORM 2070-SELECT-FEE-PCT
           PERFORM 2080-ASSESS-SUSPEND
           PERFORM 2090-EDIT-NAME
           PERFORM 2100-SPLIT-PRINTED
           PERFORM 2110-AGE-CLOSE
           PERFORM 2120-PRORATE-PRINTED
           PERFORM 2130-EDIT-CUST-ID
           PERFORM 2140-PRORATE-INTEREST
           PERFORM 2150-SELECT-FEE-PCT
           PERFORM 2160-ACCUM-REGION
           PERFORM 2170-SELECT-MARGIN
           PERFORM 2180-COUNT-CYCLE
           PERFORM 2190-COUNT-RETRY
           PERFORM 2200-LIMIT-UNPOSTED
           PERFORM 2210-AGE-NEXT-DUE
           PERFORM 2220-DESCRIBE-SOURCE
           PERFORM 2230-SPLIT-PRINTED
           PERFORM 2240-LIMIT-ITEM
           PERFORM 2250-SELECT-FEE-PCT
           PERFORM 2260-SET-BRANCH
           PERFORM 2270-COUNT-RETRY
           PERFORM 2280-SELECT-FEE-PCT
           PERFORM 2290-ASSESS-EXEMPT
           PERFORM 2300-ACCUM-REGION
           PERFORM 2310-EDIT-CUST-ID
           PERFORM 2320-ACCRUE-FEE-PCT
           PERFORM 2330-FLAG-INTEREST
           PERFORM 2340-AGE-CLOSE
           PERFORM 2350-LIMIT-CARRYFWD
           PERFORM 2360-PRORATE-CARRYFWD
           PERFORM 2370-DESCRIBE-BRANCH
           PERFORM 2380-COUNT-RETRY
           PERFORM 2390-ACCUM-SOURCE
           PERFORM 2400-SET-PRINTER
           PERFORM 2410-ACCUM-BRANCH
           PERFORM 2420-SPLIT-CARRYFWD
           PERFORM 2430-ASSESS-SUSPEND
           PERFORM 2440-EDIT-NAME
           PERFORM 2450-COUNT-RETRY
           PERFORM 2460-AGE-NEXT-DUE
           PERFORM 2470-LIMIT-INTEREST
           PERFORM 2480-ASSESS-SUSPEND
           PERFORM 2490-ASSESS-REVIEW
           PERFORM 2500-SET-BRANCH
           PERFORM 2510-LIMIT-ITEM
           PERFORM 2520-SELECT-FEE-PCT
           PERFORM 2530-COUNT-RETRY
           PERFORM 2540-SET-PRINTER
           PERFORM 2550-ACCRUE-UNPOSTED
           PERFORM 2560-LIMIT-ITEM
           PERFORM 2570-ACCRUE-FEE-PCT
           PERFORM 2580-DESCRIBE-PRINTER
           PERFORM 2590-EDIT-CUST-ID
           PERFORM 2600-LIMIT-INTEREST
           PERFORM 2610-PRORATE-UNPOSTED
           PERFORM 2620-EDIT-CUST-ID
           PERFORM 2630-LIMIT-ITEM
           PERFORM 2640-FLAG-ITEM
           PERFORM 2650-DESCRIBE-BRANCH
           PERFORM 2660-CHECK-MATURITY
           PERFORM 2670-LIMIT-ITEM
           PERFORM 2680-DESCRIBE-BRANCH
           PERFORM 2690-CHECK-CLOSE
           PERFORM 2700-ACCUM-PRINTER
           PERFORM 2710-AGE-OPEN
           PERFORM 2720-EDIT-CUST-ID
           PERFORM 2730-DESCRIBE-REGION
           PERFORM 2740-FLAG-CARRYFWD
           PERFORM 2750-EDIT-CUST-ID
           PERFORM 2760-EDIT-CUST-ID
           PERFORM 2770-SET-REGION
           PERFORM 2780-CHECK-CLOSE
           PERFORM 2790-DESCRIBE-PRINTER
           PERFORM 2800-ASSESS-REVIEW
           PERFORM 2810-SELECT-MARGIN
           PERFORM 2820-ACCRUE-CARRYFWD
           PERFORM 2830-AGE-NEXT-DUE
           PERFORM 2840-CHECK-OPEN
           PERFORM 2850-CHECK-MATURITY
           PERFORM 2860-ASSESS-SUSPEND
           PERFORM 2870-CHECK-NEXT-DUE
           PERFORM 2880-SET-PRINTER
           PERFORM 2890-SPLIT-UNPOSTED
           PERFORM 2900-ACCRUE-UNPOSTED
           PERFORM 2910-ACCUM-BRANCH
           PERFORM 2920-ACCUM-PRINTER
           PERFORM 2930-AGE-MATURITY
           PERFORM 2940-SELECT-FEE-PCT
           PERFORM 2950-FLAG-ITEM
           PERFORM 2960-LIMIT-ITEM
           PERFORM 2970-ACCUM-BRANCH
           PERFORM 2980-FLAG-CARRYFWD
           PERFORM 2990-FLAG-INTEREST
           PERFORM 3000-FLAG-ITEM
           PERFORM 3010-ASSESS-EXEMPT
           PERFORM 3020-DESCRIBE-BRANCH
           PERFORM 3030-COUNT-CYCLE
           PERFORM 3040-ACCUM-BRANCH
           PERFORM 3050-CHECK-CLOSE
           PERFORM 3060-FLAG-UNPOSTED
           PERFORM 3070-SPLIT-CARRYFWD
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO PBDTE-RETURN-CODE.

      *----------------------------------------------------------------*
      * PRINTED YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2000-FLAG-PRINTED.
           IF PBDTE-PRINTED-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBDTE-PRINTED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * ER -> GL WHEN MATURITY DATE IS PAST
      * GL -> PE WHEN MATURITY DATE IS PAST
      * PE -> RJ WHEN MATURITY DATE IS PAST
      * RJ -> VB WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2010-SET-REGION.
           EVALUATE PBDTE-REGION-CD
               WHEN 'ER'
                   IF PBDTE-MATURITY-DATE > 0 AND PBDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO PBDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF PBDTE-MATURITY-DATE > 0 AND PBDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PE' TO PBDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PE'
                   IF PBDTE-MATURITY-DATE > 0 AND PBDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO PBDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF PBDTE-MATURITY-DATE > 0 AND PBDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VB' TO PBDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CARRYFWD YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2020-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBDTE-SOURCE-CD
                   ADD PBDTE-CARRYFWD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBDTE-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBDTE-CARRYFWD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E716 CUST ID REQUIRED
      * E492 CARRYFWD YTD NEGATIVE
      * E278 CARRYFWD YTD OVER 5000
      * E867 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2030-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBDTE-CUST-ID = SPACES
                   MOVE 'E716' TO WS-REJECT-CODE
               WHEN PBDTE-CARRYFWD-YTD < 0
                   MOVE 'E492' TO WS-REJECT-CODE
               WHEN PBDTE-CARRYFWD-YTD > 5000.00
                   MOVE 'E278' TO WS-REJECT-CODE
               WHEN PBDTE-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E867' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2040-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBDTE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBDTE-UNPOSTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBDTE-UNPOSTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PBDTE-UNPOSTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBDTE-UNPOSTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBDTE-UNPOSTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DF -> NV WHEN NEXT DUE DATE IS PAST
      * NV -> PP WHEN NEXT DUE DATE IS PAST
      * PP -> SH WHEN NEXT DUE DATE IS PAST
      * SH -> ZP WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2050-SET-BRANCH.
           EVALUATE PBDTE-BRANCH-CD
               WHEN 'DF'
                   IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NV' TO PBDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NV'
                   IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PP' TO PBDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PP'
                   IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO PBDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZP' TO PBDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE INTEREST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO UNPOSTED YTD.
      *----------------------------------------------------------------*
       2060-PRORATE-UNPOSTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBDTE-UNPOSTED-YTD ROUNDED =
                   PBDTE-INTEREST-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBDTE-UNPOSTED-YTD
           END-IF
           ADD PBDTE-UNPOSTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE LINE BAND TIER FOR THE CARRYFWD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2070-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LINE-BAND-RATE(1) TO PBDTE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PBDTE-CARRYFWD-YTD NOT < WS-LINE-BAND-LOW(WS-TX)
                   MOVE WS-LINE-BAND-RATE(WS-TX) TO PBDTE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBDTE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBDTE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CARRYFWD YTD FEE ON THE CARRYFWD AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2080-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PBDTE-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PBDTE-CARRYFWD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBDTE-REVIEW-FLAG = 'Y'
                   COMPUTE PBDTE-CARRYFWD-YTD ROUNDED =
                       PBDTE-CARRYFWD-AMT * PBDTE-FEE-PCT / 200
               WHEN PBDTE-PRINTER-CD = 'CB'
                   MOVE 20.00 TO PBDTE-CARRYFWD-YTD
               WHEN PBDTE-PRINTER-CD = 'EP'
                   MOVE 10.00 TO PBDTE-CARRYFWD-YTD
               WHEN OTHER
                   COMPUTE PBDTE-CARRYFWD-YTD ROUNDED =
                       PBDTE-CARRYFWD-AMT * PBDTE-FEE-PCT / 100
           END-EVALUATE
           IF PBDTE-CARRYFWD-YTD > 0 AND PBDTE-CARRYFWD-YTD < 2.00
               MOVE 2.00 TO PBDTE-CARRYFWD-YTD
           END-IF
           IF PBDTE-CARRYFWD-YTD > 50.00
               MOVE 50.00 TO PBDTE-CARRYFWD-YTD
           END-IF
           ADD PBDTE-CARRYFWD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E249 NAME REQUIRED
      * E635 CARRYFWD YTD NEGATIVE
      * E112 CARRYFWD YTD OVER 250000
      * E541 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2090-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBDTE-NAME = SPACES
                   MOVE 'E249' TO WS-REJECT-CODE
               WHEN PBDTE-CARRYFWD-YTD < 0
                   MOVE 'E635' TO WS-REJECT-CODE
               WHEN PBDTE-CARRYFWD-YTD > 250000.00
                   MOVE 'E112' TO WS-REJECT-CODE
               WHEN PBDTE-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E541' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINTED YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2100-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBDTE-PRINTED-YTD / 4
           COMPUTE WS-WORK-AMT-2 = PBDTE-PRINTED-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2110-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBDTE-CLOSE-DATE > 0 AND PBDTE-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBDTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBDTE-UNPOSTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD PBDTE-UNPOSTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PBDTE-UNPOSTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBDTE-UNPOSTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBDTE-UNPOSTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINTED YTD.
      *----------------------------------------------------------------*
       2120-PRORATE-PRINTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBDTE-PRINTED-YTD ROUNDED =
                   PBDTE-CARRYFWD-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBDTE-PRINTED-YTD
           END-IF
           ADD PBDTE-PRINTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E313 CUST ID REQUIRED
      * E907 WORK AMT 2 NEGATIVE
      * E824 WORK AMT 2 OVER 10000
      * E428 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2130-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBDTE-CUST-ID = SPACES
                   MOVE 'E313' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E907' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E824' TO WS-REJECT-CODE
               WHEN PBDTE-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E428' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE UNPOSTED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       2140-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBDTE-INTEREST-YTD ROUNDED =
                   PBDTE-UNPOSTED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBDTE-INTEREST-YTD
           END-IF
           ADD PBDTE-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE LINE BAND TIER FOR THE UNPOSTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2150-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LINE-BAND-RATE(1) TO PBDTE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PBDTE-UNPOSTED-AMT NOT < WS-LINE-BAND-LOW(WS-TX)
                   MOVE WS-LINE-BAND-RATE(WS-TX) TO PBDTE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBDTE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBDTE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CARRYFWD YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2160-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBDTE-REGION-CD
                   ADD PBDTE-CARRYFWD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBDTE-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBDTE-CARRYFWD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST GRID TIER FOR THE UNPOSTED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2170-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-GRID-RATE(1) TO PBDTE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PBDTE-UNPOSTED-YTD NOT < WS-INTEREST-GRID-LOW(WS-TX)
                   MOVE WS-INTEREST-GRID-RATE(WS-TX) TO PBDTE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBDTE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBDTE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2180-COUNT-CYCLE.
           IF PBDTE-CYCLE-CNT < 9999999
               ADD 1 TO PBDTE-CYCLE-CNT
           END-IF
           IF PBDTE-CYCLE-CNT NOT < 3
               MOVE 'Y' TO PBDTE-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2190-COUNT-RETRY.
           IF PBDTE-RETRY-CNT < 9999999
               ADD 1 TO PBDTE-RETRY-CNT
           END-IF
           IF PBDTE-RETRY-CNT NOT < 6
               MOVE 'Y' TO PBDTE-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * UNPOSTED YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2200-LIMIT-UNPOSTED.
           IF PBDTE-UNPOSTED-YTD < 10.00
               MOVE 10.00 TO PBDTE-UNPOSTED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBDTE-UNPOSTED-YTD > 50000.00
               MOVE 50000.00 TO PBDTE-UNPOSTED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2210-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBDTE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2220-DESCRIBE-SOURCE.
           EVALUATE PBDTE-SOURCE-CD
               WHEN 'JX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PRINTED YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2230-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBDTE-PRINTED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PBDTE-PRINTED-YTD - (WS-WORK-AMT * 3)
           COMPUTE PBDTE-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2240-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2250-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-GRID-RATE(1) TO PBDTE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-INTEREST-GRID-LOW(WS-TX)
                   MOVE WS-INTEREST-GRID-RATE(WS-TX) TO PBDTE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBDTE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBDTE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DF -> NV WHEN NEXT DUE DATE IS PAST
      * NV -> PP WHEN NEXT DUE DATE IS PAST
      * PP -> SH WHEN NEXT DUE DATE IS PAST
      * SH -> ZP WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2260-SET-BRANCH.
           EVALUATE PBDTE-BRANCH-CD
               WHEN 'DF'
                   IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NV' TO PBDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NV'
                   IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PP' TO PBDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PP'
                   IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO PBDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZP' TO PBDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2270-COUNT-RETRY.
           IF PBDTE-RETRY-CNT < 9999999
               ADD 1 TO PBDTE-RETRY-CNT
           END-IF
           IF PBDTE-RETRY-CNT NOT < 6
               MOVE 'Y' TO PBDTE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LINE BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2280-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LINE-BAND-RATE(1) TO PBDTE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-LINE-BAND-LOW(WS-TX)
                   MOVE WS-LINE-BAND-RATE(WS-TX) TO PBDTE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBDTE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBDTE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE UNPOSTED YTD FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2290-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PBDTE-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PBDTE-UNPOSTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBDTE-SUSPEND-FLAG = 'Y'
                   COMPUTE PBDTE-UNPOSTED-YTD ROUNDED = WS-WORK-AMT *
                       PBDTE-FEE-PCT / 200
               WHEN PBDTE-SOURCE-CD = 'JX'
                   MOVE 20.00 TO PBDTE-UNPOSTED-YTD
               WHEN PBDTE-SOURCE-CD = 'ZC'
                   MOVE 20.00 TO PBDTE-UNPOSTED-YTD
               WHEN OTHER
                   COMPUTE PBDTE-UNPOSTED-YTD ROUNDED = WS-WORK-AMT *
                       PBDTE-FEE-PCT / 100
           END-EVALUATE
           IF PBDTE-UNPOSTED-YTD > 0 AND PBDTE-UNPOSTED-YTD < 1.00
               MOVE 1.00 TO PBDTE-UNPOSTED-YTD
           END-IF
           IF PBDTE-UNPOSTED-YTD > 250.00
               MOVE 250.00 TO PBDTE-UNPOSTED-YTD
           END-IF
           ADD PBDTE-UNPOSTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE UNPOSTED YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2300-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBDTE-REGION-CD
                   ADD PBDTE-UNPOSTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBDTE-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBDTE-UNPOSTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E801 CUST ID REQUIRED
      * E126 WORK AMT 2 NEGATIVE
      * E151 WORK AMT 2 OVER 250000
      * E285 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2310-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBDTE-CUST-ID = SPACES
                   MOVE 'E801' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E126' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E151' TO WS-REJECT-CODE
               WHEN PBDTE-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E285' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2320-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * PBDTE-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBDTE-CARRYFWD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2330-FLAG-INTEREST.
           IF PBDTE-INTEREST-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBDTE-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2340-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBDTE-CLOSE-DATE > 0 AND PBDTE-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBDTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBDTE-CARRYFWD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBDTE-CARRYFWD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PBDTE-CARRYFWD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBDTE-CARRYFWD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBDTE-CARRYFWD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CARRYFWD AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2350-LIMIT-CARRYFWD.
           IF PBDTE-CARRYFWD-AMT < 0.00
               MOVE 0.00 TO PBDTE-CARRYFWD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBDTE-CARRYFWD-AMT > 2500.00
               MOVE 2500.00 TO PBDTE-CARRYFWD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2360-PRORATE-CARRYFWD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBDTE-CARRYFWD-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2370-DESCRIBE-BRANCH.
           EVALUATE PBDTE-BRANCH-CD
               WHEN 'DF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2380-COUNT-RETRY.
           IF PBDTE-RETRY-CNT < 9999999
               ADD 1 TO PBDTE-RETRY-CNT
           END-IF
           IF PBDTE-RETRY-CNT NOT < 3
               MOVE 'Y' TO PBDTE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE UNPOSTED YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2390-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBDTE-SOURCE-CD
                   ADD PBDTE-UNPOSTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBDTE-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBDTE-UNPOSTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PRINTER CD:
      * CB -> EP WHEN CLOSE DATE IS PAST
      * EP -> EX WHEN CLOSE DATE IS PAST
      * EX -> JE WHEN CLOSE DATE IS PAST
      * JE -> MR WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2400-SET-PRINTER.
           EVALUATE PBDTE-PRINTER-CD
               WHEN 'CB'
                   IF PBDTE-CLOSE-DATE > 0 AND PBDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EP' TO PBDTE-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EP'
                   IF PBDTE-CLOSE-DATE > 0 AND PBDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EX' TO PBDTE-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF PBDTE-CLOSE-DATE > 0 AND PBDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JE' TO PBDTE-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF PBDTE-CLOSE-DATE > 0 AND PBDTE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO PBDTE-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CARRYFWD YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2410-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBDTE-BRANCH-CD
                   ADD PBDTE-CARRYFWD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBDTE-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBDTE-CARRYFWD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CARRYFWD YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2420-SPLIT-CARRYFWD.
           COMPUTE WS-WORK-AMT = PBDTE-CARRYFWD-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PBDTE-CARRYFWD-YTD - (WS-WORK-AMT *
               2)
           COMPUTE PBDTE-PRINTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE UNPOSTED FEE ON THE CARRYFWD AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2430-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PBDTE-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PBDTE-UNPOSTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBDTE-EXEMPT-FLAG = 'Y'
                   COMPUTE PBDTE-UNPOSTED-AMT ROUNDED =
                       PBDTE-CARRYFWD-AMT * PBDTE-FEE-PCT / 200
               WHEN PBDTE-BRANCH-CD = 'DF'
                   MOVE 15.00 TO PBDTE-UNPOSTED-AMT
               WHEN PBDTE-BRANCH-CD = 'NV'
                   MOVE 7.00 TO PBDTE-UNPOSTED-AMT
               WHEN OTHER
                   COMPUTE PBDTE-UNPOSTED-AMT ROUNDED =
                       PBDTE-CARRYFWD-AMT * PBDTE-FEE-PCT / 100
           END-EVALUATE
           IF PBDTE-UNPOSTED-AMT > 0 AND PBDTE-UNPOSTED-AMT < 5.00
               MOVE 5.00 TO PBDTE-UNPOSTED-AMT
           END-IF
           IF PBDTE-UNPOSTED-AMT > 75.00
               MOVE 75.00 TO PBDTE-UNPOSTED-AMT
           END-IF
           ADD PBDTE-UNPOSTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E587 NAME REQUIRED
      * E466 WORK AMT NEGATIVE
      * E615 WORK AMT OVER 5000
      * E526 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2440-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBDTE-NAME = SPACES
                   MOVE 'E587' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E466' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E615' TO WS-REJECT-CODE
               WHEN PBDTE-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E526' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2450-COUNT-RETRY.
           IF PBDTE-RETRY-CNT < 9999999
               ADD 1 TO PBDTE-RETRY-CNT
           END-IF
           IF PBDTE-RETRY-CNT NOT < 12
               MOVE 'Y' TO PBDTE-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2460-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBDTE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBDTE-CARRYFWD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PBDTE-CARRYFWD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBDTE-CARRYFWD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBDTE-CARRYFWD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBDTE-CARRYFWD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2470-LIMIT-INTEREST.
           IF PBDTE-INTEREST-YTD < 50.00
               MOVE 50.00 TO PBDTE-INTEREST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBDTE-INTEREST-YTD > 50000.00
               MOVE 50000.00 TO PBDTE-INTEREST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CARRYFWD YTD FEE ON THE INTEREST YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2480-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PBDTE-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PBDTE-CARRYFWD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBDTE-REVIEW-FLAG = 'Y'
                   COMPUTE PBDTE-CARRYFWD-YTD ROUNDED =
                       PBDTE-INTEREST-YTD * PBDTE-FEE-PCT / 200
               WHEN PBDTE-REGION-CD = 'ER'
                   MOVE 5.00 TO PBDTE-CARRYFWD-YTD
               WHEN PBDTE-REGION-CD = 'GL'
                   MOVE 10.00 TO PBDTE-CARRYFWD-YTD
               WHEN OTHER
                   COMPUTE PBDTE-CARRYFWD-YTD ROUNDED =
                       PBDTE-INTEREST-YTD * PBDTE-FEE-PCT / 100
           END-EVALUATE
           IF PBDTE-CARRYFWD-YTD > 0 AND PBDTE-CARRYFWD-YTD < 2.00
               MOVE 2.00 TO PBDTE-CARRYFWD-YTD
           END-IF
           IF PBDTE-CARRYFWD-YTD > 100.00
               MOVE 100.00 TO PBDTE-CARRYFWD-YTD
           END-IF
           ADD PBDTE-CARRYFWD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE UNPOSTED YTD FEE ON THE WORK AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2490-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN PBDTE-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO PBDTE-UNPOSTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBDTE-EXEMPT-FLAG = 'Y'
                   COMPUTE PBDTE-UNPOSTED-YTD ROUNDED = WS-WORK-AMT *
                       PBDTE-FEE-PCT / 200
               WHEN PBDTE-PRINTER-CD = 'CB'
                   MOVE 7.00 TO PBDTE-UNPOSTED-YTD
               WHEN PBDTE-PRINTER-CD = 'EP'
                   MOVE 20.00 TO PBDTE-UNPOSTED-YTD
               WHEN OTHER
                   COMPUTE PBDTE-UNPOSTED-YTD ROUNDED = WS-WORK-AMT *
                       PBDTE-FEE-PCT / 100
           END-EVALUATE
           IF PBDTE-UNPOSTED-YTD > 0 AND PBDTE-UNPOSTED-YTD < 2.00
               MOVE 2.00 TO PBDTE-UNPOSTED-YTD
           END-IF
           IF PBDTE-UNPOSTED-YTD > 75.00
               MOVE 75.00 TO PBDTE-UNPOSTED-YTD
           END-IF
           ADD PBDTE-UNPOSTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DF -> NV WHEN NEXT DUE DATE IS PAST
      * NV -> PP WHEN NEXT DUE DATE IS PAST
      * PP -> SH WHEN NEXT DUE DATE IS PAST
      * SH -> ZP WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2500-SET-BRANCH.
           EVALUATE PBDTE-BRANCH-CD
               WHEN 'DF'
                   IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NV' TO PBDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NV'
                   IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PP' TO PBDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PP'
                   IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO PBDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZP' TO PBDTE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2510-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LINE BAND TIER FOR THE PRINTED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2520-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LINE-BAND-RATE(1) TO PBDTE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PBDTE-PRINTED-YTD NOT < WS-LINE-BAND-LOW(WS-TX)
                   MOVE WS-LINE-BAND-RATE(WS-TX) TO PBDTE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBDTE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBDTE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2530-COUNT-RETRY.
           IF PBDTE-RETRY-CNT < 9999999
               ADD 1 TO PBDTE-RETRY-CNT
           END-IF
           IF PBDTE-RETRY-CNT NOT < 3
               MOVE 'Y' TO PBDTE-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PRINTER CD:
      * CB -> EP WHEN MATURITY DATE IS PAST
      * EP -> EX WHEN MATURITY DATE IS PAST
      * EX -> JE WHEN MATURITY DATE IS PAST
      * JE -> MR WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2540-SET-PRINTER.
           EVALUATE PBDTE-PRINTER-CD
               WHEN 'CB'
                   IF PBDTE-MATURITY-DATE > 0 AND PBDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EP' TO PBDTE-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EP'
                   IF PBDTE-MATURITY-DATE > 0 AND PBDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EX' TO PBDTE-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF PBDTE-MATURITY-DATE > 0 AND PBDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JE' TO PBDTE-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF PBDTE-MATURITY-DATE > 0 AND PBDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO PBDTE-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON UNPOSTED AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2550-ACCRUE-UNPOSTED.
           IF PBDTE-UNPOSTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBDTE-UNPOSTED-AMT * PBDTE-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2560-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2570-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PBDTE-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBDTE-CARRYFWD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PRINTER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2580-DESCRIBE-PRINTER.
           EVALUATE PBDTE-PRINTER-CD
               WHEN 'CB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'EP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'EX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E741 CUST ID REQUIRED
      * E942 CARRYFWD YTD NEGATIVE
      * E267 CARRYFWD YTD OVER 10000
      * E337 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2590-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBDTE-CUST-ID = SPACES
                   MOVE 'E741' TO WS-REJECT-CODE
               WHEN PBDTE-CARRYFWD-YTD < 0
                   MOVE 'E942' TO WS-REJECT-CODE
               WHEN PBDTE-CARRYFWD-YTD > 10000.00
                   MOVE 'E267' TO WS-REJECT-CODE
               WHEN PBDTE-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E337' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2600-LIMIT-INTEREST.
           IF PBDTE-INTEREST-YTD < 50.00
               MOVE 50.00 TO PBDTE-INTEREST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBDTE-INTEREST-YTD > 5000.00
               MOVE 5000.00 TO PBDTE-INTEREST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO UNPOSTED YTD.
      *----------------------------------------------------------------*
       2610-PRORATE-UNPOSTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBDTE-UNPOSTED-YTD ROUNDED =
                   PBDTE-CARRYFWD-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBDTE-UNPOSTED-YTD
           END-IF
           ADD PBDTE-UNPOSTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E676 CUST ID REQUIRED
      * E872 CARRYFWD YTD NEGATIVE
      * E472 CARRYFWD YTD OVER 250000
      * E534 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2620-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBDTE-CUST-ID = SPACES
                   MOVE 'E676' TO WS-REJECT-CODE
               WHEN PBDTE-CARRYFWD-YTD < 0
                   MOVE 'E872' TO WS-REJECT-CODE
               WHEN PBDTE-CARRYFWD-YTD > 250000.00
                   MOVE 'E472' TO WS-REJECT-CODE
               WHEN PBDTE-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E534' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2630-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2640-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2650-DESCRIBE-BRANCH.
           EVALUATE PBDTE-BRANCH-CD
               WHEN 'DF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2660-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF PBDTE-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBDTE-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PBDTE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2670-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2680-DESCRIBE-BRANCH.
           EVALUATE PBDTE-BRANCH-CD
               WHEN 'DF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2690-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBDTE-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBDTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO PBDTE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE UNPOSTED YTD BY PRINTER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2700-ACCUM-PRINTER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBDTE-PRINTER-CD
                   ADD PBDTE-UNPOSTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBDTE-PRINTER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBDTE-UNPOSTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2710-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PBDTE-OPEN-DATE > 0 AND PBDTE-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBDTE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBDTE-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PBDTE-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PBDTE-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PBDTE-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBDTE-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E025 CUST ID REQUIRED
      * E851 WORK AMT 2 NEGATIVE
      * E311 WORK AMT 2 OVER 25000
      * E434 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2720-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBDTE-CUST-ID = SPACES
                   MOVE 'E025' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E851' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E311' TO WS-REJECT-CODE
               WHEN PBDTE-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E434' TO WS-REJECT-CODE
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
       2730-DESCRIBE-REGION.
           EVALUATE PBDTE-REGION-CD
               WHEN 'ER'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CARRYFWD AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2740-FLAG-CARRYFWD.
           IF PBDTE-CARRYFWD-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBDTE-CARRYFWD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E845 CUST ID REQUIRED
      * E590 UNPOSTED AMT NEGATIVE
      * E875 UNPOSTED AMT OVER 1000000
      * E652 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2750-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBDTE-CUST-ID = SPACES
                   MOVE 'E845' TO WS-REJECT-CODE
               WHEN PBDTE-UNPOSTED-AMT < 0
                   MOVE 'E590' TO WS-REJECT-CODE
               WHEN PBDTE-UNPOSTED-AMT > 1000000.00
                   MOVE 'E875' TO WS-REJECT-CODE
               WHEN PBDTE-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E652' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E469 CUST ID REQUIRED
      * E722 INTEREST YTD NEGATIVE
      * E306 INTEREST YTD OVER 5000
      * E969 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2760-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBDTE-CUST-ID = SPACES
                   MOVE 'E469' TO WS-REJECT-CODE
               WHEN PBDTE-INTEREST-YTD < 0
                   MOVE 'E722' TO WS-REJECT-CODE
               WHEN PBDTE-INTEREST-YTD > 5000.00
                   MOVE 'E306' TO WS-REJECT-CODE
               WHEN PBDTE-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E969' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * ER -> GL WHEN MATURITY DATE IS PAST
      * GL -> PE WHEN MATURITY DATE IS PAST
      * PE -> RJ WHEN MATURITY DATE IS PAST
      * RJ -> VB WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2770-SET-REGION.
           EVALUATE PBDTE-REGION-CD
               WHEN 'ER'
                   IF PBDTE-MATURITY-DATE > 0 AND PBDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO PBDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF PBDTE-MATURITY-DATE > 0 AND PBDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PE' TO PBDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PE'
                   IF PBDTE-MATURITY-DATE > 0 AND PBDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO PBDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF PBDTE-MATURITY-DATE > 0 AND PBDTE-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VB' TO PBDTE-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2780-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBDTE-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBDTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO PBDTE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PRINTER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2790-DESCRIBE-PRINTER.
           EVALUATE PBDTE-PRINTER-CD
               WHEN 'CB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'EP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'EX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE UNPOSTED FEE ON THE CARRYFWD YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2800-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN PBDTE-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO PBDTE-UNPOSTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBDTE-EXEMPT-FLAG = 'Y'
                   COMPUTE PBDTE-UNPOSTED-AMT ROUNDED =
                       PBDTE-CARRYFWD-YTD * PBDTE-MARGIN / 200
               WHEN PBDTE-PRINTER-CD = 'CB'
                   MOVE 15.00 TO PBDTE-UNPOSTED-AMT
               WHEN PBDTE-PRINTER-CD = 'EP'
                   MOVE 7.00 TO PBDTE-UNPOSTED-AMT
               WHEN OTHER
                   COMPUTE PBDTE-UNPOSTED-AMT ROUNDED =
                       PBDTE-CARRYFWD-YTD * PBDTE-MARGIN / 100
           END-EVALUATE
           IF PBDTE-UNPOSTED-AMT > 0 AND PBDTE-UNPOSTED-AMT < 5.00
               MOVE 5.00 TO PBDTE-UNPOSTED-AMT
           END-IF
           IF PBDTE-UNPOSTED-AMT > 100.00
               MOVE 100.00 TO PBDTE-UNPOSTED-AMT
           END-IF
           ADD PBDTE-UNPOSTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE LINE BAND TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2810-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-LINE-BAND-RATE(1) TO PBDTE-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PBDTE-INTEREST-YTD NOT < WS-LINE-BAND-LOW(WS-TX)
                   MOVE WS-LINE-BAND-RATE(WS-TX) TO PBDTE-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBDTE-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBDTE-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CARRYFWD AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2820-ACCRUE-CARRYFWD.
           IF PBDTE-CARRYFWD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBDTE-CARRYFWD-AMT * PBDTE-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2830-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBDTE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBDTE-PRINTED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PBDTE-PRINTED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PBDTE-PRINTED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBDTE-PRINTED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBDTE-PRINTED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2840-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PBDTE-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBDTE-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PBDTE-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2850-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF PBDTE-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBDTE-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PBDTE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CARRYFWD YTD FEE ON THE PRINTED YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2860-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PBDTE-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PBDTE-CARRYFWD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBDTE-REVIEW-FLAG = 'Y'
                   COMPUTE PBDTE-CARRYFWD-YTD ROUNDED =
                       PBDTE-PRINTED-YTD * PBDTE-MARGIN / 200
               WHEN PBDTE-SOURCE-CD = 'JX'
                   MOVE 7.00 TO PBDTE-CARRYFWD-YTD
               WHEN PBDTE-SOURCE-CD = 'ZC'
                   MOVE 20.00 TO PBDTE-CARRYFWD-YTD
               WHEN OTHER
                   COMPUTE PBDTE-CARRYFWD-YTD ROUNDED =
                       PBDTE-PRINTED-YTD * PBDTE-MARGIN / 100
           END-EVALUATE
           IF PBDTE-CARRYFWD-YTD > 0 AND PBDTE-CARRYFWD-YTD < 2.00
               MOVE 2.00 TO PBDTE-CARRYFWD-YTD
           END-IF
           IF PBDTE-CARRYFWD-YTD > 35.00
               MOVE 35.00 TO PBDTE-CARRYFWD-YTD
           END-IF
           ADD PBDTE-CARRYFWD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2870-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PBDTE-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBDTE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PBDTE-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PRINTER CD:
      * CB -> EP WHEN NEXT DUE DATE IS PAST
      * EP -> EX WHEN NEXT DUE DATE IS PAST
      * EX -> JE WHEN NEXT DUE DATE IS PAST
      * JE -> MR WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2880-SET-PRINTER.
           EVALUATE PBDTE-PRINTER-CD
               WHEN 'CB'
                   IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EP' TO PBDTE-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EP'
                   IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EX' TO PBDTE-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JE' TO PBDTE-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF PBDTE-NEXT-DUE-DATE > 0 AND PBDTE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO PBDTE-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT UNPOSTED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2890-SPLIT-UNPOSTED.
           COMPUTE WS-WORK-AMT = PBDTE-UNPOSTED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PBDTE-UNPOSTED-AMT - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON UNPOSTED YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2900-ACCRUE-UNPOSTED.
           IF PBDTE-UNPOSTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBDTE-UNPOSTED-YTD * PBDTE-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBDTE-UNPOSTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CARRYFWD AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2910-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBDTE-BRANCH-CD
                   ADD PBDTE-CARRYFWD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBDTE-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBDTE-CARRYFWD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY PRINTER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2920-ACCUM-PRINTER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBDTE-PRINTER-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBDTE-PRINTER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2930-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF PBDTE-MATURITY-DATE > 0 AND PBDTE-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBDTE-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST GRID TIER FOR THE UNPOSTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2940-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-GRID-RATE(1) TO PBDTE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PBDTE-UNPOSTED-AMT NOT < WS-INTEREST-GRID-LOW(WS-TX)
                   MOVE WS-INTEREST-GRID-RATE(WS-TX) TO PBDTE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBDTE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBDTE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2950-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2960-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2970-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBDTE-BRANCH-CD
                   ADD PBDTE-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBDTE-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBDTE-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2980-FLAG-CARRYFWD.
           IF PBDTE-CARRYFWD-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBDTE-CARRYFWD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2990-FLAG-INTEREST.
           IF PBDTE-INTEREST-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBDTE-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3000-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE UNPOSTED YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3010-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PBDTE-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PBDTE-UNPOSTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBDTE-SUSPEND-FLAG = 'Y'
                   COMPUTE PBDTE-UNPOSTED-YTD ROUNDED = WS-WORK-AMT-2 *
                       PBDTE-FEE-PCT / 200
               WHEN PBDTE-REGION-CD = 'ER'
                   MOVE 5.00 TO PBDTE-UNPOSTED-YTD
               WHEN PBDTE-REGION-CD = 'GL'
                   MOVE 7.00 TO PBDTE-UNPOSTED-YTD
               WHEN OTHER
                   COMPUTE PBDTE-UNPOSTED-YTD ROUNDED = WS-WORK-AMT-2 *
                       PBDTE-FEE-PCT / 100
           END-EVALUATE
           IF PBDTE-UNPOSTED-YTD > 0 AND PBDTE-UNPOSTED-YTD < 1.00
               MOVE 1.00 TO PBDTE-UNPOSTED-YTD
           END-IF
           IF PBDTE-UNPOSTED-YTD > 100.00
               MOVE 100.00 TO PBDTE-UNPOSTED-YTD
           END-IF
           ADD PBDTE-UNPOSTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3020-DESCRIBE-BRANCH.
           EVALUATE PBDTE-BRANCH-CD
               WHEN 'DF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3030-COUNT-CYCLE.
           IF PBDTE-CYCLE-CNT < 9999999
               ADD 1 TO PBDTE-CYCLE-CNT
           END-IF
           IF PBDTE-CYCLE-CNT NOT < 12
               MOVE 'Y' TO PBDTE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CARRYFWD YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3040-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBDTE-BRANCH-CD
                   ADD PBDTE-CARRYFWD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBDTE-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBDTE-CARRYFWD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3050-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBDTE-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBDTE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO PBDTE-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * UNPOSTED AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3060-FLAG-UNPOSTED.
           IF PBDTE-UNPOSTED-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBDTE-UNPOSTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3070-SPLIT-CARRYFWD.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE PBDTE-CARRYFWD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO PBDTE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO PBDTE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO PBDTE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'PBDTE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
