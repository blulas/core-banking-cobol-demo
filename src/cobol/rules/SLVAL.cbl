      *================================================================*
      * PROGRAM  : SLVAL                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : STUDENT LOANS RULE - STATUS. CALLED BY THE STUDENT  *
      *            LOANS BATCH AND ONLINE PROGRAMS.                    *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1987-12-01 MKL  ORIGINAL                                       *
      * 1988-04-07 RJT  PERFORMANCE - PRINCIPAL READ AHEAD INC6767     *
      * 1988-11-25 LMB  DISBURSE CUTOFF MOVED CHG9261                  *
      * 1991-04-06 TWH  DEFERINT TABLE EXPANDED INC3206                *
      * 1994-05-21 GAF  NEW PRINCIPAL REPORT COLUMNS INC5810           *
      * 1996-01-02 LMB  PRINCIPAL TABLE EXPANDED INC3987               *
      * 1997-04-22 JLM  NEW DISBURSE REPORT COLUMNS PRB6430            *
      * 2001-11-28 LMB  DISBURSE TABLE EXPANDED INC8828                *
      * 2001-06-15 TWH  AUDIT FINDING - DEFERINT CONTROL TOTALS REQ1159*
      * 2002-02-24 RJT  PERFORMANCE - DISBURSE READ AHEAD CHG3455      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SLVAL.
       AUTHOR. B H SANDERS.

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
                                           VALUE 17.000000.
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
      * WS-SUBSIDY-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SUBSIDY-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000230500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000296500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000255000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000219000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011250000000067000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011350000000728000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016350000000187000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021350000000546000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021400000000794500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021450000000901500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021550000000589500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021800000000744000'.
       01  WS-SUBSIDY-SCHED-TBL REDEFINES WS-SUBSIDY-SCHED-VALUES.
           05  WS-SUBSIDY-SCHED-ROW OCCURS 12 TIMES.
               10  WS-SUBSIDY-SCHED-LOW    PIC 9(09)V99.
               10  WS-SUBSIDY-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DEFERINT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEFERINT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000712500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000880500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000849000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000800000000581000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005800000000883000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005900000000268000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005950000000531000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006200000000431500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006700000000812000'.
       01  WS-DEFERINT-SCHED-TBL REDEFINES WS-DEFERINT-SCHED-VALUES.
           05  WS-DEFERINT-SCHED-ROW OCCURS 9 TIMES.
               10  WS-DEFERINT-SCHED-LOW   PIC 9(09)V99.
               10  WS-DEFERINT-SCHED-RATE  PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY SLVALP.

       PROCEDURE DIVISION USING SLVAL-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-CHECK-LAST-MAINT
           PERFORM 2010-PRORATE-CAPINT
           PERFORM 2020-SELECT-DISCOUNT
           PERFORM 2030-SET-DEFER-CODE
           PERFORM 2040-FLAG-CAPINT
           PERFORM 2050-SELECT-FEE-PCT
           PERFORM 2060-AGE-EXPIRY
           PERFORM 2070-SELECT-DISCOUNT
           PERFORM 2080-DESCRIBE-CYCLE
           PERFORM 2090-AGE-EXPIRY
           PERFORM 2100-PRORATE-SUBSIDY
           PERFORM 2110-EDIT-CUST-ID
           PERFORM 2120-EDIT-CUST-ID
           PERFORM 2130-SET-DEFER-CODE
           PERFORM 2140-ASSESS-SUSPEND
           PERFORM 2150-ASSESS-REVIEW
           PERFORM 2160-COUNT-ITEM
           PERFORM 2170-COUNT-PAYMENT
           PERFORM 2180-AGE-EXPIRY
           PERFORM 2190-ACCUM-SOURCE
           PERFORM 2200-EDIT-DESC
           PERFORM 2210-EDIT-DESC
           PERFORM 2220-DESCRIBE-DEFER-CODE
           PERFORM 2230-FLAG-ITEM
           PERFORM 2240-ACCRUE-DISBURSE
           PERFORM 2250-SET-CYCLE
           PERFORM 2260-ASSESS-REVIEW
           PERFORM 2270-EDIT-CUST-ID
           PERFORM 2280-CHECK-EXPIRY
           PERFORM 2290-CHECK-EXPIRY
           PERFORM 2300-SELECT-FEE-PCT
           PERFORM 2310-EDIT-DESC
           PERFORM 2320-ACCRUE-DISBURSE
           PERFORM 2330-ACCRUE-CAPINT
           PERFORM 2340-EDIT-CUST-ID
           PERFORM 2350-FLAG-CAPINT
           PERFORM 2360-DESCRIBE-DEFER-CODE
           PERFORM 2370-SET-SOURCE
           PERFORM 2380-ACCRUE-CAPINT
           PERFORM 2390-AGE-EXPIRY
           PERFORM 2400-CHECK-LAST-PAY
           PERFORM 2410-EDIT-DESC
           PERFORM 2420-SPLIT-PAYMENT
           PERFORM 2430-ASSESS-REVIEW
           PERFORM 2440-SELECT-FEE-PCT
           PERFORM 2450-ASSESS-SUSPEND
           PERFORM 2460-DESCRIBE-DEFER-CODE
           PERFORM 2470-DESCRIBE-SOURCE
           PERFORM 2480-CHECK-LAST-PAY
           PERFORM 2490-DESCRIBE-LOAN-TYPE
           PERFORM 2500-SELECT-FEE-PCT
           PERFORM 2510-ASSESS-SUSPEND
           PERFORM 2520-CHECK-LAST-MAINT
           PERFORM 2530-EDIT-DESC
           PERFORM 2540-LIMIT-SUBSIDY
           PERFORM 2550-EDIT-DESC
           PERFORM 2560-SELECT-DISCOUNT
           PERFORM 2570-COUNT-ITEM
           PERFORM 2580-ACCUM-CYCLE
           PERFORM 2590-AGE-EXPIRY
           PERFORM 2600-CHECK-LAST-PAY
           PERFORM 2610-ACCRUE-CAPINT
           PERFORM 2620-SPLIT-CAPINT
           PERFORM 2630-SELECT-DISCOUNT
           PERFORM 2640-FLAG-SUBSIDY
           PERFORM 2650-COUNT-ITEM
           PERFORM 2660-LIMIT-CAPINT
           PERFORM 2670-EDIT-DESC
           PERFORM 2680-AGE-LAST-MAINT
           PERFORM 2690-SELECT-DISCOUNT
           PERFORM 2700-SPLIT-DISBURSE
           PERFORM 2710-ASSESS-SUSPEND
           PERFORM 2720-ACCRUE-PAYMENT
           PERFORM 2730-FLAG-DISBURSE
           PERFORM 2740-ASSESS-PREFERRED
           PERFORM 2750-SET-LOAN-TYPE
           PERFORM 2760-LIMIT-CAPINT
           PERFORM 2770-ASSESS-REVIEW
           PERFORM 2780-FLAG-ITEM
           PERFORM 2790-ACCUM-SOURCE
           PERFORM 2800-CHECK-LAST-ACT
           PERFORM 2810-CHECK-EXPIRY
           PERFORM 2820-EDIT-DESC
           PERFORM 2830-ASSESS-PREFERRED
           PERFORM 2840-EDIT-DESC
           PERFORM 2850-CHECK-LAST-ACT
           PERFORM 2860-FLAG-ITEM
           PERFORM 2870-DESCRIBE-SOURCE
           PERFORM 2880-SPLIT-CAPINT
           PERFORM 2890-COUNT-PAYMENT
           PERFORM 2900-SELECT-FEE-PCT
           PERFORM 2910-CHECK-LAST-MAINT
           PERFORM 2920-FLAG-DEFERINT
           PERFORM 2930-ACCRUE-DISCOUNT
           PERFORM 2940-ASSESS-PREFERRED
           PERFORM 2950-PRORATE-CAPINT
           PERFORM 2960-AGE-EXPIRY
           PERFORM 2970-SPLIT-SUBSIDY
           PERFORM 2980-PRORATE-DEFERINT
           PERFORM 2990-SELECT-DISCOUNT
           PERFORM 3000-AGE-LAST-MAINT
           PERFORM 3010-SET-CYCLE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO SLVAL-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2000-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SLVAL-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO SLVAL-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SUBSIDY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CAPINT YTD.
      *----------------------------------------------------------------*
       2010-PRORATE-CAPINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SLVAL-CAPINT-YTD ROUNDED =
                   SLVAL-SUBSIDY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SLVAL-CAPINT-YTD
           END-IF
           ADD SLVAL-CAPINT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT SCHED TIER FOR THE CAPINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2020-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-SCHED-RATE(1) TO SLVAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SLVAL-CAPINT-AMT NOT < WS-DEFERINT-SCHED-LOW(WS-TX)
                   MOVE WS-DEFERINT-SCHED-RATE(WS-TX) TO SLVAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SLVAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SLVAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEFER CODE CD:
      * AH -> CZ WHEN EXPIRY DATE IS PAST
      * CZ -> GM WHEN EXPIRY DATE IS PAST
      * GM -> ND WHEN EXPIRY DATE IS PAST
      * ND -> UC WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2030-SET-DEFER-CODE.
           EVALUATE SLVAL-DEFER-CODE-CD
               WHEN 'AH'
                   IF SLVAL-EXPIRY-DATE > 0 AND SLVAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CZ' TO SLVAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CZ'
                   IF SLVAL-EXPIRY-DATE > 0 AND SLVAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO SLVAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF SLVAL-EXPIRY-DATE > 0 AND SLVAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ND' TO SLVAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF SLVAL-EXPIRY-DATE > 0 AND SLVAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO SLVAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CAPINT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2040-FLAG-CAPINT.
           IF SLVAL-CAPINT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SLVAL-CAPINT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT SCHED TIER FOR THE CAPINT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2050-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-SCHED-RATE(1) TO SLVAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SLVAL-CAPINT-YTD NOT < WS-DEFERINT-SCHED-LOW(WS-TX)
                   MOVE WS-DEFERINT-SCHED-RATE(WS-TX) TO SLVAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SLVAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SLVAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2060-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF SLVAL-EXPIRY-DATE > 0 AND SLVAL-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLVAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SLVAL-DEFERINT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD SLVAL-DEFERINT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SLVAL-DEFERINT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SLVAL-DEFERINT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SLVAL-DEFERINT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT SCHED TIER FOR THE CAPINT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2070-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-SCHED-RATE(1) TO SLVAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SLVAL-CAPINT-YTD NOT < WS-DEFERINT-SCHED-LOW(WS-TX)
                   MOVE WS-DEFERINT-SCHED-RATE(WS-TX) TO SLVAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SLVAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SLVAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2080-DESCRIBE-CYCLE.
           EVALUATE SLVAL-CYCLE-CD
               WHEN 'CFJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KHC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SEU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XKA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2090-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF SLVAL-EXPIRY-DATE > 0 AND SLVAL-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLVAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CAPINT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SUBSIDY YTD.
      *----------------------------------------------------------------*
       2100-PRORATE-SUBSIDY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SLVAL-SUBSIDY-YTD ROUNDED =
                   SLVAL-CAPINT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SLVAL-SUBSIDY-YTD
           END-IF
           ADD SLVAL-SUBSIDY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E047 CUST ID REQUIRED
      * E075 CAPINT AMT NEGATIVE
      * E020 CAPINT AMT OVER 10000
      * E676 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2110-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLVAL-CUST-ID = SPACES
                   MOVE 'E047' TO WS-REJECT-CODE
               WHEN SLVAL-CAPINT-AMT < 0
                   MOVE 'E075' TO WS-REJECT-CODE
               WHEN SLVAL-CAPINT-AMT > 10000.00
                   MOVE 'E020' TO WS-REJECT-CODE
               WHEN SLVAL-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E676' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E933 CUST ID REQUIRED
      * E713 CAPINT YTD NEGATIVE
      * E111 CAPINT YTD OVER 1000000
      * E822 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2120-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLVAL-CUST-ID = SPACES
                   MOVE 'E933' TO WS-REJECT-CODE
               WHEN SLVAL-CAPINT-YTD < 0
                   MOVE 'E713' TO WS-REJECT-CODE
               WHEN SLVAL-CAPINT-YTD > 1000000.00
                   MOVE 'E111' TO WS-REJECT-CODE
               WHEN SLVAL-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E822' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEFER CODE CD:
      * AH -> CZ WHEN LAST MAINT DATE IS PAST
      * CZ -> GM WHEN LAST MAINT DATE IS PAST
      * GM -> ND WHEN LAST MAINT DATE IS PAST
      * ND -> UC WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2130-SET-DEFER-CODE.
           EVALUATE SLVAL-DEFER-CODE-CD
               WHEN 'AH'
                   IF SLVAL-LAST-MAINT-DATE > 0 AND
                       SLVAL-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'CZ' TO SLVAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CZ'
                   IF SLVAL-LAST-MAINT-DATE > 0 AND
                       SLVAL-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'GM' TO SLVAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF SLVAL-LAST-MAINT-DATE > 0 AND
                       SLVAL-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'ND' TO SLVAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF SLVAL-LAST-MAINT-DATE > 0 AND
                       SLVAL-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'UC' TO SLVAL-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT YTD FEE ON THE DEFERINT YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2140-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN SLVAL-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO SLVAL-CAPINT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLVAL-PREFERRED-FLAG = 'Y'
                   COMPUTE SLVAL-CAPINT-YTD ROUNDED = SLVAL-DEFERINT-YTD
                       * SLVAL-DISCOUNT / 200
               WHEN SLVAL-DEFER-CODE-CD = 'AH'
                   MOVE 15.00 TO SLVAL-CAPINT-YTD
               WHEN SLVAL-DEFER-CODE-CD = 'CZ'
                   MOVE 10.00 TO SLVAL-CAPINT-YTD
               WHEN OTHER
                   COMPUTE SLVAL-CAPINT-YTD ROUNDED = SLVAL-DEFERINT-YTD
                       * SLVAL-DISCOUNT / 100
           END-EVALUATE
           IF SLVAL-CAPINT-YTD > 0 AND SLVAL-CAPINT-YTD < 2.00
               MOVE 2.00 TO SLVAL-CAPINT-YTD
           END-IF
           IF SLVAL-CAPINT-YTD > 75.00
               MOVE 75.00 TO SLVAL-CAPINT-YTD
           END-IF
           ADD SLVAL-CAPINT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE DISBURSE YTD FEE ON THE CAPINT YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2150-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SLVAL-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SLVAL-DISBURSE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLVAL-SUSPEND-FLAG = 'Y'
                   COMPUTE SLVAL-DISBURSE-YTD ROUNDED = SLVAL-CAPINT-YTD
                       * SLVAL-FEE-PCT / 200
               WHEN SLVAL-SOURCE-CD = 'AST'
                   MOVE 12.00 TO SLVAL-DISBURSE-YTD
               WHEN SLVAL-SOURCE-CD = 'BCK'
                   MOVE 3.00 TO SLVAL-DISBURSE-YTD
               WHEN OTHER
                   COMPUTE SLVAL-DISBURSE-YTD ROUNDED = SLVAL-CAPINT-YTD
                       * SLVAL-FEE-PCT / 100
           END-EVALUATE
           IF SLVAL-DISBURSE-YTD > 0 AND SLVAL-DISBURSE-YTD < 10.00
               MOVE 10.00 TO SLVAL-DISBURSE-YTD
           END-IF
           IF SLVAL-DISBURSE-YTD > 25.00
               MOVE 25.00 TO SLVAL-DISBURSE-YTD
           END-IF
           ADD SLVAL-DISBURSE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2160-COUNT-ITEM.
           IF SLVAL-ITEM-CNT < 9999999
               ADD 1 TO SLVAL-ITEM-CNT
           END-IF
           IF SLVAL-ITEM-CNT NOT < 12
               MOVE 'Y' TO SLVAL-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2170-COUNT-PAYMENT.
           IF SLVAL-PAYMENT-CNT < 9999999
               ADD 1 TO SLVAL-PAYMENT-CNT
           END-IF
           IF SLVAL-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO SLVAL-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2180-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF SLVAL-EXPIRY-DATE > 0 AND SLVAL-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLVAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SLVAL-DISBURSE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SLVAL-DISBURSE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SLVAL-DISBURSE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SLVAL-DISBURSE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SLVAL-DISBURSE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2190-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLVAL-SOURCE-CD
                   ADD SLVAL-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLVAL-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SLVAL-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E793 DESC REQUIRED
      * E144 WORK AMT 2 NEGATIVE
      * E436 WORK AMT 2 OVER 1000000
      * E644 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2200-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLVAL-DESC = SPACES
                   MOVE 'E793' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E144' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E436' TO WS-REJECT-CODE
               WHEN SLVAL-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E644' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E481 DESC REQUIRED
      * E296 DEFERINT YTD NEGATIVE
      * E735 DEFERINT YTD OVER 50000
      * E920 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2210-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLVAL-DESC = SPACES
                   MOVE 'E481' TO WS-REJECT-CODE
               WHEN SLVAL-DEFERINT-YTD < 0
                   MOVE 'E296' TO WS-REJECT-CODE
               WHEN SLVAL-DEFERINT-YTD > 50000.00
                   MOVE 'E735' TO WS-REJECT-CODE
               WHEN SLVAL-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E920' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DEFER CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2220-DESCRIBE-DEFER-CODE.
           EVALUATE SLVAL-DEFER-CODE-CD
               WHEN 'AH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ND'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2230-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2240-ACCRUE-DISBURSE.
           IF SLVAL-DISBURSE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SLVAL-DISBURSE-YTD * SLVAL-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CFJ -> KHC WHEN LAST ACT DATE IS PAST
      * KHC -> SEU WHEN LAST ACT DATE IS PAST
      * SEU -> XKA WHEN LAST ACT DATE IS PAST
      * XKA -> CFJ WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2250-SET-CYCLE.
           EVALUATE SLVAL-CYCLE-CD
               WHEN 'CFJ'
                   IF SLVAL-LAST-ACT-DATE > 0 AND SLVAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHC' TO SLVAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KHC'
                   IF SLVAL-LAST-ACT-DATE > 0 AND SLVAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SEU' TO SLVAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SEU'
                   IF SLVAL-LAST-ACT-DATE > 0 AND SLVAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XKA' TO SLVAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XKA'
                   IF SLVAL-LAST-ACT-DATE > 0 AND SLVAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CFJ' TO SLVAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2260-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SLVAL-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SLVAL-CAPINT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLVAL-SUSPEND-FLAG = 'Y'
                   COMPUTE SLVAL-CAPINT-YTD ROUNDED = WS-WORK-AMT-2 *
                       SLVAL-FEE-PCT / 200
               WHEN SLVAL-DEFER-CODE-CD = 'AH'
                   MOVE 7.00 TO SLVAL-CAPINT-YTD
               WHEN SLVAL-DEFER-CODE-CD = 'CZ'
                   MOVE 5.00 TO SLVAL-CAPINT-YTD
               WHEN OTHER
                   COMPUTE SLVAL-CAPINT-YTD ROUNDED = WS-WORK-AMT-2 *
                       SLVAL-FEE-PCT / 100
           END-EVALUATE
           IF SLVAL-CAPINT-YTD > 0 AND SLVAL-CAPINT-YTD < 10.00
               MOVE 10.00 TO SLVAL-CAPINT-YTD
           END-IF
           IF SLVAL-CAPINT-YTD > 75.00
               MOVE 75.00 TO SLVAL-CAPINT-YTD
           END-IF
           ADD SLVAL-CAPINT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E653 CUST ID REQUIRED
      * E156 DISBURSE YTD NEGATIVE
      * E546 DISBURSE YTD OVER 10000
      * E678 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2270-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLVAL-CUST-ID = SPACES
                   MOVE 'E653' TO WS-REJECT-CODE
               WHEN SLVAL-DISBURSE-YTD < 0
                   MOVE 'E156' TO WS-REJECT-CODE
               WHEN SLVAL-DISBURSE-YTD > 10000.00
                   MOVE 'E546' TO WS-REJECT-CODE
               WHEN SLVAL-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E678' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2280-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF SLVAL-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLVAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SLVAL-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2290-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF SLVAL-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLVAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO SLVAL-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2300-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-SCHED-RATE(1) TO SLVAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-DEFERINT-SCHED-LOW(WS-TX)
                   MOVE WS-DEFERINT-SCHED-RATE(WS-TX) TO SLVAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SLVAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SLVAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E540 DESC REQUIRED
      * E467 DEFERINT YTD NEGATIVE
      * E092 DEFERINT YTD OVER 250000
      * E377 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2310-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLVAL-DESC = SPACES
                   MOVE 'E540' TO WS-REJECT-CODE
               WHEN SLVAL-DEFERINT-YTD < 0
                   MOVE 'E467' TO WS-REJECT-CODE
               WHEN SLVAL-DEFERINT-YTD > 250000.00
                   MOVE 'E092' TO WS-REJECT-CODE
               WHEN SLVAL-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E377' TO WS-REJECT-CODE
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
       2320-ACCRUE-DISBURSE.
           IF SLVAL-DISBURSE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SLVAL-DISBURSE-YTD * SLVAL-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO SLVAL-CAPINT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CAPINT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2330-ACCRUE-CAPINT.
           IF SLVAL-CAPINT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SLVAL-CAPINT-AMT * SLVAL-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SLVAL-CAPINT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E644 CUST ID REQUIRED
      * E232 DEFERINT YTD NEGATIVE
      * E882 DEFERINT YTD OVER 250000
      * E943 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2340-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLVAL-CUST-ID = SPACES
                   MOVE 'E644' TO WS-REJECT-CODE
               WHEN SLVAL-DEFERINT-YTD < 0
                   MOVE 'E232' TO WS-REJECT-CODE
               WHEN SLVAL-DEFERINT-YTD > 250000.00
                   MOVE 'E882' TO WS-REJECT-CODE
               WHEN SLVAL-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E943' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CAPINT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2350-FLAG-CAPINT.
           IF SLVAL-CAPINT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SLVAL-CAPINT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DEFER CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2360-DESCRIBE-DEFER-CODE.
           EVALUATE SLVAL-DEFER-CODE-CD
               WHEN 'AH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'CZ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ND'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AST -> BCK WHEN LAST ACT DATE IS PAST
      * BCK -> CJG WHEN LAST ACT DATE IS PAST
      * CJG -> DSJ WHEN LAST ACT DATE IS PAST
      * DSJ -> JVY WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2370-SET-SOURCE.
           EVALUATE SLVAL-SOURCE-CD
               WHEN 'AST'
                   IF SLVAL-LAST-ACT-DATE > 0 AND SLVAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BCK' TO SLVAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BCK'
                   IF SLVAL-LAST-ACT-DATE > 0 AND SLVAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CJG' TO SLVAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJG'
                   IF SLVAL-LAST-ACT-DATE > 0 AND SLVAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DSJ' TO SLVAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DSJ'
                   IF SLVAL-LAST-ACT-DATE > 0 AND SLVAL-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JVY' TO SLVAL-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CAPINT YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2380-ACCRUE-CAPINT.
           IF SLVAL-CAPINT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SLVAL-CAPINT-YTD * SLVAL-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2390-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF SLVAL-EXPIRY-DATE > 0 AND SLVAL-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLVAL-EXPIRY-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2400-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF SLVAL-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLVAL-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO SLVAL-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E835 DESC REQUIRED
      * E288 WORK AMT NEGATIVE
      * E005 WORK AMT OVER 5000
      * E665 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2410-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLVAL-DESC = SPACES
                   MOVE 'E835' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E288' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E005' TO WS-REJECT-CODE
               WHEN SLVAL-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E665' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2420-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = SLVAL-PAYMENT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = SLVAL-PAYMENT-YTD - (WS-WORK-AMT * 3)
           COMPUTE SLVAL-DISBURSE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE CAPINT YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2430-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SLVAL-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLVAL-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SLVAL-CAPINT-YTD *
                       SLVAL-DISCOUNT / 200
               WHEN SLVAL-DEFER-CODE-CD = 'AH'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN SLVAL-DEFER-CODE-CD = 'CZ'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SLVAL-CAPINT-YTD *
                       SLVAL-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT SCHED TIER FOR THE CAPINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2440-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-SCHED-RATE(1) TO SLVAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SLVAL-CAPINT-AMT NOT < WS-DEFERINT-SCHED-LOW(WS-TX)
                   MOVE WS-DEFERINT-SCHED-RATE(WS-TX) TO SLVAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SLVAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SLVAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE CAPINT YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2450-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN SLVAL-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLVAL-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SLVAL-CAPINT-YTD *
                       SLVAL-DISCOUNT / 200
               WHEN SLVAL-DEFER-CODE-CD = 'AH'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN SLVAL-DEFER-CODE-CD = 'CZ'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SLVAL-CAPINT-YTD *
                       SLVAL-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE DEFER CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2460-DESCRIBE-DEFER-CODE.
           EVALUATE SLVAL-DEFER-CODE-CD
               WHEN 'AH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ND'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2470-DESCRIBE-SOURCE.
           EVALUATE SLVAL-SOURCE-CD
               WHEN 'AST'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BCK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CJG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DSJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JVY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VEF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2480-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF SLVAL-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLVAL-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO SLVAL-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LOAN TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2490-DESCRIBE-LOAN-TYPE.
           EVALUATE SLVAL-LOAN-TYPE-CD
               WHEN 'CS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SUBSIDY SCHED TIER FOR THE CAPINT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2500-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SUBSIDY-SCHED-RATE(1) TO SLVAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SLVAL-CAPINT-YTD NOT < WS-SUBSIDY-SCHED-LOW(WS-TX)
                   MOVE WS-SUBSIDY-SCHED-RATE(WS-TX) TO SLVAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SLVAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SLVAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE DEFERINT YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2510-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN SLVAL-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLVAL-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SLVAL-DEFERINT-YTD *
                       SLVAL-DISCOUNT / 200
               WHEN SLVAL-DEFER-CODE-CD = 'AH'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN SLVAL-DEFER-CODE-CD = 'CZ'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SLVAL-DEFERINT-YTD *
                       SLVAL-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2520-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SLVAL-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SLVAL-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E513 DESC REQUIRED
      * E231 CAPINT AMT NEGATIVE
      * E755 CAPINT AMT OVER 1000000
      * E240 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2530-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLVAL-DESC = SPACES
                   MOVE 'E513' TO WS-REJECT-CODE
               WHEN SLVAL-CAPINT-AMT < 0
                   MOVE 'E231' TO WS-REJECT-CODE
               WHEN SLVAL-CAPINT-AMT > 1000000.00
                   MOVE 'E755' TO WS-REJECT-CODE
               WHEN SLVAL-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E240' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SUBSIDY YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2540-LIMIT-SUBSIDY.
           IF SLVAL-SUBSIDY-YTD < 0.00
               MOVE 0.00 TO SLVAL-SUBSIDY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SLVAL-SUBSIDY-YTD > 500.00
               MOVE 500.00 TO SLVAL-SUBSIDY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E989 DESC REQUIRED
      * E694 WORK AMT 2 NEGATIVE
      * E571 WORK AMT 2 OVER 99999
      * E857 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2550-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLVAL-DESC = SPACES
                   MOVE 'E989' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E694' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E571' TO WS-REJECT-CODE
               WHEN SLVAL-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E857' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT SCHED TIER FOR THE CAPINT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2560-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-SCHED-RATE(1) TO SLVAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SLVAL-CAPINT-YTD NOT < WS-DEFERINT-SCHED-LOW(WS-TX)
                   MOVE WS-DEFERINT-SCHED-RATE(WS-TX) TO SLVAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SLVAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SLVAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2570-COUNT-ITEM.
           IF SLVAL-ITEM-CNT < 9999999
               ADD 1 TO SLVAL-ITEM-CNT
           END-IF
           IF SLVAL-ITEM-CNT NOT < 6
               MOVE 'Y' TO SLVAL-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CAPINT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2580-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLVAL-CYCLE-CD
                   ADD SLVAL-CAPINT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLVAL-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SLVAL-CAPINT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2590-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF SLVAL-EXPIRY-DATE > 0 AND SLVAL-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLVAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SLVAL-DEFERINT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SLVAL-DEFERINT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SLVAL-DEFERINT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SLVAL-DEFERINT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SLVAL-DEFERINT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2600-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF SLVAL-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLVAL-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO SLVAL-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CAPINT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2610-ACCRUE-CAPINT.
           IF SLVAL-CAPINT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SLVAL-CAPINT-AMT * SLVAL-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO SLVAL-PAYMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2620-SPLIT-CAPINT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE SLVAL-CAPINT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2630-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-SCHED-RATE(1) TO SLVAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-DEFERINT-SCHED-LOW(WS-TX)
                   MOVE WS-DEFERINT-SCHED-RATE(WS-TX) TO SLVAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SLVAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SLVAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SUBSIDY YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2640-FLAG-SUBSIDY.
           IF SLVAL-SUBSIDY-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SLVAL-SUBSIDY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2650-COUNT-ITEM.
           IF SLVAL-ITEM-CNT < 9999999
               ADD 1 TO SLVAL-ITEM-CNT
           END-IF
           IF SLVAL-ITEM-CNT NOT < 3
               MOVE 'Y' TO SLVAL-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CAPINT AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2660-LIMIT-CAPINT.
           IF SLVAL-CAPINT-AMT < 10.00
               MOVE 10.00 TO SLVAL-CAPINT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SLVAL-CAPINT-AMT > 5000.00
               MOVE 5000.00 TO SLVAL-CAPINT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E466 DESC REQUIRED
      * E891 SUBSIDY YTD NEGATIVE
      * E944 SUBSIDY YTD OVER 99999
      * E214 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2670-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLVAL-DESC = SPACES
                   MOVE 'E466' TO WS-REJECT-CODE
               WHEN SLVAL-SUBSIDY-YTD < 0
                   MOVE 'E891' TO WS-REJECT-CODE
               WHEN SLVAL-SUBSIDY-YTD > 99999.00
                   MOVE 'E944' TO WS-REJECT-CODE
               WHEN SLVAL-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E214' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2680-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SLVAL-LAST-MAINT-DATE > 0 AND SLVAL-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SLVAL-DEFERINT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD SLVAL-DEFERINT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SLVAL-DEFERINT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SLVAL-DEFERINT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SLVAL-DEFERINT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT SCHED TIER FOR THE CAPINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2690-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-SCHED-RATE(1) TO SLVAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SLVAL-CAPINT-AMT NOT < WS-DEFERINT-SCHED-LOW(WS-TX)
                   MOVE WS-DEFERINT-SCHED-RATE(WS-TX) TO SLVAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SLVAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SLVAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2700-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = SLVAL-DISBURSE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = SLVAL-DISBURSE-YTD - (WS-WORK-AMT *
               3)
           COMPUTE SLVAL-CAPINT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2710-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN SLVAL-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLVAL-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       SLVAL-FEE-PCT / 200
               WHEN SLVAL-CYCLE-CD = 'CFJ'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN SLVAL-CYCLE-CD = 'KHC'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       SLVAL-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2720-ACCRUE-PAYMENT.
           IF SLVAL-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SLVAL-PAYMENT-YTD * SLVAL-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SLVAL-DISBURSE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2730-FLAG-DISBURSE.
           IF SLVAL-DISBURSE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SLVAL-DISBURSE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE DEFERINT YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2740-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SLVAL-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SLVAL-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLVAL-SUSPEND-FLAG = 'Y'
                   COMPUTE SLVAL-PAYMENT-YTD ROUNDED =
                       SLVAL-DEFERINT-YTD * SLVAL-FEE-PCT / 200
               WHEN SLVAL-CYCLE-CD = 'CFJ'
                   MOVE 12.00 TO SLVAL-PAYMENT-YTD
               WHEN SLVAL-CYCLE-CD = 'KHC'
                   MOVE 10.00 TO SLVAL-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE SLVAL-PAYMENT-YTD ROUNDED =
                       SLVAL-DEFERINT-YTD * SLVAL-FEE-PCT / 100
           END-EVALUATE
           IF SLVAL-PAYMENT-YTD > 0 AND SLVAL-PAYMENT-YTD < 2.00
               MOVE 2.00 TO SLVAL-PAYMENT-YTD
           END-IF
           IF SLVAL-PAYMENT-YTD > 150.00
               MOVE 150.00 TO SLVAL-PAYMENT-YTD
           END-IF
           ADD SLVAL-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOAN TYPE CD:
      * CS -> DV WHEN LAST PAY DATE IS PAST
      * DV -> JL WHEN LAST PAY DATE IS PAST
      * JL -> NE WHEN LAST PAY DATE IS PAST
      * NE -> SH WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2750-SET-LOAN-TYPE.
           EVALUATE SLVAL-LOAN-TYPE-CD
               WHEN 'CS'
                   IF SLVAL-LAST-PAY-DATE > 0 AND SLVAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DV' TO SLVAL-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DV'
                   IF SLVAL-LAST-PAY-DATE > 0 AND SLVAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO SLVAL-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JL'
                   IF SLVAL-LAST-PAY-DATE > 0 AND SLVAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NE' TO SLVAL-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NE'
                   IF SLVAL-LAST-PAY-DATE > 0 AND SLVAL-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO SLVAL-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CAPINT YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2760-LIMIT-CAPINT.
           IF SLVAL-CAPINT-YTD < 50.00
               MOVE 50.00 TO SLVAL-CAPINT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SLVAL-CAPINT-YTD > 50000.00
               MOVE 50000.00 TO SLVAL-CAPINT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT YTD FEE ON THE SUBSIDY YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2770-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SLVAL-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SLVAL-CAPINT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLVAL-SUSPEND-FLAG = 'Y'
                   COMPUTE SLVAL-CAPINT-YTD ROUNDED = SLVAL-SUBSIDY-YTD
                       * SLVAL-DISCOUNT / 200
               WHEN SLVAL-LOAN-TYPE-CD = 'CS'
                   MOVE 7.00 TO SLVAL-CAPINT-YTD
               WHEN SLVAL-LOAN-TYPE-CD = 'DV'
                   MOVE 5.00 TO SLVAL-CAPINT-YTD
               WHEN OTHER
                   COMPUTE SLVAL-CAPINT-YTD ROUNDED = SLVAL-SUBSIDY-YTD
                       * SLVAL-DISCOUNT / 100
           END-EVALUATE
           IF SLVAL-CAPINT-YTD > 0 AND SLVAL-CAPINT-YTD < 1.00
               MOVE 1.00 TO SLVAL-CAPINT-YTD
           END-IF
           IF SLVAL-CAPINT-YTD > 250.00
               MOVE 250.00 TO SLVAL-CAPINT-YTD
           END-IF
           ADD SLVAL-CAPINT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2780-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2790-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SLVAL-SOURCE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SLVAL-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2800-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SLVAL-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLVAL-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO SLVAL-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2810-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF SLVAL-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLVAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO SLVAL-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E294 DESC REQUIRED
      * E331 CAPINT YTD NEGATIVE
      * E095 CAPINT YTD OVER 5000
      * E744 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2820-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLVAL-DESC = SPACES
                   MOVE 'E294' TO WS-REJECT-CODE
               WHEN SLVAL-CAPINT-YTD < 0
                   MOVE 'E331' TO WS-REJECT-CODE
               WHEN SLVAL-CAPINT-YTD > 5000.00
                   MOVE 'E095' TO WS-REJECT-CODE
               WHEN SLVAL-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E744' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT YTD FEE ON THE DEFERINT YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2830-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SLVAL-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SLVAL-CAPINT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLVAL-REVIEW-FLAG = 'Y'
                   COMPUTE SLVAL-CAPINT-YTD ROUNDED = SLVAL-DEFERINT-YTD
                       * SLVAL-FEE-PCT / 200
               WHEN SLVAL-CYCLE-CD = 'CFJ'
                   MOVE 5.00 TO SLVAL-CAPINT-YTD
               WHEN SLVAL-CYCLE-CD = 'KHC'
                   MOVE 3.00 TO SLVAL-CAPINT-YTD
               WHEN OTHER
                   COMPUTE SLVAL-CAPINT-YTD ROUNDED = SLVAL-DEFERINT-YTD
                       * SLVAL-FEE-PCT / 100
           END-EVALUATE
           IF SLVAL-CAPINT-YTD > 0 AND SLVAL-CAPINT-YTD < 5.00
               MOVE 5.00 TO SLVAL-CAPINT-YTD
           END-IF
           IF SLVAL-CAPINT-YTD > 150.00
               MOVE 150.00 TO SLVAL-CAPINT-YTD
           END-IF
           ADD SLVAL-CAPINT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E793 DESC REQUIRED
      * E586 CAPINT YTD NEGATIVE
      * E433 CAPINT YTD OVER 1000000
      * E535 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2840-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SLVAL-DESC = SPACES
                   MOVE 'E793' TO WS-REJECT-CODE
               WHEN SLVAL-CAPINT-YTD < 0
                   MOVE 'E586' TO WS-REJECT-CODE
               WHEN SLVAL-CAPINT-YTD > 1000000.00
                   MOVE 'E433' TO WS-REJECT-CODE
               WHEN SLVAL-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E535' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2850-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SLVAL-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLVAL-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO SLVAL-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2860-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2870-DESCRIBE-SOURCE.
           EVALUATE SLVAL-SOURCE-CD
               WHEN 'AST'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BCK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'CJG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DSJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JVY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VEF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CAPINT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2880-SPLIT-CAPINT.
           COMPUTE WS-WORK-AMT = SLVAL-CAPINT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SLVAL-CAPINT-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2890-COUNT-PAYMENT.
           IF SLVAL-PAYMENT-CNT < 9999999
               ADD 1 TO SLVAL-PAYMENT-CNT
           END-IF
           IF SLVAL-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO SLVAL-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SUBSIDY SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2900-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SUBSIDY-SCHED-RATE(1) TO SLVAL-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-SUBSIDY-SCHED-LOW(WS-TX)
                   MOVE WS-SUBSIDY-SCHED-RATE(WS-TX) TO SLVAL-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SLVAL-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SLVAL-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2910-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SLVAL-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLVAL-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO SLVAL-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEFERINT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2920-FLAG-DEFERINT.
           IF SLVAL-DEFERINT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SLVAL-DEFERINT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2930-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * SLVAL-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO SLVAL-SUBSIDY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE DISBURSE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2940-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SLVAL-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SLVAL-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SLVAL-SUSPEND-FLAG = 'Y'
                   COMPUTE SLVAL-PAYMENT-YTD ROUNDED =
                       SLVAL-DISBURSE-YTD * SLVAL-FEE-PCT / 200
               WHEN SLVAL-CYCLE-CD = 'CFJ'
                   MOVE 12.00 TO SLVAL-PAYMENT-YTD
               WHEN SLVAL-CYCLE-CD = 'KHC'
                   MOVE 15.00 TO SLVAL-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE SLVAL-PAYMENT-YTD ROUNDED =
                       SLVAL-DISBURSE-YTD * SLVAL-FEE-PCT / 100
           END-EVALUATE
           IF SLVAL-PAYMENT-YTD > 0 AND SLVAL-PAYMENT-YTD < 5.00
               MOVE 5.00 TO SLVAL-PAYMENT-YTD
           END-IF
           IF SLVAL-PAYMENT-YTD > 50.00
               MOVE 50.00 TO SLVAL-PAYMENT-YTD
           END-IF
           ADD SLVAL-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CAPINT YTD.
      *----------------------------------------------------------------*
       2950-PRORATE-CAPINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SLVAL-CAPINT-YTD ROUNDED =
                   SLVAL-DISBURSE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SLVAL-CAPINT-YTD
           END-IF
           ADD SLVAL-CAPINT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2960-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF SLVAL-EXPIRY-DATE > 0 AND SLVAL-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLVAL-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SLVAL-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SLVAL-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SLVAL-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SLVAL-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SLVAL-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SUBSIDY YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2970-SPLIT-SUBSIDY.
           COMPUTE WS-WORK-AMT = SLVAL-SUBSIDY-YTD / 12
           COMPUTE WS-WORK-AMT-2 = SLVAL-SUBSIDY-YTD - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEFERINT YTD.
      *----------------------------------------------------------------*
       2980-PRORATE-DEFERINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SLVAL-DEFERINT-YTD ROUNDED =
                   SLVAL-DISBURSE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SLVAL-DEFERINT-YTD
           END-IF
           ADD SLVAL-DEFERINT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE SUBSIDY SCHED TIER FOR THE SUBSIDY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2990-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SUBSIDY-SCHED-RATE(1) TO SLVAL-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SLVAL-SUBSIDY-YTD NOT < WS-SUBSIDY-SCHED-LOW(WS-TX)
                   MOVE WS-SUBSIDY-SCHED-RATE(WS-TX) TO SLVAL-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SLVAL-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SLVAL-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3000-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SLVAL-LAST-MAINT-DATE > 0 AND SLVAL-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SLVAL-LAST-MAINT-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CFJ -> KHC WHEN EXPIRY DATE IS PAST
      * KHC -> SEU WHEN EXPIRY DATE IS PAST
      * SEU -> XKA WHEN EXPIRY DATE IS PAST
      * XKA -> CFJ WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3010-SET-CYCLE.
           EVALUATE SLVAL-CYCLE-CD
               WHEN 'CFJ'
                   IF SLVAL-EXPIRY-DATE > 0 AND SLVAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHC' TO SLVAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KHC'
                   IF SLVAL-EXPIRY-DATE > 0 AND SLVAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SEU' TO SLVAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SEU'
                   IF SLVAL-EXPIRY-DATE > 0 AND SLVAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XKA' TO SLVAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XKA'
                   IF SLVAL-EXPIRY-DATE > 0 AND SLVAL-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CFJ' TO SLVAL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO SLVAL-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO SLVAL-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO SLVAL-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'SLVAL EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
