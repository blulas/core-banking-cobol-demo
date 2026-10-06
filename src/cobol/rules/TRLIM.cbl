      *================================================================*
      * PROGRAM  : TRLIM                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : PERSONAL TRUST RULE - ELIGIBILITY. CALLED BY THE    *
      *            PERSONAL TRUST BATCH AND ONLINE PROGRAMS.           *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-11-07 BHS  ORIGINAL                                       *
      * 1990-01-27 RJT  INCOME NOTICE TEXT PER LEGAL PRB1727           *
      * 1995-08-27 BHS  INCOME CALCULATION CORRECTED PRB4267           *
      * 1995-06-13 JLM  DISTRIB LIMIT REVISED CHG5797                  *
      * 2000-12-20 GAF  DISTRIB CUTOFF MOVED INC5847                   *
      * 2005-02-10 BHS  DISTRIB FEE SCHEDULE UPDATE INC2841            *
      * 2007-06-07 RJT  DISTRIB RERUN PROTECTION CHG0934               *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRLIM.
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
                                           VALUE 10.000000.
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
      * WS-DISTRIB-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISTRIB-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000865000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000101500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000869000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000333000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013100000000698000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013350000000350500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015850000000722000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015950000000290000'.
       01  WS-DISTRIB-SCHED-TBL REDEFINES WS-DISTRIB-SCHED-VALUES.
           05  WS-DISTRIB-SCHED-ROW OCCURS 8 TIMES.
               10  WS-DISTRIB-SCHED-LOW    PIC 9(09)V99.
               10  WS-DISTRIB-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PRINCIPA-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINCIPA-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000345500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000544500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000624000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000144000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000210500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008500000000819500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000325000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000099000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012100000000383000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014600000000869000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024600000000114000'.
       01  WS-PRINCIPA-GRID-TBL REDEFINES WS-PRINCIPA-GRID-VALUES.
           05  WS-PRINCIPA-GRID-ROW OCCURS 11 TIMES.
               10  WS-PRINCIPA-GRID-LOW    PIC 9(09)V99.
               10  WS-PRINCIPA-GRID-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY TRLIMP.

       PROCEDURE DIVISION USING TRLIM-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-LIMIT-DISTRIB
           PERFORM 2010-FLAG-DISTRIB
           PERFORM 2020-CHECK-NOTICE
           PERFORM 2030-EDIT-NAME
           PERFORM 2040-LIMIT-INCOME
           PERFORM 2050-SELECT-RATE
           PERFORM 2060-ASSESS-EXEMPT
           PERFORM 2070-CHECK-OPEN
           PERFORM 2080-COUNT-PAYMENT
           PERFORM 2090-DESCRIBE-TRUST-TYPE
           PERFORM 2100-EDIT-NAME
           PERFORM 2110-AGE-NOTICE
           PERFORM 2120-AGE-NOTICE
           PERFORM 2130-LIMIT-INCOME
           PERFORM 2140-CHECK-CLOSE
           PERFORM 2150-FLAG-ITEM
           PERFORM 2160-SET-REASON
           PERFORM 2170-DESCRIBE-CYCLE
           PERFORM 2180-SET-TRUST-TYPE
           PERFORM 2190-LIMIT-INCOME
           PERFORM 2200-DESCRIBE-TRUST-TYPE
           PERFORM 2210-ASSESS-SUSPEND
           PERFORM 2220-ACCRUE-MARGIN
           PERFORM 2230-LIMIT-INCOME
           PERFORM 2240-SPLIT-INCOME
           PERFORM 2250-FLAG-DISTRIB
           PERFORM 2260-FLAG-DISTRIB
           PERFORM 2270-ACCRUE-TRUSTEE-FEE
           PERFORM 2280-ACCRUE-MARGIN
           PERFORM 2290-SPLIT-PRINCIPAL
           PERFORM 2300-PRORATE-DISTRIB
           PERFORM 2310-ASSESS-SUSPEND
           PERFORM 2320-CHECK-CLOSE
           PERFORM 2330-SELECT-MARGIN
           PERFORM 2340-EDIT-NAME
           PERFORM 2350-CHECK-OPEN
           PERFORM 2360-ASSESS-EXEMPT
           PERFORM 2370-COUNT-RETRY
           PERFORM 2380-ACCRUE-INCOME
           PERFORM 2390-CHECK-NOTICE
           PERFORM 2400-DESCRIBE-STATUS
           PERFORM 2410-SELECT-MARGIN
           PERFORM 2420-DESCRIBE-STATUS
           PERFORM 2430-SELECT-RATE
           PERFORM 2440-SELECT-RATE
           PERFORM 2450-SELECT-RATE
           PERFORM 2460-ACCRUE-MARGIN
           PERFORM 2470-LIMIT-ITEM
           PERFORM 2480-LIMIT-INCOME
           PERFORM 2490-PRORATE-DISTRIB
           PERFORM 2500-CHECK-NOTICE
           PERFORM 2510-FLAG-INCOME
           PERFORM 2520-DESCRIBE-STATUS
           PERFORM 2530-FLAG-PRINCIPAL
           PERFORM 2540-SET-TRUST-TYPE
           PERFORM 2550-EDIT-NAME
           PERFORM 2560-ACCUM-STATUS
           PERFORM 2570-SPLIT-DISTRIB
           PERFORM 2580-PRORATE-DISTRIB
           PERFORM 2590-LIMIT-ITEM
           PERFORM 2600-CHECK-CLOSE
           PERFORM 2610-SELECT-RATE
           PERFORM 2620-ACCRUE-MARGIN
           PERFORM 2630-ASSESS-STOP
           PERFORM 2640-SPLIT-DISTRIB
           PERFORM 2650-EDIT-NAME
           PERFORM 2660-ACCRUE-RATE
           PERFORM 2670-AGE-OPEN
           PERFORM 2680-ACCUM-STATUS
           PERFORM 2690-LIMIT-INCOME
           PERFORM 2700-SET-REASON
           PERFORM 2710-SET-REASON
           PERFORM 2720-SELECT-RATE
           PERFORM 2730-SPLIT-INCOME
           PERFORM 2740-SET-STATUS
           PERFORM 2750-COUNT-RETRY
           PERFORM 2760-SELECT-MARGIN
           PERFORM 2770-SET-CYCLE
           PERFORM 2780-SELECT-RATE
           PERFORM 2790-ACCRUE-RATE
           PERFORM 2800-LIMIT-DISTRIB
           PERFORM 2810-ASSESS-EXEMPT
           PERFORM 2820-LIMIT-TRUSTEE-FEE
           PERFORM 2830-COUNT-RETRY
           PERFORM 2840-FLAG-INCOME
           PERFORM 2850-ACCUM-REASON
           PERFORM 2860-SET-TRUST-TYPE
           PERFORM 2870-COUNT-RETRY
           PERFORM 2880-SELECT-RATE
           PERFORM 2890-SELECT-RATE
           PERFORM 2900-SELECT-MARGIN
           PERFORM 2910-FLAG-DISTRIB
           PERFORM 2920-CHECK-OPEN
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO TRLIM-RETURN-CODE.

      *----------------------------------------------------------------*
      * DISTRIB YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2000-LIMIT-DISTRIB.
           IF TRLIM-DISTRIB-YTD < 0.00
               MOVE 0.00 TO TRLIM-DISTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRLIM-DISTRIB-YTD > 50000.00
               MOVE 50000.00 TO TRLIM-DISTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2010-FLAG-DISTRIB.
           IF TRLIM-DISTRIB-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRLIM-DISTRIB-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2020-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TRLIM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRLIM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TRLIM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E070 NAME REQUIRED
      * E404 INCOME YTD NEGATIVE
      * E013 INCOME YTD OVER 99999
      * E677 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2030-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRLIM-NAME = SPACES
                   MOVE 'E070' TO WS-REJECT-CODE
               WHEN TRLIM-INCOME-YTD < 0
                   MOVE 'E404' TO WS-REJECT-CODE
               WHEN TRLIM-INCOME-YTD > 99999.00
                   MOVE 'E013' TO WS-REJECT-CODE
               WHEN TRLIM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E677' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INCOME YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2040-LIMIT-INCOME.
           IF TRLIM-INCOME-YTD < 100.00
               MOVE 100.00 TO TRLIM-INCOME-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRLIM-INCOME-YTD > 50000.00
               MOVE 50000.00 TO TRLIM-INCOME-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2050-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-SCHED-RATE(1) TO TRLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-DISTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-DISTRIB-SCHED-RATE(WS-TX) TO TRLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB YTD FEE ON THE TRUSTEE FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2060-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRLIM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRLIM-DISTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRLIM-STOP-FLAG = 'Y'
                   COMPUTE TRLIM-DISTRIB-YTD ROUNDED =
                       TRLIM-TRUSTEE-FEE-AMT * TRLIM-RATE / 200
               WHEN TRLIM-TRUST-TYPE-CD = 'FLJ'
                   MOVE 20.00 TO TRLIM-DISTRIB-YTD
               WHEN TRLIM-TRUST-TYPE-CD = 'MNG'
                   MOVE 15.00 TO TRLIM-DISTRIB-YTD
               WHEN OTHER
                   COMPUTE TRLIM-DISTRIB-YTD ROUNDED =
                       TRLIM-TRUSTEE-FEE-AMT * TRLIM-RATE / 100
           END-EVALUATE
           IF TRLIM-DISTRIB-YTD > 0 AND TRLIM-DISTRIB-YTD < 5.00
               MOVE 5.00 TO TRLIM-DISTRIB-YTD
           END-IF
           IF TRLIM-DISTRIB-YTD > 75.00
               MOVE 75.00 TO TRLIM-DISTRIB-YTD
           END-IF
           ADD TRLIM-DISTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2070-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TRLIM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRLIM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TRLIM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2080-COUNT-PAYMENT.
           IF TRLIM-PAYMENT-CNT < 9999999
               ADD 1 TO TRLIM-PAYMENT-CNT
           END-IF
           IF TRLIM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO TRLIM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRUST TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2090-DESCRIBE-TRUST-TYPE.
           EVALUATE TRLIM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MNG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VFN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XPL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E348 NAME REQUIRED
      * E697 WORK AMT NEGATIVE
      * E257 WORK AMT OVER 10000
      * E509 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2100-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRLIM-NAME = SPACES
                   MOVE 'E348' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E697' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E257' TO WS-REJECT-CODE
               WHEN TRLIM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E509' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2110-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TRLIM-NOTICE-DATE > 0 AND TRLIM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRLIM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRLIM-DISTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRLIM-DISTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TRLIM-DISTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TRLIM-DISTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRLIM-DISTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2120-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TRLIM-NOTICE-DATE > 0 AND TRLIM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRLIM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INCOME YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2130-LIMIT-INCOME.
           IF TRLIM-INCOME-YTD < 50.00
               MOVE 50.00 TO TRLIM-INCOME-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRLIM-INCOME-YTD > 5000.00
               MOVE 5000.00 TO TRLIM-INCOME-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2140-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRLIM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRLIM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TRLIM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2150-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * H -> M WHEN NOTICE DATE IS PAST
      * M -> T WHEN NOTICE DATE IS PAST
      * T -> H WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2160-SET-REASON.
           EVALUATE TRLIM-REASON-CD
               WHEN 'H'
                   IF TRLIM-NOTICE-DATE > 0 AND TRLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TRLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TRLIM-NOTICE-DATE > 0 AND TRLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRLIM-NOTICE-DATE > 0 AND TRLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO TRLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2170-DESCRIBE-CYCLE.
           EVALUATE TRLIM-CYCLE-CD
               WHEN 'EN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN OPEN DATE IS PAST
      * MNG -> VFN WHEN OPEN DATE IS PAST
      * VFN -> XPL WHEN OPEN DATE IS PAST
      * XPL -> FLJ WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2180-SET-TRUST-TYPE.
           EVALUATE TRLIM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRLIM-OPEN-DATE > 0 AND TRLIM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRLIM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRLIM-OPEN-DATE > 0 AND TRLIM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRLIM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRLIM-OPEN-DATE > 0 AND TRLIM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRLIM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRLIM-OPEN-DATE > 0 AND TRLIM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRLIM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INCOME AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2190-LIMIT-INCOME.
           IF TRLIM-INCOME-AMT < 100.00
               MOVE 100.00 TO TRLIM-INCOME-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRLIM-INCOME-AMT > 5000.00
               MOVE 5000.00 TO TRLIM-INCOME-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRUST TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2200-DESCRIBE-TRUST-TYPE.
           EVALUATE TRLIM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MNG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VFN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XPL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE WORK AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2210-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRLIM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRLIM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       TRLIM-MARGIN / 200
               WHEN TRLIM-TRUST-TYPE-CD = 'FLJ'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN TRLIM-TRUST-TYPE-CD = 'MNG'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       TRLIM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2220-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TRLIM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRLIM-DISTRIB-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INCOME AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2230-LIMIT-INCOME.
           IF TRLIM-INCOME-AMT < 10.00
               MOVE 10.00 TO TRLIM-INCOME-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRLIM-INCOME-AMT > 50000.00
               MOVE 50000.00 TO TRLIM-INCOME-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2240-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE TRLIM-INCOME-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2250-FLAG-DISTRIB.
           IF TRLIM-DISTRIB-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRLIM-DISTRIB-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2260-FLAG-DISTRIB.
           IF TRLIM-DISTRIB-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRLIM-DISTRIB-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TRUSTEE FEE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2270-ACCRUE-TRUSTEE-FEE.
           IF TRLIM-TRUSTEE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRLIM-TRUSTEE-FEE-AMT * TRLIM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRLIM-DISTRIB-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2280-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TRLIM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRLIM-TRUSTEE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2290-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE TRLIM-PRINCIPAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISTRIB YTD.
      *----------------------------------------------------------------*
       2300-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRLIM-DISTRIB-YTD ROUNDED =
                   TRLIM-DISTRIB-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRLIM-DISTRIB-YTD
           END-IF
           ADD TRLIM-DISTRIB-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2310-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRLIM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TRLIM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRLIM-STOP-FLAG = 'Y'
                   COMPUTE TRLIM-PRINCIPAL-YTD ROUNDED = WS-WORK-AMT-2 *
                       TRLIM-MARGIN / 200
               WHEN TRLIM-CYCLE-CD = 'EN'
                   MOVE 5.00 TO TRLIM-PRINCIPAL-YTD
               WHEN TRLIM-CYCLE-CD = 'FY'
                   MOVE 7.00 TO TRLIM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE TRLIM-PRINCIPAL-YTD ROUNDED = WS-WORK-AMT-2 *
                       TRLIM-MARGIN / 100
           END-EVALUATE
           IF TRLIM-PRINCIPAL-YTD > 0 AND TRLIM-PRINCIPAL-YTD < 2.00
               MOVE 2.00 TO TRLIM-PRINCIPAL-YTD
           END-IF
           IF TRLIM-PRINCIPAL-YTD > 75.00
               MOVE 75.00 TO TRLIM-PRINCIPAL-YTD
           END-IF
           ADD TRLIM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2320-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRLIM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRLIM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TRLIM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB SCHED TIER FOR THE TRUSTEE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2330-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-SCHED-RATE(1) TO TRLIM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRLIM-TRUSTEE-FEE-AMT NOT <
                   WS-DISTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-DISTRIB-SCHED-RATE(WS-TX) TO TRLIM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRLIM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRLIM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E790 NAME REQUIRED
      * E761 DISTRIB AMT NEGATIVE
      * E138 DISTRIB AMT OVER 99999
      * E854 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2340-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRLIM-NAME = SPACES
                   MOVE 'E790' TO WS-REJECT-CODE
               WHEN TRLIM-DISTRIB-AMT < 0
                   MOVE 'E761' TO WS-REJECT-CODE
               WHEN TRLIM-DISTRIB-AMT > 99999.00
                   MOVE 'E138' TO WS-REJECT-CODE
               WHEN TRLIM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E854' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2350-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TRLIM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRLIM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TRLIM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE DISTRIB YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2360-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRLIM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRLIM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRLIM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRLIM-PRINCIPAL-YTD ROUNDED =
                       TRLIM-DISTRIB-YTD * TRLIM-MARGIN / 200
               WHEN TRLIM-STATUS-CD = 'EH'
                   MOVE 10.00 TO TRLIM-PRINCIPAL-YTD
               WHEN TRLIM-STATUS-CD = 'GG'
                   MOVE 3.00 TO TRLIM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE TRLIM-PRINCIPAL-YTD ROUNDED =
                       TRLIM-DISTRIB-YTD * TRLIM-MARGIN / 100
           END-EVALUATE
           IF TRLIM-PRINCIPAL-YTD > 0 AND TRLIM-PRINCIPAL-YTD < 2.00
               MOVE 2.00 TO TRLIM-PRINCIPAL-YTD
           END-IF
           IF TRLIM-PRINCIPAL-YTD > 150.00
               MOVE 150.00 TO TRLIM-PRINCIPAL-YTD
           END-IF
           ADD TRLIM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2370-COUNT-RETRY.
           IF TRLIM-RETRY-CNT < 9999999
               ADD 1 TO TRLIM-RETRY-CNT
           END-IF
           IF TRLIM-RETRY-CNT NOT < 5
               MOVE 'Y' TO TRLIM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INCOME AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2380-ACCRUE-INCOME.
           IF TRLIM-INCOME-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRLIM-INCOME-AMT * TRLIM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2390-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TRLIM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRLIM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TRLIM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2400-DESCRIBE-STATUS.
           EVALUATE TRLIM-STATUS-CD
               WHEN 'EH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB SCHED TIER FOR THE INCOME AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2410-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-SCHED-RATE(1) TO TRLIM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRLIM-INCOME-AMT NOT < WS-DISTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-DISTRIB-SCHED-RATE(WS-TX) TO TRLIM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRLIM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRLIM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2420-DESCRIBE-STATUS.
           EVALUATE TRLIM-STATUS-CD
               WHEN 'EH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE INCOME YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2430-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO TRLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TRLIM-INCOME-YTD NOT < WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO TRLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE PRINCIPAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2440-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO TRLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TRLIM-PRINCIPAL-YTD NOT < WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO TRLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE TRUSTEE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2450-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO TRLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TRLIM-TRUSTEE-FEE-AMT NOT <
                   WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO TRLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2460-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TRLIM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRLIM-PRINCIPAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2470-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INCOME AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2480-LIMIT-INCOME.
           IF TRLIM-INCOME-AMT < 25.00
               MOVE 25.00 TO TRLIM-INCOME-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRLIM-INCOME-AMT > 50000.00
               MOVE 50000.00 TO TRLIM-INCOME-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INCOME YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISTRIB YTD.
      *----------------------------------------------------------------*
       2490-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRLIM-DISTRIB-YTD ROUNDED =
                   TRLIM-INCOME-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRLIM-DISTRIB-YTD
           END-IF
           ADD TRLIM-DISTRIB-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2500-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TRLIM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRLIM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TRLIM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INCOME AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2510-FLAG-INCOME.
           IF TRLIM-INCOME-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRLIM-INCOME-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2520-DESCRIBE-STATUS.
           EVALUATE TRLIM-STATUS-CD
               WHEN 'EH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2530-FLAG-PRINCIPAL.
           IF TRLIM-PRINCIPAL-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRLIM-PRINCIPAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN LAST MAINT DATE IS PAST
      * MNG -> VFN WHEN LAST MAINT DATE IS PAST
      * VFN -> XPL WHEN LAST MAINT DATE IS PAST
      * XPL -> FLJ WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2540-SET-TRUST-TYPE.
           EVALUATE TRLIM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRLIM-LAST-MAINT-DATE > 0 AND
                       TRLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRLIM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRLIM-LAST-MAINT-DATE > 0 AND
                       TRLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRLIM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRLIM-LAST-MAINT-DATE > 0 AND
                       TRLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRLIM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRLIM-LAST-MAINT-DATE > 0 AND
                       TRLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRLIM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E752 NAME REQUIRED
      * E958 DISTRIB AMT NEGATIVE
      * E612 DISTRIB AMT OVER 50000
      * E172 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2550-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRLIM-NAME = SPACES
                   MOVE 'E752' TO WS-REJECT-CODE
               WHEN TRLIM-DISTRIB-AMT < 0
                   MOVE 'E958' TO WS-REJECT-CODE
               WHEN TRLIM-DISTRIB-AMT > 50000.00
                   MOVE 'E612' TO WS-REJECT-CODE
               WHEN TRLIM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E172' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2560-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRLIM-STATUS-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRLIM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2570-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = TRLIM-DISTRIB-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TRLIM-DISTRIB-YTD - (WS-WORK-AMT * 3)
           COMPUTE TRLIM-INCOME-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISTRIB AMT.
      *----------------------------------------------------------------*
       2580-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRLIM-DISTRIB-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRLIM-DISTRIB-AMT
           END-IF
           ADD TRLIM-DISTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2590-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2600-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRLIM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRLIM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TRLIM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB SCHED TIER FOR THE DISTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2610-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-SCHED-RATE(1) TO TRLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRLIM-DISTRIB-AMT NOT < WS-DISTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-DISTRIB-SCHED-RATE(WS-TX) TO TRLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2620-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TRLIM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRLIM-INCOME-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE DISTRIB AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2630-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRLIM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRLIM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRLIM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRLIM-PRINCIPAL-YTD ROUNDED =
                       TRLIM-DISTRIB-AMT * TRLIM-RATE / 200
               WHEN TRLIM-REASON-CD = 'H'
                   MOVE 10.00 TO TRLIM-PRINCIPAL-YTD
               WHEN TRLIM-REASON-CD = 'M'
                   MOVE 7.00 TO TRLIM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE TRLIM-PRINCIPAL-YTD ROUNDED =
                       TRLIM-DISTRIB-AMT * TRLIM-RATE / 100
           END-EVALUATE
           IF TRLIM-PRINCIPAL-YTD > 0 AND TRLIM-PRINCIPAL-YTD < 2.00
               MOVE 2.00 TO TRLIM-PRINCIPAL-YTD
           END-IF
           IF TRLIM-PRINCIPAL-YTD > 150.00
               MOVE 150.00 TO TRLIM-PRINCIPAL-YTD
           END-IF
           ADD TRLIM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2640-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = TRLIM-DISTRIB-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TRLIM-DISTRIB-AMT - (WS-WORK-AMT *
               12)
           COMPUTE TRLIM-INCOME-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E162 NAME REQUIRED
      * E601 WORK AMT 2 NEGATIVE
      * E905 WORK AMT 2 OVER 10000
      * E046 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2650-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRLIM-NAME = SPACES
                   MOVE 'E162' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E601' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E905' TO WS-REJECT-CODE
               WHEN TRLIM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E046' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2660-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TRLIM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRLIM-INCOME-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2670-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TRLIM-OPEN-DATE > 0 AND TRLIM-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRLIM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRLIM-PRINCIPAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRLIM-PRINCIPAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRLIM-PRINCIPAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRLIM-PRINCIPAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRLIM-PRINCIPAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2680-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRLIM-STATUS-CD
                   ADD TRLIM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRLIM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRLIM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * INCOME AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2690-LIMIT-INCOME.
           IF TRLIM-INCOME-AMT < 10.00
               MOVE 10.00 TO TRLIM-INCOME-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRLIM-INCOME-AMT > 1000.00
               MOVE 1000.00 TO TRLIM-INCOME-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * H -> M WHEN CLOSE DATE IS PAST
      * M -> T WHEN CLOSE DATE IS PAST
      * T -> H WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2700-SET-REASON.
           EVALUATE TRLIM-REASON-CD
               WHEN 'H'
                   IF TRLIM-CLOSE-DATE > 0 AND TRLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TRLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TRLIM-CLOSE-DATE > 0 AND TRLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRLIM-CLOSE-DATE > 0 AND TRLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO TRLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * H -> M WHEN OPEN DATE IS PAST
      * M -> T WHEN OPEN DATE IS PAST
      * T -> H WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2710-SET-REASON.
           EVALUATE TRLIM-REASON-CD
               WHEN 'H'
                   IF TRLIM-OPEN-DATE > 0 AND TRLIM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TRLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TRLIM-OPEN-DATE > 0 AND TRLIM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRLIM-OPEN-DATE > 0 AND TRLIM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO TRLIM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB SCHED TIER FOR THE PRINCIPAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2720-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-SCHED-RATE(1) TO TRLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRLIM-PRINCIPAL-YTD NOT < WS-DISTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-DISTRIB-SCHED-RATE(WS-TX) TO TRLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INCOME YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2730-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = TRLIM-INCOME-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TRLIM-INCOME-YTD - (WS-WORK-AMT * 4)
           COMPUTE TRLIM-DISTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EH -> GG WHEN LAST MAINT DATE IS PAST
      * GG -> JG WHEN LAST MAINT DATE IS PAST
      * JG -> EH WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2740-SET-STATUS.
           EVALUATE TRLIM-STATUS-CD
               WHEN 'EH'
                   IF TRLIM-LAST-MAINT-DATE > 0 AND
                       TRLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'GG' TO TRLIM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF TRLIM-LAST-MAINT-DATE > 0 AND
                       TRLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'JG' TO TRLIM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JG'
                   IF TRLIM-LAST-MAINT-DATE > 0 AND
                       TRLIM-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'EH' TO TRLIM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2750-COUNT-RETRY.
           IF TRLIM-RETRY-CNT < 9999999
               ADD 1 TO TRLIM-RETRY-CNT
           END-IF
           IF TRLIM-RETRY-CNT NOT < 12
               MOVE 'Y' TO TRLIM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB SCHED TIER FOR THE INCOME YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2760-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-SCHED-RATE(1) TO TRLIM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRLIM-INCOME-YTD NOT < WS-DISTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-DISTRIB-SCHED-RATE(WS-TX) TO TRLIM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRLIM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRLIM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EN -> FY WHEN NOTICE DATE IS PAST
      * FY -> HP WHEN NOTICE DATE IS PAST
      * HP -> JB WHEN NOTICE DATE IS PAST
      * JB -> KZ WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2770-SET-CYCLE.
           EVALUATE TRLIM-CYCLE-CD
               WHEN 'EN'
                   IF TRLIM-NOTICE-DATE > 0 AND TRLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FY' TO TRLIM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FY'
                   IF TRLIM-NOTICE-DATE > 0 AND TRLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HP' TO TRLIM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HP'
                   IF TRLIM-NOTICE-DATE > 0 AND TRLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JB' TO TRLIM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JB'
                   IF TRLIM-NOTICE-DATE > 0 AND TRLIM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KZ' TO TRLIM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB SCHED TIER FOR THE DISTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2780-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-SCHED-RATE(1) TO TRLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRLIM-DISTRIB-AMT NOT < WS-DISTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-DISTRIB-SCHED-RATE(WS-TX) TO TRLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2790-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TRLIM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRLIM-DISTRIB-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2800-LIMIT-DISTRIB.
           IF TRLIM-DISTRIB-YTD < 100.00
               MOVE 100.00 TO TRLIM-DISTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRLIM-DISTRIB-YTD > 1000.00
               MOVE 1000.00 TO TRLIM-DISTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2810-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRLIM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRLIM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRLIM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRLIM-PRINCIPAL-YTD ROUNDED = WS-WORK-AMT-2 *
                       TRLIM-MARGIN / 200
               WHEN TRLIM-REASON-CD = 'H'
                   MOVE 12.00 TO TRLIM-PRINCIPAL-YTD
               WHEN TRLIM-REASON-CD = 'M'
                   MOVE 12.00 TO TRLIM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE TRLIM-PRINCIPAL-YTD ROUNDED = WS-WORK-AMT-2 *
                       TRLIM-MARGIN / 100
           END-EVALUATE
           IF TRLIM-PRINCIPAL-YTD > 0 AND TRLIM-PRINCIPAL-YTD < 5.00
               MOVE 5.00 TO TRLIM-PRINCIPAL-YTD
           END-IF
           IF TRLIM-PRINCIPAL-YTD > 250.00
               MOVE 250.00 TO TRLIM-PRINCIPAL-YTD
           END-IF
           ADD TRLIM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRUSTEE FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2820-LIMIT-TRUSTEE-FEE.
           IF TRLIM-TRUSTEE-FEE-AMT < 100.00
               MOVE 100.00 TO TRLIM-TRUSTEE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRLIM-TRUSTEE-FEE-AMT > 5000.00
               MOVE 5000.00 TO TRLIM-TRUSTEE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2830-COUNT-RETRY.
           IF TRLIM-RETRY-CNT < 9999999
               ADD 1 TO TRLIM-RETRY-CNT
           END-IF
           IF TRLIM-RETRY-CNT NOT < 12
               MOVE 'Y' TO TRLIM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INCOME AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2840-FLAG-INCOME.
           IF TRLIM-INCOME-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRLIM-INCOME-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2850-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRLIM-REASON-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRLIM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN CLOSE DATE IS PAST
      * MNG -> VFN WHEN CLOSE DATE IS PAST
      * VFN -> XPL WHEN CLOSE DATE IS PAST
      * XPL -> FLJ WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2860-SET-TRUST-TYPE.
           EVALUATE TRLIM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRLIM-CLOSE-DATE > 0 AND TRLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRLIM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRLIM-CLOSE-DATE > 0 AND TRLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRLIM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRLIM-CLOSE-DATE > 0 AND TRLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRLIM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRLIM-CLOSE-DATE > 0 AND TRLIM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRLIM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2870-COUNT-RETRY.
           IF TRLIM-RETRY-CNT < 9999999
               ADD 1 TO TRLIM-RETRY-CNT
           END-IF
           IF TRLIM-RETRY-CNT NOT < 5
               MOVE 'Y' TO TRLIM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB SCHED TIER FOR THE INCOME YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2880-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-SCHED-RATE(1) TO TRLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRLIM-INCOME-YTD NOT < WS-DISTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-DISTRIB-SCHED-RATE(WS-TX) TO TRLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB SCHED TIER FOR THE PRINCIPAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2890-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-SCHED-RATE(1) TO TRLIM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRLIM-PRINCIPAL-YTD NOT < WS-DISTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-DISTRIB-SCHED-RATE(WS-TX) TO TRLIM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRLIM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRLIM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB SCHED TIER FOR THE TRUSTEE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2900-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-SCHED-RATE(1) TO TRLIM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRLIM-TRUSTEE-FEE-AMT NOT <
                   WS-DISTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-DISTRIB-SCHED-RATE(WS-TX) TO TRLIM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRLIM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRLIM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2910-FLAG-DISTRIB.
           IF TRLIM-DISTRIB-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRLIM-DISTRIB-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2920-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TRLIM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRLIM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TRLIM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO TRLIM-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO TRLIM-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO TRLIM-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'TRLIM EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
