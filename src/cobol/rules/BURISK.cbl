      *================================================================*
      * PROGRAM  : BURISK                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : BUMP-UP CERTIFICATES RULE - VALIDATION. CALLED BY   *
      *            THE BUMP-UP CERTIFICATES BATCH AND ONLINE PROGRAMS. *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-02-18 GAF  ORIGINAL                                       *
      * 1994-08-26 BHS  ADDON LIMIT REVISED REQ8341                    *
      * 1995-07-25 CRP  INTEREST EXCEPTIONS TO OPS QUEUE PRB5405       *
      * 1996-11-25 LMB  PENALTY EXCEPTIONS TO OPS QUEUE INC1762        *
      * 1996-08-26 GAF  PRINCIPAL NOTICE TEXT PER LEGAL PRB9036        *
      * 1997-11-17 TWH  AUDIT FINDING - PENALTY CONTROL TOTALS INC0315 *
      * 2001-09-10 MKL  PENALTY TABLE EXPANDED REQ8685                 *
      * 2005-02-04 DPS  ADDON NOTICE TEXT PER LEGAL PRB9243            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BURISK.
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
      * WS-PRINCIPA-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINCIPA-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000785000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000503000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000615000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005150000000390000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005200000000777000'.
       01  WS-PRINCIPA-BAND-TBL REDEFINES WS-PRINCIPA-BAND-VALUES.
           05  WS-PRINCIPA-BAND-ROW OCCURS 5 TIMES.
               10  WS-PRINCIPA-BAND-LOW    PIC 9(09)V99.
               10  WS-PRINCIPA-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PENALTY-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PENALTY-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000096000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000047500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000906500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000631000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000826500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000017000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017000000000675000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027000000000726000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027250000000649000'.
       01  WS-PENALTY-SCHED-TBL REDEFINES WS-PENALTY-SCHED-VALUES.
           05  WS-PENALTY-SCHED-ROW OCCURS 9 TIMES.
               10  WS-PENALTY-SCHED-LOW    PIC 9(09)V99.
               10  WS-PENALTY-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PRINCIPA-BANDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINCIPA-BANDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000746500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000806000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000402500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000260000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000367500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012000000000150000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017000000000613000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017250000000868000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017500000000356500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000320000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030000000000808000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030500000000828000'.
       01  WS-PRINCIPA-BANDX-TBL REDEFINES WS-PRINCIPA-BANDX-VALUES.
           05  WS-PRINCIPA-BANDX-ROW OCCURS 12 TIMES.
               10  WS-PRINCIPA-BANDX-LOW   PIC 9(09)V99.
               10  WS-PRINCIPA-BANDX-RATE  PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY BURISKP.

       PROCEDURE DIVISION USING BURISK-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-AGE-NEXT-REVIEW
           PERFORM 2010-COUNT-EXCEPT
           PERFORM 2020-SPLIT-INTEREST
           PERFORM 2030-COUNT-EXCEPT
           PERFORM 2040-ASSESS-HOLD
           PERFORM 2050-EDIT-DESC
           PERFORM 2060-ASSESS-EXEMPT
           PERFORM 2070-ASSESS-HOLD
           PERFORM 2080-AGE-LAST-STMT
           PERFORM 2090-DESCRIBE-REGION
           PERFORM 2100-CHECK-NEXT-REVIEW
           PERFORM 2110-ACCRUE-INTEREST
           PERFORM 2120-SPLIT-INTEREST
           PERFORM 2130-EDIT-DESC
           PERFORM 2140-PRORATE-ADDON
           PERFORM 2150-SET-REGION
           PERFORM 2160-ACCRUE-PENALTY-PCT
           PERFORM 2170-ASSESS-REVIEW
           PERFORM 2180-FLAG-INTEREST
           PERFORM 2190-ASSESS-HOLD
           PERFORM 2200-ASSESS-REVIEW
           PERFORM 2210-FLAG-ADDON
           PERFORM 2220-EDIT-DESC
           PERFORM 2230-FLAG-ADDON
           PERFORM 2240-ACCRUE-ADDON
           PERFORM 2250-COUNT-EXCEPT
           PERFORM 2260-ACCRUE-INTEREST
           PERFORM 2270-SET-ELECTION
           PERFORM 2280-LIMIT-INTEREST
           PERFORM 2290-LIMIT-ITEM
           PERFORM 2300-ASSESS-REVIEW
           PERFORM 2310-DESCRIBE-ELECTION
           PERFORM 2320-ACCUM-REGION
           PERFORM 2330-COUNT-EXCEPT
           PERFORM 2340-ACCUM-ELECTION
           PERFORM 2350-ACCRUE-MARGIN
           PERFORM 2360-ACCRUE-ADDON
           PERFORM 2370-SELECT-PENALTY-PCT
           PERFORM 2380-SET-ELECTION
           PERFORM 2390-ASSESS-HOLD
           PERFORM 2400-FLAG-ADDON
           PERFORM 2410-DESCRIBE-ELECTION
           PERFORM 2420-SET-ELECTION
           PERFORM 2430-SET-ELECTION
           PERFORM 2440-SELECT-MARGIN
           PERFORM 2450-ACCRUE-PRINCIPAL
           PERFORM 2460-ACCRUE-PRINCIPAL
           PERFORM 2470-CHECK-LAST-STMT
           PERFORM 2480-AGE-LAST-STMT
           PERFORM 2490-DESCRIBE-CYCLE
           PERFORM 2500-COUNT-CYCLE
           PERFORM 2510-AGE-LAST-STMT
           PERFORM 2520-EDIT-CUST-ID
           PERFORM 2530-DESCRIBE-ELECTION
           PERFORM 2540-SPLIT-ADDON
           PERFORM 2550-AGE-LAST-STMT
           PERFORM 2560-DESCRIBE-ELECTION
           PERFORM 2570-COUNT-CYCLE
           PERFORM 2580-LIMIT-PENALTY
           PERFORM 2590-SET-REGION
           PERFORM 2600-EDIT-CUST-ID
           PERFORM 2610-FLAG-INTEREST
           PERFORM 2620-EDIT-CUST-ID
           PERFORM 2630-SELECT-PENALTY-PCT
           PERFORM 2640-ASSESS-REVIEW
           PERFORM 2650-DESCRIBE-REGION
           PERFORM 2660-EDIT-DESC
           PERFORM 2670-PRORATE-ITEM
           PERFORM 2680-CHECK-LAST-STMT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO BURISK-RETURN-CODE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2000-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF BURISK-NEXT-REVIEW-DATE > 0 AND BURISK-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BURISK-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BURISK-PENALTY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BURISK-PENALTY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BURISK-PENALTY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BURISK-PENALTY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BURISK-PENALTY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2010-COUNT-EXCEPT.
           IF BURISK-EXCEPT-CNT < 9999999
               ADD 1 TO BURISK-EXCEPT-CNT
           END-IF
           IF BURISK-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO BURISK-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2020-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BURISK-INTEREST-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BURISK-INTEREST-AMT - (WS-WORK-AMT *
               3)
           COMPUTE BURISK-ADDON-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2030-COUNT-EXCEPT.
           IF BURISK-EXCEPT-CNT < 9999999
               ADD 1 TO BURISK-EXCEPT-CNT
           END-IF
           IF BURISK-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO BURISK-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY YTD FEE ON THE ADDON AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2040-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BURISK-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BURISK-PENALTY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BURISK-EXEMPT-FLAG = 'Y'
                   COMPUTE BURISK-PENALTY-YTD ROUNDED = BURISK-ADDON-AMT
                       * BURISK-PENALTY-PCT / 200
               WHEN BURISK-REGION-CD = 'BGJ'
                   MOVE 15.00 TO BURISK-PENALTY-YTD
               WHEN BURISK-REGION-CD = 'LFX'
                   MOVE 7.00 TO BURISK-PENALTY-YTD
               WHEN OTHER
                   COMPUTE BURISK-PENALTY-YTD ROUNDED = BURISK-ADDON-AMT
                       * BURISK-PENALTY-PCT / 100
           END-EVALUATE
           IF BURISK-PENALTY-YTD > 0 AND BURISK-PENALTY-YTD < 1.00
               MOVE 1.00 TO BURISK-PENALTY-YTD
           END-IF
           IF BURISK-PENALTY-YTD > 50.00
               MOVE 50.00 TO BURISK-PENALTY-YTD
           END-IF
           ADD BURISK-PENALTY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E814 DESC REQUIRED
      * E314 INTEREST AMT NEGATIVE
      * E362 INTEREST AMT OVER 250000
      * E554 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2050-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BURISK-DESC = SPACES
                   MOVE 'E814' TO WS-REJECT-CODE
               WHEN BURISK-INTEREST-AMT < 0
                   MOVE 'E314' TO WS-REJECT-CODE
               WHEN BURISK-INTEREST-AMT > 250000.00
                   MOVE 'E362' TO WS-REJECT-CODE
               WHEN BURISK-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E554' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2060-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BURISK-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BURISK-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BURISK-HOLD-FLAG = 'Y'
                   COMPUTE BURISK-INTEREST-AMT ROUNDED =
                       BURISK-PRINCIPAL-AMT * BURISK-MARGIN / 200
               WHEN BURISK-CYCLE-CD = 'A'
                   MOVE 10.00 TO BURISK-INTEREST-AMT
               WHEN BURISK-CYCLE-CD = 'S'
                   MOVE 7.00 TO BURISK-INTEREST-AMT
               WHEN OTHER
                   COMPUTE BURISK-INTEREST-AMT ROUNDED =
                       BURISK-PRINCIPAL-AMT * BURISK-MARGIN / 100
           END-EVALUATE
           IF BURISK-INTEREST-AMT > 0 AND BURISK-INTEREST-AMT < 1.00
               MOVE 1.00 TO BURISK-INTEREST-AMT
           END-IF
           IF BURISK-INTEREST-AMT > 100.00
               MOVE 100.00 TO BURISK-INTEREST-AMT
           END-IF
           ADD BURISK-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY YTD FEE ON THE ADDON YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2070-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BURISK-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BURISK-PENALTY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BURISK-EXEMPT-FLAG = 'Y'
                   COMPUTE BURISK-PENALTY-YTD ROUNDED = BURISK-ADDON-YTD
                       * BURISK-PENALTY-PCT / 200
               WHEN BURISK-CHANNEL-CD = 'EK'
                   MOVE 3.00 TO BURISK-PENALTY-YTD
               WHEN BURISK-CHANNEL-CD = 'JE'
                   MOVE 7.00 TO BURISK-PENALTY-YTD
               WHEN OTHER
                   COMPUTE BURISK-PENALTY-YTD ROUNDED = BURISK-ADDON-YTD
                       * BURISK-PENALTY-PCT / 100
           END-EVALUATE
           IF BURISK-PENALTY-YTD > 0 AND BURISK-PENALTY-YTD < 1.00
               MOVE 1.00 TO BURISK-PENALTY-YTD
           END-IF
           IF BURISK-PENALTY-YTD > 250.00
               MOVE 250.00 TO BURISK-PENALTY-YTD
           END-IF
           ADD BURISK-PENALTY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2080-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BURISK-LAST-STMT-DATE > 0 AND BURISK-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BURISK-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BURISK-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BURISK-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BURISK-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BURISK-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BURISK-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2090-DESCRIBE-REGION.
           EVALUATE BURISK-REGION-CD
               WHEN 'BGJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LFX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MLA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ULA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2100-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF BURISK-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BURISK-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO BURISK-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2110-ACCRUE-INTEREST.
           IF BURISK-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BURISK-INTEREST-YTD * BURISK-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2120-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE BURISK-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E816 DESC REQUIRED
      * E299 ADDON AMT NEGATIVE
      * E177 ADDON AMT OVER 1000000
      * E229 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2130-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BURISK-DESC = SPACES
                   MOVE 'E816' TO WS-REJECT-CODE
               WHEN BURISK-ADDON-AMT < 0
                   MOVE 'E299' TO WS-REJECT-CODE
               WHEN BURISK-ADDON-AMT > 1000000.00
                   MOVE 'E177' TO WS-REJECT-CODE
               WHEN BURISK-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E229' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ADDON AMT.
      *----------------------------------------------------------------*
       2140-PRORATE-ADDON.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BURISK-ADDON-AMT ROUNDED =
                   BURISK-INTEREST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BURISK-ADDON-AMT
           END-IF
           ADD BURISK-ADDON-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BGJ -> LFX WHEN NEXT REVIEW DATE IS PAST
      * LFX -> MLA WHEN NEXT REVIEW DATE IS PAST
      * MLA -> ULA WHEN NEXT REVIEW DATE IS PAST
      * ULA -> BGJ WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2150-SET-REGION.
           EVALUATE BURISK-REGION-CD
               WHEN 'BGJ'
                   IF BURISK-NEXT-REVIEW-DATE > 0 AND
                       BURISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'LFX' TO BURISK-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LFX'
                   IF BURISK-NEXT-REVIEW-DATE > 0 AND
                       BURISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MLA' TO BURISK-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MLA'
                   IF BURISK-NEXT-REVIEW-DATE > 0 AND
                       BURISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ULA' TO BURISK-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULA'
                   IF BURISK-NEXT-REVIEW-DATE > 0 AND
                       BURISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BGJ' TO BURISK-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2160-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * BURISK-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BURISK-ADDON-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE PENALTY YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2170-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN BURISK-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BURISK-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BURISK-PENALTY-YTD *
                       BURISK-MARGIN / 200
               WHEN BURISK-CYCLE-CD = 'A'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN BURISK-CYCLE-CD = 'S'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BURISK-PENALTY-YTD *
                       BURISK-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2180-FLAG-INTEREST.
           IF BURISK-INTEREST-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BURISK-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2190-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BURISK-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BURISK-ADDON-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BURISK-EXEMPT-FLAG = 'Y'
                   COMPUTE BURISK-ADDON-AMT ROUNDED =
                       BURISK-PRINCIPAL-AMT * BURISK-PENALTY-PCT / 200
               WHEN BURISK-CHANNEL-CD = 'EK'
                   MOVE 5.00 TO BURISK-ADDON-AMT
               WHEN BURISK-CHANNEL-CD = 'JE'
                   MOVE 20.00 TO BURISK-ADDON-AMT
               WHEN OTHER
                   COMPUTE BURISK-ADDON-AMT ROUNDED =
                       BURISK-PRINCIPAL-AMT * BURISK-PENALTY-PCT / 100
           END-EVALUATE
           IF BURISK-ADDON-AMT > 0 AND BURISK-ADDON-AMT < 10.00
               MOVE 10.00 TO BURISK-ADDON-AMT
           END-IF
           IF BURISK-ADDON-AMT > 50.00
               MOVE 50.00 TO BURISK-ADDON-AMT
           END-IF
           ADD BURISK-ADDON-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE INTEREST YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2200-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN BURISK-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BURISK-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BURISK-INTEREST-YTD *
                       BURISK-MARGIN / 200
               WHEN BURISK-ELECTION-CD = 'L'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN BURISK-ELECTION-CD = 'M'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BURISK-INTEREST-YTD *
                       BURISK-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ADDON YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2210-FLAG-ADDON.
           IF BURISK-ADDON-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BURISK-ADDON-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E545 DESC REQUIRED
      * E668 WORK AMT 2 NEGATIVE
      * E969 WORK AMT 2 OVER 50000
      * E337 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2220-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BURISK-DESC = SPACES
                   MOVE 'E545' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E668' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E969' TO WS-REJECT-CODE
               WHEN BURISK-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E337' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ADDON AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2230-FLAG-ADDON.
           IF BURISK-ADDON-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BURISK-ADDON-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADDON AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2240-ACCRUE-ADDON.
           IF BURISK-ADDON-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BURISK-ADDON-AMT * BURISK-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BURISK-ADDON-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2250-COUNT-EXCEPT.
           IF BURISK-EXCEPT-CNT < 9999999
               ADD 1 TO BURISK-EXCEPT-CNT
           END-IF
           IF BURISK-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO BURISK-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2260-ACCRUE-INTEREST.
           IF BURISK-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BURISK-INTEREST-AMT * BURISK-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BURISK-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN LAST STMT DATE IS PAST
      * M -> N WHEN LAST STMT DATE IS PAST
      * N -> R WHEN LAST STMT DATE IS PAST
      * R -> V WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2270-SET-ELECTION.
           EVALUATE BURISK-ELECTION-CD
               WHEN 'L'
                   IF BURISK-LAST-STMT-DATE > 0 AND
                       BURISK-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'M' TO BURISK-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BURISK-LAST-STMT-DATE > 0 AND
                       BURISK-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'N' TO BURISK-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BURISK-LAST-STMT-DATE > 0 AND
                       BURISK-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'R' TO BURISK-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BURISK-LAST-STMT-DATE > 0 AND
                       BURISK-LAST-STMT-DATE < CTL-BUSINESS-DATE
                       MOVE 'V' TO BURISK-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2280-LIMIT-INTEREST.
           IF BURISK-INTEREST-YTD < 100.00
               MOVE 100.00 TO BURISK-INTEREST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BURISK-INTEREST-YTD > 1000.00
               MOVE 1000.00 TO BURISK-INTEREST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2290-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY YTD FEE ON THE INTEREST AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2300-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN BURISK-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO BURISK-PENALTY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BURISK-HOLD-FLAG = 'Y'
                   COMPUTE BURISK-PENALTY-YTD ROUNDED =
                       BURISK-INTEREST-AMT * BURISK-PENALTY-PCT / 200
               WHEN BURISK-ELECTION-CD = 'L'
                   MOVE 12.00 TO BURISK-PENALTY-YTD
               WHEN BURISK-ELECTION-CD = 'M'
                   MOVE 12.00 TO BURISK-PENALTY-YTD
               WHEN OTHER
                   COMPUTE BURISK-PENALTY-YTD ROUNDED =
                       BURISK-INTEREST-AMT * BURISK-PENALTY-PCT / 100
           END-EVALUATE
           IF BURISK-PENALTY-YTD > 0 AND BURISK-PENALTY-YTD < 1.00
               MOVE 1.00 TO BURISK-PENALTY-YTD
           END-IF
           IF BURISK-PENALTY-YTD > 250.00
               MOVE 250.00 TO BURISK-PENALTY-YTD
           END-IF
           ADD BURISK-PENALTY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE ELECTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2310-DESCRIBE-ELECTION.
           EVALUATE BURISK-ELECTION-CD
               WHEN 'L'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'R'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'V'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2320-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BURISK-REGION-CD
                   ADD BURISK-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BURISK-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BURISK-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2330-COUNT-EXCEPT.
           IF BURISK-EXCEPT-CNT < 9999999
               ADD 1 TO BURISK-EXCEPT-CNT
           END-IF
           IF BURISK-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO BURISK-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY ELECTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2340-ACCUM-ELECTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BURISK-ELECTION-CD
                   ADD BURISK-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BURISK-ELECTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BURISK-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2350-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * BURISK-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADDON YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2360-ACCRUE-ADDON.
           IF BURISK-ADDON-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BURISK-ADDON-YTD * BURISK-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BURISK-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA BANDX TIER FOR THE ADDON AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2370-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-BANDX-RATE(1) TO BURISK-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BURISK-ADDON-AMT NOT < WS-PRINCIPA-BANDX-LOW(WS-TX)
                   MOVE WS-PRINCIPA-BANDX-RATE(WS-TX) TO
                       BURISK-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BURISK-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BURISK-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN LAST PAY DATE IS PAST
      * M -> N WHEN LAST PAY DATE IS PAST
      * N -> R WHEN LAST PAY DATE IS PAST
      * R -> V WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2380-SET-ELECTION.
           EVALUATE BURISK-ELECTION-CD
               WHEN 'L'
                   IF BURISK-LAST-PAY-DATE > 0 AND BURISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'M' TO BURISK-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BURISK-LAST-PAY-DATE > 0 AND BURISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'N' TO BURISK-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BURISK-LAST-PAY-DATE > 0 AND BURISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'R' TO BURISK-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BURISK-LAST-PAY-DATE > 0 AND BURISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'V' TO BURISK-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2390-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BURISK-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BURISK-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BURISK-EXEMPT-FLAG = 'Y'
                   COMPUTE BURISK-INTEREST-YTD ROUNDED = WS-WORK-AMT-2 *
                       BURISK-MARGIN / 200
               WHEN BURISK-REGION-CD = 'BGJ'
                   MOVE 7.00 TO BURISK-INTEREST-YTD
               WHEN BURISK-REGION-CD = 'LFX'
                   MOVE 12.00 TO BURISK-INTEREST-YTD
               WHEN OTHER
                   COMPUTE BURISK-INTEREST-YTD ROUNDED = WS-WORK-AMT-2 *
                       BURISK-MARGIN / 100
           END-EVALUATE
           IF BURISK-INTEREST-YTD > 0 AND BURISK-INTEREST-YTD < 2.00
               MOVE 2.00 TO BURISK-INTEREST-YTD
           END-IF
           IF BURISK-INTEREST-YTD > 250.00
               MOVE 250.00 TO BURISK-INTEREST-YTD
           END-IF
           ADD BURISK-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ADDON AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2400-FLAG-ADDON.
           IF BURISK-ADDON-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BURISK-ADDON-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ELECTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2410-DESCRIBE-ELECTION.
           EVALUATE BURISK-ELECTION-CD
               WHEN 'L'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'M'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'N'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'R'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'V'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN NEXT REVIEW DATE IS PAST
      * M -> N WHEN NEXT REVIEW DATE IS PAST
      * N -> R WHEN NEXT REVIEW DATE IS PAST
      * R -> V WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2420-SET-ELECTION.
           EVALUATE BURISK-ELECTION-CD
               WHEN 'L'
                   IF BURISK-NEXT-REVIEW-DATE > 0 AND
                       BURISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'M' TO BURISK-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BURISK-NEXT-REVIEW-DATE > 0 AND
                       BURISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'N' TO BURISK-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BURISK-NEXT-REVIEW-DATE > 0 AND
                       BURISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'R' TO BURISK-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BURISK-NEXT-REVIEW-DATE > 0 AND
                       BURISK-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'V' TO BURISK-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN LAST PAY DATE IS PAST
      * M -> N WHEN LAST PAY DATE IS PAST
      * N -> R WHEN LAST PAY DATE IS PAST
      * R -> V WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2430-SET-ELECTION.
           EVALUATE BURISK-ELECTION-CD
               WHEN 'L'
                   IF BURISK-LAST-PAY-DATE > 0 AND BURISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'M' TO BURISK-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BURISK-LAST-PAY-DATE > 0 AND BURISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'N' TO BURISK-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BURISK-LAST-PAY-DATE > 0 AND BURISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'R' TO BURISK-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BURISK-LAST-PAY-DATE > 0 AND BURISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'V' TO BURISK-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA BANDX TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2440-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-BANDX-RATE(1) TO BURISK-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BURISK-PRINCIPAL-AMT NOT <
                   WS-PRINCIPA-BANDX-LOW(WS-TX)
                   MOVE WS-PRINCIPA-BANDX-RATE(WS-TX) TO BURISK-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BURISK-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BURISK-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2450-ACCRUE-PRINCIPAL.
           IF BURISK-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BURISK-PRINCIPAL-AMT * BURISK-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2460-ACCRUE-PRINCIPAL.
           IF BURISK-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BURISK-PRINCIPAL-AMT * BURISK-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2470-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BURISK-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BURISK-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BURISK-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2480-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BURISK-LAST-STMT-DATE > 0 AND BURISK-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BURISK-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BURISK-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BURISK-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BURISK-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BURISK-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BURISK-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2490-DESCRIBE-CYCLE.
           EVALUATE BURISK-CYCLE-CD
               WHEN 'A'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'U'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2500-COUNT-CYCLE.
           IF BURISK-CYCLE-CNT < 9999999
               ADD 1 TO BURISK-CYCLE-CNT
           END-IF
           IF BURISK-CYCLE-CNT NOT < 12
               MOVE 'Y' TO BURISK-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2510-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BURISK-LAST-STMT-DATE > 0 AND BURISK-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BURISK-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BURISK-PENALTY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BURISK-PENALTY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BURISK-PENALTY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BURISK-PENALTY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BURISK-PENALTY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E084 CUST ID REQUIRED
      * E582 INTEREST AMT NEGATIVE
      * E618 INTEREST AMT OVER 250000
      * E145 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2520-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BURISK-CUST-ID = SPACES
                   MOVE 'E084' TO WS-REJECT-CODE
               WHEN BURISK-INTEREST-AMT < 0
                   MOVE 'E582' TO WS-REJECT-CODE
               WHEN BURISK-INTEREST-AMT > 250000.00
                   MOVE 'E618' TO WS-REJECT-CODE
               WHEN BURISK-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E145' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ELECTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2530-DESCRIBE-ELECTION.
           EVALUATE BURISK-ELECTION-CD
               WHEN 'L'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'N'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ADDON YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2540-SPLIT-ADDON.
           COMPUTE WS-WORK-AMT = BURISK-ADDON-YTD / 3
           COMPUTE WS-WORK-AMT-2 = BURISK-ADDON-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2550-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BURISK-LAST-STMT-DATE > 0 AND BURISK-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BURISK-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ELECTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2560-DESCRIBE-ELECTION.
           EVALUATE BURISK-ELECTION-CD
               WHEN 'L'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'R'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'V'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2570-COUNT-CYCLE.
           IF BURISK-CYCLE-CNT < 9999999
               ADD 1 TO BURISK-CYCLE-CNT
           END-IF
           IF BURISK-CYCLE-CNT NOT < 6
               MOVE 'Y' TO BURISK-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2580-LIMIT-PENALTY.
           IF BURISK-PENALTY-YTD < 10.00
               MOVE 10.00 TO BURISK-PENALTY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BURISK-PENALTY-YTD > 5000.00
               MOVE 5000.00 TO BURISK-PENALTY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BGJ -> LFX WHEN LAST PAY DATE IS PAST
      * LFX -> MLA WHEN LAST PAY DATE IS PAST
      * MLA -> ULA WHEN LAST PAY DATE IS PAST
      * ULA -> BGJ WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2590-SET-REGION.
           EVALUATE BURISK-REGION-CD
               WHEN 'BGJ'
                   IF BURISK-LAST-PAY-DATE > 0 AND BURISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LFX' TO BURISK-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LFX'
                   IF BURISK-LAST-PAY-DATE > 0 AND BURISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MLA' TO BURISK-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MLA'
                   IF BURISK-LAST-PAY-DATE > 0 AND BURISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ULA' TO BURISK-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULA'
                   IF BURISK-LAST-PAY-DATE > 0 AND BURISK-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BGJ' TO BURISK-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E125 CUST ID REQUIRED
      * E908 PRINCIPAL AMT NEGATIVE
      * E215 PRINCIPAL AMT OVER 250000
      * E563 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2600-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BURISK-CUST-ID = SPACES
                   MOVE 'E125' TO WS-REJECT-CODE
               WHEN BURISK-PRINCIPAL-AMT < 0
                   MOVE 'E908' TO WS-REJECT-CODE
               WHEN BURISK-PRINCIPAL-AMT > 250000.00
                   MOVE 'E215' TO WS-REJECT-CODE
               WHEN BURISK-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E563' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2610-FLAG-INTEREST.
           IF BURISK-INTEREST-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BURISK-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E371 CUST ID REQUIRED
      * E429 INTEREST YTD NEGATIVE
      * E450 INTEREST YTD OVER 250000
      * E244 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2620-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BURISK-CUST-ID = SPACES
                   MOVE 'E371' TO WS-REJECT-CODE
               WHEN BURISK-INTEREST-YTD < 0
                   MOVE 'E429' TO WS-REJECT-CODE
               WHEN BURISK-INTEREST-YTD > 250000.00
                   MOVE 'E450' TO WS-REJECT-CODE
               WHEN BURISK-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E244' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY SCHED TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2630-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-SCHED-RATE(1) TO BURISK-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF BURISK-INTEREST-AMT NOT < WS-PENALTY-SCHED-LOW(WS-TX)
                   MOVE WS-PENALTY-SCHED-RATE(WS-TX) TO
                       BURISK-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BURISK-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BURISK-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2640-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN BURISK-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO BURISK-ADDON-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BURISK-HOLD-FLAG = 'Y'
                   COMPUTE BURISK-ADDON-AMT ROUNDED =
                       BURISK-PRINCIPAL-AMT * BURISK-PENALTY-PCT / 200
               WHEN BURISK-ELECTION-CD = 'L'
                   MOVE 10.00 TO BURISK-ADDON-AMT
               WHEN BURISK-ELECTION-CD = 'M'
                   MOVE 5.00 TO BURISK-ADDON-AMT
               WHEN OTHER
                   COMPUTE BURISK-ADDON-AMT ROUNDED =
                       BURISK-PRINCIPAL-AMT * BURISK-PENALTY-PCT / 100
           END-EVALUATE
           IF BURISK-ADDON-AMT > 0 AND BURISK-ADDON-AMT < 5.00
               MOVE 5.00 TO BURISK-ADDON-AMT
           END-IF
           IF BURISK-ADDON-AMT > 100.00
               MOVE 100.00 TO BURISK-ADDON-AMT
           END-IF
           ADD BURISK-ADDON-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2650-DESCRIBE-REGION.
           EVALUATE BURISK-REGION-CD
               WHEN 'BGJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LFX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MLA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ULA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E053 DESC REQUIRED
      * E061 INTEREST AMT NEGATIVE
      * E914 INTEREST AMT OVER 1000000
      * E658 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2660-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BURISK-DESC = SPACES
                   MOVE 'E053' TO WS-REJECT-CODE
               WHEN BURISK-INTEREST-AMT < 0
                   MOVE 'E061' TO WS-REJECT-CODE
               WHEN BURISK-INTEREST-AMT > 1000000.00
                   MOVE 'E914' TO WS-REJECT-CODE
               WHEN BURISK-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E658' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2670-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2680-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BURISK-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BURISK-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO BURISK-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO BURISK-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO BURISK-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO BURISK-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'BURISK EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
