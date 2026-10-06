      *================================================================*
      * PROGRAM  : MORISK                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : MONEY ORDERS RULE - VALIDATION. CALLED BY THE MONEY *
      *            ORDERS BATCH AND ONLINE PROGRAMS.                   *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-04-26 CRP  ORIGINAL                                       *
      * 1994-06-24 TWH  OUTSTAND RERUN PROTECTION PRB8453              *
      * 1996-08-03 EWS  NEW PAID REPORT COLUMNS REQ6239                *
      * 1999-10-24 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-12-09 EWS  OUTSTAND FEE SCHEDULE UPDATE INC2393           *
      * 2000-07-04 JLM  PERFORMANCE - PAID READ AHEAD CHG7897          *
      * 2001-03-25 DPS  FEE CUTOFF MOVED PRB9958                       *
      * 2003-08-15 DPS  NEW FEE REPORT COLUMNS PRB3506                 *
      * 2003-01-21 TWH  OUTSTAND TABLE EXPANDED REQ6601                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MORISK.
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
                                           VALUE 10.000000.
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
      * WS-FACE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FACE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000011000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000617000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000671000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000255500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000891000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013050000000664000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013550000000820000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013600000000184500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023600000000599000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024100000000705000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024200000000893000'.
       01  WS-FACE-SCHED-TBL REDEFINES WS-FACE-SCHED-VALUES.
           05  WS-FACE-SCHED-ROW OCCURS 11 TIMES.
               10  WS-FACE-SCHED-LOW       PIC 9(09)V99.
               10  WS-FACE-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VOIDED-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOIDED-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000438000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000875000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000344000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000850000000775000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000950000000865000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001200000000491000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001700000000331000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006700000000616000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006800000000708000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007300000000732000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007400000000392000'.
       01  WS-VOIDED-SCHED-TBL REDEFINES WS-VOIDED-SCHED-VALUES.
           05  WS-VOIDED-SCHED-ROW OCCURS 11 TIMES.
               10  WS-VOIDED-SCHED-LOW     PIC 9(09)V99.
               10  WS-VOIDED-SCHED-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY MORISKP.

       PROCEDURE DIVISION USING MORISK-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-PRORATE-OUTSTAND
           PERFORM 2010-COUNT-RETRY
           PERFORM 2020-COUNT-RETRY
           PERFORM 2030-PRORATE-FEE
           PERFORM 2040-COUNT-RETRY
           PERFORM 2050-EDIT-ADDR
           PERFORM 2060-ACCRUE-OUTSTAND
           PERFORM 2070-EDIT-DESC
           PERFORM 2080-ASSESS-PREFERRED
           PERFORM 2090-SPLIT-ESCHEAT
           PERFORM 2100-ASSESS-PREFERRED
           PERFORM 2110-SET-CHANNEL
           PERFORM 2120-FLAG-ESCHEAT
           PERFORM 2130-EDIT-DESC
           PERFORM 2140-PRORATE-FEE
           PERFORM 2150-LIMIT-ESCHEAT
           PERFORM 2160-SPLIT-OUTSTAND
           PERFORM 2170-FLAG-FEE
           PERFORM 2180-AGE-EXPIRY
           PERFORM 2190-PRORATE-PAID
           PERFORM 2200-PRORATE-OUTSTAND
           PERFORM 2210-ACCRUE-PAID
           PERFORM 2220-ACCRUE-PAID
           PERFORM 2230-COUNT-RETRY
           PERFORM 2240-COUNT-RETRY
           PERFORM 2250-FLAG-ITEM
           PERFORM 2260-CHECK-LAST-ACT
           PERFORM 2270-ASSESS-STOP
           PERFORM 2280-DESCRIBE-CYCLE
           PERFORM 2290-SPLIT-FEE
           PERFORM 2300-ACCUM-REASON
           PERFORM 2310-SELECT-FEE-PCT
           PERFORM 2320-ACCUM-REASON
           PERFORM 2330-ACCRUE-OUTSTAND
           PERFORM 2340-PRORATE-OUTSTAND
           PERFORM 2350-ASSESS-PREFERRED
           PERFORM 2360-LIMIT-OUTSTAND
           PERFORM 2370-PRORATE-OUTSTAND
           PERFORM 2380-SET-PAYEE-TYPE
           PERFORM 2390-PRORATE-ESCHEAT
           PERFORM 2400-SELECT-PENALTY-PCT
           PERFORM 2410-EDIT-DESC
           PERFORM 2420-DESCRIBE-PAYEE-TYPE
           PERFORM 2430-FLAG-ESCHEAT
           PERFORM 2440-FLAG-ESCHEAT
           PERFORM 2450-PRORATE-ESCHEAT
           PERFORM 2460-AGE-LAST-ACT
           PERFORM 2470-SET-CYCLE
           PERFORM 2480-LIMIT-OUTSTAND
           PERFORM 2490-EDIT-ADDR
           PERFORM 2500-SET-CYCLE
           PERFORM 2510-SPLIT-FEE
           PERFORM 2520-COUNT-RETRY
           PERFORM 2530-SPLIT-FEE
           PERFORM 2540-SET-CYCLE
           PERFORM 2550-COUNT-RETRY
           PERFORM 2560-ACCRUE-FEE
           PERFORM 2570-ACCUM-REASON
           PERFORM 2580-FLAG-ITEM
           PERFORM 2590-ACCRUE-FEE
           PERFORM 2600-DESCRIBE-REASON
           PERFORM 2610-SELECT-FEE-PCT
           PERFORM 2620-FLAG-FEE
           PERFORM 2630-CHECK-OPEN
           PERFORM 2640-DESCRIBE-PAYEE-TYPE
           PERFORM 2650-SET-CYCLE
           PERFORM 2660-SET-CHANNEL
           PERFORM 2670-COUNT-RETRY
           PERFORM 2680-DESCRIBE-CHANNEL
           PERFORM 2690-SET-CHANNEL
           PERFORM 2700-SET-REASON
           PERFORM 2710-EDIT-DESC
           PERFORM 2720-LIMIT-ITEM
           PERFORM 2730-EDIT-ADDR
           PERFORM 2740-DESCRIBE-CHANNEL
           PERFORM 2750-EDIT-DESC
           PERFORM 2760-AGE-EXPIRY
           PERFORM 2770-COUNT-ITEM
           PERFORM 2780-AGE-EXPIRY
           PERFORM 2790-SELECT-PENALTY-PCT
           PERFORM 2800-DESCRIBE-PAYEE-TYPE
           PERFORM 2810-ACCUM-PAYEE-TYPE
           PERFORM 2820-ACCUM-CHANNEL
           PERFORM 2830-SPLIT-FEE
           PERFORM 2840-PRORATE-OUTSTAND
           PERFORM 2850-DESCRIBE-PAYEE-TYPE
           PERFORM 2860-SET-REASON
           PERFORM 2870-FLAG-FEE
           PERFORM 2880-SPLIT-FEE
           PERFORM 2890-PRORATE-FEE
           PERFORM 2900-DESCRIBE-REASON
           PERFORM 2910-AGE-OPEN
           PERFORM 2920-CHECK-LAST-ACT
           PERFORM 2930-FLAG-OUTSTAND
           PERFORM 2940-PRORATE-PAID
           PERFORM 2950-SELECT-PENALTY-PCT
           PERFORM 2960-FLAG-OUTSTAND
           PERFORM 2970-LIMIT-ITEM
           PERFORM 2980-LIMIT-ESCHEAT
           PERFORM 2990-SELECT-FEE-PCT
           PERFORM 3000-EDIT-ADDR
           PERFORM 3010-SPLIT-PAID
           PERFORM 3020-LIMIT-PAID
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO MORISK-RETURN-CODE.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OUTSTAND YTD.
      *----------------------------------------------------------------*
       2000-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORISK-OUTSTAND-YTD ROUNDED =
                   MORISK-PAID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MORISK-OUTSTAND-YTD
           END-IF
           ADD MORISK-OUTSTAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2010-COUNT-RETRY.
           IF MORISK-RETRY-CNT < 9999999
               ADD 1 TO MORISK-RETRY-CNT
           END-IF
           IF MORISK-RETRY-CNT NOT < 3
               MOVE 'Y' TO MORISK-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2020-COUNT-RETRY.
           IF MORISK-RETRY-CNT < 9999999
               ADD 1 TO MORISK-RETRY-CNT
           END-IF
           IF MORISK-RETRY-CNT NOT < 3
               MOVE 'Y' TO MORISK-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       2030-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORISK-FEE-AMT ROUNDED =
                   MORISK-PAID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORISK-FEE-AMT
           END-IF
           ADD MORISK-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2040-COUNT-RETRY.
           IF MORISK-RETRY-CNT < 9999999
               ADD 1 TO MORISK-RETRY-CNT
           END-IF
           IF MORISK-RETRY-CNT NOT < 6
               MOVE 'Y' TO MORISK-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E311 ADDR REQUIRED
      * E363 PAID AMT NEGATIVE
      * E205 PAID AMT OVER 25000
      * E537 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2050-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORISK-ADDR = SPACES
                   MOVE 'E311' TO WS-REJECT-CODE
               WHEN MORISK-PAID-AMT < 0
                   MOVE 'E363' TO WS-REJECT-CODE
               WHEN MORISK-PAID-AMT > 25000.00
                   MOVE 'E205' TO WS-REJECT-CODE
               WHEN MORISK-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E537' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2060-ACCRUE-OUTSTAND.
           IF MORISK-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORISK-OUTSTAND-AMT * MORISK-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E093 DESC REQUIRED
      * E661 OUTSTAND AMT NEGATIVE
      * E177 OUTSTAND AMT OVER 10000
      * E397 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2070-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORISK-DESC = SPACES
                   MOVE 'E093' TO WS-REJECT-CODE
               WHEN MORISK-OUTSTAND-AMT < 0
                   MOVE 'E661' TO WS-REJECT-CODE
               WHEN MORISK-OUTSTAND-AMT > 10000.00
                   MOVE 'E177' TO WS-REJECT-CODE
               WHEN MORISK-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E397' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2080-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN MORISK-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO MORISK-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORISK-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MORISK-PAID-AMT ROUNDED = WS-WORK-AMT-2 *
                       MORISK-FEE-PCT / 200
               WHEN MORISK-CHANNEL-CD = 'DR'
                   MOVE 10.00 TO MORISK-PAID-AMT
               WHEN MORISK-CHANNEL-CD = 'DX'
                   MOVE 7.00 TO MORISK-PAID-AMT
               WHEN OTHER
                   COMPUTE MORISK-PAID-AMT ROUNDED = WS-WORK-AMT-2 *
                       MORISK-FEE-PCT / 100
           END-EVALUATE
           IF MORISK-PAID-AMT > 0 AND MORISK-PAID-AMT < 5.00
               MOVE 5.00 TO MORISK-PAID-AMT
           END-IF
           IF MORISK-PAID-AMT > 75.00
               MOVE 75.00 TO MORISK-PAID-AMT
           END-IF
           ADD MORISK-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2090-SPLIT-ESCHEAT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE MORISK-ESCHEAT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND YTD FEE ON THE FEE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2100-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN MORISK-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO MORISK-OUTSTAND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORISK-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MORISK-OUTSTAND-YTD ROUNDED = MORISK-FEE-AMT
                       * MORISK-FEE-PCT / 200
               WHEN MORISK-PAYEE-TYPE-CD = 'CJ'
                   MOVE 20.00 TO MORISK-OUTSTAND-YTD
               WHEN MORISK-PAYEE-TYPE-CD = 'FY'
                   MOVE 20.00 TO MORISK-OUTSTAND-YTD
               WHEN OTHER
                   COMPUTE MORISK-OUTSTAND-YTD ROUNDED = MORISK-FEE-AMT
                       * MORISK-FEE-PCT / 100
           END-EVALUATE
           IF MORISK-OUTSTAND-YTD > 0 AND MORISK-OUTSTAND-YTD < 2.00
               MOVE 2.00 TO MORISK-OUTSTAND-YTD
           END-IF
           IF MORISK-OUTSTAND-YTD > 100.00
               MOVE 100.00 TO MORISK-OUTSTAND-YTD
           END-IF
           ADD MORISK-OUTSTAND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * DR -> DX WHEN LAST ACT DATE IS PAST
      * DX -> PT WHEN LAST ACT DATE IS PAST
      * PT -> RN WHEN LAST ACT DATE IS PAST
      * RN -> YD WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2110-SET-CHANNEL.
           EVALUATE MORISK-CHANNEL-CD
               WHEN 'DR'
                   IF MORISK-LAST-ACT-DATE > 0 AND MORISK-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DX' TO MORISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DX'
                   IF MORISK-LAST-ACT-DATE > 0 AND MORISK-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PT' TO MORISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF MORISK-LAST-ACT-DATE > 0 AND MORISK-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RN' TO MORISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RN'
                   IF MORISK-LAST-ACT-DATE > 0 AND MORISK-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'YD' TO MORISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ESCHEAT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2120-FLAG-ESCHEAT.
           IF MORISK-ESCHEAT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MORISK-ESCHEAT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E378 DESC REQUIRED
      * E620 FEE YTD NEGATIVE
      * E011 FEE YTD OVER 1000000
      * E407 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2130-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORISK-DESC = SPACES
                   MOVE 'E378' TO WS-REJECT-CODE
               WHEN MORISK-FEE-YTD < 0
                   MOVE 'E620' TO WS-REJECT-CODE
               WHEN MORISK-FEE-YTD > 1000000.00
                   MOVE 'E011' TO WS-REJECT-CODE
               WHEN MORISK-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E407' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       2140-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORISK-FEE-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORISK-FEE-YTD
           END-IF
           ADD MORISK-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ESCHEAT YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2150-LIMIT-ESCHEAT.
           IF MORISK-ESCHEAT-YTD < 100.00
               MOVE 100.00 TO MORISK-ESCHEAT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORISK-ESCHEAT-YTD > 50000.00
               MOVE 50000.00 TO MORISK-ESCHEAT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2160-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = MORISK-OUTSTAND-AMT / 3
           COMPUTE WS-WORK-AMT-2 = MORISK-OUTSTAND-AMT - (WS-WORK-AMT *
               3)
           COMPUTE MORISK-PAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2170-FLAG-FEE.
           IF MORISK-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORISK-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2180-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MORISK-EXPIRY-DATE > 0 AND MORISK-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORISK-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORISK-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORISK-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MORISK-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD MORISK-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORISK-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       2190-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORISK-PAID-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORISK-PAID-AMT
           END-IF
           ADD MORISK-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OUTSTAND YTD.
      *----------------------------------------------------------------*
       2200-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORISK-OUTSTAND-YTD ROUNDED =
                   MORISK-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MORISK-OUTSTAND-YTD
           END-IF
           ADD MORISK-OUTSTAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2210-ACCRUE-PAID.
           IF MORISK-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORISK-PAID-AMT * MORISK-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORISK-OUTSTAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2220-ACCRUE-PAID.
           IF MORISK-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORISK-PAID-AMT * MORISK-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO MORISK-OUTSTAND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2230-COUNT-RETRY.
           IF MORISK-RETRY-CNT < 9999999
               ADD 1 TO MORISK-RETRY-CNT
           END-IF
           IF MORISK-RETRY-CNT NOT < 5
               MOVE 'Y' TO MORISK-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2240-COUNT-RETRY.
           IF MORISK-RETRY-CNT < 9999999
               ADD 1 TO MORISK-RETRY-CNT
           END-IF
           IF MORISK-RETRY-CNT NOT < 5
               MOVE 'Y' TO MORISK-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2250-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2260-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF MORISK-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORISK-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO MORISK-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2270-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MORISK-STOP-FLAG = 'Y'
                   MOVE ZERO TO MORISK-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORISK-PREFERRED-FLAG = 'Y'
                   COMPUTE MORISK-PAID-AMT ROUNDED = WS-WORK-AMT *
                       MORISK-FEE-PCT / 200
               WHEN MORISK-PAYEE-TYPE-CD = 'CJ'
                   MOVE 10.00 TO MORISK-PAID-AMT
               WHEN MORISK-PAYEE-TYPE-CD = 'FY'
                   MOVE 7.00 TO MORISK-PAID-AMT
               WHEN OTHER
                   COMPUTE MORISK-PAID-AMT ROUNDED = WS-WORK-AMT *
                       MORISK-FEE-PCT / 100
           END-EVALUATE
           IF MORISK-PAID-AMT > 0 AND MORISK-PAID-AMT < 2.00
               MOVE 2.00 TO MORISK-PAID-AMT
           END-IF
           IF MORISK-PAID-AMT > 100.00
               MOVE 100.00 TO MORISK-PAID-AMT
           END-IF
           ADD MORISK-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2280-DESCRIBE-CYCLE.
           EVALUATE MORISK-CYCLE-CD
               WHEN 'DGU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GEJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JBE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2290-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MORISK-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MORISK-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE MORISK-OUTSTAND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2300-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORISK-REASON-CD
                   ADD MORISK-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORISK-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORISK-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FACE SCHED TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2310-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-SCHED-RATE(1) TO MORISK-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF MORISK-FEE-AMT NOT < WS-FACE-SCHED-LOW(WS-TX)
                   MOVE WS-FACE-SCHED-RATE(WS-TX) TO MORISK-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORISK-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORISK-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2320-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORISK-REASON-CD
                   ADD MORISK-PAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORISK-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORISK-PAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2330-ACCRUE-OUTSTAND.
           IF MORISK-OUTSTAND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORISK-OUTSTAND-YTD * MORISK-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORISK-OUTSTAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OUTSTAND AMT.
      *----------------------------------------------------------------*
       2340-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORISK-OUTSTAND-AMT ROUNDED =
                   MORISK-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORISK-OUTSTAND-AMT
           END-IF
           ADD MORISK-OUTSTAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2350-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN MORISK-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORISK-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       MORISK-PENALTY-PCT / 200
               WHEN MORISK-CYCLE-CD = 'DGU'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN MORISK-CYCLE-CD = 'GEJ'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       MORISK-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * OUTSTAND YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2360-LIMIT-OUTSTAND.
           IF MORISK-OUTSTAND-YTD < 100.00
               MOVE 100.00 TO MORISK-OUTSTAND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORISK-OUTSTAND-YTD > 1000.00
               MOVE 1000.00 TO MORISK-OUTSTAND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ESCHEAT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OUTSTAND YTD.
      *----------------------------------------------------------------*
       2370-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORISK-OUTSTAND-YTD ROUNDED =
                   MORISK-ESCHEAT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORISK-OUTSTAND-YTD
           END-IF
           ADD MORISK-OUTSTAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * CJ -> FY WHEN OPEN DATE IS PAST
      * FY -> HX WHEN OPEN DATE IS PAST
      * HX -> UV WHEN OPEN DATE IS PAST
      * UV -> VV WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2380-SET-PAYEE-TYPE.
           EVALUATE MORISK-PAYEE-TYPE-CD
               WHEN 'CJ'
                   IF MORISK-OPEN-DATE > 0 AND MORISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FY' TO MORISK-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FY'
                   IF MORISK-OPEN-DATE > 0 AND MORISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO MORISK-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF MORISK-OPEN-DATE > 0 AND MORISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UV' TO MORISK-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF MORISK-OPEN-DATE > 0 AND MORISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VV' TO MORISK-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ESCHEAT YTD.
      *----------------------------------------------------------------*
       2390-PRORATE-ESCHEAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORISK-ESCHEAT-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MORISK-ESCHEAT-YTD
           END-IF
           ADD MORISK-ESCHEAT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED SCHED TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2400-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-SCHED-RATE(1) TO MORISK-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF MORISK-FEE-AMT NOT < WS-VOIDED-SCHED-LOW(WS-TX)
                   MOVE WS-VOIDED-SCHED-RATE(WS-TX) TO
                       MORISK-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORISK-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORISK-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E716 DESC REQUIRED
      * E496 WORK AMT 2 NEGATIVE
      * E926 WORK AMT 2 OVER 5000
      * E693 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2410-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORISK-DESC = SPACES
                   MOVE 'E716' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E496' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E926' TO WS-REJECT-CODE
               WHEN MORISK-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E693' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2420-DESCRIBE-PAYEE-TYPE.
           EVALUATE MORISK-PAYEE-TYPE-CD
               WHEN 'CJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ESCHEAT YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2430-FLAG-ESCHEAT.
           IF MORISK-ESCHEAT-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORISK-ESCHEAT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2440-FLAG-ESCHEAT.
           IF MORISK-ESCHEAT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORISK-ESCHEAT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ESCHEAT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2450-PRORATE-ESCHEAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MORISK-ESCHEAT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2460-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF MORISK-LAST-ACT-DATE > 0 AND MORISK-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORISK-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORISK-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORISK-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MORISK-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORISK-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORISK-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DGU -> GEJ WHEN EXPIRY DATE IS PAST
      * GEJ -> JBE WHEN EXPIRY DATE IS PAST
      * JBE -> DGU WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2470-SET-CYCLE.
           EVALUATE MORISK-CYCLE-CD
               WHEN 'DGU'
                   IF MORISK-EXPIRY-DATE > 0 AND MORISK-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GEJ' TO MORISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GEJ'
                   IF MORISK-EXPIRY-DATE > 0 AND MORISK-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBE' TO MORISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBE'
                   IF MORISK-EXPIRY-DATE > 0 AND MORISK-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO MORISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OUTSTAND YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2480-LIMIT-OUTSTAND.
           IF MORISK-OUTSTAND-YTD < 10.00
               MOVE 10.00 TO MORISK-OUTSTAND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORISK-OUTSTAND-YTD > 5000.00
               MOVE 5000.00 TO MORISK-OUTSTAND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E022 ADDR REQUIRED
      * E099 WORK AMT NEGATIVE
      * E766 WORK AMT OVER 250000
      * E010 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2490-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORISK-ADDR = SPACES
                   MOVE 'E022' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E099' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E766' TO WS-REJECT-CODE
               WHEN MORISK-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E010' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DGU -> GEJ WHEN EXPIRY DATE IS PAST
      * GEJ -> JBE WHEN EXPIRY DATE IS PAST
      * JBE -> DGU WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2500-SET-CYCLE.
           EVALUATE MORISK-CYCLE-CD
               WHEN 'DGU'
                   IF MORISK-EXPIRY-DATE > 0 AND MORISK-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GEJ' TO MORISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GEJ'
                   IF MORISK-EXPIRY-DATE > 0 AND MORISK-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBE' TO MORISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBE'
                   IF MORISK-EXPIRY-DATE > 0 AND MORISK-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO MORISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2510-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MORISK-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MORISK-FEE-YTD - (WS-WORK-AMT * 2)
           COMPUTE MORISK-OUTSTAND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2520-COUNT-RETRY.
           IF MORISK-RETRY-CNT < 9999999
               ADD 1 TO MORISK-RETRY-CNT
           END-IF
           IF MORISK-RETRY-CNT NOT < 5
               MOVE 'Y' TO MORISK-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2530-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MORISK-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = MORISK-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE MORISK-PAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DGU -> GEJ WHEN OPEN DATE IS PAST
      * GEJ -> JBE WHEN OPEN DATE IS PAST
      * JBE -> DGU WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2540-SET-CYCLE.
           EVALUATE MORISK-CYCLE-CD
               WHEN 'DGU'
                   IF MORISK-OPEN-DATE > 0 AND MORISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GEJ' TO MORISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GEJ'
                   IF MORISK-OPEN-DATE > 0 AND MORISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBE' TO MORISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBE'
                   IF MORISK-OPEN-DATE > 0 AND MORISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO MORISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2550-COUNT-RETRY.
           IF MORISK-RETRY-CNT < 9999999
               ADD 1 TO MORISK-RETRY-CNT
           END-IF
           IF MORISK-RETRY-CNT NOT < 10
               MOVE 'Y' TO MORISK-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2560-ACCRUE-FEE.
           IF MORISK-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORISK-FEE-AMT * MORISK-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OUTSTAND AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2570-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORISK-REASON-CD
                   ADD MORISK-OUTSTAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORISK-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORISK-OUTSTAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2580-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2590-ACCRUE-FEE.
           IF MORISK-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORISK-FEE-YTD * MORISK-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORISK-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2600-DESCRIBE-REASON.
           EVALUATE MORISK-REASON-CD
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'P'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Z'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FACE SCHED TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2610-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-SCHED-RATE(1) TO MORISK-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF MORISK-PAID-AMT NOT < WS-FACE-SCHED-LOW(WS-TX)
                   MOVE WS-FACE-SCHED-RATE(WS-TX) TO MORISK-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORISK-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORISK-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2620-FLAG-FEE.
           IF MORISK-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MORISK-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2630-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MORISK-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORISK-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO MORISK-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2640-DESCRIBE-PAYEE-TYPE.
           EVALUATE MORISK-PAYEE-TYPE-CD
               WHEN 'CJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DGU -> GEJ WHEN OPEN DATE IS PAST
      * GEJ -> JBE WHEN OPEN DATE IS PAST
      * JBE -> DGU WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2650-SET-CYCLE.
           EVALUATE MORISK-CYCLE-CD
               WHEN 'DGU'
                   IF MORISK-OPEN-DATE > 0 AND MORISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GEJ' TO MORISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GEJ'
                   IF MORISK-OPEN-DATE > 0 AND MORISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBE' TO MORISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBE'
                   IF MORISK-OPEN-DATE > 0 AND MORISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO MORISK-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * DR -> DX WHEN NOTICE DATE IS PAST
      * DX -> PT WHEN NOTICE DATE IS PAST
      * PT -> RN WHEN NOTICE DATE IS PAST
      * RN -> YD WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2660-SET-CHANNEL.
           EVALUATE MORISK-CHANNEL-CD
               WHEN 'DR'
                   IF MORISK-NOTICE-DATE > 0 AND MORISK-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO MORISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DX'
                   IF MORISK-NOTICE-DATE > 0 AND MORISK-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO MORISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF MORISK-NOTICE-DATE > 0 AND MORISK-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RN' TO MORISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RN'
                   IF MORISK-NOTICE-DATE > 0 AND MORISK-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YD' TO MORISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2670-COUNT-RETRY.
           IF MORISK-RETRY-CNT < 9999999
               ADD 1 TO MORISK-RETRY-CNT
           END-IF
           IF MORISK-RETRY-CNT NOT < 3
               MOVE 'Y' TO MORISK-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2680-DESCRIBE-CHANNEL.
           EVALUATE MORISK-CHANNEL-CD
               WHEN 'DR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * DR -> DX WHEN EXPIRY DATE IS PAST
      * DX -> PT WHEN EXPIRY DATE IS PAST
      * PT -> RN WHEN EXPIRY DATE IS PAST
      * RN -> YD WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2690-SET-CHANNEL.
           EVALUATE MORISK-CHANNEL-CD
               WHEN 'DR'
                   IF MORISK-EXPIRY-DATE > 0 AND MORISK-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO MORISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DX'
                   IF MORISK-EXPIRY-DATE > 0 AND MORISK-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO MORISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF MORISK-EXPIRY-DATE > 0 AND MORISK-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RN' TO MORISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RN'
                   IF MORISK-EXPIRY-DATE > 0 AND MORISK-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YD' TO MORISK-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> L WHEN NOTICE DATE IS PAST
      * L -> P WHEN NOTICE DATE IS PAST
      * P -> S WHEN NOTICE DATE IS PAST
      * S -> T WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2700-SET-REASON.
           EVALUATE MORISK-REASON-CD
               WHEN 'B'
                   IF MORISK-NOTICE-DATE > 0 AND MORISK-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO MORISK-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF MORISK-NOTICE-DATE > 0 AND MORISK-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO MORISK-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF MORISK-NOTICE-DATE > 0 AND MORISK-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MORISK-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MORISK-NOTICE-DATE > 0 AND MORISK-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MORISK-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E068 DESC REQUIRED
      * E445 FEE YTD NEGATIVE
      * E085 FEE YTD OVER 50000
      * E273 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2710-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORISK-DESC = SPACES
                   MOVE 'E068' TO WS-REJECT-CODE
               WHEN MORISK-FEE-YTD < 0
                   MOVE 'E445' TO WS-REJECT-CODE
               WHEN MORISK-FEE-YTD > 50000.00
                   MOVE 'E085' TO WS-REJECT-CODE
               WHEN MORISK-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E273' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2720-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E884 ADDR REQUIRED
      * E705 WORK AMT 2 NEGATIVE
      * E392 WORK AMT 2 OVER 99999
      * E187 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2730-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORISK-ADDR = SPACES
                   MOVE 'E884' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E705' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E392' TO WS-REJECT-CODE
               WHEN MORISK-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E187' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2740-DESCRIBE-CHANNEL.
           EVALUATE MORISK-CHANNEL-CD
               WHEN 'DR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E011 DESC REQUIRED
      * E605 WORK AMT 2 NEGATIVE
      * E984 WORK AMT 2 OVER 5000
      * E464 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2750-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORISK-DESC = SPACES
                   MOVE 'E011' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E984' TO WS-REJECT-CODE
               WHEN MORISK-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E464' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2760-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MORISK-EXPIRY-DATE > 0 AND MORISK-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORISK-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORISK-PAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORISK-PAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MORISK-PAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD MORISK-PAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORISK-PAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2770-COUNT-ITEM.
           IF MORISK-ITEM-CNT < 9999999
               ADD 1 TO MORISK-ITEM-CNT
           END-IF
           IF MORISK-ITEM-CNT NOT < 10
               MOVE 'Y' TO MORISK-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2780-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MORISK-EXPIRY-DATE > 0 AND MORISK-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORISK-EXPIRY-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FACE SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2790-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-SCHED-RATE(1) TO MORISK-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-FACE-SCHED-LOW(WS-TX)
                   MOVE WS-FACE-SCHED-RATE(WS-TX) TO MORISK-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORISK-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORISK-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2800-DESCRIBE-PAYEE-TYPE.
           EVALUATE MORISK-PAYEE-TYPE-CD
               WHEN 'CJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE OUTSTAND YTD BY PAYEE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2810-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORISK-PAYEE-TYPE-CD
                   ADD MORISK-OUTSTAND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORISK-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORISK-OUTSTAND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2820-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORISK-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORISK-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2830-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MORISK-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = MORISK-FEE-YTD - (WS-WORK-AMT * 3)
           COMPUTE MORISK-OUTSTAND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OUTSTAND AMT.
      *----------------------------------------------------------------*
       2840-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORISK-OUTSTAND-AMT ROUNDED =
                   MORISK-OUTSTAND-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MORISK-OUTSTAND-AMT
           END-IF
           ADD MORISK-OUTSTAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2850-DESCRIBE-PAYEE-TYPE.
           EVALUATE MORISK-PAYEE-TYPE-CD
               WHEN 'CJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> L WHEN EXPIRY DATE IS PAST
      * L -> P WHEN EXPIRY DATE IS PAST
      * P -> S WHEN EXPIRY DATE IS PAST
      * S -> T WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2860-SET-REASON.
           EVALUATE MORISK-REASON-CD
               WHEN 'B'
                   IF MORISK-EXPIRY-DATE > 0 AND MORISK-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO MORISK-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF MORISK-EXPIRY-DATE > 0 AND MORISK-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO MORISK-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF MORISK-EXPIRY-DATE > 0 AND MORISK-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MORISK-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MORISK-EXPIRY-DATE > 0 AND MORISK-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MORISK-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2870-FLAG-FEE.
           IF MORISK-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORISK-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2880-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE MORISK-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2890-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORISK-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2900-DESCRIBE-REASON.
           EVALUATE MORISK-REASON-CD
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2910-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MORISK-OPEN-DATE > 0 AND MORISK-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORISK-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORISK-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORISK-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MORISK-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORISK-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORISK-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2920-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF MORISK-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORISK-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO MORISK-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2930-FLAG-OUTSTAND.
           IF MORISK-OUTSTAND-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORISK-OUTSTAND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       2940-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORISK-PAID-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MORISK-PAID-AMT
           END-IF
           ADD MORISK-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FACE SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2950-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-SCHED-RATE(1) TO MORISK-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-FACE-SCHED-LOW(WS-TX)
                   MOVE WS-FACE-SCHED-RATE(WS-TX) TO MORISK-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORISK-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORISK-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2960-FLAG-OUTSTAND.
           IF MORISK-OUTSTAND-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORISK-OUTSTAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2970-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2980-LIMIT-ESCHEAT.
           IF MORISK-ESCHEAT-YTD < 25.00
               MOVE 25.00 TO MORISK-ESCHEAT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORISK-ESCHEAT-YTD > 500.00
               MOVE 500.00 TO MORISK-ESCHEAT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FACE SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2990-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-SCHED-RATE(1) TO MORISK-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-FACE-SCHED-LOW(WS-TX)
                   MOVE WS-FACE-SCHED-RATE(WS-TX) TO MORISK-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORISK-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORISK-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E092 ADDR REQUIRED
      * E860 FEE YTD NEGATIVE
      * E920 FEE YTD OVER 5000
      * E502 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3000-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORISK-ADDR = SPACES
                   MOVE 'E092' TO WS-REJECT-CODE
               WHEN MORISK-FEE-YTD < 0
                   MOVE 'E860' TO WS-REJECT-CODE
               WHEN MORISK-FEE-YTD > 5000.00
                   MOVE 'E920' TO WS-REJECT-CODE
               WHEN MORISK-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E502' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3010-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = MORISK-PAID-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MORISK-PAID-AMT - (WS-WORK-AMT * 4)
           COMPUTE MORISK-OUTSTAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3020-LIMIT-PAID.
           IF MORISK-PAID-AMT < 0.00
               MOVE 0.00 TO MORISK-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORISK-PAID-AMT > 2500.00
               MOVE 2500.00 TO MORISK-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO MORISK-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO MORISK-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO MORISK-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'MORISK EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
