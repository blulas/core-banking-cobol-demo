      *================================================================*
      * PROGRAM  : EUPRC                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : EURO CONVERSION RULE - DATE RULES. CALLED BY THE    *
      *            EURO CONVERSION BATCH AND ONLINE PROGRAMS.          *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-01-17 GAF  ORIGINAL                                       *
      * 1996-03-13 EWS  EURO LIMIT REVISED INC5890                     *
      * 1998-03-27 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-11-22 LMB  NATIONAL EXCEPTIONS TO OPS QUEUE PRB4324       *
      * 1999-06-17 GAF  RESIDUAL CALCULATION CORRECTED INC7254         *
      * 2000-06-03 MKL  PERFORMANCE - EURO READ AHEAD INC4340          *
      * 2002-06-17 EWS  RESIDUAL RERUN PROTECTION INC7431              *
      * 2003-04-21 TWH  NATIONAL FEE SCHEDULE UPDATE CHG8552           *
      * 2005-07-05 TWH  CONVERTED CUTOFF MOVED INC6614                 *
      * 2007-04-10 MKL  ROUNDING LIMIT REVISED INC8240                 *
      * 2008-03-02 CRP  RESIDUAL ROUNDING - HALF UP REQ0855            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EUPRC.
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
                                           VALUE 20.000000.
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
      * WS-FEE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000569000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000512000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000553000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004000000000483000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004500000000424000'.
       01  WS-FEE-GRID-TBL REDEFINES WS-FEE-GRID-VALUES.
           05  WS-FEE-GRID-ROW OCCURS 5 TIMES.
               10  WS-FEE-GRID-LOW         PIC 9(09)V99.
               10  WS-FEE-GRID-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RESIDUAL-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RESIDUAL-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000233500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000452000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000881000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011050000000379000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000867000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013600000000581000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013850000000590000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013900000000138000'.
       01  WS-RESIDUAL-TIER-TBL REDEFINES WS-RESIDUAL-TIER-VALUES.
           05  WS-RESIDUAL-TIER-ROW OCCURS 8 TIMES.
               10  WS-RESIDUAL-TIER-LOW    PIC 9(09)V99.
               10  WS-RESIDUAL-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-GRIDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-GRIDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000859500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000014000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000611500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008500000000660000'.
       01  WS-FEE-GRIDX-TBL REDEFINES WS-FEE-GRIDX-VALUES.
           05  WS-FEE-GRIDX-ROW OCCURS 4 TIMES.
               10  WS-FEE-GRIDX-LOW        PIC 9(09)V99.
               10  WS-FEE-GRIDX-RATE       PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY EUPRCP.

       PROCEDURE DIVISION USING EUPRC-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-AGE-NEXT-REVIEW
           PERFORM 2010-SPLIT-CONVERTED
           PERFORM 2020-EDIT-CONTACT
           PERFORM 2030-ASSESS-AUTO-RENEW
           PERFORM 2040-ACCRUE-FEE
           PERFORM 2050-LIMIT-ITEM
           PERFORM 2060-DESCRIBE-CHANNEL
           PERFORM 2070-PRORATE-ROUNDING
           PERFORM 2080-SET-CONV-METHOD
           PERFORM 2090-SET-TYPE
           PERFORM 2100-ASSESS-REVIEW
           PERFORM 2110-SELECT-RATE
           PERFORM 2120-ASSESS-OVERRIDE
           PERFORM 2130-SELECT-RATE
           PERFORM 2140-CHECK-NEXT-REVIEW
           PERFORM 2150-AGE-LAST-PAY
           PERFORM 2160-ASSESS-OVERRIDE
           PERFORM 2170-PRORATE-CONVERTED
           PERFORM 2180-DESCRIBE-CHANNEL
           PERFORM 2190-SELECT-RATE
           PERFORM 2200-SPLIT-ITEM
           PERFORM 2210-EDIT-CONTACT
           PERFORM 2220-SELECT-PENALTY-PCT
           PERFORM 2230-PRORATE-CONVERTED
           PERFORM 2240-PRORATE-EURO
           PERFORM 2250-SPLIT-ROUNDING
           PERFORM 2260-CHECK-LAST-STMT
           PERFORM 2270-FLAG-EURO
           PERFORM 2280-COUNT-CYCLE
           PERFORM 2290-AGE-LAST-STMT
           PERFORM 2300-EDIT-DESC
           PERFORM 2310-CHECK-LAST-STMT
           PERFORM 2320-DESCRIBE-TYPE
           PERFORM 2330-ACCRUE-PENALTY-PCT
           PERFORM 2340-SPLIT-FEE
           PERFORM 2350-COUNT-EXCEPT
           PERFORM 2360-ACCRUE-RATE
           PERFORM 2370-CHECK-LAST-PAY
           PERFORM 2380-CHECK-LAST-STMT
           PERFORM 2390-COUNT-CYCLE
           PERFORM 2400-CHECK-LAST-STMT
           PERFORM 2410-ACCRUE-EURO
           PERFORM 2420-LIMIT-ITEM
           PERFORM 2430-ACCRUE-ROUNDING
           PERFORM 2440-CHECK-NEXT-REVIEW
           PERFORM 2450-FLAG-EURO
           PERFORM 2460-SELECT-PENALTY-PCT
           PERFORM 2470-SET-CHANNEL
           PERFORM 2480-EDIT-CONTACT
           PERFORM 2490-COUNT-EXCEPT
           PERFORM 2500-SPLIT-EURO
           PERFORM 2510-ACCUM-CONV-METHOD
           PERFORM 2520-EDIT-DESC
           PERFORM 2530-COUNT-EXCEPT
           PERFORM 2540-SELECT-PENALTY-PCT
           PERFORM 2550-SPLIT-EURO
           PERFORM 2560-SET-TYPE
           PERFORM 2570-ASSESS-REVIEW
           PERFORM 2580-ASSESS-REVIEW
           PERFORM 2590-FLAG-CONVERTED
           PERFORM 2600-ACCUM-CONV-METHOD
           PERFORM 2610-PRORATE-CONVERTED
           PERFORM 2620-ASSESS-AUTO-RENEW
           PERFORM 2630-FLAG-FEE
           PERFORM 2640-PRORATE-EURO
           PERFORM 2650-SPLIT-EURO
           PERFORM 2660-LIMIT-EURO
           PERFORM 2670-ASSESS-REVIEW
           PERFORM 2680-CHECK-LAST-PAY
           PERFORM 2690-SELECT-PENALTY-PCT
           PERFORM 2700-SPLIT-CONVERTED
           PERFORM 2710-ACCRUE-EURO
           PERFORM 2720-LIMIT-CONVERTED
           PERFORM 2730-AGE-LAST-MAINT
           PERFORM 2740-FLAG-CONVERTED
           PERFORM 2750-ACCRUE-PENALTY-PCT
           PERFORM 2760-AGE-LAST-STMT
           PERFORM 2770-ACCRUE-CONVERTED
           PERFORM 2780-ASSESS-REVIEW
           PERFORM 2790-ACCUM-TYPE
           PERFORM 2800-AGE-LAST-PAY
           PERFORM 2810-EDIT-CONTACT
           PERFORM 2820-COUNT-CYCLE
           PERFORM 2830-COUNT-EXCEPT
           PERFORM 2840-AGE-LAST-PAY
           PERFORM 2850-LIMIT-CONVERTED
           PERFORM 2860-ACCRUE-ROUNDING
           PERFORM 2870-SPLIT-EURO
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO EUPRC-RETURN-CODE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2000-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF EUPRC-NEXT-REVIEW-DATE > 0 AND EUPRC-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUPRC-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUPRC-EURO-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EUPRC-EURO-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EUPRC-EURO-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EUPRC-EURO-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUPRC-EURO-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CONVERTED YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2010-SPLIT-CONVERTED.
           COMPUTE WS-WORK-AMT = EUPRC-CONVERTED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = EUPRC-CONVERTED-YTD - (WS-WORK-AMT *
               3)
           COMPUTE EUPRC-CONVERTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E093 CONTACT REQUIRED
      * E138 CONVERTED AMT NEGATIVE
      * E910 CONVERTED AMT OVER 1000000
      * E486 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2020-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUPRC-CONTACT = SPACES
                   MOVE 'E093' TO WS-REJECT-CODE
               WHEN EUPRC-CONVERTED-AMT < 0
                   MOVE 'E138' TO WS-REJECT-CODE
               WHEN EUPRC-CONVERTED-AMT > 1000000.00
                   MOVE 'E910' TO WS-REJECT-CODE
               WHEN EUPRC-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E486' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE ROUNDING YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2030-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN EUPRC-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUPRC-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = EUPRC-ROUNDING-YTD *
                       EUPRC-PENALTY-PCT / 200
               WHEN EUPRC-CONV-METHOD-CD = 'GL'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN EUPRC-CONV-METHOD-CD = 'JA'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = EUPRC-ROUNDING-YTD *
                       EUPRC-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2040-ACCRUE-FEE.
           IF EUPRC-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUPRC-FEE-AMT * EUPRC-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO EUPRC-ROUNDING-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2050-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2060-DESCRIBE-CHANNEL.
           EVALUATE EUPRC-CHANNEL-CD
               WHEN 'AEA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'EBM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GAU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GSU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VWP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XDF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ROUNDING YTD.
      *----------------------------------------------------------------*
       2070-PRORATE-ROUNDING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUPRC-ROUNDING-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EUPRC-ROUNDING-YTD
           END-IF
           ADD EUPRC-ROUNDING-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CONV METHOD CD:
      * GL -> JA WHEN LAST PAY DATE IS PAST
      * JA -> LD WHEN LAST PAY DATE IS PAST
      * LD -> MR WHEN LAST PAY DATE IS PAST
      * MR -> VC WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2080-SET-CONV-METHOD.
           EVALUATE EUPRC-CONV-METHOD-CD
               WHEN 'GL'
                   IF EUPRC-LAST-PAY-DATE > 0 AND EUPRC-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JA' TO EUPRC-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF EUPRC-LAST-PAY-DATE > 0 AND EUPRC-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO EUPRC-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF EUPRC-LAST-PAY-DATE > 0 AND EUPRC-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO EUPRC-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF EUPRC-LAST-PAY-DATE > 0 AND EUPRC-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VC' TO EUPRC-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGL -> SMF WHEN NEXT REVIEW DATE IS PAST
      * SMF -> UEC WHEN NEXT REVIEW DATE IS PAST
      * UEC -> VJA WHEN NEXT REVIEW DATE IS PAST
      * VJA -> WPH WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2090-SET-TYPE.
           EVALUATE EUPRC-TYPE-CD
               WHEN 'AGL'
                   IF EUPRC-NEXT-REVIEW-DATE > 0 AND
                       EUPRC-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'SMF' TO EUPRC-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMF'
                   IF EUPRC-NEXT-REVIEW-DATE > 0 AND
                       EUPRC-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UEC' TO EUPRC-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UEC'
                   IF EUPRC-NEXT-REVIEW-DATE > 0 AND
                       EUPRC-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VJA' TO EUPRC-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJA'
                   IF EUPRC-NEXT-REVIEW-DATE > 0 AND
                       EUPRC-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'WPH' TO EUPRC-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE CONVERTED AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2100-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN EUPRC-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUPRC-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = EUPRC-CONVERTED-AMT *
                       EUPRC-PENALTY-PCT / 200
               WHEN EUPRC-CHANNEL-CD = 'AEA'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN EUPRC-CHANNEL-CD = 'EBM'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = EUPRC-CONVERTED-AMT *
                       EUPRC-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE EURO YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2110-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO EUPRC-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF EUPRC-EURO-YTD NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO EUPRC-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUPRC-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUPRC-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EURO YTD FEE ON THE EURO AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2120-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN EUPRC-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO EUPRC-EURO-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUPRC-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE EUPRC-EURO-YTD ROUNDED = EUPRC-EURO-AMT *
                       EUPRC-RATE / 200
               WHEN EUPRC-CONV-METHOD-CD = 'GL'
                   MOVE 7.00 TO EUPRC-EURO-YTD
               WHEN EUPRC-CONV-METHOD-CD = 'JA'
                   MOVE 5.00 TO EUPRC-EURO-YTD
               WHEN OTHER
                   COMPUTE EUPRC-EURO-YTD ROUNDED = EUPRC-EURO-AMT *
                       EUPRC-RATE / 100
           END-EVALUATE
           IF EUPRC-EURO-YTD > 0 AND EUPRC-EURO-YTD < 10.00
               MOVE 10.00 TO EUPRC-EURO-YTD
           END-IF
           IF EUPRC-EURO-YTD > 75.00
               MOVE 75.00 TO EUPRC-EURO-YTD
           END-IF
           ADD EUPRC-EURO-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRIDX TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2130-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRIDX-RATE(1) TO EUPRC-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF EUPRC-FEE-AMT NOT < WS-FEE-GRIDX-LOW(WS-TX)
                   MOVE WS-FEE-GRIDX-RATE(WS-TX) TO EUPRC-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUPRC-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUPRC-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2140-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF EUPRC-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUPRC-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO EUPRC-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2150-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EUPRC-LAST-PAY-DATE > 0 AND EUPRC-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUPRC-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CONVERTED YTD FEE ON THE CONVERTED AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2160-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN EUPRC-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO EUPRC-CONVERTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUPRC-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE EUPRC-CONVERTED-YTD ROUNDED =
                       EUPRC-CONVERTED-AMT * EUPRC-PENALTY-PCT / 200
               WHEN EUPRC-CURRENCY-CD = 'EW'
                   MOVE 10.00 TO EUPRC-CONVERTED-YTD
               WHEN EUPRC-CURRENCY-CD = 'FJ'
                   MOVE 12.00 TO EUPRC-CONVERTED-YTD
               WHEN OTHER
                   COMPUTE EUPRC-CONVERTED-YTD ROUNDED =
                       EUPRC-CONVERTED-AMT * EUPRC-PENALTY-PCT / 100
           END-EVALUATE
           IF EUPRC-CONVERTED-YTD > 0 AND EUPRC-CONVERTED-YTD < 2.00
               MOVE 2.00 TO EUPRC-CONVERTED-YTD
           END-IF
           IF EUPRC-CONVERTED-YTD > 100.00
               MOVE 100.00 TO EUPRC-CONVERTED-YTD
           END-IF
           ADD EUPRC-CONVERTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE EURO AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONVERTED AMT.
      *----------------------------------------------------------------*
       2170-PRORATE-CONVERTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUPRC-CONVERTED-AMT ROUNDED =
                   EUPRC-EURO-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EUPRC-CONVERTED-AMT
           END-IF
           ADD EUPRC-CONVERTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2180-DESCRIBE-CHANNEL.
           EVALUATE EUPRC-CHANNEL-CD
               WHEN 'AEA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'EBM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GAU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GSU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VWP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XDF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RESIDUAL TIER TIER FOR THE CONVERTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2190-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RESIDUAL-TIER-RATE(1) TO EUPRC-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF EUPRC-CONVERTED-AMT NOT < WS-RESIDUAL-TIER-LOW(WS-TX)
                   MOVE WS-RESIDUAL-TIER-RATE(WS-TX) TO EUPRC-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUPRC-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUPRC-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2200-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E633 CONTACT REQUIRED
      * E631 WORK AMT NEGATIVE
      * E023 WORK AMT OVER 99999
      * E641 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2210-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUPRC-CONTACT = SPACES
                   MOVE 'E633' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E631' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E023' TO WS-REJECT-CODE
               WHEN EUPRC-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E641' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRIDX TIER FOR THE EURO YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2220-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRIDX-RATE(1) TO EUPRC-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF EUPRC-EURO-YTD NOT < WS-FEE-GRIDX-LOW(WS-TX)
                   MOVE WS-FEE-GRIDX-RATE(WS-TX) TO EUPRC-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUPRC-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUPRC-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONVERTED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2230-PRORATE-CONVERTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   EUPRC-CONVERTED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE EURO AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2240-PRORATE-EURO.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUPRC-EURO-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT ROUNDING YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2250-SPLIT-ROUNDING.
           COMPUTE WS-WORK-AMT = EUPRC-ROUNDING-YTD / 3
           COMPUTE WS-WORK-AMT-2 = EUPRC-ROUNDING-YTD - (WS-WORK-AMT *
               3)
           COMPUTE EUPRC-CONVERTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2260-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EUPRC-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUPRC-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO EUPRC-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EURO AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2270-FLAG-EURO.
           IF EUPRC-EURO-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EUPRC-EURO-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2280-COUNT-CYCLE.
           IF EUPRC-CYCLE-CNT < 9999999
               ADD 1 TO EUPRC-CYCLE-CNT
           END-IF
           IF EUPRC-CYCLE-CNT NOT < 5
               MOVE 'Y' TO EUPRC-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2290-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EUPRC-LAST-STMT-DATE > 0 AND EUPRC-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUPRC-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUPRC-ROUNDING-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EUPRC-ROUNDING-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EUPRC-ROUNDING-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EUPRC-ROUNDING-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUPRC-ROUNDING-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E224 DESC REQUIRED
      * E226 CONVERTED YTD NEGATIVE
      * E079 CONVERTED YTD OVER 5000
      * E153 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       2300-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUPRC-DESC = SPACES
                   MOVE 'E224' TO WS-REJECT-CODE
               WHEN EUPRC-CONVERTED-YTD < 0
                   MOVE 'E226' TO WS-REJECT-CODE
               WHEN EUPRC-CONVERTED-YTD > 5000.00
                   MOVE 'E079' TO WS-REJECT-CODE
               WHEN EUPRC-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E153' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2310-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EUPRC-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUPRC-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO EUPRC-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2320-DESCRIBE-TYPE.
           EVALUATE EUPRC-TYPE-CD
               WHEN 'AGL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SMF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UEC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VJA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WPH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2330-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * EUPRC-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO EUPRC-EURO-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2340-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE EUPRC-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2350-COUNT-EXCEPT.
           IF EUPRC-EXCEPT-CNT < 9999999
               ADD 1 TO EUPRC-EXCEPT-CNT
           END-IF
           IF EUPRC-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO EUPRC-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2360-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * EUPRC-RATE / 100 / 360
               ADD WS-WORK-AMT TO EUPRC-CONVERTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2370-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EUPRC-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUPRC-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO EUPRC-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2380-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EUPRC-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUPRC-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO EUPRC-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2390-COUNT-CYCLE.
           IF EUPRC-CYCLE-CNT < 9999999
               ADD 1 TO EUPRC-CYCLE-CNT
           END-IF
           IF EUPRC-CYCLE-CNT NOT < 3
               MOVE 'Y' TO EUPRC-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2400-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EUPRC-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUPRC-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO EUPRC-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EURO YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2410-ACCRUE-EURO.
           IF EUPRC-EURO-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUPRC-EURO-YTD * EUPRC-RATE / 100 / 365
               ADD WS-WORK-AMT TO EUPRC-CONVERTED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2420-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROUNDING YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2430-ACCRUE-ROUNDING.
           IF EUPRC-ROUNDING-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUPRC-ROUNDING-YTD * EUPRC-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2440-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF EUPRC-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUPRC-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO EUPRC-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EURO AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2450-FLAG-EURO.
           IF EUPRC-EURO-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EUPRC-EURO-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRIDX TIER FOR THE EURO AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2460-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRIDX-RATE(1) TO EUPRC-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF EUPRC-EURO-AMT NOT < WS-FEE-GRIDX-LOW(WS-TX)
                   MOVE WS-FEE-GRIDX-RATE(WS-TX) TO EUPRC-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUPRC-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUPRC-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AEA -> EBM WHEN NEXT REVIEW DATE IS PAST
      * EBM -> GAU WHEN NEXT REVIEW DATE IS PAST
      * GAU -> GSU WHEN NEXT REVIEW DATE IS PAST
      * GSU -> VWP WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2470-SET-CHANNEL.
           EVALUATE EUPRC-CHANNEL-CD
               WHEN 'AEA'
                   IF EUPRC-NEXT-REVIEW-DATE > 0 AND
                       EUPRC-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'EBM' TO EUPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EBM'
                   IF EUPRC-NEXT-REVIEW-DATE > 0 AND
                       EUPRC-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GAU' TO EUPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAU'
                   IF EUPRC-NEXT-REVIEW-DATE > 0 AND
                       EUPRC-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GSU' TO EUPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GSU'
                   IF EUPRC-NEXT-REVIEW-DATE > 0 AND
                       EUPRC-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VWP' TO EUPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E542 CONTACT REQUIRED
      * E518 CONVERTED AMT NEGATIVE
      * E440 CONVERTED AMT OVER 50000
      * E157 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2480-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUPRC-CONTACT = SPACES
                   MOVE 'E542' TO WS-REJECT-CODE
               WHEN EUPRC-CONVERTED-AMT < 0
                   MOVE 'E518' TO WS-REJECT-CODE
               WHEN EUPRC-CONVERTED-AMT > 50000.00
                   MOVE 'E440' TO WS-REJECT-CODE
               WHEN EUPRC-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E157' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2490-COUNT-EXCEPT.
           IF EUPRC-EXCEPT-CNT < 9999999
               ADD 1 TO EUPRC-EXCEPT-CNT
           END-IF
           IF EUPRC-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO EUPRC-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EURO YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2500-SPLIT-EURO.
           COMPUTE WS-WORK-AMT = EUPRC-EURO-YTD / 12
           COMPUTE WS-WORK-AMT-2 = EUPRC-EURO-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EURO AMT BY CONV METHOD CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2510-ACCUM-CONV-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUPRC-CONV-METHOD-CD
                   ADD EUPRC-EURO-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUPRC-CONV-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUPRC-EURO-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E881 DESC REQUIRED
      * E108 ROUNDING YTD NEGATIVE
      * E928 ROUNDING YTD OVER 250000
      * E764 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2520-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUPRC-DESC = SPACES
                   MOVE 'E881' TO WS-REJECT-CODE
               WHEN EUPRC-ROUNDING-YTD < 0
                   MOVE 'E108' TO WS-REJECT-CODE
               WHEN EUPRC-ROUNDING-YTD > 250000.00
                   MOVE 'E928' TO WS-REJECT-CODE
               WHEN EUPRC-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E764' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2530-COUNT-EXCEPT.
           IF EUPRC-EXCEPT-CNT < 9999999
               ADD 1 TO EUPRC-EXCEPT-CNT
           END-IF
           IF EUPRC-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO EUPRC-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2540-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO EUPRC-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO EUPRC-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUPRC-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUPRC-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EURO YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2550-SPLIT-EURO.
           COMPUTE WS-WORK-AMT = EUPRC-EURO-YTD / 4
           COMPUTE WS-WORK-AMT-2 = EUPRC-EURO-YTD - (WS-WORK-AMT * 4)
           COMPUTE EUPRC-CONVERTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGL -> SMF WHEN LAST PAY DATE IS PAST
      * SMF -> UEC WHEN LAST PAY DATE IS PAST
      * UEC -> VJA WHEN LAST PAY DATE IS PAST
      * VJA -> WPH WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2560-SET-TYPE.
           EVALUATE EUPRC-TYPE-CD
               WHEN 'AGL'
                   IF EUPRC-LAST-PAY-DATE > 0 AND EUPRC-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SMF' TO EUPRC-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMF'
                   IF EUPRC-LAST-PAY-DATE > 0 AND EUPRC-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UEC' TO EUPRC-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UEC'
                   IF EUPRC-LAST-PAY-DATE > 0 AND EUPRC-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJA' TO EUPRC-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJA'
                   IF EUPRC-LAST-PAY-DATE > 0 AND EUPRC-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WPH' TO EUPRC-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE CONVERTED AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2570-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN EUPRC-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO EUPRC-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUPRC-OVERRIDE-FLAG = 'Y'
                   COMPUTE EUPRC-FEE-AMT ROUNDED = EUPRC-CONVERTED-AMT *
                       EUPRC-RATE / 200
               WHEN EUPRC-TYPE-CD = 'AGL'
                   MOVE 5.00 TO EUPRC-FEE-AMT
               WHEN EUPRC-TYPE-CD = 'SMF'
                   MOVE 12.00 TO EUPRC-FEE-AMT
               WHEN OTHER
                   COMPUTE EUPRC-FEE-AMT ROUNDED = EUPRC-CONVERTED-AMT *
                       EUPRC-RATE / 100
           END-EVALUATE
           IF EUPRC-FEE-AMT > 0 AND EUPRC-FEE-AMT < 1.00
               MOVE 1.00 TO EUPRC-FEE-AMT
           END-IF
           IF EUPRC-FEE-AMT > 250.00
               MOVE 250.00 TO EUPRC-FEE-AMT
           END-IF
           ADD EUPRC-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CONVERTED FEE ON THE EURO AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2580-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN EUPRC-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO EUPRC-CONVERTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUPRC-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE EUPRC-CONVERTED-AMT ROUNDED = EUPRC-EURO-AMT
                       * EUPRC-PENALTY-PCT / 200
               WHEN EUPRC-CONV-METHOD-CD = 'GL'
                   MOVE 10.00 TO EUPRC-CONVERTED-AMT
               WHEN EUPRC-CONV-METHOD-CD = 'JA'
                   MOVE 20.00 TO EUPRC-CONVERTED-AMT
               WHEN OTHER
                   COMPUTE EUPRC-CONVERTED-AMT ROUNDED = EUPRC-EURO-AMT
                       * EUPRC-PENALTY-PCT / 100
           END-EVALUATE
           IF EUPRC-CONVERTED-AMT > 0 AND EUPRC-CONVERTED-AMT < 5.00
               MOVE 5.00 TO EUPRC-CONVERTED-AMT
           END-IF
           IF EUPRC-CONVERTED-AMT > 35.00
               MOVE 35.00 TO EUPRC-CONVERTED-AMT
           END-IF
           ADD EUPRC-CONVERTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CONVERTED YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2590-FLAG-CONVERTED.
           IF EUPRC-CONVERTED-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EUPRC-CONVERTED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CONV METHOD CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2600-ACCUM-CONV-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUPRC-CONV-METHOD-CD
                   ADD EUPRC-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUPRC-CONV-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUPRC-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONVERTED YTD.
      *----------------------------------------------------------------*
       2610-PRORATE-CONVERTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUPRC-CONVERTED-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EUPRC-CONVERTED-YTD
           END-IF
           ADD EUPRC-CONVERTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE CONVERTED YTD FEE ON THE EURO AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2620-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN EUPRC-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO EUPRC-CONVERTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUPRC-REVIEW-FLAG = 'Y'
                   COMPUTE EUPRC-CONVERTED-YTD ROUNDED = EUPRC-EURO-AMT
                       * EUPRC-RATE / 200
               WHEN EUPRC-CURRENCY-CD = 'EW'
                   MOVE 3.00 TO EUPRC-CONVERTED-YTD
               WHEN EUPRC-CURRENCY-CD = 'FJ'
                   MOVE 5.00 TO EUPRC-CONVERTED-YTD
               WHEN OTHER
                   COMPUTE EUPRC-CONVERTED-YTD ROUNDED = EUPRC-EURO-AMT
                       * EUPRC-RATE / 100
           END-EVALUATE
           IF EUPRC-CONVERTED-YTD > 0 AND EUPRC-CONVERTED-YTD < 5.00
               MOVE 5.00 TO EUPRC-CONVERTED-YTD
           END-IF
           IF EUPRC-CONVERTED-YTD > 35.00
               MOVE 35.00 TO EUPRC-CONVERTED-YTD
           END-IF
           ADD EUPRC-CONVERTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FEE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2630-FLAG-FEE.
           IF EUPRC-FEE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EUPRC-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONVERTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO EURO AMT.
      *----------------------------------------------------------------*
       2640-PRORATE-EURO.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EUPRC-EURO-AMT ROUNDED =
                   EUPRC-CONVERTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EUPRC-EURO-AMT
           END-IF
           ADD EUPRC-EURO-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT EURO AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2650-SPLIT-EURO.
           COMPUTE WS-WORK-AMT = EUPRC-EURO-AMT / 12
           COMPUTE WS-WORK-AMT-2 = EUPRC-EURO-AMT - (WS-WORK-AMT * 12)
           COMPUTE EUPRC-CONVERTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EURO YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2660-LIMIT-EURO.
           IF EUPRC-EURO-YTD < 100.00
               MOVE 100.00 TO EUPRC-EURO-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EUPRC-EURO-YTD > 5000.00
               MOVE 5000.00 TO EUPRC-EURO-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EURO FEE ON THE CONVERTED YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2670-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN EUPRC-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO EUPRC-EURO-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUPRC-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE EUPRC-EURO-AMT ROUNDED = EUPRC-CONVERTED-YTD
                       * EUPRC-RATE / 200
               WHEN EUPRC-CONV-METHOD-CD = 'GL'
                   MOVE 20.00 TO EUPRC-EURO-AMT
               WHEN EUPRC-CONV-METHOD-CD = 'JA'
                   MOVE 5.00 TO EUPRC-EURO-AMT
               WHEN OTHER
                   COMPUTE EUPRC-EURO-AMT ROUNDED = EUPRC-CONVERTED-YTD
                       * EUPRC-RATE / 100
           END-EVALUATE
           IF EUPRC-EURO-AMT > 0 AND EUPRC-EURO-AMT < 2.00
               MOVE 2.00 TO EUPRC-EURO-AMT
           END-IF
           IF EUPRC-EURO-AMT > 100.00
               MOVE 100.00 TO EUPRC-EURO-AMT
           END-IF
           ADD EUPRC-EURO-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2680-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EUPRC-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUPRC-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO EUPRC-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2690-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO EUPRC-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO EUPRC-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EUPRC-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EUPRC-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2700-SPLIT-CONVERTED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE EUPRC-CONVERTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EURO AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2710-ACCRUE-EURO.
           IF EUPRC-EURO-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUPRC-EURO-AMT * EUPRC-RATE / 100 / 360
               ADD WS-WORK-AMT TO EUPRC-CONVERTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONVERTED YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2720-LIMIT-CONVERTED.
           IF EUPRC-CONVERTED-YTD < 0.00
               MOVE 0.00 TO EUPRC-CONVERTED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EUPRC-CONVERTED-YTD > 1000.00
               MOVE 1000.00 TO EUPRC-CONVERTED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2730-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF EUPRC-LAST-MAINT-DATE > 0 AND EUPRC-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUPRC-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUPRC-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EUPRC-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EUPRC-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EUPRC-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUPRC-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONVERTED AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2740-FLAG-CONVERTED.
           IF EUPRC-CONVERTED-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EUPRC-CONVERTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2750-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * EUPRC-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO EUPRC-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2760-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EUPRC-LAST-STMT-DATE > 0 AND EUPRC-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUPRC-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONVERTED AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2770-ACCRUE-CONVERTED.
           IF EUPRC-CONVERTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUPRC-CONVERTED-AMT * EUPRC-RATE / 100 / 360
               ADD WS-WORK-AMT TO EUPRC-EURO-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE CONVERTED AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2780-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN EUPRC-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO EUPRC-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EUPRC-OVERRIDE-FLAG = 'Y'
                   COMPUTE EUPRC-FEE-AMT ROUNDED = EUPRC-CONVERTED-AMT *
                       EUPRC-RATE / 200
               WHEN EUPRC-CURRENCY-CD = 'EW'
                   MOVE 7.00 TO EUPRC-FEE-AMT
               WHEN EUPRC-CURRENCY-CD = 'FJ'
                   MOVE 15.00 TO EUPRC-FEE-AMT
               WHEN OTHER
                   COMPUTE EUPRC-FEE-AMT ROUNDED = EUPRC-CONVERTED-AMT *
                       EUPRC-RATE / 100
           END-EVALUATE
           IF EUPRC-FEE-AMT > 0 AND EUPRC-FEE-AMT < 2.00
               MOVE 2.00 TO EUPRC-FEE-AMT
           END-IF
           IF EUPRC-FEE-AMT > 250.00
               MOVE 250.00 TO EUPRC-FEE-AMT
           END-IF
           ADD EUPRC-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE CONVERTED AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2790-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EUPRC-TYPE-CD
                   ADD EUPRC-CONVERTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EUPRC-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EUPRC-CONVERTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2800-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EUPRC-LAST-PAY-DATE > 0 AND EUPRC-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUPRC-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUPRC-EURO-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD EUPRC-EURO-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD EUPRC-EURO-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EUPRC-EURO-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUPRC-EURO-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E952 CONTACT REQUIRED
      * E584 EURO YTD NEGATIVE
      * E350 EURO YTD OVER 25000
      * E266 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       2810-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EUPRC-CONTACT = SPACES
                   MOVE 'E952' TO WS-REJECT-CODE
               WHEN EUPRC-EURO-YTD < 0
                   MOVE 'E584' TO WS-REJECT-CODE
               WHEN EUPRC-EURO-YTD > 25000.00
                   MOVE 'E350' TO WS-REJECT-CODE
               WHEN EUPRC-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E266' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2820-COUNT-CYCLE.
           IF EUPRC-CYCLE-CNT < 9999999
               ADD 1 TO EUPRC-CYCLE-CNT
           END-IF
           IF EUPRC-CYCLE-CNT NOT < 3
               MOVE 'Y' TO EUPRC-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2830-COUNT-EXCEPT.
           IF EUPRC-EXCEPT-CNT < 9999999
               ADD 1 TO EUPRC-EXCEPT-CNT
           END-IF
           IF EUPRC-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO EUPRC-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2840-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EUPRC-LAST-PAY-DATE > 0 AND EUPRC-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EUPRC-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EUPRC-CONVERTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EUPRC-CONVERTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EUPRC-CONVERTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD EUPRC-CONVERTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EUPRC-CONVERTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONVERTED YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2850-LIMIT-CONVERTED.
           IF EUPRC-CONVERTED-YTD < 0.00
               MOVE 0.00 TO EUPRC-CONVERTED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EUPRC-CONVERTED-YTD > 2500.00
               MOVE 2500.00 TO EUPRC-CONVERTED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROUNDING YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2860-ACCRUE-ROUNDING.
           IF EUPRC-ROUNDING-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EUPRC-ROUNDING-YTD * EUPRC-RATE / 100 / 360
               ADD WS-WORK-AMT TO EUPRC-CONVERTED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EURO YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2870-SPLIT-EURO.
           COMPUTE WS-WORK-AMT = EUPRC-EURO-YTD / 4
           COMPUTE WS-WORK-AMT-2 = EUPRC-EURO-YTD - (WS-WORK-AMT * 4)
           COMPUTE EUPRC-EURO-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO EUPRC-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO EUPRC-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO EUPRC-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'EUPRC EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
