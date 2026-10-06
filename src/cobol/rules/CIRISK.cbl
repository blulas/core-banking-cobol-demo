      *================================================================*
      * PROGRAM  : CIRISK                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : CHECK IMAGE ARCHIVE RULE - DATE RULES. CALLED BY THE*
      *            CHECK IMAGE ARCHIVE BATCH AND ONLINE PROGRAMS.      *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-08-26 LMB  ORIGINAL                                       *
      * 1991-06-02 BHS  STORAGE ROUNDING - HALF UP INC6035             *
      * 1991-01-21 LMB  IMAGE-FEE TABLE EXPANDED PRB4392               *
      * 1991-12-09 TWH  ITEM STATUS CODES ADDED INC9001                *
      * 1991-11-08 JLM  STORAGE STATUS CODES ADDED REQ8483             *
      * 1995-02-23 DPS  AUDIT FINDING - ITEM CONTROL TOTALS INC1966    *
      * 2003-09-03 MKL  STORAGE RERUN PROTECTION REQ9123               *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIRISK.
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
      * WS-ITEM-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ITEM-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000496000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000411000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000131000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025000000000551500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035000000000680500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035050000000204500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035300000000090000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035800000000726500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040800000000042000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00050800000000660000'.
       01  WS-ITEM-GRID-TBL REDEFINES WS-ITEM-GRID-VALUES.
           05  WS-ITEM-GRID-ROW OCCURS 10 TIMES.
               10  WS-ITEM-GRID-LOW        PIC 9(09)V99.
               10  WS-ITEM-GRID-RATE       PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY CIRISKP.

       PROCEDURE DIVISION USING CIRISK-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCRUE-STORAGE
           PERFORM 2010-EDIT-NAME
           PERFORM 2020-FLAG-ITEM
           PERFORM 2030-SET-TYPE
           PERFORM 2040-SET-STATUS
           PERFORM 2050-COUNT-NOTICE
           PERFORM 2060-ASSESS-REVIEW
           PERFORM 2070-LIMIT-ITEM
           PERFORM 2080-COUNT-PAYMENT
           PERFORM 2090-FLAG-STORAGE
           PERFORM 2100-SET-ITEM-TYPE
           PERFORM 2110-EDIT-REF-NO
           PERFORM 2120-ASSESS-HOLD
           PERFORM 2130-ACCUM-ITEM-TYPE
           PERFORM 2140-COUNT-NOTICE
           PERFORM 2150-SELECT-FEE-PCT
           PERFORM 2160-ACCRUE-STORAGE
           PERFORM 2170-COUNT-PAYMENT
           PERFORM 2180-SET-ITEM-TYPE
           PERFORM 2190-COUNT-NOTICE
           PERFORM 2200-SPLIT-STORAGE
           PERFORM 2210-SELECT-FEE-PCT
           PERFORM 2220-COUNT-PAYMENT
           PERFORM 2230-PRORATE-ITEM
           PERFORM 2240-ASSESS-REVIEW
           PERFORM 2250-ACCUM-ITEM-TYPE
           PERFORM 2260-COUNT-NOTICE
           PERFORM 2270-SELECT-FEE-PCT
           PERFORM 2280-CHECK-LAST-MAINT
           PERFORM 2290-LIMIT-RETRIEVE-FEE
           PERFORM 2300-ASSESS-REVIEW
           PERFORM 2310-EDIT-REF-NO
           PERFORM 2320-ACCUM-ITEM-TYPE
           PERFORM 2330-SELECT-FEE-PCT
           PERFORM 2340-DESCRIBE-TYPE
           PERFORM 2350-LIMIT-STORAGE
           PERFORM 2360-EDIT-REF-NO
           PERFORM 2370-LIMIT-RETRIEVE-FEE
           PERFORM 2380-ACCRUE-RETRIEVE-FEE
           PERFORM 2390-SET-CAPTURE-SITE
           PERFORM 2400-EDIT-REF-NO
           PERFORM 2410-SELECT-FEE-PCT
           PERFORM 2420-ASSESS-REVIEW
           PERFORM 2430-SET-STATUS
           PERFORM 2440-EDIT-REF-NO
           PERFORM 2450-CHECK-NEXT-DUE
           PERFORM 2460-LIMIT-STORAGE
           PERFORM 2470-EDIT-NAME
           PERFORM 2480-CHECK-LAST-MAINT
           PERFORM 2490-CHECK-OPEN
           PERFORM 2500-ACCUM-STATUS
           PERFORM 2510-ACCRUE-IMAGE-FEE
           PERFORM 2520-FLAG-STORAGE
           PERFORM 2530-FLAG-STORAGE
           PERFORM 2540-AGE-NEXT-DUE
           PERFORM 2550-SELECT-MARGIN
           PERFORM 2560-ASSESS-REVIEW
           PERFORM 2570-SPLIT-STORAGE
           PERFORM 2580-ACCRUE-RETRIEVE-FEE
           PERFORM 2590-CHECK-CLOSE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO CIRISK-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2000-ACCRUE-STORAGE.
           IF CIRISK-STORAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIRISK-STORAGE-YTD * CIRISK-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO CIRISK-ITEM-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E242 NAME REQUIRED
      * E766 RETRIEVE FEE AMT NEGATIVE
      * E606 RETRIEVE FEE AMT OVER 10000
      * E330 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2010-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIRISK-NAME = SPACES
                   MOVE 'E242' TO WS-REJECT-CODE
               WHEN CIRISK-RETRIEVE-FEE-AMT < 0
                   MOVE 'E766' TO WS-REJECT-CODE
               WHEN CIRISK-RETRIEVE-FEE-AMT > 10000.00
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN CIRISK-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E330' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2020-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CT -> NS WHEN OPEN DATE IS PAST
      * NS -> TR WHEN OPEN DATE IS PAST
      * TR -> CT WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2030-SET-TYPE.
           EVALUATE CIRISK-TYPE-CD
               WHEN 'CT'
                   IF CIRISK-OPEN-DATE > 0 AND CIRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NS' TO CIRISK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NS'
                   IF CIRISK-OPEN-DATE > 0 AND CIRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TR' TO CIRISK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TR'
                   IF CIRISK-OPEN-DATE > 0 AND CIRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CT' TO CIRISK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AJ -> JY WHEN NEXT DUE DATE IS PAST
      * JY -> RU WHEN NEXT DUE DATE IS PAST
      * RU -> UR WHEN NEXT DUE DATE IS PAST
      * UR -> YW WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2040-SET-STATUS.
           EVALUATE CIRISK-STATUS-CD
               WHEN 'AJ'
                   IF CIRISK-NEXT-DUE-DATE > 0 AND CIRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JY' TO CIRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JY'
                   IF CIRISK-NEXT-DUE-DATE > 0 AND CIRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RU' TO CIRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF CIRISK-NEXT-DUE-DATE > 0 AND CIRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UR' TO CIRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF CIRISK-NEXT-DUE-DATE > 0 AND CIRISK-NEXT-DUE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'YW' TO CIRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2050-COUNT-NOTICE.
           IF CIRISK-NOTICE-CNT < 9999999
               ADD 1 TO CIRISK-NOTICE-CNT
           END-IF
           IF CIRISK-NOTICE-CNT NOT < 5
               MOVE 'Y' TO CIRISK-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE IMAGE FEE FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2060-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN CIRISK-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO CIRISK-IMAGE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIRISK-HOLD-FLAG = 'Y'
                   COMPUTE CIRISK-IMAGE-FEE-AMT ROUNDED = WS-WORK-AMT-2
                       * CIRISK-MARGIN / 200
               WHEN CIRISK-CAPTURE-SITE-CD = 'BB'
                   MOVE 12.00 TO CIRISK-IMAGE-FEE-AMT
               WHEN CIRISK-CAPTURE-SITE-CD = 'CA'
                   MOVE 3.00 TO CIRISK-IMAGE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIRISK-IMAGE-FEE-AMT ROUNDED = WS-WORK-AMT-2
                       * CIRISK-MARGIN / 100
           END-EVALUATE
           IF CIRISK-IMAGE-FEE-AMT > 0 AND CIRISK-IMAGE-FEE-AMT < 10.00
               MOVE 10.00 TO CIRISK-IMAGE-FEE-AMT
           END-IF
           IF CIRISK-IMAGE-FEE-AMT > 250.00
               MOVE 250.00 TO CIRISK-IMAGE-FEE-AMT
           END-IF
           ADD CIRISK-IMAGE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2070-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2080-COUNT-PAYMENT.
           IF CIRISK-PAYMENT-CNT < 9999999
               ADD 1 TO CIRISK-PAYMENT-CNT
           END-IF
           IF CIRISK-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO CIRISK-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2090-FLAG-STORAGE.
           IF CIRISK-STORAGE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIRISK-STORAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * CW -> FB WHEN LAST MAINT DATE IS PAST
      * FB -> HN WHEN LAST MAINT DATE IS PAST
      * HN -> KX WHEN LAST MAINT DATE IS PAST
      * KX -> CW WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2100-SET-ITEM-TYPE.
           EVALUATE CIRISK-ITEM-TYPE-CD
               WHEN 'CW'
                   IF CIRISK-LAST-MAINT-DATE > 0 AND
                       CIRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'FB' TO CIRISK-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF CIRISK-LAST-MAINT-DATE > 0 AND
                       CIRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'HN' TO CIRISK-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HN'
                   IF CIRISK-LAST-MAINT-DATE > 0 AND
                       CIRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'KX' TO CIRISK-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KX'
                   IF CIRISK-LAST-MAINT-DATE > 0 AND
                       CIRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'CW' TO CIRISK-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E080 REF NO REQUIRED
      * E716 IMAGE FEE AMT NEGATIVE
      * E927 IMAGE FEE AMT OVER 1000000
      * E921 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2110-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIRISK-REF-NO = SPACES
                   MOVE 'E080' TO WS-REJECT-CODE
               WHEN CIRISK-IMAGE-FEE-AMT < 0
                   MOVE 'E716' TO WS-REJECT-CODE
               WHEN CIRISK-IMAGE-FEE-AMT > 1000000.00
                   MOVE 'E927' TO WS-REJECT-CODE
               WHEN CIRISK-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E921' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE RETRIEVE FEE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2120-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CIRISK-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CIRISK-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIRISK-OVERRIDE-FLAG = 'Y'
                   COMPUTE CIRISK-STORAGE-AMT ROUNDED =
                       CIRISK-RETRIEVE-FEE-YTD * CIRISK-MARGIN / 200
               WHEN CIRISK-CAPTURE-SITE-CD = 'BB'
                   MOVE 3.00 TO CIRISK-STORAGE-AMT
               WHEN CIRISK-CAPTURE-SITE-CD = 'CA'
                   MOVE 20.00 TO CIRISK-STORAGE-AMT
               WHEN OTHER
                   COMPUTE CIRISK-STORAGE-AMT ROUNDED =
                       CIRISK-RETRIEVE-FEE-YTD * CIRISK-MARGIN / 100
           END-EVALUATE
           IF CIRISK-STORAGE-AMT > 0 AND CIRISK-STORAGE-AMT < 10.00
               MOVE 10.00 TO CIRISK-STORAGE-AMT
           END-IF
           IF CIRISK-STORAGE-AMT > 25.00
               MOVE 25.00 TO CIRISK-STORAGE-AMT
           END-IF
           ADD CIRISK-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE RETRIEVE FEE YTD BY ITEM TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2130-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIRISK-ITEM-TYPE-CD
                   ADD CIRISK-RETRIEVE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIRISK-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIRISK-RETRIEVE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2140-COUNT-NOTICE.
           IF CIRISK-NOTICE-CNT < 9999999
               ADD 1 TO CIRISK-NOTICE-CNT
           END-IF
           IF CIRISK-NOTICE-CNT NOT < 12
               MOVE 'Y' TO CIRISK-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRID TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2150-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRID-RATE(1) TO CIRISK-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CIRISK-STORAGE-AMT NOT < WS-ITEM-GRID-LOW(WS-TX)
                   MOVE WS-ITEM-GRID-RATE(WS-TX) TO CIRISK-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIRISK-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIRISK-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2160-ACCRUE-STORAGE.
           IF CIRISK-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIRISK-STORAGE-AMT * CIRISK-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIRISK-IMAGE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2170-COUNT-PAYMENT.
           IF CIRISK-PAYMENT-CNT < 9999999
               ADD 1 TO CIRISK-PAYMENT-CNT
           END-IF
           IF CIRISK-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO CIRISK-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * CW -> FB WHEN OPEN DATE IS PAST
      * FB -> HN WHEN OPEN DATE IS PAST
      * HN -> KX WHEN OPEN DATE IS PAST
      * KX -> CW WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2180-SET-ITEM-TYPE.
           EVALUATE CIRISK-ITEM-TYPE-CD
               WHEN 'CW'
                   IF CIRISK-OPEN-DATE > 0 AND CIRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO CIRISK-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF CIRISK-OPEN-DATE > 0 AND CIRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HN' TO CIRISK-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HN'
                   IF CIRISK-OPEN-DATE > 0 AND CIRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO CIRISK-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KX'
                   IF CIRISK-OPEN-DATE > 0 AND CIRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CW' TO CIRISK-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2190-COUNT-NOTICE.
           IF CIRISK-NOTICE-CNT < 9999999
               ADD 1 TO CIRISK-NOTICE-CNT
           END-IF
           IF CIRISK-NOTICE-CNT NOT < 10
               MOVE 'Y' TO CIRISK-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STORAGE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2200-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = CIRISK-STORAGE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CIRISK-STORAGE-AMT - (WS-WORK-AMT *
               2)
           COMPUTE CIRISK-STORAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2210-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRID-RATE(1) TO CIRISK-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-ITEM-GRID-LOW(WS-TX)
                   MOVE WS-ITEM-GRID-RATE(WS-TX) TO CIRISK-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIRISK-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIRISK-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2220-COUNT-PAYMENT.
           IF CIRISK-PAYMENT-CNT < 9999999
               ADD 1 TO CIRISK-PAYMENT-CNT
           END-IF
           IF CIRISK-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO CIRISK-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETRIEVE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ITEM YTD.
      *----------------------------------------------------------------*
       2230-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIRISK-ITEM-YTD ROUNDED =
                   CIRISK-RETRIEVE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIRISK-ITEM-YTD
           END-IF
           ADD CIRISK-ITEM-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE STORAGE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2240-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN CIRISK-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIRISK-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CIRISK-STORAGE-YTD *
                       CIRISK-FEE-PCT / 200
               WHEN CIRISK-STATUS-CD = 'AJ'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN CIRISK-STATUS-CD = 'JY'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CIRISK-STORAGE-YTD *
                       CIRISK-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE RETRIEVE FEE YTD BY ITEM TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2250-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIRISK-ITEM-TYPE-CD
                   ADD CIRISK-RETRIEVE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIRISK-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIRISK-RETRIEVE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2260-COUNT-NOTICE.
           IF CIRISK-NOTICE-CNT < 9999999
               ADD 1 TO CIRISK-NOTICE-CNT
           END-IF
           IF CIRISK-NOTICE-CNT NOT < 5
               MOVE 'Y' TO CIRISK-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRID TIER FOR THE IMAGE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2270-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRID-RATE(1) TO CIRISK-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CIRISK-IMAGE-FEE-AMT NOT < WS-ITEM-GRID-LOW(WS-TX)
                   MOVE WS-ITEM-GRID-RATE(WS-TX) TO CIRISK-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIRISK-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIRISK-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2280-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF CIRISK-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIRISK-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO CIRISK-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRIEVE FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2290-LIMIT-RETRIEVE-FEE.
           IF CIRISK-RETRIEVE-FEE-YTD < 25.00
               MOVE 25.00 TO CIRISK-RETRIEVE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIRISK-RETRIEVE-FEE-YTD > 5000.00
               MOVE 5000.00 TO CIRISK-RETRIEVE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE ITEM YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2300-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN CIRISK-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO CIRISK-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIRISK-HOLD-FLAG = 'Y'
                   COMPUTE CIRISK-STORAGE-AMT ROUNDED = CIRISK-ITEM-YTD
                       * CIRISK-MARGIN / 200
               WHEN CIRISK-ITEM-TYPE-CD = 'CW'
                   MOVE 20.00 TO CIRISK-STORAGE-AMT
               WHEN CIRISK-ITEM-TYPE-CD = 'FB'
                   MOVE 12.00 TO CIRISK-STORAGE-AMT
               WHEN OTHER
                   COMPUTE CIRISK-STORAGE-AMT ROUNDED = CIRISK-ITEM-YTD
                       * CIRISK-MARGIN / 100
           END-EVALUATE
           IF CIRISK-STORAGE-AMT > 0 AND CIRISK-STORAGE-AMT < 1.00
               MOVE 1.00 TO CIRISK-STORAGE-AMT
           END-IF
           IF CIRISK-STORAGE-AMT > 250.00
               MOVE 250.00 TO CIRISK-STORAGE-AMT
           END-IF
           ADD CIRISK-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E742 REF NO REQUIRED
      * E402 IMAGE FEE AMT NEGATIVE
      * E751 IMAGE FEE AMT OVER 250000
      * E524 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2310-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIRISK-REF-NO = SPACES
                   MOVE 'E742' TO WS-REJECT-CODE
               WHEN CIRISK-IMAGE-FEE-AMT < 0
                   MOVE 'E402' TO WS-REJECT-CODE
               WHEN CIRISK-IMAGE-FEE-AMT > 250000.00
                   MOVE 'E751' TO WS-REJECT-CODE
               WHEN CIRISK-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E524' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ITEM YTD BY ITEM TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2320-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIRISK-ITEM-TYPE-CD
                   ADD CIRISK-ITEM-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIRISK-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIRISK-ITEM-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRID TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2330-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRID-RATE(1) TO CIRISK-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CIRISK-STORAGE-AMT NOT < WS-ITEM-GRID-LOW(WS-TX)
                   MOVE WS-ITEM-GRID-RATE(WS-TX) TO CIRISK-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIRISK-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIRISK-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2340-DESCRIBE-TYPE.
           EVALUATE CIRISK-TYPE-CD
               WHEN 'CT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STORAGE YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2350-LIMIT-STORAGE.
           IF CIRISK-STORAGE-YTD < 0.00
               MOVE 0.00 TO CIRISK-STORAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIRISK-STORAGE-YTD > 50000.00
               MOVE 50000.00 TO CIRISK-STORAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E415 REF NO REQUIRED
      * E848 RETRIEVE FEE AMT NEGATIVE
      * E244 RETRIEVE FEE AMT OVER 25000
      * E566 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2360-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIRISK-REF-NO = SPACES
                   MOVE 'E415' TO WS-REJECT-CODE
               WHEN CIRISK-RETRIEVE-FEE-AMT < 0
                   MOVE 'E848' TO WS-REJECT-CODE
               WHEN CIRISK-RETRIEVE-FEE-AMT > 25000.00
                   MOVE 'E244' TO WS-REJECT-CODE
               WHEN CIRISK-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E566' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRIEVE FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2370-LIMIT-RETRIEVE-FEE.
           IF CIRISK-RETRIEVE-FEE-AMT < 50.00
               MOVE 50.00 TO CIRISK-RETRIEVE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIRISK-RETRIEVE-FEE-AMT > 2500.00
               MOVE 2500.00 TO CIRISK-RETRIEVE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETRIEVE FEE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2380-ACCRUE-RETRIEVE-FEE.
           IF CIRISK-RETRIEVE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIRISK-RETRIEVE-FEE-YTD * CIRISK-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIRISK-STORAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CAPTURE SITE CD:
      * BB -> CA WHEN OPEN DATE IS PAST
      * CA -> PC WHEN OPEN DATE IS PAST
      * PC -> BB WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2390-SET-CAPTURE-SITE.
           EVALUATE CIRISK-CAPTURE-SITE-CD
               WHEN 'BB'
                   IF CIRISK-OPEN-DATE > 0 AND CIRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CA' TO CIRISK-CAPTURE-SITE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CA'
                   IF CIRISK-OPEN-DATE > 0 AND CIRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO CIRISK-CAPTURE-SITE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF CIRISK-OPEN-DATE > 0 AND CIRISK-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BB' TO CIRISK-CAPTURE-SITE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E714 REF NO REQUIRED
      * E832 IMAGE FEE AMT NEGATIVE
      * E701 IMAGE FEE AMT OVER 25000
      * E509 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2400-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIRISK-REF-NO = SPACES
                   MOVE 'E714' TO WS-REJECT-CODE
               WHEN CIRISK-IMAGE-FEE-AMT < 0
                   MOVE 'E832' TO WS-REJECT-CODE
               WHEN CIRISK-IMAGE-FEE-AMT > 25000.00
                   MOVE 'E701' TO WS-REJECT-CODE
               WHEN CIRISK-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E509' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRID TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2410-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRID-RATE(1) TO CIRISK-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CIRISK-STORAGE-AMT NOT < WS-ITEM-GRID-LOW(WS-TX)
                   MOVE WS-ITEM-GRID-RATE(WS-TX) TO CIRISK-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIRISK-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIRISK-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETRIEVE FEE YTD FEE ON THE STORAGE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2420-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN CIRISK-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO CIRISK-RETRIEVE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIRISK-OVERRIDE-FLAG = 'Y'
                   COMPUTE CIRISK-RETRIEVE-FEE-YTD ROUNDED =
                       CIRISK-STORAGE-AMT * CIRISK-FEE-PCT / 200
               WHEN CIRISK-CAPTURE-SITE-CD = 'BB'
                   MOVE 20.00 TO CIRISK-RETRIEVE-FEE-YTD
               WHEN CIRISK-CAPTURE-SITE-CD = 'CA'
                   MOVE 12.00 TO CIRISK-RETRIEVE-FEE-YTD
               WHEN OTHER
                   COMPUTE CIRISK-RETRIEVE-FEE-YTD ROUNDED =
                       CIRISK-STORAGE-AMT * CIRISK-FEE-PCT / 100
           END-EVALUATE
           IF CIRISK-RETRIEVE-FEE-YTD > 0 AND CIRISK-RETRIEVE-FEE-YTD <
               10.00
               MOVE 10.00 TO CIRISK-RETRIEVE-FEE-YTD
           END-IF
           IF CIRISK-RETRIEVE-FEE-YTD > 25.00
               MOVE 25.00 TO CIRISK-RETRIEVE-FEE-YTD
           END-IF
           ADD CIRISK-RETRIEVE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AJ -> JY WHEN LAST MAINT DATE IS PAST
      * JY -> RU WHEN LAST MAINT DATE IS PAST
      * RU -> UR WHEN LAST MAINT DATE IS PAST
      * UR -> YW WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2430-SET-STATUS.
           EVALUATE CIRISK-STATUS-CD
               WHEN 'AJ'
                   IF CIRISK-LAST-MAINT-DATE > 0 AND
                       CIRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'JY' TO CIRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JY'
                   IF CIRISK-LAST-MAINT-DATE > 0 AND
                       CIRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'RU' TO CIRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF CIRISK-LAST-MAINT-DATE > 0 AND
                       CIRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'UR' TO CIRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF CIRISK-LAST-MAINT-DATE > 0 AND
                       CIRISK-LAST-MAINT-DATE < CTL-BUSINESS-DATE
                       MOVE 'YW' TO CIRISK-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E124 REF NO REQUIRED
      * E429 IMAGE FEE AMT NEGATIVE
      * E300 IMAGE FEE AMT OVER 250000
      * E001 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2440-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIRISK-REF-NO = SPACES
                   MOVE 'E124' TO WS-REJECT-CODE
               WHEN CIRISK-IMAGE-FEE-AMT < 0
                   MOVE 'E429' TO WS-REJECT-CODE
               WHEN CIRISK-IMAGE-FEE-AMT > 250000.00
                   MOVE 'E300' TO WS-REJECT-CODE
               WHEN CIRISK-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E001' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2450-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CIRISK-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIRISK-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CIRISK-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2460-LIMIT-STORAGE.
           IF CIRISK-STORAGE-AMT < 25.00
               MOVE 25.00 TO CIRISK-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIRISK-STORAGE-AMT > 50000.00
               MOVE 50000.00 TO CIRISK-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E361 NAME REQUIRED
      * E427 STORAGE AMT NEGATIVE
      * E839 STORAGE AMT OVER 250000
      * E672 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       2470-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIRISK-NAME = SPACES
                   MOVE 'E361' TO WS-REJECT-CODE
               WHEN CIRISK-STORAGE-AMT < 0
                   MOVE 'E427' TO WS-REJECT-CODE
               WHEN CIRISK-STORAGE-AMT > 250000.00
                   MOVE 'E839' TO WS-REJECT-CODE
               WHEN CIRISK-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E672' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2480-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF CIRISK-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIRISK-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO CIRISK-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2490-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CIRISK-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIRISK-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CIRISK-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2500-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIRISK-STATUS-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIRISK-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON IMAGE FEE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2510-ACCRUE-IMAGE-FEE.
           IF CIRISK-IMAGE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIRISK-IMAGE-FEE-AMT * CIRISK-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO CIRISK-ITEM-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2520-FLAG-STORAGE.
           IF CIRISK-STORAGE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CIRISK-STORAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2530-FLAG-STORAGE.
           IF CIRISK-STORAGE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CIRISK-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2540-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CIRISK-NEXT-DUE-DATE > 0 AND CIRISK-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIRISK-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIRISK-RETRIEVE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIRISK-RETRIEVE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CIRISK-RETRIEVE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIRISK-RETRIEVE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIRISK-RETRIEVE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRID TIER FOR THE STORAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2550-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRID-RATE(1) TO CIRISK-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CIRISK-STORAGE-YTD NOT < WS-ITEM-GRID-LOW(WS-TX)
                   MOVE WS-ITEM-GRID-RATE(WS-TX) TO CIRISK-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIRISK-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIRISK-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETRIEVE FEE FEE ON THE STORAGE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2560-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN CIRISK-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO CIRISK-RETRIEVE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIRISK-HOLD-FLAG = 'Y'
                   COMPUTE CIRISK-RETRIEVE-FEE-AMT ROUNDED =
                       CIRISK-STORAGE-AMT * CIRISK-MARGIN / 200
               WHEN CIRISK-CAPTURE-SITE-CD = 'BB'
                   MOVE 10.00 TO CIRISK-RETRIEVE-FEE-AMT
               WHEN CIRISK-CAPTURE-SITE-CD = 'CA'
                   MOVE 15.00 TO CIRISK-RETRIEVE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIRISK-RETRIEVE-FEE-AMT ROUNDED =
                       CIRISK-STORAGE-AMT * CIRISK-MARGIN / 100
           END-EVALUATE
           IF CIRISK-RETRIEVE-FEE-AMT > 0 AND CIRISK-RETRIEVE-FEE-AMT <
               2.00
               MOVE 2.00 TO CIRISK-RETRIEVE-FEE-AMT
           END-IF
           IF CIRISK-RETRIEVE-FEE-AMT > 250.00
               MOVE 250.00 TO CIRISK-RETRIEVE-FEE-AMT
           END-IF
           ADD CIRISK-RETRIEVE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT STORAGE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2570-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = CIRISK-STORAGE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CIRISK-STORAGE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE CIRISK-IMAGE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETRIEVE FEE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2580-ACCRUE-RETRIEVE-FEE.
           IF CIRISK-RETRIEVE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIRISK-RETRIEVE-FEE-YTD * CIRISK-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIRISK-IMAGE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2590-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CIRISK-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIRISK-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO CIRISK-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO CIRISK-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO CIRISK-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO CIRISK-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'CIRISK EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
