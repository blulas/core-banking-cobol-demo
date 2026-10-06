      *================================================================*
      * PROGRAM  : PPELG                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : POSITIVE PAY RULE - LIMITS. CALLED BY THE POSITIVE  *
      *            PAY BATCH AND ONLINE PROGRAMS.                      *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-11-06 PQR  ORIGINAL                                       *
      * 1991-03-07 RJT  PERFORMANCE - VOID READ AHEAD INC9632          *
      * 1993-07-14 CRP  STALE EXCEPTIONS TO OPS QUEUE INC4293          *
      * 1993-10-27 BHS  EXCEPT LIMIT REVISED REQ7923                   *
      * 1995-11-08 BHS  ISSUED ROUNDING - HALF UP INC1988              *
      * 1996-05-06 DPS  STALE NOTICE TEXT PER LEGAL CHG9555            *
      * 2000-12-12 EWS  VOID LIMIT REVISED CHG9032                     *
      * 2002-06-26 LMB  EXCEPT ROUNDING - HALF UP REQ0527              *
      * 2003-02-17 LMB  PAID EXCEPTIONS TO OPS QUEUE REQ7495           *
      * 2003-08-02 TWH  PAID STATUS CODES ADDED INC6243                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPELG.
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
      * WS-ISSUED-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ISSUED-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000859000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000549500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000016500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015050000000278000'.
       01  WS-ISSUED-GRID-TBL REDEFINES WS-ISSUED-GRID-VALUES.
           05  WS-ISSUED-GRID-ROW OCCURS 4 TIMES.
               10  WS-ISSUED-GRID-LOW      PIC 9(09)V99.
               10  WS-ISSUED-GRID-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAID-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAID-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000756500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000125500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000651000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005750000000371500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000641500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000383000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011050000000746000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011300000000417000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013800000000082000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018800000000821500'.
       01  WS-PAID-BAND-TBL REDEFINES WS-PAID-BAND-VALUES.
           05  WS-PAID-BAND-ROW OCCURS 10 TIMES.
               10  WS-PAID-BAND-LOW        PIC 9(09)V99.
               10  WS-PAID-BAND-RATE       PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY PPELGP.

       PROCEDURE DIVISION USING PPELG-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCRUE-EXCEPT
           PERFORM 2010-AGE-CLOSE
           PERFORM 2020-ASSESS-HOLD
           PERFORM 2030-ACCRUE-PAID
           PERFORM 2040-SPLIT-PAID
           PERFORM 2050-AGE-CLOSE
           PERFORM 2060-COUNT-EXCEPT
           PERFORM 2070-SET-CYCLE
           PERFORM 2080-FLAG-ITEM
           PERFORM 2090-CHECK-EXPIRY
           PERFORM 2100-FLAG-ITEM
           PERFORM 2110-ACCUM-REASON
           PERFORM 2120-SET-REASON
           PERFORM 2130-DESCRIBE-CYCLE
           PERFORM 2140-COUNT-EXCEPT
           PERFORM 2150-EDIT-REF-NO
           PERFORM 2160-SPLIT-VOID
           PERFORM 2170-ACCRUE-EXCEPT
           PERFORM 2180-ACCRUE-VOID
           PERFORM 2190-LIMIT-ITEM
           PERFORM 2200-SET-REASON
           PERFORM 2210-ACCUM-CYCLE
           PERFORM 2220-SPLIT-VOID
           PERFORM 2230-DESCRIBE-STATUS
           PERFORM 2240-EDIT-CONTACT
           PERFORM 2250-ACCRUE-EXCEPT
           PERFORM 2260-COUNT-CYCLE
           PERFORM 2270-SELECT-MARGIN
           PERFORM 2280-ACCUM-REGION
           PERFORM 2290-CHECK-CLOSE
           PERFORM 2300-EDIT-REF-NO
           PERFORM 2310-ASSESS-NOTICE-SENT
           PERFORM 2320-ACCUM-STATUS
           PERFORM 2330-AGE-CLOSE
           PERFORM 2340-COUNT-EXCEPT
           PERFORM 2350-ASSESS-STOP
           PERFORM 2360-SET-STATUS
           PERFORM 2370-LIMIT-ISSUED
           PERFORM 2380-ACCRUE-ISSUED
           PERFORM 2390-COUNT-EXCEPT
           PERFORM 2400-SET-STATUS
           PERFORM 2410-FLAG-PAID
           PERFORM 2420-FLAG-ITEM
           PERFORM 2430-COUNT-CYCLE
           PERFORM 2440-DESCRIBE-STATUS
           PERFORM 2450-CHECK-CLOSE
           PERFORM 2460-ASSESS-HOLD
           PERFORM 2470-FLAG-ISSUED
           PERFORM 2480-ACCRUE-PAID
           PERFORM 2490-SPLIT-STALE
           PERFORM 2500-ASSESS-NOTICE-SENT
           PERFORM 2510-PRORATE-STALE
           PERFORM 2520-ASSESS-HOLD
           PERFORM 2530-CHECK-CLOSE
           PERFORM 2540-LIMIT-EXCEPT
           PERFORM 2550-SELECT-MARGIN
           PERFORM 2560-FLAG-ITEM
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO PPELG-RETURN-CODE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCEPT AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2000-ACCRUE-EXCEPT.
           IF PPELG-EXCEPT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPELG-EXCEPT-AMT * PPELG-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PPELG-ISSUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2010-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PPELG-CLOSE-DATE > 0 AND PPELG-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPELG-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPELG-STALE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPELG-STALE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPELG-STALE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPELG-STALE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPELG-STALE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE VOID AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2020-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PPELG-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PPELG-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPELG-STOP-FLAG = 'Y'
                   COMPUTE PPELG-PAID-AMT ROUNDED = PPELG-VOID-AMT *
                       PPELG-DISCOUNT / 200
               WHEN PPELG-CYCLE-CD = 'A'
                   MOVE 7.00 TO PPELG-PAID-AMT
               WHEN PPELG-CYCLE-CD = 'B'
                   MOVE 20.00 TO PPELG-PAID-AMT
               WHEN OTHER
                   COMPUTE PPELG-PAID-AMT ROUNDED = PPELG-VOID-AMT *
                       PPELG-DISCOUNT / 100
           END-EVALUATE
           IF PPELG-PAID-AMT > 0 AND PPELG-PAID-AMT < 10.00
               MOVE 10.00 TO PPELG-PAID-AMT
           END-IF
           IF PPELG-PAID-AMT > 25.00
               MOVE 25.00 TO PPELG-PAID-AMT
           END-IF
           ADD PPELG-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2030-ACCRUE-PAID.
           IF PPELG-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPELG-PAID-AMT * PPELG-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2040-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = PPELG-PAID-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPELG-PAID-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPELG-ISSUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2050-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PPELG-CLOSE-DATE > 0 AND PPELG-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPELG-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPELG-ISSUED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPELG-ISSUED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPELG-ISSUED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPELG-ISSUED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPELG-ISSUED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2060-COUNT-EXCEPT.
           IF PPELG-EXCEPT-CNT < 9999999
               ADD 1 TO PPELG-EXCEPT-CNT
           END-IF
           IF PPELG-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO PPELG-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> B WHEN EFFECTIVE DATE IS PAST
      * B -> C WHEN EFFECTIVE DATE IS PAST
      * C -> R WHEN EFFECTIVE DATE IS PAST
      * R -> T WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2070-SET-CYCLE.
           EVALUATE PPELG-CYCLE-CD
               WHEN 'A'
                   IF PPELG-EFFECTIVE-DATE > 0 AND PPELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'B' TO PPELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPELG-EFFECTIVE-DATE > 0 AND PPELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'C' TO PPELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PPELG-EFFECTIVE-DATE > 0 AND PPELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'R' TO PPELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF PPELG-EFFECTIVE-DATE > 0 AND PPELG-EFFECTIVE-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'T' TO PPELG-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2080-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2090-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PPELG-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPELG-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PPELG-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2100-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2110-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPELG-REASON-CD
                   ADD PPELG-PAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPELG-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPELG-PAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> M WHEN CLOSE DATE IS PAST
      * M -> N WHEN CLOSE DATE IS PAST
      * N -> U WHEN CLOSE DATE IS PAST
      * U -> V WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2120-SET-REASON.
           EVALUATE PPELG-REASON-CD
               WHEN 'B'
                   IF PPELG-CLOSE-DATE > 0 AND PPELG-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPELG-CLOSE-DATE > 0 AND PPELG-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF PPELG-CLOSE-DATE > 0 AND PPELG-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO PPELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF PPELG-CLOSE-DATE > 0 AND PPELG-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO PPELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2130-DESCRIBE-CYCLE.
           EVALUATE PPELG-CYCLE-CD
               WHEN 'A'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'C'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2140-COUNT-EXCEPT.
           IF PPELG-EXCEPT-CNT < 9999999
               ADD 1 TO PPELG-EXCEPT-CNT
           END-IF
           IF PPELG-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO PPELG-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E386 REF NO REQUIRED
      * E998 WORK AMT 2 NEGATIVE
      * E780 WORK AMT 2 OVER 250000
      * E663 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2150-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPELG-REF-NO = SPACES
                   MOVE 'E386' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E998' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E780' TO WS-REJECT-CODE
               WHEN PPELG-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E663' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2160-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = PPELG-VOID-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPELG-VOID-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPELG-PAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCEPT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2170-ACCRUE-EXCEPT.
           IF PPELG-EXCEPT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPELG-EXCEPT-AMT * PPELG-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2180-ACCRUE-VOID.
           IF PPELG-VOID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPELG-VOID-AMT * PPELG-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO PPELG-PAID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2190-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> M WHEN CLOSE DATE IS PAST
      * M -> N WHEN CLOSE DATE IS PAST
      * N -> U WHEN CLOSE DATE IS PAST
      * U -> V WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2200-SET-REASON.
           EVALUATE PPELG-REASON-CD
               WHEN 'B'
                   IF PPELG-CLOSE-DATE > 0 AND PPELG-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPELG-CLOSE-DATE > 0 AND PPELG-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF PPELG-CLOSE-DATE > 0 AND PPELG-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO PPELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF PPELG-CLOSE-DATE > 0 AND PPELG-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO PPELG-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2210-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPELG-CYCLE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPELG-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2220-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = PPELG-VOID-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPELG-VOID-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPELG-PAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2230-DESCRIBE-STATUS.
           EVALUATE PPELG-STATUS-CD
               WHEN 'DR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E052 CONTACT REQUIRED
      * E803 PAID YTD NEGATIVE
      * E589 PAID YTD OVER 99999
      * E452 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2240-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPELG-CONTACT = SPACES
                   MOVE 'E052' TO WS-REJECT-CODE
               WHEN PPELG-PAID-YTD < 0
                   MOVE 'E803' TO WS-REJECT-CODE
               WHEN PPELG-PAID-YTD > 99999.00
                   MOVE 'E589' TO WS-REJECT-CODE
               WHEN PPELG-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E452' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCEPT AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2250-ACCRUE-EXCEPT.
           IF PPELG-EXCEPT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPELG-EXCEPT-AMT * PPELG-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PPELG-PAID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2260-COUNT-CYCLE.
           IF PPELG-CYCLE-CNT < 9999999
               ADD 1 TO PPELG-CYCLE-CNT
           END-IF
           IF PPELG-CYCLE-CNT NOT < 5
               MOVE 'Y' TO PPELG-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED GRID TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2270-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-GRID-RATE(1) TO PPELG-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PPELG-PAID-AMT NOT < WS-ISSUED-GRID-LOW(WS-TX)
                   MOVE WS-ISSUED-GRID-RATE(WS-TX) TO PPELG-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPELG-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPELG-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2280-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPELG-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPELG-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2290-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PPELG-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPELG-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO PPELG-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E663 REF NO REQUIRED
      * E401 STALE AMT NEGATIVE
      * E632 STALE AMT OVER 10000
      * E004 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2300-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPELG-REF-NO = SPACES
                   MOVE 'E663' TO WS-REJECT-CODE
               WHEN PPELG-STALE-AMT < 0
                   MOVE 'E401' TO WS-REJECT-CODE
               WHEN PPELG-STALE-AMT > 10000.00
                   MOVE 'E632' TO WS-REJECT-CODE
               WHEN PPELG-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E004' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STALE FEE ON THE ISSUED YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2310-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPELG-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPELG-STALE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPELG-HOLD-FLAG = 'Y'
                   COMPUTE PPELG-STALE-AMT ROUNDED = PPELG-ISSUED-YTD *
                       PPELG-DISCOUNT / 200
               WHEN PPELG-REGION-CD = 'A'
                   MOVE 10.00 TO PPELG-STALE-AMT
               WHEN PPELG-REGION-CD = 'B'
                   MOVE 7.00 TO PPELG-STALE-AMT
               WHEN OTHER
                   COMPUTE PPELG-STALE-AMT ROUNDED = PPELG-ISSUED-YTD *
                       PPELG-DISCOUNT / 100
           END-EVALUATE
           IF PPELG-STALE-AMT > 0 AND PPELG-STALE-AMT < 5.00
               MOVE 5.00 TO PPELG-STALE-AMT
           END-IF
           IF PPELG-STALE-AMT > 150.00
               MOVE 150.00 TO PPELG-STALE-AMT
           END-IF
           ADD PPELG-STALE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2320-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPELG-STATUS-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPELG-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2330-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PPELG-CLOSE-DATE > 0 AND PPELG-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPELG-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPELG-ISSUED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PPELG-ISSUED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPELG-ISSUED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPELG-ISSUED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPELG-ISSUED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2340-COUNT-EXCEPT.
           IF PPELG-EXCEPT-CNT < 9999999
               ADD 1 TO PPELG-EXCEPT-CNT
           END-IF
           IF PPELG-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO PPELG-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2350-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PPELG-STOP-FLAG = 'Y'
                   MOVE ZERO TO PPELG-ISSUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPELG-HOLD-FLAG = 'Y'
                   COMPUTE PPELG-ISSUED-YTD ROUNDED = WS-WORK-AMT-2 *
                       PPELG-MARGIN / 200
               WHEN PPELG-CYCLE-CD = 'A'
                   MOVE 12.00 TO PPELG-ISSUED-YTD
               WHEN PPELG-CYCLE-CD = 'B'
                   MOVE 7.00 TO PPELG-ISSUED-YTD
               WHEN OTHER
                   COMPUTE PPELG-ISSUED-YTD ROUNDED = WS-WORK-AMT-2 *
                       PPELG-MARGIN / 100
           END-EVALUATE
           IF PPELG-ISSUED-YTD > 0 AND PPELG-ISSUED-YTD < 1.00
               MOVE 1.00 TO PPELG-ISSUED-YTD
           END-IF
           IF PPELG-ISSUED-YTD > 35.00
               MOVE 35.00 TO PPELG-ISSUED-YTD
           END-IF
           ADD PPELG-ISSUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DR -> TK WHEN CLOSE DATE IS PAST
      * TK -> WX WHEN CLOSE DATE IS PAST
      * WX -> DR WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2360-SET-STATUS.
           EVALUATE PPELG-STATUS-CD
               WHEN 'DR'
                   IF PPELG-CLOSE-DATE > 0 AND PPELG-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TK' TO PPELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TK'
                   IF PPELG-CLOSE-DATE > 0 AND PPELG-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WX' TO PPELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WX'
                   IF PPELG-CLOSE-DATE > 0 AND PPELG-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DR' TO PPELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ISSUED YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2370-LIMIT-ISSUED.
           IF PPELG-ISSUED-YTD < 0.00
               MOVE 0.00 TO PPELG-ISSUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPELG-ISSUED-YTD > 1000.00
               MOVE 1000.00 TO PPELG-ISSUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2380-ACCRUE-ISSUED.
           IF PPELG-ISSUED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPELG-ISSUED-YTD * PPELG-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PPELG-PAID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2390-COUNT-EXCEPT.
           IF PPELG-EXCEPT-CNT < 9999999
               ADD 1 TO PPELG-EXCEPT-CNT
           END-IF
           IF PPELG-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO PPELG-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DR -> TK WHEN EXPIRY DATE IS PAST
      * TK -> WX WHEN EXPIRY DATE IS PAST
      * WX -> DR WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2400-SET-STATUS.
           EVALUATE PPELG-STATUS-CD
               WHEN 'DR'
                   IF PPELG-EXPIRY-DATE > 0 AND PPELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TK' TO PPELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TK'
                   IF PPELG-EXPIRY-DATE > 0 AND PPELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WX' TO PPELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WX'
                   IF PPELG-EXPIRY-DATE > 0 AND PPELG-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DR' TO PPELG-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAID YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2410-FLAG-PAID.
           IF PPELG-PAID-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPELG-PAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2420-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2430-COUNT-CYCLE.
           IF PPELG-CYCLE-CNT < 9999999
               ADD 1 TO PPELG-CYCLE-CNT
           END-IF
           IF PPELG-CYCLE-CNT NOT < 6
               MOVE 'Y' TO PPELG-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2440-DESCRIBE-STATUS.
           EVALUATE PPELG-STATUS-CD
               WHEN 'DR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2450-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PPELG-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPELG-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PPELG-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE VOID AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2460-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PPELG-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPELG-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PPELG-VOID-AMT *
                       PPELG-DISCOUNT / 200
               WHEN PPELG-REGION-CD = 'A'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN PPELG-REGION-CD = 'B'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PPELG-VOID-AMT *
                       PPELG-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ISSUED YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2470-FLAG-ISSUED.
           IF PPELG-ISSUED-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPELG-ISSUED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2480-ACCRUE-PAID.
           IF PPELG-PAID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPELG-PAID-YTD * PPELG-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PPELG-STALE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2490-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE PPELG-STALE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED YTD FEE ON THE PAID YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2500-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPELG-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPELG-ISSUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPELG-STOP-FLAG = 'Y'
                   COMPUTE PPELG-ISSUED-YTD ROUNDED = PPELG-PAID-YTD *
                       PPELG-MARGIN / 200
               WHEN PPELG-REASON-CD = 'B'
                   MOVE 12.00 TO PPELG-ISSUED-YTD
               WHEN PPELG-REASON-CD = 'M'
                   MOVE 20.00 TO PPELG-ISSUED-YTD
               WHEN OTHER
                   COMPUTE PPELG-ISSUED-YTD ROUNDED = PPELG-PAID-YTD *
                       PPELG-MARGIN / 100
           END-EVALUATE
           IF PPELG-ISSUED-YTD > 0 AND PPELG-ISSUED-YTD < 1.00
               MOVE 1.00 TO PPELG-ISSUED-YTD
           END-IF
           IF PPELG-ISSUED-YTD > 100.00
               MOVE 100.00 TO PPELG-ISSUED-YTD
           END-IF
           ADD PPELG-ISSUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE STALE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2510-PRORATE-STALE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPELG-STALE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED YTD FEE ON THE EXCEPT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2520-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PPELG-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PPELG-ISSUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPELG-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PPELG-ISSUED-YTD ROUNDED = PPELG-EXCEPT-AMT *
                       PPELG-MARGIN / 200
               WHEN PPELG-REGION-CD = 'A'
                   MOVE 5.00 TO PPELG-ISSUED-YTD
               WHEN PPELG-REGION-CD = 'B'
                   MOVE 7.00 TO PPELG-ISSUED-YTD
               WHEN OTHER
                   COMPUTE PPELG-ISSUED-YTD ROUNDED = PPELG-EXCEPT-AMT *
                       PPELG-MARGIN / 100
           END-EVALUATE
           IF PPELG-ISSUED-YTD > 0 AND PPELG-ISSUED-YTD < 10.00
               MOVE 10.00 TO PPELG-ISSUED-YTD
           END-IF
           IF PPELG-ISSUED-YTD > 250.00
               MOVE 250.00 TO PPELG-ISSUED-YTD
           END-IF
           ADD PPELG-ISSUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2530-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PPELG-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPELG-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PPELG-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2540-LIMIT-EXCEPT.
           IF PPELG-EXCEPT-AMT < 50.00
               MOVE 50.00 TO PPELG-EXCEPT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPELG-EXCEPT-AMT > 10000.00
               MOVE 10000.00 TO PPELG-EXCEPT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2550-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-GRID-RATE(1) TO PPELG-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-ISSUED-GRID-LOW(WS-TX)
                   MOVE WS-ISSUED-GRID-RATE(WS-TX) TO PPELG-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPELG-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPELG-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2560-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO PPELG-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO PPELG-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO PPELG-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'PPELG EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
