      *================================================================*
      * PROGRAM  : PPSTAT                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : POSITIVE PAY RULE - ELIGIBILITY. CALLED BY THE      *
      *            POSITIVE PAY BATCH AND ONLINE PROGRAMS.             *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-11-15 TWH  ORIGINAL                                       *
      * 1994-11-18 CRP  ISSUED EXCEPTIONS TO OPS QUEUE PRB6901         *
      * 1995-09-15 SKN  STALE FEE SCHEDULE UPDATE PRB5902              *
      * 1995-06-16 RJT  PAID ROUNDING - HALF UP PRB1620                *
      * 1995-01-03 RJT  STALE FEE SCHEDULE UPDATE PRB1807              *
      * 1999-04-07 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-11-18 CRP  VOID ROUNDING - HALF UP INC7097                *
      * 2003-06-23 PQR  PAID EXCEPTIONS TO OPS QUEUE INC7361           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPSTAT.
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
                                           VALUE 16.000000.
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
      * WS-STALE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STALE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000434000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000523000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000462500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000833500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010750000000258000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020750000000224000'.
       01  WS-STALE-BAND-TBL REDEFINES WS-STALE-BAND-VALUES.
           05  WS-STALE-BAND-ROW OCCURS 6 TIMES.
               10  WS-STALE-BAND-LOW       PIC 9(09)V99.
               10  WS-STALE-BAND-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VOID-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOID-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000103000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000752500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000848500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005350000000187500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010350000000189000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012850000000614500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013350000000871000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013450000000016000'.
       01  WS-VOID-SCHED-TBL REDEFINES WS-VOID-SCHED-VALUES.
           05  WS-VOID-SCHED-ROW OCCURS 8 TIMES.
               10  WS-VOID-SCHED-LOW       PIC 9(09)V99.
               10  WS-VOID-SCHED-RATE      PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY PPSTATP.

       PROCEDURE DIVISION USING PPSTAT-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-EDIT-CONTACT
           PERFORM 2010-CHECK-EFFECTIVE
           PERFORM 2020-CHECK-CLOSE
           PERFORM 2030-COUNT-PAYMENT
           PERFORM 2040-EDIT-REF-NO
           PERFORM 2050-ACCRUE-MARGIN
           PERFORM 2060-FLAG-EXCEPT
           PERFORM 2070-SET-REGION
           PERFORM 2080-ACCRUE-MARGIN
           PERFORM 2090-ASSESS-EXEMPT
           PERFORM 2100-SET-REASON
           PERFORM 2110-SPLIT-PAID
           PERFORM 2120-CHECK-EFFECTIVE
           PERFORM 2130-SPLIT-EXCEPT
           PERFORM 2140-LIMIT-VOID
           PERFORM 2150-ACCRUE-EXCEPT
           PERFORM 2160-ACCRUE-PAID
           PERFORM 2170-COUNT-NOTICE
           PERFORM 2180-LIMIT-STALE
           PERFORM 2190-EDIT-CONTACT
           PERFORM 2200-CHECK-EFFECTIVE
           PERFORM 2210-DESCRIBE-REASON
           PERFORM 2220-SELECT-MARGIN
           PERFORM 2230-SET-CYCLE
           PERFORM 2240-DESCRIBE-REGION
           PERFORM 2250-EDIT-CONTACT
           PERFORM 2260-PRORATE-EXCEPT
           PERFORM 2270-DESCRIBE-CYCLE
           PERFORM 2280-EDIT-REF-NO
           PERFORM 2290-SET-MATCH-RULE
           PERFORM 2300-CHECK-CLOSE
           PERFORM 2310-CHECK-LAST-PAY
           PERFORM 2320-COUNT-NOTICE
           PERFORM 2330-AGE-EFFECTIVE
           PERFORM 2340-ASSESS-OVERRIDE
           PERFORM 2350-LIMIT-ITEM
           PERFORM 2360-ACCRUE-EXCEPT
           PERFORM 2370-ACCUM-CYCLE
           PERFORM 2380-SELECT-MARGIN
           PERFORM 2390-SPLIT-VOID
           PERFORM 2400-DESCRIBE-CYCLE
           PERFORM 2410-FLAG-STALE
           PERFORM 2420-LIMIT-PAID
           PERFORM 2430-ACCUM-CYCLE
           PERFORM 2440-CHECK-LAST-PAY
           PERFORM 2450-SPLIT-PAID
           PERFORM 2460-FLAG-PAID
           PERFORM 2470-SET-CYCLE
           PERFORM 2480-AGE-EFFECTIVE
           PERFORM 2490-FLAG-EXCEPT
           PERFORM 2500-COUNT-PAYMENT
           PERFORM 2510-PRORATE-VOID
           PERFORM 2520-SPLIT-STALE
           PERFORM 2530-SELECT-MARGIN
           PERFORM 2540-EDIT-REF-NO
           PERFORM 2550-FLAG-STALE
           PERFORM 2560-FLAG-PAID
           PERFORM 2570-FLAG-PAID
           PERFORM 2580-SPLIT-EXCEPT
           PERFORM 2590-LIMIT-STALE
           PERFORM 2600-PRORATE-PAID
           PERFORM 2610-LIMIT-PAID
           PERFORM 2620-SET-MATCH-RULE
           PERFORM 2630-SET-MATCH-RULE
           PERFORM 2640-SET-REASON
           PERFORM 2650-SET-CYCLE
           PERFORM 2660-LIMIT-ITEM
           PERFORM 2670-DESCRIBE-REGION
           PERFORM 2680-PRORATE-PAID
           PERFORM 2690-AGE-LAST-PAY
           PERFORM 2700-FLAG-ITEM
           PERFORM 2710-COUNT-PAYMENT
           PERFORM 2720-SPLIT-EXCEPT
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO PPSTAT-RETURN-CODE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E140 CONTACT REQUIRED
      * E912 WORK AMT 2 NEGATIVE
      * E035 WORK AMT 2 OVER 99999
      * E671 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2000-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPSTAT-CONTACT = SPACES
                   MOVE 'E140' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E912' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E035' TO WS-REJECT-CODE
               WHEN PPSTAT-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E671' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2010-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PPSTAT-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPSTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PPSTAT-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2020-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PPSTAT-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPSTAT-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO PPSTAT-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2030-COUNT-PAYMENT.
           IF PPSTAT-PAYMENT-CNT < 9999999
               ADD 1 TO PPSTAT-PAYMENT-CNT
           END-IF
           IF PPSTAT-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO PPSTAT-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E256 REF NO REQUIRED
      * E982 VOID AMT NEGATIVE
      * E725 VOID AMT OVER 1000000
      * E879 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2040-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPSTAT-REF-NO = SPACES
                   MOVE 'E256' TO WS-REJECT-CODE
               WHEN PPSTAT-VOID-AMT < 0
                   MOVE 'E982' TO WS-REJECT-CODE
               WHEN PPSTAT-VOID-AMT > 1000000.00
                   MOVE 'E725' TO WS-REJECT-CODE
               WHEN PPSTAT-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E879' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2050-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PPSTAT-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PPSTAT-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2060-FLAG-EXCEPT.
           IF PPSTAT-EXCEPT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPSTAT-EXCEPT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> B WHEN EFFECTIVE DATE IS PAST
      * B -> D WHEN EFFECTIVE DATE IS PAST
      * D -> H WHEN EFFECTIVE DATE IS PAST
      * H -> N WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2070-SET-REGION.
           EVALUATE PPSTAT-REGION-CD
               WHEN 'A'
                   IF PPSTAT-EFFECTIVE-DATE > 0 AND
                       PPSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'B' TO PPSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPSTAT-EFFECTIVE-DATE > 0 AND
                       PPSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'D' TO PPSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF PPSTAT-EFFECTIVE-DATE > 0 AND
                       PPSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'H' TO PPSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PPSTAT-EFFECTIVE-DATE > 0 AND
                       PPSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'N' TO PPSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2080-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PPSTAT-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PPSTAT-STALE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE STALE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2090-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PPSTAT-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PPSTAT-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPSTAT-OVERRIDE-FLAG = 'Y'
                   COMPUTE PPSTAT-VOID-AMT ROUNDED = PPSTAT-STALE-YTD *
                       PPSTAT-MARGIN / 200
               WHEN PPSTAT-REASON-CD = 'B'
                   MOVE 10.00 TO PPSTAT-VOID-AMT
               WHEN PPSTAT-REASON-CD = 'M'
                   MOVE 20.00 TO PPSTAT-VOID-AMT
               WHEN OTHER
                   COMPUTE PPSTAT-VOID-AMT ROUNDED = PPSTAT-STALE-YTD *
                       PPSTAT-MARGIN / 100
           END-EVALUATE
           IF PPSTAT-VOID-AMT > 0 AND PPSTAT-VOID-AMT < 10.00
               MOVE 10.00 TO PPSTAT-VOID-AMT
           END-IF
           IF PPSTAT-VOID-AMT > 100.00
               MOVE 100.00 TO PPSTAT-VOID-AMT
           END-IF
           ADD PPSTAT-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> M WHEN LAST PAY DATE IS PAST
      * M -> N WHEN LAST PAY DATE IS PAST
      * N -> U WHEN LAST PAY DATE IS PAST
      * U -> V WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2100-SET-REASON.
           EVALUATE PPSTAT-REASON-CD
               WHEN 'B'
                   IF PPSTAT-LAST-PAY-DATE > 0 AND PPSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'M' TO PPSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPSTAT-LAST-PAY-DATE > 0 AND PPSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'N' TO PPSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF PPSTAT-LAST-PAY-DATE > 0 AND PPSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'U' TO PPSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF PPSTAT-LAST-PAY-DATE > 0 AND PPSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'V' TO PPSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2110-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE PPSTAT-PAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2120-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PPSTAT-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPSTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PPSTAT-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2130-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE PPSTAT-EXCEPT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2140-LIMIT-VOID.
           IF PPSTAT-VOID-AMT < 0.00
               MOVE 0.00 TO PPSTAT-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPSTAT-VOID-AMT > 10000.00
               MOVE 10000.00 TO PPSTAT-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCEPT YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2150-ACCRUE-EXCEPT.
           IF PPSTAT-EXCEPT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPSTAT-EXCEPT-YTD * PPSTAT-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2160-ACCRUE-PAID.
           IF PPSTAT-PAID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPSTAT-PAID-YTD * PPSTAT-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PPSTAT-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2170-COUNT-NOTICE.
           IF PPSTAT-NOTICE-CNT < 9999999
               ADD 1 TO PPSTAT-NOTICE-CNT
           END-IF
           IF PPSTAT-NOTICE-CNT NOT < 10
               MOVE 'Y' TO PPSTAT-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STALE YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2180-LIMIT-STALE.
           IF PPSTAT-STALE-YTD < 100.00
               MOVE 100.00 TO PPSTAT-STALE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPSTAT-STALE-YTD > 1000.00
               MOVE 1000.00 TO PPSTAT-STALE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E066 CONTACT REQUIRED
      * E777 EXCEPT YTD NEGATIVE
      * E544 EXCEPT YTD OVER 10000
      * E864 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2190-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPSTAT-CONTACT = SPACES
                   MOVE 'E066' TO WS-REJECT-CODE
               WHEN PPSTAT-EXCEPT-YTD < 0
                   MOVE 'E777' TO WS-REJECT-CODE
               WHEN PPSTAT-EXCEPT-YTD > 10000.00
                   MOVE 'E544' TO WS-REJECT-CODE
               WHEN PPSTAT-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E864' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2200-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PPSTAT-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPSTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PPSTAT-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2210-DESCRIBE-REASON.
           EVALUATE PPSTAT-REASON-CD
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'M'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'N'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STALE BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2220-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STALE-BAND-RATE(1) TO PPSTAT-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-STALE-BAND-LOW(WS-TX)
                   MOVE WS-STALE-BAND-RATE(WS-TX) TO PPSTAT-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPSTAT-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPSTAT-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> B WHEN LAST PAY DATE IS PAST
      * B -> C WHEN LAST PAY DATE IS PAST
      * C -> R WHEN LAST PAY DATE IS PAST
      * R -> T WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2230-SET-CYCLE.
           EVALUATE PPSTAT-CYCLE-CD
               WHEN 'A'
                   IF PPSTAT-LAST-PAY-DATE > 0 AND PPSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'B' TO PPSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPSTAT-LAST-PAY-DATE > 0 AND PPSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'C' TO PPSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PPSTAT-LAST-PAY-DATE > 0 AND PPSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'R' TO PPSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF PPSTAT-LAST-PAY-DATE > 0 AND PPSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'T' TO PPSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2240-DESCRIBE-REGION.
           EVALUATE PPSTAT-REGION-CD
               WHEN 'A'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'B'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'D'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E109 CONTACT REQUIRED
      * E198 EXCEPT AMT NEGATIVE
      * E963 EXCEPT AMT OVER 1000000
      * E455 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2250-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPSTAT-CONTACT = SPACES
                   MOVE 'E109' TO WS-REJECT-CODE
               WHEN PPSTAT-EXCEPT-AMT < 0
                   MOVE 'E198' TO WS-REJECT-CODE
               WHEN PPSTAT-EXCEPT-AMT > 1000000.00
                   MOVE 'E963' TO WS-REJECT-CODE
               WHEN PPSTAT-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E455' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2260-PRORATE-EXCEPT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PPSTAT-EXCEPT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2270-DESCRIBE-CYCLE.
           EVALUATE PPSTAT-CYCLE-CD
               WHEN 'A'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'B'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'C'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E026 REF NO REQUIRED
      * E884 STALE YTD NEGATIVE
      * E995 STALE YTD OVER 25000
      * E805 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2280-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPSTAT-REF-NO = SPACES
                   MOVE 'E026' TO WS-REJECT-CODE
               WHEN PPSTAT-STALE-YTD < 0
                   MOVE 'E884' TO WS-REJECT-CODE
               WHEN PPSTAT-STALE-YTD > 25000.00
                   MOVE 'E995' TO WS-REJECT-CODE
               WHEN PPSTAT-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E805' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MATCH RULE CD:
      * A -> B WHEN CLOSE DATE IS PAST
      * B -> F WHEN CLOSE DATE IS PAST
      * F -> M WHEN CLOSE DATE IS PAST
      * M -> S WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2290-SET-MATCH-RULE.
           EVALUATE PPSTAT-MATCH-RULE-CD
               WHEN 'A'
                   IF PPSTAT-CLOSE-DATE > 0 AND PPSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPSTAT-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPSTAT-CLOSE-DATE > 0 AND PPSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO PPSTAT-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF PPSTAT-CLOSE-DATE > 0 AND PPSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPSTAT-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPSTAT-CLOSE-DATE > 0 AND PPSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO PPSTAT-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2300-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PPSTAT-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPSTAT-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PPSTAT-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2310-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PPSTAT-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPSTAT-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PPSTAT-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2320-COUNT-NOTICE.
           IF PPSTAT-NOTICE-CNT < 9999999
               ADD 1 TO PPSTAT-NOTICE-CNT
           END-IF
           IF PPSTAT-NOTICE-CNT NOT < 3
               MOVE 'Y' TO PPSTAT-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2330-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PPSTAT-EFFECTIVE-DATE > 0 AND PPSTAT-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPSTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPSTAT-STALE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PPSTAT-STALE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PPSTAT-STALE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPSTAT-STALE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPSTAT-STALE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT YTD FEE ON THE PAID AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2340-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN PPSTAT-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO PPSTAT-EXCEPT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPSTAT-EXEMPT-FLAG = 'Y'
                   COMPUTE PPSTAT-EXCEPT-YTD ROUNDED = PPSTAT-PAID-AMT *
                       PPSTAT-MARGIN / 200
               WHEN PPSTAT-REASON-CD = 'B'
                   MOVE 7.00 TO PPSTAT-EXCEPT-YTD
               WHEN PPSTAT-REASON-CD = 'M'
                   MOVE 5.00 TO PPSTAT-EXCEPT-YTD
               WHEN OTHER
                   COMPUTE PPSTAT-EXCEPT-YTD ROUNDED = PPSTAT-PAID-AMT *
                       PPSTAT-MARGIN / 100
           END-EVALUATE
           IF PPSTAT-EXCEPT-YTD > 0 AND PPSTAT-EXCEPT-YTD < 10.00
               MOVE 10.00 TO PPSTAT-EXCEPT-YTD
           END-IF
           IF PPSTAT-EXCEPT-YTD > 100.00
               MOVE 100.00 TO PPSTAT-EXCEPT-YTD
           END-IF
           ADD PPSTAT-EXCEPT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2350-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCEPT YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2360-ACCRUE-EXCEPT.
           IF PPSTAT-EXCEPT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPSTAT-EXCEPT-YTD * PPSTAT-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PPSTAT-VOID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2370-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPSTAT-CYCLE-CD
                   ADD PPSTAT-VOID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPSTAT-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPSTAT-VOID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID SCHED TIER FOR THE STALE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2380-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-SCHED-RATE(1) TO PPSTAT-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PPSTAT-STALE-YTD NOT < WS-VOID-SCHED-LOW(WS-TX)
                   MOVE WS-VOID-SCHED-RATE(WS-TX) TO PPSTAT-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPSTAT-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPSTAT-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2390-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = PPSTAT-VOID-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPSTAT-VOID-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2400-DESCRIBE-CYCLE.
           EVALUATE PPSTAT-CYCLE-CD
               WHEN 'A'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'B'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'C'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STALE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2410-FLAG-STALE.
           IF PPSTAT-STALE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPSTAT-STALE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2420-LIMIT-PAID.
           IF PPSTAT-PAID-AMT < 0.00
               MOVE 0.00 TO PPSTAT-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPSTAT-PAID-AMT > 5000.00
               MOVE 5000.00 TO PPSTAT-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2430-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPSTAT-CYCLE-CD
                   ADD PPSTAT-VOID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPSTAT-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPSTAT-VOID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2440-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PPSTAT-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPSTAT-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PPSTAT-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2450-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = PPSTAT-PAID-YTD / 12
           COMPUTE WS-WORK-AMT-2 = PPSTAT-PAID-YTD - (WS-WORK-AMT * 12)
           COMPUTE PPSTAT-VOID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2460-FLAG-PAID.
           IF PPSTAT-PAID-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPSTAT-PAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> B WHEN LAST PAY DATE IS PAST
      * B -> C WHEN LAST PAY DATE IS PAST
      * C -> R WHEN LAST PAY DATE IS PAST
      * R -> T WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2470-SET-CYCLE.
           EVALUATE PPSTAT-CYCLE-CD
               WHEN 'A'
                   IF PPSTAT-LAST-PAY-DATE > 0 AND PPSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'B' TO PPSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPSTAT-LAST-PAY-DATE > 0 AND PPSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'C' TO PPSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PPSTAT-LAST-PAY-DATE > 0 AND PPSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'R' TO PPSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF PPSTAT-LAST-PAY-DATE > 0 AND PPSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'T' TO PPSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2480-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PPSTAT-EFFECTIVE-DATE > 0 AND PPSTAT-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPSTAT-EFFECTIVE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2490-FLAG-EXCEPT.
           IF PPSTAT-EXCEPT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPSTAT-EXCEPT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2500-COUNT-PAYMENT.
           IF PPSTAT-PAYMENT-CNT < 9999999
               ADD 1 TO PPSTAT-PAYMENT-CNT
           END-IF
           IF PPSTAT-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO PPSTAT-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2510-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PPSTAT-VOID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2520-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE PPSTAT-STALE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID SCHED TIER FOR THE PAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2530-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-SCHED-RATE(1) TO PPSTAT-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PPSTAT-PAID-YTD NOT < WS-VOID-SCHED-LOW(WS-TX)
                   MOVE WS-VOID-SCHED-RATE(WS-TX) TO PPSTAT-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPSTAT-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPSTAT-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E642 REF NO REQUIRED
      * E067 PAID AMT NEGATIVE
      * E545 PAID AMT OVER 25000
      * E337 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2540-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPSTAT-REF-NO = SPACES
                   MOVE 'E642' TO WS-REJECT-CODE
               WHEN PPSTAT-PAID-AMT < 0
                   MOVE 'E067' TO WS-REJECT-CODE
               WHEN PPSTAT-PAID-AMT > 25000.00
                   MOVE 'E545' TO WS-REJECT-CODE
               WHEN PPSTAT-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E337' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STALE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2550-FLAG-STALE.
           IF PPSTAT-STALE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPSTAT-STALE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2560-FLAG-PAID.
           IF PPSTAT-PAID-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPSTAT-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2570-FLAG-PAID.
           IF PPSTAT-PAID-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPSTAT-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2580-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE PPSTAT-EXCEPT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STALE YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2590-LIMIT-STALE.
           IF PPSTAT-STALE-YTD < 50.00
               MOVE 50.00 TO PPSTAT-STALE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPSTAT-STALE-YTD > 10000.00
               MOVE 10000.00 TO PPSTAT-STALE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAID YTD.
      *----------------------------------------------------------------*
       2600-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPSTAT-PAID-YTD ROUNDED =
                   PPSTAT-VOID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPSTAT-PAID-YTD
           END-IF
           ADD PPSTAT-PAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAID YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2610-LIMIT-PAID.
           IF PPSTAT-PAID-YTD < 50.00
               MOVE 50.00 TO PPSTAT-PAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPSTAT-PAID-YTD > 10000.00
               MOVE 10000.00 TO PPSTAT-PAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MATCH RULE CD:
      * A -> B WHEN CLOSE DATE IS PAST
      * B -> F WHEN CLOSE DATE IS PAST
      * F -> M WHEN CLOSE DATE IS PAST
      * M -> S WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2620-SET-MATCH-RULE.
           EVALUATE PPSTAT-MATCH-RULE-CD
               WHEN 'A'
                   IF PPSTAT-CLOSE-DATE > 0 AND PPSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPSTAT-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPSTAT-CLOSE-DATE > 0 AND PPSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO PPSTAT-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF PPSTAT-CLOSE-DATE > 0 AND PPSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPSTAT-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPSTAT-CLOSE-DATE > 0 AND PPSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO PPSTAT-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MATCH RULE CD:
      * A -> B WHEN LAST PAY DATE IS PAST
      * B -> F WHEN LAST PAY DATE IS PAST
      * F -> M WHEN LAST PAY DATE IS PAST
      * M -> S WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2630-SET-MATCH-RULE.
           EVALUATE PPSTAT-MATCH-RULE-CD
               WHEN 'A'
                   IF PPSTAT-LAST-PAY-DATE > 0 AND PPSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'B' TO PPSTAT-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPSTAT-LAST-PAY-DATE > 0 AND PPSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'F' TO PPSTAT-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF PPSTAT-LAST-PAY-DATE > 0 AND PPSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'M' TO PPSTAT-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPSTAT-LAST-PAY-DATE > 0 AND PPSTAT-LAST-PAY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'S' TO PPSTAT-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> M WHEN EFFECTIVE DATE IS PAST
      * M -> N WHEN EFFECTIVE DATE IS PAST
      * N -> U WHEN EFFECTIVE DATE IS PAST
      * U -> V WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2640-SET-REASON.
           EVALUATE PPSTAT-REASON-CD
               WHEN 'B'
                   IF PPSTAT-EFFECTIVE-DATE > 0 AND
                       PPSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'M' TO PPSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPSTAT-EFFECTIVE-DATE > 0 AND
                       PPSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'N' TO PPSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF PPSTAT-EFFECTIVE-DATE > 0 AND
                       PPSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'U' TO PPSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF PPSTAT-EFFECTIVE-DATE > 0 AND
                       PPSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'V' TO PPSTAT-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> B WHEN EFFECTIVE DATE IS PAST
      * B -> C WHEN EFFECTIVE DATE IS PAST
      * C -> R WHEN EFFECTIVE DATE IS PAST
      * R -> T WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2650-SET-CYCLE.
           EVALUATE PPSTAT-CYCLE-CD
               WHEN 'A'
                   IF PPSTAT-EFFECTIVE-DATE > 0 AND
                       PPSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'B' TO PPSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPSTAT-EFFECTIVE-DATE > 0 AND
                       PPSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'C' TO PPSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PPSTAT-EFFECTIVE-DATE > 0 AND
                       PPSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'R' TO PPSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF PPSTAT-EFFECTIVE-DATE > 0 AND
                       PPSTAT-EFFECTIVE-DATE < CTL-BUSINESS-DATE
                       MOVE 'T' TO PPSTAT-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2660-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2670-DESCRIBE-REGION.
           EVALUATE PPSTAT-REGION-CD
               WHEN 'A'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'N'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       2680-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPSTAT-PAID-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPSTAT-PAID-AMT
           END-IF
           ADD PPSTAT-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2690-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PPSTAT-LAST-PAY-DATE > 0 AND PPSTAT-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPSTAT-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPSTAT-STALE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPSTAT-STALE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PPSTAT-STALE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PPSTAT-STALE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPSTAT-STALE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2700-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       2710-COUNT-PAYMENT.
           IF PPSTAT-PAYMENT-CNT < 9999999
               ADD 1 TO PPSTAT-PAYMENT-CNT
           END-IF
           IF PPSTAT-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PPSTAT-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2720-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = PPSTAT-EXCEPT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPSTAT-EXCEPT-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPSTAT-EXCEPT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO PPSTAT-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO PPSTAT-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO PPSTAT-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'PPSTAT EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
