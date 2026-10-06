      *================================================================*
      * PROGRAM  : PBSTAT                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : PASSBOOK PRINTING RULE - STATUS. CALLED BY THE      *
      *            PASSBOOK PRINTING BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-01-14 BHS  ORIGINAL                                       *
      * 2000-02-09 CRP  NEW BALANCE REPORT COLUMNS REQ8548             *
      * 2001-11-25 EWS  UNPOSTED NOTICE TEXT PER LEGAL PRB2974         *
      * 2003-10-04 GAF  UNPOSTED NOTICE TEXT PER LEGAL REQ6679         *
      * 2004-02-10 DPS  INTEREST CUTOFF MOVED CHG5742                  *
      * 2006-11-09 JLM  CARRYFWD CUTOFF MOVED REQ1934                  *
      * 2007-01-20 MKL  PRINTED FEE SCHEDULE UPDATE INC1724            *
      * 2008-09-16 BHS  PRINTED CUTOFF MOVED PRB9875                   *
      * 2008-03-15 GAF  INTEREST RERUN PROTECTION PRB3201              *
      * 2009-01-14 MKL  BALANCE EXCEPTIONS TO OPS QUEUE INC6675        *
      * 2011-04-22 JLM  BALANCE RERUN PROTECTION INC4021               *
      * 2011-05-11 LMB  CARRYFWD ROUNDING - HALF UP REQ3961            *
      * 2012-03-21 MKL  BALANCE CALCULATION CORRECTED CHG0936          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBSTAT.
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
                                           VALUE 8.000000.
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
      * WS-BALANCE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BALANCE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000690000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000485500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000235000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000219000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003650000000106000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003900000000779500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003950000000721000'.
       01  WS-BALANCE-TIER-TBL REDEFINES WS-BALANCE-TIER-VALUES.
           05  WS-BALANCE-TIER-ROW OCCURS 7 TIMES.
               10  WS-BALANCE-TIER-LOW     PIC 9(09)V99.
               10  WS-BALANCE-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INTEREST-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000706500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000679500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000748000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000240000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010200000000203000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015200000000144000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015250000000073000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020250000000183000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025250000000122500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025300000000023500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030300000000399000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030400000000154000'.
       01  WS-INTEREST-GRID-TBL REDEFINES WS-INTEREST-GRID-VALUES.
           05  WS-INTEREST-GRID-ROW OCCURS 12 TIMES.
               10  WS-INTEREST-GRID-LOW    PIC 9(09)V99.
               10  WS-INTEREST-GRID-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY PBSTATP.

       PROCEDURE DIVISION USING PBSTAT-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-COUNT-RETRY
           PERFORM 2010-COUNT-CYCLE
           PERFORM 2020-FLAG-LINE
           PERFORM 2030-SELECT-PENALTY-PCT
           PERFORM 2040-ACCRUE-UNPOSTED
           PERFORM 2050-AGE-CLOSE
           PERFORM 2060-LIMIT-ITEM
           PERFORM 2070-SET-REGION
           PERFORM 2080-LIMIT-UNPOSTED
           PERFORM 2090-SELECT-PENALTY-PCT
           PERFORM 2100-COUNT-RETRY
           PERFORM 2110-ASSESS-NOTICE-SENT
           PERFORM 2120-COUNT-RETRY
           PERFORM 2130-LIMIT-UNPOSTED
           PERFORM 2140-PRORATE-LINE
           PERFORM 2150-FLAG-LINE
           PERFORM 2160-FLAG-LINE
           PERFORM 2170-CHECK-EFFECTIVE
           PERFORM 2180-CHECK-EFFECTIVE
           PERFORM 2190-PRORATE-BALANCE
           PERFORM 2200-COUNT-RETRY
           PERFORM 2210-PRORATE-BALANCE
           PERFORM 2220-ASSESS-NOTICE-SENT
           PERFORM 2230-AGE-EFFECTIVE
           PERFORM 2240-COUNT-CYCLE
           PERFORM 2250-PRORATE-BALANCE
           PERFORM 2260-COUNT-RETRY
           PERFORM 2270-AGE-NEXT-REVIEW
           PERFORM 2280-ACCRUE-LINE
           PERFORM 2290-CHECK-EFFECTIVE
           PERFORM 2300-SPLIT-LINE
           PERFORM 2310-AGE-EFFECTIVE
           PERFORM 2320-DESCRIBE-SOURCE
           PERFORM 2330-LIMIT-CARRYFWD
           PERFORM 2340-ASSESS-AUTO-RENEW
           PERFORM 2350-LIMIT-CARRYFWD
           PERFORM 2360-COUNT-CYCLE
           PERFORM 2370-SPLIT-UNPOSTED
           PERFORM 2380-CHECK-NEXT-DUE
           PERFORM 2390-SELECT-PENALTY-PCT
           PERFORM 2400-CHECK-CLOSE
           PERFORM 2410-PRORATE-CARRYFWD
           PERFORM 2420-LIMIT-UNPOSTED
           PERFORM 2430-ACCUM-REGION
           PERFORM 2440-SELECT-PENALTY-PCT
           PERFORM 2450-SET-SOURCE
           PERFORM 2460-FLAG-CARRYFWD
           PERFORM 2470-ACCRUE-LINE
           PERFORM 2480-DESCRIBE-BOOK-TYPE
           PERFORM 2490-SET-SOURCE
           PERFORM 2500-PRORATE-LINE
           PERFORM 2510-FLAG-LINE
           PERFORM 2520-ACCRUE-LINE
           PERFORM 2530-DESCRIBE-REGION
           PERFORM 2540-COUNT-CYCLE
           PERFORM 2550-ASSESS-REVIEW
           PERFORM 2560-SET-SOURCE
           PERFORM 2570-SET-PRINTER
           PERFORM 2580-CHECK-EFFECTIVE
           PERFORM 2590-COUNT-RETRY
           PERFORM 2600-DESCRIBE-BOOK-TYPE
           PERFORM 2610-PRORATE-UNPOSTED
           PERFORM 2620-PRORATE-UNPOSTED
           PERFORM 2630-FLAG-LINE
           PERFORM 2640-PRORATE-CARRYFWD
           PERFORM 2650-CHECK-NEXT-DUE
           PERFORM 2660-AGE-CLOSE
           PERFORM 2670-CHECK-CLOSE
           PERFORM 2680-COUNT-RETRY
           PERFORM 2690-EDIT-CUST-ID
           PERFORM 2700-FLAG-UNPOSTED
           PERFORM 2710-ACCRUE-UNPOSTED
           PERFORM 2720-SET-PRINTER
           PERFORM 2730-ASSESS-NOTICE-SENT
           PERFORM 2740-DESCRIBE-BOOK-TYPE
           PERFORM 2750-ACCUM-SOURCE
           PERFORM 2760-LIMIT-LINE
           PERFORM 2770-DESCRIBE-PRINTER
           PERFORM 2780-FLAG-UNPOSTED
           PERFORM 2790-ACCUM-PRINTER
           PERFORM 2800-SPLIT-CARRYFWD
           PERFORM 2810-AGE-NEXT-DUE
           PERFORM 2820-EDIT-CONTACT
           PERFORM 2830-EDIT-CONTACT
           PERFORM 2840-SET-REGION
           PERFORM 2850-SET-REGION
           PERFORM 2860-SELECT-MARGIN
           PERFORM 2870-LIMIT-BALANCE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO PBSTAT-RETURN-CODE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2000-COUNT-RETRY.
           IF PBSTAT-RETRY-CNT < 9999999
               ADD 1 TO PBSTAT-RETRY-CNT
           END-IF
           IF PBSTAT-RETRY-CNT NOT < 3
               MOVE 'Y' TO PBSTAT-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2010-COUNT-CYCLE.
           IF PBSTAT-CYCLE-CNT < 9999999
               ADD 1 TO PBSTAT-CYCLE-CNT
           END-IF
           IF PBSTAT-CYCLE-CNT NOT < 10
               MOVE 'Y' TO PBSTAT-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * LINE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2020-FLAG-LINE.
           IF PBSTAT-LINE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBSTAT-LINE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST GRID TIER FOR THE UNPOSTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2030-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-GRID-RATE(1) TO PBSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PBSTAT-UNPOSTED-AMT NOT < WS-INTEREST-GRID-LOW(WS-TX)
                   MOVE WS-INTEREST-GRID-RATE(WS-TX) TO
                       PBSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON UNPOSTED AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2040-ACCRUE-UNPOSTED.
           IF PBSTAT-UNPOSTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBSTAT-UNPOSTED-AMT * PBSTAT-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBSTAT-CARRYFWD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2050-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBSTAT-CLOSE-DATE > 0 AND PBSTAT-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBSTAT-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBSTAT-UNPOSTED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD PBSTAT-UNPOSTED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PBSTAT-UNPOSTED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBSTAT-UNPOSTED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBSTAT-UNPOSTED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2060-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * ER -> GL WHEN NEXT REVIEW DATE IS PAST
      * GL -> PE WHEN NEXT REVIEW DATE IS PAST
      * PE -> RJ WHEN NEXT REVIEW DATE IS PAST
      * RJ -> VB WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2070-SET-REGION.
           EVALUATE PBSTAT-REGION-CD
               WHEN 'ER'
                   IF PBSTAT-NEXT-REVIEW-DATE > 0 AND
                       PBSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GL' TO PBSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF PBSTAT-NEXT-REVIEW-DATE > 0 AND
                       PBSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PE' TO PBSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PE'
                   IF PBSTAT-NEXT-REVIEW-DATE > 0 AND
                       PBSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'RJ' TO PBSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF PBSTAT-NEXT-REVIEW-DATE > 0 AND
                       PBSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VB' TO PBSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * UNPOSTED YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2080-LIMIT-UNPOSTED.
           IF PBSTAT-UNPOSTED-YTD < 0.00
               MOVE 0.00 TO PBSTAT-UNPOSTED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBSTAT-UNPOSTED-YTD > 1000.00
               MOVE 1000.00 TO PBSTAT-UNPOSTED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2090-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-GRID-RATE(1) TO PBSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-INTEREST-GRID-LOW(WS-TX)
                   MOVE WS-INTEREST-GRID-RATE(WS-TX) TO
                       PBSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2100-COUNT-RETRY.
           IF PBSTAT-RETRY-CNT < 9999999
               ADD 1 TO PBSTAT-RETRY-CNT
           END-IF
           IF PBSTAT-RETRY-CNT NOT < 12
               MOVE 'Y' TO PBSTAT-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE UNPOSTED AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2110-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PBSTAT-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBSTAT-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PBSTAT-UNPOSTED-AMT *
                       PBSTAT-PENALTY-PCT / 200
               WHEN PBSTAT-PRINTER-CD = 'CB'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN PBSTAT-PRINTER-CD = 'EP'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PBSTAT-UNPOSTED-AMT *
                       PBSTAT-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       2120-COUNT-RETRY.
           IF PBSTAT-RETRY-CNT < 9999999
               ADD 1 TO PBSTAT-RETRY-CNT
           END-IF
           IF PBSTAT-RETRY-CNT NOT < 12
               MOVE 'Y' TO PBSTAT-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * UNPOSTED YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2130-LIMIT-UNPOSTED.
           IF PBSTAT-UNPOSTED-YTD < 25.00
               MOVE 25.00 TO PBSTAT-UNPOSTED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBSTAT-UNPOSTED-YTD > 1000.00
               MOVE 1000.00 TO PBSTAT-UNPOSTED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BALANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LINE AMT.
      *----------------------------------------------------------------*
       2140-PRORATE-LINE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBSTAT-LINE-AMT ROUNDED =
                   PBSTAT-BALANCE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBSTAT-LINE-AMT
           END-IF
           ADD PBSTAT-LINE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * LINE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2150-FLAG-LINE.
           IF PBSTAT-LINE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBSTAT-LINE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * LINE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2160-FLAG-LINE.
           IF PBSTAT-LINE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBSTAT-LINE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2170-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PBSTAT-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBSTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO PBSTAT-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2180-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PBSTAT-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBSTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO PBSTAT-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE UNPOSTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BALANCE AMT.
      *----------------------------------------------------------------*
       2190-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBSTAT-BALANCE-AMT ROUNDED =
                   PBSTAT-UNPOSTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBSTAT-BALANCE-AMT
           END-IF
           ADD PBSTAT-BALANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2200-COUNT-RETRY.
           IF PBSTAT-RETRY-CNT < 9999999
               ADD 1 TO PBSTAT-RETRY-CNT
           END-IF
           IF PBSTAT-RETRY-CNT NOT < 12
               MOVE 'Y' TO PBSTAT-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BALANCE AMT.
      *----------------------------------------------------------------*
       2210-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBSTAT-BALANCE-AMT ROUNDED =
                   PBSTAT-CARRYFWD-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBSTAT-BALANCE-AMT
           END-IF
           ADD PBSTAT-BALANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE LINE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2220-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PBSTAT-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBSTAT-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = PBSTAT-LINE-AMT *
                       PBSTAT-PENALTY-PCT / 200
               WHEN PBSTAT-REGION-CD = 'ER'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN PBSTAT-REGION-CD = 'GL'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = PBSTAT-LINE-AMT *
                       PBSTAT-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2230-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PBSTAT-EFFECTIVE-DATE > 0 AND PBSTAT-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBSTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2240-COUNT-CYCLE.
           IF PBSTAT-CYCLE-CNT < 9999999
               ADD 1 TO PBSTAT-CYCLE-CNT
           END-IF
           IF PBSTAT-CYCLE-CNT NOT < 5
               MOVE 'Y' TO PBSTAT-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LINE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BALANCE AMT.
      *----------------------------------------------------------------*
       2250-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBSTAT-BALANCE-AMT ROUNDED =
                   PBSTAT-LINE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBSTAT-BALANCE-AMT
           END-IF
           ADD PBSTAT-BALANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2260-COUNT-RETRY.
           IF PBSTAT-RETRY-CNT < 9999999
               ADD 1 TO PBSTAT-RETRY-CNT
           END-IF
           IF PBSTAT-RETRY-CNT NOT < 3
               MOVE 'Y' TO PBSTAT-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2270-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PBSTAT-NEXT-REVIEW-DATE > 0 AND PBSTAT-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBSTAT-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBSTAT-CARRYFWD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PBSTAT-CARRYFWD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBSTAT-CARRYFWD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBSTAT-CARRYFWD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBSTAT-CARRYFWD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LINE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2280-ACCRUE-LINE.
           IF PBSTAT-LINE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBSTAT-LINE-AMT * PBSTAT-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBSTAT-UNPOSTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2290-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PBSTAT-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBSTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PBSTAT-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LINE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2300-SPLIT-LINE.
           COMPUTE WS-WORK-AMT = PBSTAT-LINE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = PBSTAT-LINE-YTD - (WS-WORK-AMT * 4)
           COMPUTE PBSTAT-LINE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2310-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PBSTAT-EFFECTIVE-DATE > 0 AND PBSTAT-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBSTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBSTAT-CARRYFWD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PBSTAT-CARRYFWD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBSTAT-CARRYFWD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBSTAT-CARRYFWD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBSTAT-CARRYFWD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2320-DESCRIBE-SOURCE.
           EVALUATE PBSTAT-SOURCE-CD
               WHEN 'JX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CARRYFWD YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2330-LIMIT-CARRYFWD.
           IF PBSTAT-CARRYFWD-YTD < 50.00
               MOVE 50.00 TO PBSTAT-CARRYFWD-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBSTAT-CARRYFWD-YTD > 2500.00
               MOVE 2500.00 TO PBSTAT-CARRYFWD-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE UNPOSTED YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2340-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PBSTAT-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBSTAT-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = PBSTAT-UNPOSTED-YTD *
                       PBSTAT-MARGIN / 200
               WHEN PBSTAT-REGION-CD = 'ER'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN PBSTAT-REGION-CD = 'GL'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = PBSTAT-UNPOSTED-YTD *
                       PBSTAT-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CARRYFWD YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2350-LIMIT-CARRYFWD.
           IF PBSTAT-CARRYFWD-YTD < 10.00
               MOVE 10.00 TO PBSTAT-CARRYFWD-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBSTAT-CARRYFWD-YTD > 10000.00
               MOVE 10000.00 TO PBSTAT-CARRYFWD-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2360-COUNT-CYCLE.
           IF PBSTAT-CYCLE-CNT < 9999999
               ADD 1 TO PBSTAT-CYCLE-CNT
           END-IF
           IF PBSTAT-CYCLE-CNT NOT < 5
               MOVE 'Y' TO PBSTAT-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT UNPOSTED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2370-SPLIT-UNPOSTED.
           COMPUTE WS-WORK-AMT = PBSTAT-UNPOSTED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PBSTAT-UNPOSTED-AMT - (WS-WORK-AMT *
               4)
           COMPUTE PBSTAT-LINE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2380-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PBSTAT-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBSTAT-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PBSTAT-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST GRID TIER FOR THE UNPOSTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2390-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-GRID-RATE(1) TO PBSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PBSTAT-UNPOSTED-AMT NOT < WS-INTEREST-GRID-LOW(WS-TX)
                   MOVE WS-INTEREST-GRID-RATE(WS-TX) TO
                       PBSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2400-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBSTAT-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBSTAT-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO PBSTAT-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE UNPOSTED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CARRYFWD YTD.
      *----------------------------------------------------------------*
       2410-PRORATE-CARRYFWD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBSTAT-CARRYFWD-YTD ROUNDED =
                   PBSTAT-UNPOSTED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBSTAT-CARRYFWD-YTD
           END-IF
           ADD PBSTAT-CARRYFWD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * UNPOSTED AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2420-LIMIT-UNPOSTED.
           IF PBSTAT-UNPOSTED-AMT < 10.00
               MOVE 10.00 TO PBSTAT-UNPOSTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBSTAT-UNPOSTED-AMT > 2500.00
               MOVE 2500.00 TO PBSTAT-UNPOSTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LINE YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2430-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBSTAT-REGION-CD
                   ADD PBSTAT-LINE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBSTAT-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBSTAT-LINE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BALANCE TIER TIER FOR THE UNPOSTED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2440-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BALANCE-TIER-RATE(1) TO PBSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF PBSTAT-UNPOSTED-YTD NOT < WS-BALANCE-TIER-LOW(WS-TX)
                   MOVE WS-BALANCE-TIER-RATE(WS-TX) TO
                       PBSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * JX -> ZC WHEN NEXT REVIEW DATE IS PAST
      * ZC -> ZY WHEN NEXT REVIEW DATE IS PAST
      * ZY -> JX WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2450-SET-SOURCE.
           EVALUATE PBSTAT-SOURCE-CD
               WHEN 'JX'
                   IF PBSTAT-NEXT-REVIEW-DATE > 0 AND
                       PBSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZC' TO PBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZC'
                   IF PBSTAT-NEXT-REVIEW-DATE > 0 AND
                       PBSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZY' TO PBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZY'
                   IF PBSTAT-NEXT-REVIEW-DATE > 0 AND
                       PBSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JX' TO PBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CARRYFWD YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2460-FLAG-CARRYFWD.
           IF PBSTAT-CARRYFWD-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBSTAT-CARRYFWD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LINE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2470-ACCRUE-LINE.
           IF PBSTAT-LINE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBSTAT-LINE-YTD * PBSTAT-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PBSTAT-UNPOSTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOOK TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2480-DESCRIBE-BOOK-TYPE.
           EVALUATE PBSTAT-BOOK-TYPE-CD
               WHEN 'CH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * JX -> ZC WHEN NEXT REVIEW DATE IS PAST
      * ZC -> ZY WHEN NEXT REVIEW DATE IS PAST
      * ZY -> JX WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2490-SET-SOURCE.
           EVALUATE PBSTAT-SOURCE-CD
               WHEN 'JX'
                   IF PBSTAT-NEXT-REVIEW-DATE > 0 AND
                       PBSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZC' TO PBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZC'
                   IF PBSTAT-NEXT-REVIEW-DATE > 0 AND
                       PBSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZY' TO PBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZY'
                   IF PBSTAT-NEXT-REVIEW-DATE > 0 AND
                       PBSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JX' TO PBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LINE AMT.
      *----------------------------------------------------------------*
       2500-PRORATE-LINE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBSTAT-LINE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBSTAT-LINE-AMT
           END-IF
           ADD PBSTAT-LINE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * LINE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2510-FLAG-LINE.
           IF PBSTAT-LINE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBSTAT-LINE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LINE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2520-ACCRUE-LINE.
           IF PBSTAT-LINE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBSTAT-LINE-AMT * PBSTAT-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBSTAT-UNPOSTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2530-DESCRIBE-REGION.
           EVALUATE PBSTAT-REGION-CD
               WHEN 'ER'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2540-COUNT-CYCLE.
           IF PBSTAT-CYCLE-CNT < 9999999
               ADD 1 TO PBSTAT-CYCLE-CNT
           END-IF
           IF PBSTAT-CYCLE-CNT NOT < 5
               MOVE 'Y' TO PBSTAT-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE UNPOSTED YTD FEE ON THE LINE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2550-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN PBSTAT-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO PBSTAT-UNPOSTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBSTAT-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PBSTAT-UNPOSTED-YTD ROUNDED = PBSTAT-LINE-AMT
                       * PBSTAT-PENALTY-PCT / 200
               WHEN PBSTAT-SOURCE-CD = 'JX'
                   MOVE 7.00 TO PBSTAT-UNPOSTED-YTD
               WHEN PBSTAT-SOURCE-CD = 'ZC'
                   MOVE 12.00 TO PBSTAT-UNPOSTED-YTD
               WHEN OTHER
                   COMPUTE PBSTAT-UNPOSTED-YTD ROUNDED = PBSTAT-LINE-AMT
                       * PBSTAT-PENALTY-PCT / 100
           END-EVALUATE
           IF PBSTAT-UNPOSTED-YTD > 0 AND PBSTAT-UNPOSTED-YTD < 5.00
               MOVE 5.00 TO PBSTAT-UNPOSTED-YTD
           END-IF
           IF PBSTAT-UNPOSTED-YTD > 100.00
               MOVE 100.00 TO PBSTAT-UNPOSTED-YTD
           END-IF
           ADD PBSTAT-UNPOSTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * JX -> ZC WHEN CLOSE DATE IS PAST
      * ZC -> ZY WHEN CLOSE DATE IS PAST
      * ZY -> JX WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2560-SET-SOURCE.
           EVALUATE PBSTAT-SOURCE-CD
               WHEN 'JX'
                   IF PBSTAT-CLOSE-DATE > 0 AND PBSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZC' TO PBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZC'
                   IF PBSTAT-CLOSE-DATE > 0 AND PBSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZY' TO PBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZY'
                   IF PBSTAT-CLOSE-DATE > 0 AND PBSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JX' TO PBSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PRINTER CD:
      * CB -> EP WHEN NEXT REVIEW DATE IS PAST
      * EP -> EX WHEN NEXT REVIEW DATE IS PAST
      * EX -> JE WHEN NEXT REVIEW DATE IS PAST
      * JE -> MR WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2570-SET-PRINTER.
           EVALUATE PBSTAT-PRINTER-CD
               WHEN 'CB'
                   IF PBSTAT-NEXT-REVIEW-DATE > 0 AND
                       PBSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'EP' TO PBSTAT-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EP'
                   IF PBSTAT-NEXT-REVIEW-DATE > 0 AND
                       PBSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'EX' TO PBSTAT-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF PBSTAT-NEXT-REVIEW-DATE > 0 AND
                       PBSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JE' TO PBSTAT-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF PBSTAT-NEXT-REVIEW-DATE > 0 AND
                       PBSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MR' TO PBSTAT-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2580-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PBSTAT-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBSTAT-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PBSTAT-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2590-COUNT-RETRY.
           IF PBSTAT-RETRY-CNT < 9999999
               ADD 1 TO PBSTAT-RETRY-CNT
           END-IF
           IF PBSTAT-RETRY-CNT NOT < 12
               MOVE 'Y' TO PBSTAT-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOOK TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2600-DESCRIBE-BOOK-TYPE.
           EVALUATE PBSTAT-BOOK-TYPE-CD
               WHEN 'CH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE UNPOSTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2610-PRORATE-UNPOSTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBSTAT-UNPOSTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO UNPOSTED YTD.
      *----------------------------------------------------------------*
       2620-PRORATE-UNPOSTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBSTAT-UNPOSTED-YTD ROUNDED =
                   PBSTAT-CARRYFWD-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBSTAT-UNPOSTED-YTD
           END-IF
           ADD PBSTAT-UNPOSTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * LINE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2630-FLAG-LINE.
           IF PBSTAT-LINE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBSTAT-LINE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LINE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CARRYFWD YTD.
      *----------------------------------------------------------------*
       2640-PRORATE-CARRYFWD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBSTAT-CARRYFWD-YTD ROUNDED =
                   PBSTAT-LINE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBSTAT-CARRYFWD-YTD
           END-IF
           ADD PBSTAT-CARRYFWD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2650-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PBSTAT-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBSTAT-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PBSTAT-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2660-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBSTAT-CLOSE-DATE > 0 AND PBSTAT-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBSTAT-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBSTAT-BALANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBSTAT-BALANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PBSTAT-BALANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PBSTAT-BALANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBSTAT-BALANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2670-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBSTAT-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBSTAT-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PBSTAT-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2680-COUNT-RETRY.
           IF PBSTAT-RETRY-CNT < 9999999
               ADD 1 TO PBSTAT-RETRY-CNT
           END-IF
           IF PBSTAT-RETRY-CNT NOT < 6
               MOVE 'Y' TO PBSTAT-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E704 CUST ID REQUIRED
      * E716 UNPOSTED AMT NEGATIVE
      * E419 UNPOSTED AMT OVER 25000
      * E102 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2690-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBSTAT-CUST-ID = SPACES
                   MOVE 'E704' TO WS-REJECT-CODE
               WHEN PBSTAT-UNPOSTED-AMT < 0
                   MOVE 'E716' TO WS-REJECT-CODE
               WHEN PBSTAT-UNPOSTED-AMT > 25000.00
                   MOVE 'E419' TO WS-REJECT-CODE
               WHEN PBSTAT-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E102' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * UNPOSTED AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2700-FLAG-UNPOSTED.
           IF PBSTAT-UNPOSTED-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBSTAT-UNPOSTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON UNPOSTED YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2710-ACCRUE-UNPOSTED.
           IF PBSTAT-UNPOSTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBSTAT-UNPOSTED-YTD * PBSTAT-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PBSTAT-LINE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PRINTER CD:
      * CB -> EP WHEN CLOSE DATE IS PAST
      * EP -> EX WHEN CLOSE DATE IS PAST
      * EX -> JE WHEN CLOSE DATE IS PAST
      * JE -> MR WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2720-SET-PRINTER.
           EVALUATE PBSTAT-PRINTER-CD
               WHEN 'CB'
                   IF PBSTAT-CLOSE-DATE > 0 AND PBSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EP' TO PBSTAT-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EP'
                   IF PBSTAT-CLOSE-DATE > 0 AND PBSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EX' TO PBSTAT-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF PBSTAT-CLOSE-DATE > 0 AND PBSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JE' TO PBSTAT-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF PBSTAT-CLOSE-DATE > 0 AND PBSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO PBSTAT-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CARRYFWD YTD FEE ON THE LINE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2730-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PBSTAT-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PBSTAT-CARRYFWD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBSTAT-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PBSTAT-CARRYFWD-YTD ROUNDED = PBSTAT-LINE-AMT
                       * PBSTAT-PENALTY-PCT / 200
               WHEN PBSTAT-BOOK-TYPE-CD = 'CH'
                   MOVE 7.00 TO PBSTAT-CARRYFWD-YTD
               WHEN PBSTAT-BOOK-TYPE-CD = 'HG'
                   MOVE 15.00 TO PBSTAT-CARRYFWD-YTD
               WHEN OTHER
                   COMPUTE PBSTAT-CARRYFWD-YTD ROUNDED = PBSTAT-LINE-AMT
                       * PBSTAT-PENALTY-PCT / 100
           END-EVALUATE
           IF PBSTAT-CARRYFWD-YTD > 0 AND PBSTAT-CARRYFWD-YTD < 5.00
               MOVE 5.00 TO PBSTAT-CARRYFWD-YTD
           END-IF
           IF PBSTAT-CARRYFWD-YTD > 250.00
               MOVE 250.00 TO PBSTAT-CARRYFWD-YTD
           END-IF
           ADD PBSTAT-CARRYFWD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE BOOK TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2740-DESCRIBE-BOOK-TYPE.
           EVALUATE PBSTAT-BOOK-TYPE-CD
               WHEN 'CH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CARRYFWD YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2750-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBSTAT-SOURCE-CD
                   ADD PBSTAT-CARRYFWD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBSTAT-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBSTAT-CARRYFWD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * LINE YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2760-LIMIT-LINE.
           IF PBSTAT-LINE-YTD < 10.00
               MOVE 10.00 TO PBSTAT-LINE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBSTAT-LINE-YTD > 5000.00
               MOVE 5000.00 TO PBSTAT-LINE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PRINTER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2770-DESCRIBE-PRINTER.
           EVALUATE PBSTAT-PRINTER-CD
               WHEN 'CB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'EP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'EX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * UNPOSTED AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2780-FLAG-UNPOSTED.
           IF PBSTAT-UNPOSTED-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBSTAT-UNPOSTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY PRINTER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2790-ACCUM-PRINTER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBSTAT-PRINTER-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBSTAT-PRINTER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CARRYFWD YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2800-SPLIT-CARRYFWD.
           COMPUTE WS-WORK-AMT = PBSTAT-CARRYFWD-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PBSTAT-CARRYFWD-YTD - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2810-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PBSTAT-NEXT-DUE-DATE > 0 AND PBSTAT-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBSTAT-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBSTAT-UNPOSTED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBSTAT-UNPOSTED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PBSTAT-UNPOSTED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBSTAT-UNPOSTED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBSTAT-UNPOSTED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E325 CONTACT REQUIRED
      * E476 UNPOSTED AMT NEGATIVE
      * E823 UNPOSTED AMT OVER 250000
      * E135 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2820-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBSTAT-CONTACT = SPACES
                   MOVE 'E325' TO WS-REJECT-CODE
               WHEN PBSTAT-UNPOSTED-AMT < 0
                   MOVE 'E476' TO WS-REJECT-CODE
               WHEN PBSTAT-UNPOSTED-AMT > 250000.00
                   MOVE 'E823' TO WS-REJECT-CODE
               WHEN PBSTAT-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E135' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E931 CONTACT REQUIRED
      * E772 CARRYFWD YTD NEGATIVE
      * E113 CARRYFWD YTD OVER 10000
      * E835 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       2830-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBSTAT-CONTACT = SPACES
                   MOVE 'E931' TO WS-REJECT-CODE
               WHEN PBSTAT-CARRYFWD-YTD < 0
                   MOVE 'E772' TO WS-REJECT-CODE
               WHEN PBSTAT-CARRYFWD-YTD > 10000.00
                   MOVE 'E113' TO WS-REJECT-CODE
               WHEN PBSTAT-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E835' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * ER -> GL WHEN NEXT REVIEW DATE IS PAST
      * GL -> PE WHEN NEXT REVIEW DATE IS PAST
      * PE -> RJ WHEN NEXT REVIEW DATE IS PAST
      * RJ -> VB WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2840-SET-REGION.
           EVALUATE PBSTAT-REGION-CD
               WHEN 'ER'
                   IF PBSTAT-NEXT-REVIEW-DATE > 0 AND
                       PBSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GL' TO PBSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF PBSTAT-NEXT-REVIEW-DATE > 0 AND
                       PBSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PE' TO PBSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PE'
                   IF PBSTAT-NEXT-REVIEW-DATE > 0 AND
                       PBSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'RJ' TO PBSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF PBSTAT-NEXT-REVIEW-DATE > 0 AND
                       PBSTAT-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VB' TO PBSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * ER -> GL WHEN CLOSE DATE IS PAST
      * GL -> PE WHEN CLOSE DATE IS PAST
      * PE -> RJ WHEN CLOSE DATE IS PAST
      * RJ -> VB WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2850-SET-REGION.
           EVALUATE PBSTAT-REGION-CD
               WHEN 'ER'
                   IF PBSTAT-CLOSE-DATE > 0 AND PBSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO PBSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF PBSTAT-CLOSE-DATE > 0 AND PBSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PE' TO PBSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PE'
                   IF PBSTAT-CLOSE-DATE > 0 AND PBSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO PBSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF PBSTAT-CLOSE-DATE > 0 AND PBSTAT-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VB' TO PBSTAT-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST GRID TIER FOR THE UNPOSTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2860-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-GRID-RATE(1) TO PBSTAT-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PBSTAT-UNPOSTED-AMT NOT < WS-INTEREST-GRID-LOW(WS-TX)
                   MOVE WS-INTEREST-GRID-RATE(WS-TX) TO PBSTAT-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBSTAT-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBSTAT-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       2870-LIMIT-BALANCE.
           IF PBSTAT-BALANCE-AMT < 10.00
               MOVE 10.00 TO PBSTAT-BALANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBSTAT-BALANCE-AMT > 5000.00
               MOVE 5000.00 TO PBSTAT-BALANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO PBSTAT-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO PBSTAT-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO PBSTAT-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'PBSTAT EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
