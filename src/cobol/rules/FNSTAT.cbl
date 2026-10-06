      *================================================================*
      * PROGRAM  : FNSTAT                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : FOREIGN BANKNOTES RULE - ELIGIBILITY. CALLED BY THE *
      *            FOREIGN BANKNOTES BATCH AND ONLINE PROGRAMS.        *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-02-23 PQR  ORIGINAL                                       *
      * 2000-09-16 SKN  BUYBACK EXCEPTIONS TO OPS QUEUE INC0196        *
      * 2000-09-13 BHS  AUDIT FINDING - SHIPPING CONTROL TOTALS PRB9590*
      * 2003-04-08 PQR  NEW MARGIN REPORT COLUMNS CHG5641              *
      * 2003-11-28 GAF  NEW SHIPPING REPORT COLUMNS PRB1544            *
      * 2004-12-10 MKL  BUYBACK FEE SCHEDULE UPDATE CHG1780            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FNSTAT.
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
                                           VALUE 15.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 28.
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
      * WS-MARGIN-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MARGIN-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000258500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000332500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000354000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000850000000430500'.
       01  WS-MARGIN-GRID-TBL REDEFINES WS-MARGIN-GRID-VALUES.
           05  WS-MARGIN-GRID-ROW OCCURS 4 TIMES.
               10  WS-MARGIN-GRID-LOW      PIC 9(09)V99.
               10  WS-MARGIN-GRID-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-BUYBACK-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BUYBACK-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000689000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000427500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000240000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000800000000552500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005800000000353000'.
       01  WS-BUYBACK-SCHED-TBL REDEFINES WS-BUYBACK-SCHED-VALUES.
           05  WS-BUYBACK-SCHED-ROW OCCURS 5 TIMES.
               10  WS-BUYBACK-SCHED-LOW    PIC 9(09)V99.
               10  WS-BUYBACK-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-MARGIN-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MARGIN-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000486000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000892500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000025500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007550000000053500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007650000000582000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012650000000295000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012750000000757000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022750000000427500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023750000000361000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026250000000450500'.
       01  WS-MARGIN-SCHED-TBL REDEFINES WS-MARGIN-SCHED-VALUES.
           05  WS-MARGIN-SCHED-ROW OCCURS 10 TIMES.
               10  WS-MARGIN-SCHED-LOW     PIC 9(09)V99.
               10  WS-MARGIN-SCHED-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY FNSTATP.

       PROCEDURE DIVISION USING FNSTAT-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-COUNT-NOTICE
           PERFORM 2010-SET-STATUS
           PERFORM 2020-SET-SOURCE
           PERFORM 2030-AGE-EXPIRY
           PERFORM 2040-ASSESS-PREFERRED
           PERFORM 2050-LIMIT-FOREIGN
           PERFORM 2060-DESCRIBE-ORDER-TYPE
           PERFORM 2070-AGE-OPEN
           PERFORM 2080-SELECT-RATE
           PERFORM 2090-SELECT-PENALTY-PCT
           PERFORM 2100-PRORATE-MARGIN
           PERFORM 2110-ASSESS-REVIEW
           PERFORM 2120-CHECK-LAST-ACT
           PERFORM 2130-LIMIT-ITEM
           PERFORM 2140-DESCRIBE-TYPE
           PERFORM 2150-DESCRIBE-TYPE
           PERFORM 2160-COUNT-PAYMENT
           PERFORM 2170-AGE-MATURITY
           PERFORM 2180-ACCUM-ORDER-TYPE
           PERFORM 2190-CHECK-MATURITY
           PERFORM 2200-SELECT-RATE
           PERFORM 2210-CHECK-LAST-ACT
           PERFORM 2220-DESCRIBE-ORDER-TYPE
           PERFORM 2230-LIMIT-BUYBACK
           PERFORM 2240-SET-SOURCE
           PERFORM 2250-AGE-LAST-ACT
           PERFORM 2260-ASSESS-REVIEW
           PERFORM 2270-ACCUM-STATUS
           PERFORM 2280-ACCRUE-PENALTY-PCT
           PERFORM 2290-EDIT-CONTACT
           PERFORM 2300-PRORATE-BUYBACK
           PERFORM 2310-LIMIT-SHIPPING
           PERFORM 2320-LIMIT-ITEM
           PERFORM 2330-COUNT-NOTICE
           PERFORM 2340-CHECK-EXPIRY
           PERFORM 2350-SET-ORDER-TYPE
           PERFORM 2360-FLAG-USD
           PERFORM 2370-COUNT-NOTICE
           PERFORM 2380-FLAG-ITEM
           PERFORM 2390-SELECT-PENALTY-PCT
           PERFORM 2400-COUNT-PAYMENT
           PERFORM 2410-SPLIT-USD
           PERFORM 2420-SPLIT-USD
           PERFORM 2430-AGE-EXPIRY
           PERFORM 2440-FLAG-MARGIN
           PERFORM 2450-PRORATE-BUYBACK
           PERFORM 2460-LIMIT-FOREIGN
           PERFORM 2470-ASSESS-OVERRIDE
           PERFORM 2480-EDIT-ADDR
           PERFORM 2490-ACCUM-SOURCE
           PERFORM 2500-AGE-OPEN
           PERFORM 2510-DESCRIBE-TYPE
           PERFORM 2520-FLAG-MARGIN
           PERFORM 2530-LIMIT-FOREIGN
           PERFORM 2540-CHECK-OPEN
           PERFORM 2550-FLAG-MARGIN
           PERFORM 2560-FLAG-MARGIN
           PERFORM 2570-COUNT-NOTICE
           PERFORM 2580-FLAG-BUYBACK
           PERFORM 2590-ACCRUE-USD
           PERFORM 2600-SET-TYPE
           PERFORM 2610-SPLIT-USD
           PERFORM 2620-ACCRUE-BUYBACK
           PERFORM 2630-AGE-OPEN
           PERFORM 2640-EDIT-CONTACT
           PERFORM 2650-SPLIT-FOREIGN
           PERFORM 2660-DESCRIBE-STATUS
           PERFORM 2670-LIMIT-USD
           PERFORM 2680-COUNT-NOTICE
           PERFORM 2690-CHECK-EXPIRY
           PERFORM 2700-FLAG-BUYBACK
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO FNSTAT-RETURN-CODE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2000-COUNT-NOTICE.
           IF FNSTAT-NOTICE-CNT < 9999999
               ADD 1 TO FNSTAT-NOTICE-CNT
           END-IF
           IF FNSTAT-NOTICE-CNT NOT < 12
               MOVE 'Y' TO FNSTAT-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CXH -> ESS WHEN EXPIRY DATE IS PAST
      * ESS -> XDE WHEN EXPIRY DATE IS PAST
      * XDE -> XFD WHEN EXPIRY DATE IS PAST
      * XFD -> CXH WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2010-SET-STATUS.
           EVALUATE FNSTAT-STATUS-CD
               WHEN 'CXH'
                   IF FNSTAT-EXPIRY-DATE > 0 AND FNSTAT-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ESS' TO FNSTAT-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ESS'
                   IF FNSTAT-EXPIRY-DATE > 0 AND FNSTAT-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XDE' TO FNSTAT-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XDE'
                   IF FNSTAT-EXPIRY-DATE > 0 AND FNSTAT-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XFD' TO FNSTAT-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XFD'
                   IF FNSTAT-EXPIRY-DATE > 0 AND FNSTAT-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CXH' TO FNSTAT-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN EXPIRY DATE IS PAST
      * FS -> HL WHEN EXPIRY DATE IS PAST
      * HL -> HS WHEN EXPIRY DATE IS PAST
      * HS -> NW WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2020-SET-SOURCE.
           EVALUATE FNSTAT-SOURCE-CD
               WHEN 'DG'
                   IF FNSTAT-EXPIRY-DATE > 0 AND FNSTAT-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FS' TO FNSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FS'
                   IF FNSTAT-EXPIRY-DATE > 0 AND FNSTAT-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO FNSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF FNSTAT-EXPIRY-DATE > 0 AND FNSTAT-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HS' TO FNSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HS'
                   IF FNSTAT-EXPIRY-DATE > 0 AND FNSTAT-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NW' TO FNSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2030-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF FNSTAT-EXPIRY-DATE > 0 AND FNSTAT-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNSTAT-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPPING YTD FEE ON THE MARGIN AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2040-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FNSTAT-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO FNSTAT-SHIPPING-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNSTAT-OVERRIDE-FLAG = 'Y'
                   COMPUTE FNSTAT-SHIPPING-YTD ROUNDED =
                       FNSTAT-MARGIN-AMT * FNSTAT-PENALTY-PCT / 200
               WHEN FNSTAT-STATUS-CD = 'CXH'
                   MOVE 12.00 TO FNSTAT-SHIPPING-YTD
               WHEN FNSTAT-STATUS-CD = 'ESS'
                   MOVE 5.00 TO FNSTAT-SHIPPING-YTD
               WHEN OTHER
                   COMPUTE FNSTAT-SHIPPING-YTD ROUNDED =
                       FNSTAT-MARGIN-AMT * FNSTAT-PENALTY-PCT / 100
           END-EVALUATE
           IF FNSTAT-SHIPPING-YTD > 0 AND FNSTAT-SHIPPING-YTD < 5.00
               MOVE 5.00 TO FNSTAT-SHIPPING-YTD
           END-IF
           IF FNSTAT-SHIPPING-YTD > 35.00
               MOVE 35.00 TO FNSTAT-SHIPPING-YTD
           END-IF
           ADD FNSTAT-SHIPPING-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2050-LIMIT-FOREIGN.
           IF FNSTAT-FOREIGN-AMT < 100.00
               MOVE 100.00 TO FNSTAT-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNSTAT-FOREIGN-AMT > 10000.00
               MOVE 10000.00 TO FNSTAT-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ORDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2060-DESCRIBE-ORDER-TYPE.
           EVALUATE FNSTAT-ORDER-TYPE-CD
               WHEN 'DTU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KZV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PHV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PZZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WMC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XWF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2070-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FNSTAT-OPEN-DATE > 0 AND FNSTAT-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNSTAT-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNSTAT-SHIPPING-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FNSTAT-SHIPPING-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD FNSTAT-SHIPPING-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FNSTAT-SHIPPING-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNSTAT-SHIPPING-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2080-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-GRID-RATE(1) TO FNSTAT-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-MARGIN-GRID-LOW(WS-TX)
                   MOVE WS-MARGIN-GRID-RATE(WS-TX) TO FNSTAT-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNSTAT-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNSTAT-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN SCHED TIER FOR THE USD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2090-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-SCHED-RATE(1) TO FNSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF FNSTAT-USD-YTD NOT < WS-MARGIN-SCHED-LOW(WS-TX)
                   MOVE WS-MARGIN-SCHED-RATE(WS-TX) TO
                       FNSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MARGIN AMT.
      *----------------------------------------------------------------*
       2100-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNSTAT-MARGIN-AMT ROUNDED =
                   FNSTAT-FOREIGN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FNSTAT-MARGIN-AMT
           END-IF
           ADD FNSTAT-MARGIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK FEE ON THE WORK AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       2110-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN FNSTAT-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO FNSTAT-BUYBACK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNSTAT-PREFERRED-FLAG = 'Y'
                   COMPUTE FNSTAT-BUYBACK-AMT ROUNDED = WS-WORK-AMT *
                       FNSTAT-RATE / 200
               WHEN FNSTAT-SOURCE-CD = 'DG'
                   MOVE 7.00 TO FNSTAT-BUYBACK-AMT
               WHEN FNSTAT-SOURCE-CD = 'FS'
                   MOVE 5.00 TO FNSTAT-BUYBACK-AMT
               WHEN OTHER
                   COMPUTE FNSTAT-BUYBACK-AMT ROUNDED = WS-WORK-AMT *
                       FNSTAT-RATE / 100
           END-EVALUATE
           IF FNSTAT-BUYBACK-AMT > 0 AND FNSTAT-BUYBACK-AMT < 1.00
               MOVE 1.00 TO FNSTAT-BUYBACK-AMT
           END-IF
           IF FNSTAT-BUYBACK-AMT > 100.00
               MOVE 100.00 TO FNSTAT-BUYBACK-AMT
           END-IF
           ADD FNSTAT-BUYBACK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2120-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF FNSTAT-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNSTAT-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO FNSTAT-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2130-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2140-DESCRIBE-TYPE.
           EVALUATE FNSTAT-TYPE-CD
               WHEN 'CFP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2150-DESCRIBE-TYPE.
           EVALUATE FNSTAT-TYPE-CD
               WHEN 'CFP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2160-COUNT-PAYMENT.
           IF FNSTAT-PAYMENT-CNT < 9999999
               ADD 1 TO FNSTAT-PAYMENT-CNT
           END-IF
           IF FNSTAT-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO FNSTAT-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2170-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF FNSTAT-MATURITY-DATE > 0 AND FNSTAT-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNSTAT-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY ORDER TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2180-ACCUM-ORDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNSTAT-ORDER-TYPE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNSTAT-ORDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2190-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF FNSTAT-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNSTAT-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO FNSTAT-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2200-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-SCHED-RATE(1) TO FNSTAT-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-BUYBACK-SCHED-LOW(WS-TX)
                   MOVE WS-BUYBACK-SCHED-RATE(WS-TX) TO FNSTAT-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNSTAT-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNSTAT-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2210-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF FNSTAT-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNSTAT-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO FNSTAT-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ORDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2220-DESCRIBE-ORDER-TYPE.
           EVALUATE FNSTAT-ORDER-TYPE-CD
               WHEN 'DTU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KZV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PHV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PZZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WMC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XWF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BUYBACK AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2230-LIMIT-BUYBACK.
           IF FNSTAT-BUYBACK-AMT < 10.00
               MOVE 10.00 TO FNSTAT-BUYBACK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNSTAT-BUYBACK-AMT > 1000.00
               MOVE 1000.00 TO FNSTAT-BUYBACK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN MATURITY DATE IS PAST
      * FS -> HL WHEN MATURITY DATE IS PAST
      * HL -> HS WHEN MATURITY DATE IS PAST
      * HS -> NW WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2240-SET-SOURCE.
           EVALUATE FNSTAT-SOURCE-CD
               WHEN 'DG'
                   IF FNSTAT-MATURITY-DATE > 0 AND FNSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FS' TO FNSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FS'
                   IF FNSTAT-MATURITY-DATE > 0 AND FNSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HL' TO FNSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF FNSTAT-MATURITY-DATE > 0 AND FNSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HS' TO FNSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HS'
                   IF FNSTAT-MATURITY-DATE > 0 AND FNSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'NW' TO FNSTAT-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2250-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF FNSTAT-LAST-ACT-DATE > 0 AND FNSTAT-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNSTAT-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNSTAT-MARGIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FNSTAT-MARGIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FNSTAT-MARGIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FNSTAT-MARGIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNSTAT-MARGIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2260-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN FNSTAT-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO FNSTAT-BUYBACK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNSTAT-PREFERRED-FLAG = 'Y'
                   COMPUTE FNSTAT-BUYBACK-AMT ROUNDED = WS-WORK-AMT-2 *
                       FNSTAT-RATE / 200
               WHEN FNSTAT-ORDER-TYPE-CD = 'DTU'
                   MOVE 20.00 TO FNSTAT-BUYBACK-AMT
               WHEN FNSTAT-ORDER-TYPE-CD = 'KZV'
                   MOVE 5.00 TO FNSTAT-BUYBACK-AMT
               WHEN OTHER
                   COMPUTE FNSTAT-BUYBACK-AMT ROUNDED = WS-WORK-AMT-2 *
                       FNSTAT-RATE / 100
           END-EVALUATE
           IF FNSTAT-BUYBACK-AMT > 0 AND FNSTAT-BUYBACK-AMT < 1.00
               MOVE 1.00 TO FNSTAT-BUYBACK-AMT
           END-IF
           IF FNSTAT-BUYBACK-AMT > 25.00
               MOVE 25.00 TO FNSTAT-BUYBACK-AMT
           END-IF
           ADD FNSTAT-BUYBACK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE USD YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2270-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNSTAT-STATUS-CD
                   ADD FNSTAT-USD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNSTAT-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FNSTAT-USD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2280-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * FNSTAT-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO FNSTAT-SHIPPING-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E366 CONTACT REQUIRED
      * E597 WORK AMT 2 NEGATIVE
      * E360 WORK AMT 2 OVER 25000
      * E326 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2290-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNSTAT-CONTACT = SPACES
                   MOVE 'E366' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E597' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E360' TO WS-REJECT-CODE
               WHEN FNSTAT-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E326' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUYBACK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2300-PRORATE-BUYBACK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   FNSTAT-BUYBACK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SHIPPING YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2310-LIMIT-SHIPPING.
           IF FNSTAT-SHIPPING-YTD < 10.00
               MOVE 10.00 TO FNSTAT-SHIPPING-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNSTAT-SHIPPING-YTD > 50000.00
               MOVE 50000.00 TO FNSTAT-SHIPPING-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2320-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       2330-COUNT-NOTICE.
           IF FNSTAT-NOTICE-CNT < 9999999
               ADD 1 TO FNSTAT-NOTICE-CNT
           END-IF
           IF FNSTAT-NOTICE-CNT NOT < 12
               MOVE 'Y' TO FNSTAT-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2340-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF FNSTAT-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNSTAT-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO FNSTAT-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ORDER TYPE CD:
      * DTU -> KZV WHEN MATURITY DATE IS PAST
      * KZV -> PHV WHEN MATURITY DATE IS PAST
      * PHV -> PZZ WHEN MATURITY DATE IS PAST
      * PZZ -> WMC WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2350-SET-ORDER-TYPE.
           EVALUATE FNSTAT-ORDER-TYPE-CD
               WHEN 'DTU'
                   IF FNSTAT-MATURITY-DATE > 0 AND FNSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KZV' TO FNSTAT-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KZV'
                   IF FNSTAT-MATURITY-DATE > 0 AND FNSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PHV' TO FNSTAT-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PHV'
                   IF FNSTAT-MATURITY-DATE > 0 AND FNSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PZZ' TO FNSTAT-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PZZ'
                   IF FNSTAT-MATURITY-DATE > 0 AND FNSTAT-MATURITY-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WMC' TO FNSTAT-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * USD AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2360-FLAG-USD.
           IF FNSTAT-USD-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FNSTAT-USD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2370-COUNT-NOTICE.
           IF FNSTAT-NOTICE-CNT < 9999999
               ADD 1 TO FNSTAT-NOTICE-CNT
           END-IF
           IF FNSTAT-NOTICE-CNT NOT < 10
               MOVE 'Y' TO FNSTAT-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2380-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN GRID TIER FOR THE BUYBACK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2390-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-GRID-RATE(1) TO FNSTAT-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF FNSTAT-BUYBACK-AMT NOT < WS-MARGIN-GRID-LOW(WS-TX)
                   MOVE WS-MARGIN-GRID-RATE(WS-TX) TO FNSTAT-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FNSTAT-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FNSTAT-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2400-COUNT-PAYMENT.
           IF FNSTAT-PAYMENT-CNT < 9999999
               ADD 1 TO FNSTAT-PAYMENT-CNT
           END-IF
           IF FNSTAT-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO FNSTAT-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2410-SPLIT-USD.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE FNSTAT-USD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2420-SPLIT-USD.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE FNSTAT-USD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2430-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF FNSTAT-EXPIRY-DATE > 0 AND FNSTAT-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNSTAT-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNSTAT-SHIPPING-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FNSTAT-SHIPPING-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FNSTAT-SHIPPING-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FNSTAT-SHIPPING-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNSTAT-SHIPPING-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MARGIN AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2440-FLAG-MARGIN.
           IF FNSTAT-MARGIN-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FNSTAT-MARGIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE USD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BUYBACK AMT.
      *----------------------------------------------------------------*
       2450-PRORATE-BUYBACK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FNSTAT-BUYBACK-AMT ROUNDED =
                   FNSTAT-USD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FNSTAT-BUYBACK-AMT
           END-IF
           ADD FNSTAT-BUYBACK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2460-LIMIT-FOREIGN.
           IF FNSTAT-FOREIGN-AMT < 100.00
               MOVE 100.00 TO FNSTAT-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNSTAT-FOREIGN-AMT > 50000.00
               MOVE 50000.00 TO FNSTAT-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK FEE ON THE SHIPPING YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2470-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN FNSTAT-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO FNSTAT-BUYBACK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FNSTAT-PREFERRED-FLAG = 'Y'
                   COMPUTE FNSTAT-BUYBACK-AMT ROUNDED =
                       FNSTAT-SHIPPING-YTD * FNSTAT-RATE / 200
               WHEN FNSTAT-STATUS-CD = 'CXH'
                   MOVE 20.00 TO FNSTAT-BUYBACK-AMT
               WHEN FNSTAT-STATUS-CD = 'ESS'
                   MOVE 12.00 TO FNSTAT-BUYBACK-AMT
               WHEN OTHER
                   COMPUTE FNSTAT-BUYBACK-AMT ROUNDED =
                       FNSTAT-SHIPPING-YTD * FNSTAT-RATE / 100
           END-EVALUATE
           IF FNSTAT-BUYBACK-AMT > 0 AND FNSTAT-BUYBACK-AMT < 2.00
               MOVE 2.00 TO FNSTAT-BUYBACK-AMT
           END-IF
           IF FNSTAT-BUYBACK-AMT > 35.00
               MOVE 35.00 TO FNSTAT-BUYBACK-AMT
           END-IF
           ADD FNSTAT-BUYBACK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E022 ADDR REQUIRED
      * E561 MARGIN AMT NEGATIVE
      * E207 MARGIN AMT OVER 25000
      * E413 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2480-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNSTAT-ADDR = SPACES
                   MOVE 'E022' TO WS-REJECT-CODE
               WHEN FNSTAT-MARGIN-AMT < 0
                   MOVE 'E561' TO WS-REJECT-CODE
               WHEN FNSTAT-MARGIN-AMT > 25000.00
                   MOVE 'E207' TO WS-REJECT-CODE
               WHEN FNSTAT-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E413' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE USD YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2490-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FNSTAT-SOURCE-CD
                   ADD FNSTAT-USD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FNSTAT-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FNSTAT-USD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2500-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FNSTAT-OPEN-DATE > 0 AND FNSTAT-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNSTAT-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNSTAT-BUYBACK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD FNSTAT-BUYBACK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD FNSTAT-BUYBACK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FNSTAT-BUYBACK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNSTAT-BUYBACK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2510-DESCRIBE-TYPE.
           EVALUATE FNSTAT-TYPE-CD
               WHEN 'CFP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MARGIN AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2520-FLAG-MARGIN.
           IF FNSTAT-MARGIN-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FNSTAT-MARGIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2530-LIMIT-FOREIGN.
           IF FNSTAT-FOREIGN-AMT < 10.00
               MOVE 10.00 TO FNSTAT-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNSTAT-FOREIGN-AMT > 2500.00
               MOVE 2500.00 TO FNSTAT-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2540-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FNSTAT-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNSTAT-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO FNSTAT-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2550-FLAG-MARGIN.
           IF FNSTAT-MARGIN-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FNSTAT-MARGIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2560-FLAG-MARGIN.
           IF FNSTAT-MARGIN-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FNSTAT-MARGIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2570-COUNT-NOTICE.
           IF FNSTAT-NOTICE-CNT < 9999999
               ADD 1 TO FNSTAT-NOTICE-CNT
           END-IF
           IF FNSTAT-NOTICE-CNT NOT < 6
               MOVE 'Y' TO FNSTAT-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2580-FLAG-BUYBACK.
           IF FNSTAT-BUYBACK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FNSTAT-BUYBACK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON USD YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2590-ACCRUE-USD.
           IF FNSTAT-USD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNSTAT-USD-YTD * FNSTAT-RATE / 100 / 365
               ADD WS-WORK-AMT TO FNSTAT-USD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CFP -> FFU WHEN LAST ACT DATE IS PAST
      * FFU -> JNR WHEN LAST ACT DATE IS PAST
      * JNR -> UGS WHEN LAST ACT DATE IS PAST
      * UGS -> XUM WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2600-SET-TYPE.
           EVALUATE FNSTAT-TYPE-CD
               WHEN 'CFP'
                   IF FNSTAT-LAST-ACT-DATE > 0 AND FNSTAT-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FFU' TO FNSTAT-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FFU'
                   IF FNSTAT-LAST-ACT-DATE > 0 AND FNSTAT-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JNR' TO FNSTAT-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JNR'
                   IF FNSTAT-LAST-ACT-DATE > 0 AND FNSTAT-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UGS' TO FNSTAT-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UGS'
                   IF FNSTAT-LAST-ACT-DATE > 0 AND FNSTAT-LAST-ACT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XUM' TO FNSTAT-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT USD YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2610-SPLIT-USD.
           COMPUTE WS-WORK-AMT = FNSTAT-USD-YTD / 12
           COMPUTE WS-WORK-AMT-2 = FNSTAT-USD-YTD - (WS-WORK-AMT * 12)
           COMPUTE FNSTAT-MARGIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUYBACK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2620-ACCRUE-BUYBACK.
           IF FNSTAT-BUYBACK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FNSTAT-BUYBACK-AMT * FNSTAT-RATE / 100 / 365
               ADD WS-WORK-AMT TO FNSTAT-USD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2630-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FNSTAT-OPEN-DATE > 0 AND FNSTAT-OPEN-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNSTAT-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FNSTAT-SHIPPING-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FNSTAT-SHIPPING-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD FNSTAT-SHIPPING-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD FNSTAT-SHIPPING-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FNSTAT-SHIPPING-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E357 CONTACT REQUIRED
      * E548 WORK AMT NEGATIVE
      * E594 WORK AMT OVER 250000
      * E849 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       2640-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FNSTAT-CONTACT = SPACES
                   MOVE 'E357' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E548' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E594' TO WS-REJECT-CODE
               WHEN FNSTAT-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E849' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FOREIGN AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2650-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = FNSTAT-FOREIGN-AMT / 2
           COMPUTE WS-WORK-AMT-2 = FNSTAT-FOREIGN-AMT - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2660-DESCRIBE-STATUS.
           EVALUATE FNSTAT-STATUS-CD
               WHEN 'CXH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ESS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XDE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XFD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * USD AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2670-LIMIT-USD.
           IF FNSTAT-USD-AMT < 50.00
               MOVE 50.00 TO FNSTAT-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FNSTAT-USD-AMT > 10000.00
               MOVE 10000.00 TO FNSTAT-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2680-COUNT-NOTICE.
           IF FNSTAT-NOTICE-CNT < 9999999
               ADD 1 TO FNSTAT-NOTICE-CNT
           END-IF
           IF FNSTAT-NOTICE-CNT NOT < 5
               MOVE 'Y' TO FNSTAT-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2690-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF FNSTAT-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FNSTAT-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO FNSTAT-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2700-FLAG-BUYBACK.
           IF FNSTAT-BUYBACK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FNSTAT-BUYBACK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO FNSTAT-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO FNSTAT-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO FNSTAT-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'FNSTAT EXCEPTION ' WS-EXC-SEVERITY ' '
               WS-EXC-AMOUNT.
