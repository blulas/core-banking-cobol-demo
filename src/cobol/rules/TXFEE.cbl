      *================================================================*
      * PROGRAM  : TXFEE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : TELEX WIRE MESSAGING RULE - ELIGIBILITY. CALLED BY  *
      *            THE TELEX WIRE MESSAGING BATCH AND ONLINE PROGRAMS. *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-10-22 GAF  ORIGINAL                                       *
      * 1997-12-23 DPS  SETTLE TABLE EXPANDED INC4515                  *
      * 1998-08-19 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-07-05 TWH  AUDIT FINDING - SETTLE CONTROL TOTALS REQ0245  *
      * 2003-12-23 BHS  CORRESP NOTICE TEXT PER LEGAL REQ1685          *
      * 2003-02-03 SKN  CORRESP TABLE EXPANDED INC4723                 *
      * 2004-04-23 TWH  QUEUED ROUNDING - HALF UP REQ0784              *
      * 2006-08-24 CRP  COVER FEE SCHEDULE UPDATE INC7909              *
      * 2007-03-10 BHS  SETTLE LIMIT REVISED INC5620                   *
      * 2008-06-02 BHS  CHARGE TABLE EXPANDED PRB7034                  *
      * 2009-03-02 RJT  AUDIT FINDING - CHARGE CONTROL TOTALS PRB6886  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXFEE.
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
                                           VALUE 20.000000.
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
      * WS-CORRESP-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CORRESP-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000489500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000624000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000054500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006050000000827500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008550000000633000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008800000000147500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011300000000898500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016300000000263000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021300000000329500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026300000000263500'.
       01  WS-CORRESP-GRID-TBL REDEFINES WS-CORRESP-GRID-VALUES.
           05  WS-CORRESP-GRID-ROW OCCURS 10 TIMES.
               10  WS-CORRESP-GRID-LOW     PIC 9(09)V99.
               10  WS-CORRESP-GRID-RATE    PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY TXFEEP.

       PROCEDURE DIVISION USING TXFEE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-COUNT-EXCEPT
           PERFORM 2010-LIMIT-SETTLE
           PERFORM 2020-SELECT-RATE
           PERFORM 2030-ACCRUE-SETTLE
           PERFORM 2040-SELECT-RATE
           PERFORM 2050-DESCRIBE-TYPE
           PERFORM 2060-COUNT-ITEM
           PERFORM 2070-ACCUM-TEST-KEY
           PERFORM 2080-LIMIT-QUEUED
           PERFORM 2090-SELECT-RATE
           PERFORM 2100-SELECT-FEE-PCT
           PERFORM 2110-FLAG-WIRE
           PERFORM 2120-FLAG-COVER
           PERFORM 2130-ASSESS-SUSPEND
           PERFORM 2140-FLAG-CORRESP
           PERFORM 2150-SELECT-RATE
           PERFORM 2160-DESCRIBE-BRANCH
           PERFORM 2170-ACCRUE-SETTLE
           PERFORM 2180-COUNT-ITEM
           PERFORM 2190-ACCUM-TEST-KEY
           PERFORM 2200-LIMIT-CORRESP
           PERFORM 2210-PRORATE-COVER
           PERFORM 2220-SET-BRANCH
           PERFORM 2230-ACCUM-TYPE
           PERFORM 2240-SET-BRANCH
           PERFORM 2250-ASSESS-PREFERRED
           PERFORM 2260-FLAG-COVER
           PERFORM 2270-SET-BRANCH
           PERFORM 2280-ACCUM-TYPE
           PERFORM 2290-DESCRIBE-TEST-KEY
           PERFORM 2300-ASSESS-NOTICE-SENT
           PERFORM 2310-ACCRUE-SETTLE
           PERFORM 2320-SELECT-FEE-PCT
           PERFORM 2330-SELECT-RATE
           PERFORM 2340-AGE-NEXT-DUE
           PERFORM 2350-ASSESS-NOTICE-SENT
           PERFORM 2360-AGE-CLOSE
           PERFORM 2370-AGE-NEXT-DUE
           PERFORM 2380-ACCUM-TEST-KEY
           PERFORM 2390-ACCUM-TYPE
           PERFORM 2400-FLAG-QUEUED
           PERFORM 2410-SELECT-RATE
           PERFORM 2420-ASSESS-SUSPEND
           PERFORM 2430-ACCUM-REGION
           PERFORM 2440-EDIT-CUST-ID
           PERFORM 2450-PRORATE-WIRE
           PERFORM 2460-PRORATE-COVER
           PERFORM 2470-ACCUM-TYPE
           PERFORM 2480-LIMIT-SETTLE
           PERFORM 2490-FLAG-COVER
           PERFORM 2500-LIMIT-ITEM
           PERFORM 2510-EDIT-CONTACT
           PERFORM 2520-SET-TEST-KEY
           PERFORM 2530-PRORATE-WIRE
           PERFORM 2540-PRORATE-QUEUED
           PERFORM 2550-ACCRUE-RATE
           PERFORM 2560-LIMIT-QUEUED
           PERFORM 2570-ASSESS-NOTICE-SENT
           PERFORM 2580-ACCUM-BRANCH
           PERFORM 2590-LIMIT-WIRE
           PERFORM 2600-COUNT-ITEM
           PERFORM 2610-ACCRUE-WIRE
           PERFORM 2620-COUNT-ITEM
           PERFORM 2630-PRORATE-CORRESP
           PERFORM 2640-AGE-CLOSE
           PERFORM 2650-DESCRIBE-REGION
           PERFORM 2660-ACCUM-REGION
           PERFORM 2670-SPLIT-WIRE
           PERFORM 2680-SET-TEST-KEY
           PERFORM 2690-ACCUM-REGION
           PERFORM 2700-DESCRIBE-BRANCH
           PERFORM 2710-FLAG-SETTLE
           PERFORM 2720-AGE-NEXT-DUE
           PERFORM 2730-ACCRUE-SETTLE
           PERFORM 2740-SET-BRANCH
           PERFORM 2750-LIMIT-ITEM
           PERFORM 2760-SET-TYPE
           PERFORM 2770-SELECT-RATE
           PERFORM 2780-COUNT-EXCEPT
           PERFORM 2790-COUNT-ITEM
           PERFORM 2800-SET-TYPE
           PERFORM 2810-COUNT-EXCEPT
           PERFORM 2820-SET-BRANCH
           PERFORM 2830-SELECT-FEE-PCT
           PERFORM 2840-EDIT-CUST-ID
           PERFORM 2850-SPLIT-SETTLE
           PERFORM 2860-AGE-NEXT-DUE
           PERFORM 2870-SPLIT-COVER
           PERFORM 2880-AGE-CLOSE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO TXFEE-RETURN-CODE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2000-COUNT-EXCEPT.
           IF TXFEE-EXCEPT-CNT < 9999999
               ADD 1 TO TXFEE-EXCEPT-CNT
           END-IF
           IF TXFEE-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO TXFEE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2010-LIMIT-SETTLE.
           IF TXFEE-SETTLE-AMT < 50.00
               MOVE 50.00 TO TXFEE-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXFEE-SETTLE-AMT > 50000.00
               MOVE 50000.00 TO TXFEE-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2020-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-GRID-RATE(1) TO TXFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-CORRESP-GRID-LOW(WS-TX)
                   MOVE WS-CORRESP-GRID-RATE(WS-TX) TO TXFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2030-ACCRUE-SETTLE.
           IF TXFEE-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXFEE-SETTLE-AMT * TXFEE-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TXFEE-COVER-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP GRID TIER FOR THE QUEUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2040-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-GRID-RATE(1) TO TXFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TXFEE-QUEUED-YTD NOT < WS-CORRESP-GRID-LOW(WS-TX)
                   MOVE WS-CORRESP-GRID-RATE(WS-TX) TO TXFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2050-DESCRIBE-TYPE.
           EVALUATE TXFEE-TYPE-CD
               WHEN 'K'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'M'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'P'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2060-COUNT-ITEM.
           IF TXFEE-ITEM-CNT < 9999999
               ADD 1 TO TXFEE-ITEM-CNT
           END-IF
           IF TXFEE-ITEM-CNT NOT < 3
               MOVE 'Y' TO TXFEE-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CORRESP AMT BY TEST KEY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2070-ACCUM-TEST-KEY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXFEE-TEST-KEY-CD
                   ADD TXFEE-CORRESP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXFEE-TEST-KEY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXFEE-CORRESP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2080-LIMIT-QUEUED.
           IF TXFEE-QUEUED-YTD < 0.00
               MOVE 0.00 TO TXFEE-QUEUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXFEE-QUEUED-YTD > 10000.00
               MOVE 10000.00 TO TXFEE-QUEUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP GRID TIER FOR THE QUEUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2090-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-GRID-RATE(1) TO TXFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TXFEE-QUEUED-YTD NOT < WS-CORRESP-GRID-LOW(WS-TX)
                   MOVE WS-CORRESP-GRID-RATE(WS-TX) TO TXFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP GRID TIER FOR THE QUEUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2100-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-GRID-RATE(1) TO TXFEE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TXFEE-QUEUED-YTD NOT < WS-CORRESP-GRID-LOW(WS-TX)
                   MOVE WS-CORRESP-GRID-RATE(WS-TX) TO TXFEE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXFEE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXFEE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WIRE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2110-FLAG-WIRE.
           IF TXFEE-WIRE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TXFEE-WIRE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * COVER AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2120-FLAG-COVER.
           IF TXFEE-COVER-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TXFEE-COVER-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WIRE YTD FEE ON THE QUEUED YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2130-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TXFEE-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TXFEE-WIRE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXFEE-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TXFEE-WIRE-YTD ROUNDED = TXFEE-QUEUED-YTD *
                       TXFEE-FEE-PCT / 200
               WHEN TXFEE-TYPE-CD = 'K'
                   MOVE 20.00 TO TXFEE-WIRE-YTD
               WHEN TXFEE-TYPE-CD = 'L'
                   MOVE 20.00 TO TXFEE-WIRE-YTD
               WHEN OTHER
                   COMPUTE TXFEE-WIRE-YTD ROUNDED = TXFEE-QUEUED-YTD *
                       TXFEE-FEE-PCT / 100
           END-EVALUATE
           IF TXFEE-WIRE-YTD > 0 AND TXFEE-WIRE-YTD < 2.00
               MOVE 2.00 TO TXFEE-WIRE-YTD
           END-IF
           IF TXFEE-WIRE-YTD > 35.00
               MOVE 35.00 TO TXFEE-WIRE-YTD
           END-IF
           ADD TXFEE-WIRE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CORRESP AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2140-FLAG-CORRESP.
           IF TXFEE-CORRESP-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TXFEE-CORRESP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP GRID TIER FOR THE SETTLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2150-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-GRID-RATE(1) TO TXFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TXFEE-SETTLE-YTD NOT < WS-CORRESP-GRID-LOW(WS-TX)
                   MOVE WS-CORRESP-GRID-RATE(WS-TX) TO TXFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2160-DESCRIBE-BRANCH.
           EVALUATE TXFEE-BRANCH-CD
               WHEN 'CC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2170-ACCRUE-SETTLE.
           IF TXFEE-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXFEE-SETTLE-AMT * TXFEE-RATE / 100 / 365
               ADD WS-WORK-AMT TO TXFEE-QUEUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2180-COUNT-ITEM.
           IF TXFEE-ITEM-CNT < 9999999
               ADD 1 TO TXFEE-ITEM-CNT
           END-IF
           IF TXFEE-ITEM-CNT NOT < 10
               MOVE 'Y' TO TXFEE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TEST KEY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2190-ACCUM-TEST-KEY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXFEE-TEST-KEY-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXFEE-TEST-KEY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CORRESP AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2200-LIMIT-CORRESP.
           IF TXFEE-CORRESP-AMT < 0.00
               MOVE 0.00 TO TXFEE-CORRESP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXFEE-CORRESP-AMT > 500.00
               MOVE 500.00 TO TXFEE-CORRESP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WIRE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COVER AMT.
      *----------------------------------------------------------------*
       2210-PRORATE-COVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXFEE-COVER-AMT ROUNDED =
                   TXFEE-WIRE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TXFEE-COVER-AMT
           END-IF
           ADD TXFEE-COVER-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CC -> KR WHEN CLOSE DATE IS PAST
      * KR -> LD WHEN CLOSE DATE IS PAST
      * LD -> LS WHEN CLOSE DATE IS PAST
      * LS -> XL WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2220-SET-BRANCH.
           EVALUATE TXFEE-BRANCH-CD
               WHEN 'CC'
                   IF TXFEE-CLOSE-DATE > 0 AND TXFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF TXFEE-CLOSE-DATE > 0 AND TXFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF TXFEE-CLOSE-DATE > 0 AND TXFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF TXFEE-CLOSE-DATE > 0 AND TXFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2230-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXFEE-TYPE-CD
                   ADD TXFEE-SETTLE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXFEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXFEE-SETTLE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CC -> KR WHEN LAST ACT DATE IS PAST
      * KR -> LD WHEN LAST ACT DATE IS PAST
      * LD -> LS WHEN LAST ACT DATE IS PAST
      * LS -> XL WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2240-SET-BRANCH.
           EVALUATE TXFEE-BRANCH-CD
               WHEN 'CC'
                   IF TXFEE-LAST-ACT-DATE > 0 AND TXFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF TXFEE-LAST-ACT-DATE > 0 AND TXFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF TXFEE-LAST-ACT-DATE > 0 AND TXFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF TXFEE-LAST-ACT-DATE > 0 AND TXFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WIRE YTD FEE ON THE SETTLE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2250-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TXFEE-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TXFEE-WIRE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXFEE-SUSPEND-FLAG = 'Y'
                   COMPUTE TXFEE-WIRE-YTD ROUNDED = TXFEE-SETTLE-YTD *
                       TXFEE-RATE / 200
               WHEN TXFEE-TYPE-CD = 'K'
                   MOVE 20.00 TO TXFEE-WIRE-YTD
               WHEN TXFEE-TYPE-CD = 'L'
                   MOVE 12.00 TO TXFEE-WIRE-YTD
               WHEN OTHER
                   COMPUTE TXFEE-WIRE-YTD ROUNDED = TXFEE-SETTLE-YTD *
                       TXFEE-RATE / 100
           END-EVALUATE
           IF TXFEE-WIRE-YTD > 0 AND TXFEE-WIRE-YTD < 5.00
               MOVE 5.00 TO TXFEE-WIRE-YTD
           END-IF
           IF TXFEE-WIRE-YTD > 150.00
               MOVE 150.00 TO TXFEE-WIRE-YTD
           END-IF
           ADD TXFEE-WIRE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * COVER AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2260-FLAG-COVER.
           IF TXFEE-COVER-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TXFEE-COVER-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CC -> KR WHEN LAST ACT DATE IS PAST
      * KR -> LD WHEN LAST ACT DATE IS PAST
      * LD -> LS WHEN LAST ACT DATE IS PAST
      * LS -> XL WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2270-SET-BRANCH.
           EVALUATE TXFEE-BRANCH-CD
               WHEN 'CC'
                   IF TXFEE-LAST-ACT-DATE > 0 AND TXFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF TXFEE-LAST-ACT-DATE > 0 AND TXFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF TXFEE-LAST-ACT-DATE > 0 AND TXFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF TXFEE-LAST-ACT-DATE > 0 AND TXFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE QUEUED YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2280-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXFEE-TYPE-CD
                   ADD TXFEE-QUEUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXFEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXFEE-QUEUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TEST KEY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2290-DESCRIBE-TEST-KEY.
           EVALUATE TXFEE-TEST-KEY-CD
               WHEN 'EA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE WIRE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2300-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TXFEE-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TXFEE-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXFEE-SUSPEND-FLAG = 'Y'
                   COMPUTE TXFEE-SETTLE-AMT ROUNDED = TXFEE-WIRE-YTD *
                       TXFEE-RATE / 200
               WHEN TXFEE-TYPE-CD = 'K'
                   MOVE 3.00 TO TXFEE-SETTLE-AMT
               WHEN TXFEE-TYPE-CD = 'L'
                   MOVE 10.00 TO TXFEE-SETTLE-AMT
               WHEN OTHER
                   COMPUTE TXFEE-SETTLE-AMT ROUNDED = TXFEE-WIRE-YTD *
                       TXFEE-RATE / 100
           END-EVALUATE
           IF TXFEE-SETTLE-AMT > 0 AND TXFEE-SETTLE-AMT < 10.00
               MOVE 10.00 TO TXFEE-SETTLE-AMT
           END-IF
           IF TXFEE-SETTLE-AMT > 250.00
               MOVE 250.00 TO TXFEE-SETTLE-AMT
           END-IF
           ADD TXFEE-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2310-ACCRUE-SETTLE.
           IF TXFEE-SETTLE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXFEE-SETTLE-YTD * TXFEE-RATE / 100 / 360
               ADD WS-WORK-AMT TO TXFEE-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2320-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-GRID-RATE(1) TO TXFEE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-CORRESP-GRID-LOW(WS-TX)
                   MOVE WS-CORRESP-GRID-RATE(WS-TX) TO TXFEE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXFEE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXFEE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP GRID TIER FOR THE CORRESP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2330-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-GRID-RATE(1) TO TXFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TXFEE-CORRESP-AMT NOT < WS-CORRESP-GRID-LOW(WS-TX)
                   MOVE WS-CORRESP-GRID-RATE(WS-TX) TO TXFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2340-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TXFEE-NEXT-DUE-DATE > 0 AND TXFEE-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXFEE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXFEE-CORRESP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TXFEE-CORRESP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TXFEE-CORRESP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TXFEE-CORRESP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXFEE-CORRESP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE SETTLE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2350-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TXFEE-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TXFEE-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXFEE-SUSPEND-FLAG = 'Y'
                   COMPUTE TXFEE-SETTLE-AMT ROUNDED = TXFEE-SETTLE-YTD *
                       TXFEE-FEE-PCT / 200
               WHEN TXFEE-REGION-CD = 'G'
                   MOVE 5.00 TO TXFEE-SETTLE-AMT
               WHEN TXFEE-REGION-CD = 'M'
                   MOVE 3.00 TO TXFEE-SETTLE-AMT
               WHEN OTHER
                   COMPUTE TXFEE-SETTLE-AMT ROUNDED = TXFEE-SETTLE-YTD *
                       TXFEE-FEE-PCT / 100
           END-EVALUATE
           IF TXFEE-SETTLE-AMT > 0 AND TXFEE-SETTLE-AMT < 2.00
               MOVE 2.00 TO TXFEE-SETTLE-AMT
           END-IF
           IF TXFEE-SETTLE-AMT > 250.00
               MOVE 250.00 TO TXFEE-SETTLE-AMT
           END-IF
           ADD TXFEE-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2360-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TXFEE-CLOSE-DATE > 0 AND TXFEE-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXFEE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXFEE-WIRE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TXFEE-WIRE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TXFEE-WIRE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TXFEE-WIRE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXFEE-WIRE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2370-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TXFEE-NEXT-DUE-DATE > 0 AND TXFEE-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXFEE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXFEE-CORRESP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TXFEE-CORRESP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TXFEE-CORRESP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TXFEE-CORRESP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXFEE-CORRESP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE AMT BY TEST KEY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2380-ACCUM-TEST-KEY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXFEE-TEST-KEY-CD
                   ADD TXFEE-SETTLE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXFEE-TEST-KEY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXFEE-SETTLE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COVER AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2390-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXFEE-TYPE-CD
                   ADD TXFEE-COVER-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXFEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXFEE-COVER-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2400-FLAG-QUEUED.
           IF TXFEE-QUEUED-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TXFEE-QUEUED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP GRID TIER FOR THE WIRE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2410-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-GRID-RATE(1) TO TXFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TXFEE-WIRE-YTD NOT < WS-CORRESP-GRID-LOW(WS-TX)
                   MOVE WS-CORRESP-GRID-RATE(WS-TX) TO TXFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COVER FEE ON THE QUEUED YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       2420-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TXFEE-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TXFEE-COVER-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXFEE-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TXFEE-COVER-AMT ROUNDED = TXFEE-QUEUED-YTD *
                       TXFEE-RATE / 200
               WHEN TXFEE-BRANCH-CD = 'CC'
                   MOVE 15.00 TO TXFEE-COVER-AMT
               WHEN TXFEE-BRANCH-CD = 'KR'
                   MOVE 15.00 TO TXFEE-COVER-AMT
               WHEN OTHER
                   COMPUTE TXFEE-COVER-AMT ROUNDED = TXFEE-QUEUED-YTD *
                       TXFEE-RATE / 100
           END-EVALUATE
           IF TXFEE-COVER-AMT > 0 AND TXFEE-COVER-AMT < 5.00
               MOVE 5.00 TO TXFEE-COVER-AMT
           END-IF
           IF TXFEE-COVER-AMT > 250.00
               MOVE 250.00 TO TXFEE-COVER-AMT
           END-IF
           ADD TXFEE-COVER-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2430-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXFEE-REGION-CD
                   ADD TXFEE-SETTLE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXFEE-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXFEE-SETTLE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E616 CUST ID REQUIRED
      * E993 WORK AMT NEGATIVE
      * E106 WORK AMT OVER 250000
      * E234 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2440-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXFEE-CUST-ID = SPACES
                   MOVE 'E616' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E993' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E106' TO WS-REJECT-CODE
               WHEN TXFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E234' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE QUEUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WIRE YTD.
      *----------------------------------------------------------------*
       2450-PRORATE-WIRE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXFEE-WIRE-YTD ROUNDED =
                   TXFEE-QUEUED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TXFEE-WIRE-YTD
           END-IF
           ADD TXFEE-WIRE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COVER AMT.
      *----------------------------------------------------------------*
       2460-PRORATE-COVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXFEE-COVER-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TXFEE-COVER-AMT
           END-IF
           ADD TXFEE-COVER-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE COVER AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2470-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXFEE-TYPE-CD
                   ADD TXFEE-COVER-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXFEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXFEE-COVER-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2480-LIMIT-SETTLE.
           IF TXFEE-SETTLE-YTD < 10.00
               MOVE 10.00 TO TXFEE-SETTLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXFEE-SETTLE-YTD > 1000.00
               MOVE 1000.00 TO TXFEE-SETTLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COVER AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2490-FLAG-COVER.
           IF TXFEE-COVER-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TXFEE-COVER-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       2500-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E192 CONTACT REQUIRED
      * E361 SETTLE YTD NEGATIVE
      * E744 SETTLE YTD OVER 50000
      * E817 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2510-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXFEE-CONTACT = SPACES
                   MOVE 'E192' TO WS-REJECT-CODE
               WHEN TXFEE-SETTLE-YTD < 0
                   MOVE 'E361' TO WS-REJECT-CODE
               WHEN TXFEE-SETTLE-YTD > 50000.00
                   MOVE 'E744' TO WS-REJECT-CODE
               WHEN TXFEE-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E817' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TEST KEY CD:
      * EA -> GA WHEN NEXT DUE DATE IS PAST
      * GA -> HL WHEN NEXT DUE DATE IS PAST
      * HL -> JC WHEN NEXT DUE DATE IS PAST
      * JC -> KC WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2520-SET-TEST-KEY.
           EVALUATE TXFEE-TEST-KEY-CD
               WHEN 'EA'
                   IF TXFEE-NEXT-DUE-DATE > 0 AND TXFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GA' TO TXFEE-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GA'
                   IF TXFEE-NEXT-DUE-DATE > 0 AND TXFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO TXFEE-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF TXFEE-NEXT-DUE-DATE > 0 AND TXFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JC' TO TXFEE-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF TXFEE-NEXT-DUE-DATE > 0 AND TXFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KC' TO TXFEE-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SETTLE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WIRE YTD.
      *----------------------------------------------------------------*
       2530-PRORATE-WIRE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXFEE-WIRE-YTD ROUNDED =
                   TXFEE-SETTLE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TXFEE-WIRE-YTD
           END-IF
           ADD TXFEE-WIRE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE CORRESP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO QUEUED YTD.
      *----------------------------------------------------------------*
       2540-PRORATE-QUEUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXFEE-QUEUED-YTD ROUNDED =
                   TXFEE-CORRESP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TXFEE-QUEUED-YTD
           END-IF
           ADD TXFEE-QUEUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2550-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TXFEE-RATE / 100 / 360
               ADD WS-WORK-AMT TO TXFEE-QUEUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2560-LIMIT-QUEUED.
           IF TXFEE-QUEUED-YTD < 0.00
               MOVE 0.00 TO TXFEE-QUEUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXFEE-QUEUED-YTD > 50000.00
               MOVE 50000.00 TO TXFEE-QUEUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE WIRE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2570-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TXFEE-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TXFEE-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TXFEE-WIRE-YTD *
                       TXFEE-RATE / 200
               WHEN TXFEE-TEST-KEY-CD = 'EA'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN TXFEE-TEST-KEY-CD = 'GA'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TXFEE-WIRE-YTD *
                       TXFEE-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2580-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXFEE-BRANCH-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXFEE-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WIRE YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2590-LIMIT-WIRE.
           IF TXFEE-WIRE-YTD < 25.00
               MOVE 25.00 TO TXFEE-WIRE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TXFEE-WIRE-YTD > 10000.00
               MOVE 10000.00 TO TXFEE-WIRE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2600-COUNT-ITEM.
           IF TXFEE-ITEM-CNT < 9999999
               ADD 1 TO TXFEE-ITEM-CNT
           END-IF
           IF TXFEE-ITEM-CNT NOT < 12
               MOVE 'Y' TO TXFEE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WIRE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2610-ACCRUE-WIRE.
           IF TXFEE-WIRE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXFEE-WIRE-YTD * TXFEE-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TXFEE-CORRESP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2620-COUNT-ITEM.
           IF TXFEE-ITEM-CNT < 9999999
               ADD 1 TO TXFEE-ITEM-CNT
           END-IF
           IF TXFEE-ITEM-CNT NOT < 3
               MOVE 'Y' TO TXFEE-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CORRESP AMT.
      *----------------------------------------------------------------*
       2630-PRORATE-CORRESP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TXFEE-CORRESP-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TXFEE-CORRESP-AMT
           END-IF
           ADD TXFEE-CORRESP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2640-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TXFEE-CLOSE-DATE > 0 AND TXFEE-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXFEE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXFEE-WIRE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TXFEE-WIRE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TXFEE-WIRE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TXFEE-WIRE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXFEE-WIRE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2650-DESCRIBE-REGION.
           EVALUATE TXFEE-REGION-CD
               WHEN 'G'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2660-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXFEE-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXFEE-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WIRE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2670-SPLIT-WIRE.
           COMPUTE WS-WORK-AMT = TXFEE-WIRE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TXFEE-WIRE-YTD - (WS-WORK-AMT * 12)
           COMPUTE TXFEE-QUEUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TEST KEY CD:
      * EA -> GA WHEN CLOSE DATE IS PAST
      * GA -> HL WHEN CLOSE DATE IS PAST
      * HL -> JC WHEN CLOSE DATE IS PAST
      * JC -> KC WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2680-SET-TEST-KEY.
           EVALUATE TXFEE-TEST-KEY-CD
               WHEN 'EA'
                   IF TXFEE-CLOSE-DATE > 0 AND TXFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GA' TO TXFEE-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GA'
                   IF TXFEE-CLOSE-DATE > 0 AND TXFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO TXFEE-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF TXFEE-CLOSE-DATE > 0 AND TXFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JC' TO TXFEE-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF TXFEE-CLOSE-DATE > 0 AND TXFEE-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KC' TO TXFEE-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WIRE YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2690-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TXFEE-REGION-CD
                   ADD TXFEE-WIRE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TXFEE-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TXFEE-WIRE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2700-DESCRIBE-BRANCH.
           EVALUATE TXFEE-BRANCH-CD
               WHEN 'CC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SETTLE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2710-FLAG-SETTLE.
           IF TXFEE-SETTLE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TXFEE-SETTLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2720-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TXFEE-NEXT-DUE-DATE > 0 AND TXFEE-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXFEE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXFEE-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TXFEE-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TXFEE-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TXFEE-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXFEE-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2730-ACCRUE-SETTLE.
           IF TXFEE-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TXFEE-SETTLE-AMT * TXFEE-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CC -> KR WHEN NEXT DUE DATE IS PAST
      * KR -> LD WHEN NEXT DUE DATE IS PAST
      * LD -> LS WHEN NEXT DUE DATE IS PAST
      * LS -> XL WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2740-SET-BRANCH.
           EVALUATE TXFEE-BRANCH-CD
               WHEN 'CC'
                   IF TXFEE-NEXT-DUE-DATE > 0 AND TXFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF TXFEE-NEXT-DUE-DATE > 0 AND TXFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF TXFEE-NEXT-DUE-DATE > 0 AND TXFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF TXFEE-NEXT-DUE-DATE > 0 AND TXFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2750-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * K -> L WHEN NEXT DUE DATE IS PAST
      * L -> M WHEN NEXT DUE DATE IS PAST
      * M -> P WHEN NEXT DUE DATE IS PAST
      * P -> W WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2760-SET-TYPE.
           EVALUATE TXFEE-TYPE-CD
               WHEN 'K'
                   IF TXFEE-NEXT-DUE-DATE > 0 AND TXFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO TXFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF TXFEE-NEXT-DUE-DATE > 0 AND TXFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TXFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TXFEE-NEXT-DUE-DATE > 0 AND TXFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TXFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TXFEE-NEXT-DUE-DATE > 0 AND TXFEE-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO TXFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP GRID TIER FOR THE COVER AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2770-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-GRID-RATE(1) TO TXFEE-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TXFEE-COVER-AMT NOT < WS-CORRESP-GRID-LOW(WS-TX)
                   MOVE WS-CORRESP-GRID-RATE(WS-TX) TO TXFEE-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXFEE-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXFEE-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2780-COUNT-EXCEPT.
           IF TXFEE-EXCEPT-CNT < 9999999
               ADD 1 TO TXFEE-EXCEPT-CNT
           END-IF
           IF TXFEE-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO TXFEE-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2790-COUNT-ITEM.
           IF TXFEE-ITEM-CNT < 9999999
               ADD 1 TO TXFEE-ITEM-CNT
           END-IF
           IF TXFEE-ITEM-CNT NOT < 3
               MOVE 'Y' TO TXFEE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * K -> L WHEN LAST PAY DATE IS PAST
      * L -> M WHEN LAST PAY DATE IS PAST
      * M -> P WHEN LAST PAY DATE IS PAST
      * P -> W WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2800-SET-TYPE.
           EVALUATE TXFEE-TYPE-CD
               WHEN 'K'
                   IF TXFEE-LAST-PAY-DATE > 0 AND TXFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO TXFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF TXFEE-LAST-PAY-DATE > 0 AND TXFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TXFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TXFEE-LAST-PAY-DATE > 0 AND TXFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TXFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TXFEE-LAST-PAY-DATE > 0 AND TXFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO TXFEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       2810-COUNT-EXCEPT.
           IF TXFEE-EXCEPT-CNT < 9999999
               ADD 1 TO TXFEE-EXCEPT-CNT
           END-IF
           IF TXFEE-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TXFEE-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CC -> KR WHEN LAST PAY DATE IS PAST
      * KR -> LD WHEN LAST PAY DATE IS PAST
      * LD -> LS WHEN LAST PAY DATE IS PAST
      * LS -> XL WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2820-SET-BRANCH.
           EVALUATE TXFEE-BRANCH-CD
               WHEN 'CC'
                   IF TXFEE-LAST-PAY-DATE > 0 AND TXFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF TXFEE-LAST-PAY-DATE > 0 AND TXFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF TXFEE-LAST-PAY-DATE > 0 AND TXFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF TXFEE-LAST-PAY-DATE > 0 AND TXFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO TXFEE-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP GRID TIER FOR THE SETTLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2830-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-GRID-RATE(1) TO TXFEE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TXFEE-SETTLE-AMT NOT < WS-CORRESP-GRID-LOW(WS-TX)
                   MOVE WS-CORRESP-GRID-RATE(WS-TX) TO TXFEE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TXFEE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TXFEE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E783 CUST ID REQUIRED
      * E109 WIRE YTD NEGATIVE
      * E484 WIRE YTD OVER 10000
      * E353 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2840-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TXFEE-CUST-ID = SPACES
                   MOVE 'E783' TO WS-REJECT-CODE
               WHEN TXFEE-WIRE-YTD < 0
                   MOVE 'E109' TO WS-REJECT-CODE
               WHEN TXFEE-WIRE-YTD > 10000.00
                   MOVE 'E484' TO WS-REJECT-CODE
               WHEN TXFEE-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E353' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SETTLE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2850-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = TXFEE-SETTLE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TXFEE-SETTLE-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2860-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TXFEE-NEXT-DUE-DATE > 0 AND TXFEE-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXFEE-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXFEE-CORRESP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TXFEE-CORRESP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TXFEE-CORRESP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TXFEE-CORRESP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXFEE-CORRESP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT COVER AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2870-SPLIT-COVER.
           COMPUTE WS-WORK-AMT = TXFEE-COVER-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TXFEE-COVER-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2880-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TXFEE-CLOSE-DATE > 0 AND TXFEE-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TXFEE-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TXFEE-QUEUED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TXFEE-QUEUED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TXFEE-QUEUED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TXFEE-QUEUED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TXFEE-QUEUED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO TXFEE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO TXFEE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO TXFEE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'TXFEE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
