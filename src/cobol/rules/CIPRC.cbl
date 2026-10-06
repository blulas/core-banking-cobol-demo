      *================================================================*
      * PROGRAM  : CIPRC                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : CHECK IMAGE ARCHIVE RULE - FEES. CALLED BY THE CHECK*
      *            IMAGE ARCHIVE BATCH AND ONLINE PROGRAMS.            *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-04-20 RJT  ORIGINAL                                       *
      * 1988-06-10 BHS  ITEM TABLE EXPANDED REQ4117                    *
      * 1994-06-11 PQR  IMAGE-FEE STATUS CODES ADDED PRB6768           *
      * 1995-03-05 PQR  NEW STORAGE REPORT COLUMNS INC3258             *
      * 1998-11-04 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-04-20 BHS  AUDIT FINDING - RETRIEVE-FEE CONTROL TOTALS REQ*
      * 2004-01-21 PQR  RETRIEVE-FEE EXCEPTIONS TO OPS QUEUE REQ1361   *
      * 2005-06-23 MKL  NEW RETRIEVE-FEE REPORT COLUMNS PRB7839        *
      * 2006-01-03 MKL  STORAGE LIMIT REVISED REQ8719                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIPRC.
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
                                           VALUE 16.000000.
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
      * WS-ITEM-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ITEM-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000810500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000782000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000322000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012750000000606000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013250000000123000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018250000000169000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018350000000609000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018400000000164500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018450000000105500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018700000000131000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028700000000009000'.
       01  WS-ITEM-SCHED-TBL REDEFINES WS-ITEM-SCHED-VALUES.
           05  WS-ITEM-SCHED-ROW OCCURS 11 TIMES.
               10  WS-ITEM-SCHED-LOW       PIC 9(09)V99.
               10  WS-ITEM-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-STORAGE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STORAGE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000529000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000780500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000769500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006050000000325000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006300000000659000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006550000000648000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009050000000326000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019050000000651000'.
       01  WS-STORAGE-BAND-TBL REDEFINES WS-STORAGE-BAND-VALUES.
           05  WS-STORAGE-BAND-ROW OCCURS 8 TIMES.
               10  WS-STORAGE-BAND-LOW     PIC 9(09)V99.
               10  WS-STORAGE-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RETRIEVE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RETRIEVE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000023500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000613000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000310000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022500000000702000'.
       01  WS-RETRIEVE-BAND-TBL REDEFINES WS-RETRIEVE-BAND-VALUES.
           05  WS-RETRIEVE-BAND-ROW OCCURS 4 TIMES.
               10  WS-RETRIEVE-BAND-LOW    PIC 9(09)V99.
               10  WS-RETRIEVE-BAND-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY CIPRCP.

       PROCEDURE DIVISION USING CIPRC-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ACCUM-CAPTURE-SITE
           PERFORM 2010-AGE-EFFECTIVE
           PERFORM 2020-SPLIT-RETRIEVE-FEE
           PERFORM 2030-ACCUM-ITEM-TYPE
           PERFORM 2040-ACCRUE-STORAGE
           PERFORM 2050-FLAG-STORAGE
           PERFORM 2060-AGE-EFFECTIVE
           PERFORM 2070-SPLIT-IMAGE-FEE
           PERFORM 2080-AGE-NEXT-DUE
           PERFORM 2090-CHECK-NOTICE
           PERFORM 2100-FLAG-ITEM
           PERFORM 2110-ASSESS-EXEMPT
           PERFORM 2120-SET-CAPTURE-SITE
           PERFORM 2130-SET-CHANNEL
           PERFORM 2140-LIMIT-ITEM
           PERFORM 2150-DESCRIBE-STATUS
           PERFORM 2160-CHECK-MATURITY
           PERFORM 2170-LIMIT-ITEM
           PERFORM 2180-ASSESS-EXEMPT
           PERFORM 2190-FLAG-ITEM
           PERFORM 2200-EDIT-REF-NO
           PERFORM 2210-FLAG-ITEM
           PERFORM 2220-EDIT-REF-NO
           PERFORM 2230-AGE-NOTICE
           PERFORM 2240-SPLIT-RETRIEVE-FEE
           PERFORM 2250-SPLIT-RETRIEVE-FEE
           PERFORM 2260-COUNT-NOTICE
           PERFORM 2270-EDIT-ADDR
           PERFORM 2280-SET-ITEM-TYPE
           PERFORM 2290-PRORATE-RETRIEVE-FEE
           PERFORM 2300-CHECK-NOTICE
           PERFORM 2310-FLAG-ITEM
           PERFORM 2320-AGE-MATURITY
           PERFORM 2330-FLAG-ITEM
           PERFORM 2340-SET-CHANNEL
           PERFORM 2350-PRORATE-RETRIEVE-FEE
           PERFORM 2360-AGE-MATURITY
           PERFORM 2370-SELECT-PENALTY-PCT
           PERFORM 2380-PRORATE-RETRIEVE-FEE
           PERFORM 2390-CHECK-MATURITY
           PERFORM 2400-EDIT-ADDR
           PERFORM 2410-COUNT-RETRY
           PERFORM 2420-COUNT-NOTICE
           PERFORM 2430-COUNT-RETRY
           PERFORM 2440-PRORATE-IMAGE-FEE
           PERFORM 2450-EDIT-REF-NO
           PERFORM 2460-EDIT-REF-NO
           PERFORM 2470-AGE-NOTICE
           PERFORM 2480-EDIT-ADDR
           PERFORM 2490-PRORATE-ITEM
           PERFORM 2500-ACCRUE-RETRIEVE-FEE
           PERFORM 2510-EDIT-REF-NO
           PERFORM 2520-ACCUM-CAPTURE-SITE
           PERFORM 2530-AGE-MATURITY
           PERFORM 2540-SET-ITEM-TYPE
           PERFORM 2550-ACCRUE-RETRIEVE-FEE
           PERFORM 2560-CHECK-EFFECTIVE
           PERFORM 2570-AGE-MATURITY
           PERFORM 2580-DESCRIBE-CAPTURE-SITE
           PERFORM 2590-SET-ITEM-TYPE
           PERFORM 2600-SPLIT-ITEM
           PERFORM 2610-ACCUM-CAPTURE-SITE
           PERFORM 2620-FLAG-ITEM
           PERFORM 2630-CHECK-NOTICE
           PERFORM 2640-ACCUM-CHANNEL
           PERFORM 2650-ACCRUE-STORAGE
           PERFORM 2660-LIMIT-ITEM
           PERFORM 2670-AGE-NOTICE
           PERFORM 2680-SET-ITEM-TYPE
           PERFORM 2690-FLAG-ITEM
           PERFORM 2700-SELECT-MARGIN
           PERFORM 2710-EDIT-REF-NO
           PERFORM 2720-SELECT-PENALTY-PCT
           PERFORM 2730-SET-STATUS
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO CIPRC-RETURN-CODE.

      *----------------------------------------------------------------*
      * ACCUMULATE ITEM AMT BY CAPTURE SITE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2000-ACCUM-CAPTURE-SITE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIPRC-CAPTURE-SITE-CD
                   ADD CIPRC-ITEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIPRC-CAPTURE-SITE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIPRC-ITEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2010-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CIPRC-EFFECTIVE-DATE > 0 AND CIPRC-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIPRC-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIPRC-RETRIEVE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIPRC-RETRIEVE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CIPRC-RETRIEVE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIPRC-RETRIEVE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIPRC-RETRIEVE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RETRIEVE FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2020-SPLIT-RETRIEVE-FEE.
           COMPUTE WS-WORK-AMT = CIPRC-RETRIEVE-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CIPRC-RETRIEVE-FEE-AMT - (WS-WORK-AMT
               * 3)
           COMPUTE CIPRC-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RETRIEVE FEE YTD BY ITEM TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2030-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIPRC-ITEM-TYPE-CD
                   ADD CIPRC-RETRIEVE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIPRC-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIPRC-RETRIEVE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2040-ACCRUE-STORAGE.
           IF CIPRC-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIPRC-STORAGE-AMT * CIPRC-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2050-FLAG-STORAGE.
           IF CIPRC-STORAGE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIPRC-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       2060-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CIPRC-EFFECTIVE-DATE > 0 AND CIPRC-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIPRC-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIPRC-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CIPRC-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CIPRC-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CIPRC-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIPRC-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT IMAGE FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2070-SPLIT-IMAGE-FEE.
           COMPUTE WS-WORK-AMT = CIPRC-IMAGE-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CIPRC-IMAGE-FEE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE CIPRC-ITEM-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2080-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIPRC-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2090-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CIPRC-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIPRC-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO CIPRC-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2100-FLAG-ITEM.
           IF CIPRC-ITEM-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIPRC-ITEM-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ITEM FEE ON THE STORAGE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2110-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CIPRC-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CIPRC-ITEM-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIPRC-SUSPEND-FLAG = 'Y'
                   COMPUTE CIPRC-ITEM-AMT ROUNDED = CIPRC-STORAGE-AMT *
                       CIPRC-MARGIN / 200
               WHEN CIPRC-CHANNEL-CD = 'FW'
                   MOVE 20.00 TO CIPRC-ITEM-AMT
               WHEN CIPRC-CHANNEL-CD = 'KL'
                   MOVE 10.00 TO CIPRC-ITEM-AMT
               WHEN OTHER
                   COMPUTE CIPRC-ITEM-AMT ROUNDED = CIPRC-STORAGE-AMT *
                       CIPRC-MARGIN / 100
           END-EVALUATE
           IF CIPRC-ITEM-AMT > 0 AND CIPRC-ITEM-AMT < 10.00
               MOVE 10.00 TO CIPRC-ITEM-AMT
           END-IF
           IF CIPRC-ITEM-AMT > 150.00
               MOVE 150.00 TO CIPRC-ITEM-AMT
           END-IF
           ADD CIPRC-ITEM-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CAPTURE SITE CD:
      * BB -> CA WHEN NEXT DUE DATE IS PAST
      * CA -> PC WHEN NEXT DUE DATE IS PAST
      * PC -> BB WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2120-SET-CAPTURE-SITE.
           EVALUATE CIPRC-CAPTURE-SITE-CD
               WHEN 'BB'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CA' TO CIPRC-CAPTURE-SITE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CA'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO CIPRC-CAPTURE-SITE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BB' TO CIPRC-CAPTURE-SITE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FW -> KL WHEN MATURITY DATE IS PAST
      * KL -> NB WHEN MATURITY DATE IS PAST
      * NB -> TP WHEN MATURITY DATE IS PAST
      * TP -> ZZ WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2130-SET-CHANNEL.
           EVALUATE CIPRC-CHANNEL-CD
               WHEN 'FW'
                   IF CIPRC-MATURITY-DATE > 0 AND CIPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KL' TO CIPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KL'
                   IF CIPRC-MATURITY-DATE > 0 AND CIPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NB' TO CIPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NB'
                   IF CIPRC-MATURITY-DATE > 0 AND CIPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TP' TO CIPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TP'
                   IF CIPRC-MATURITY-DATE > 0 AND CIPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZZ' TO CIPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       2140-LIMIT-ITEM.
           IF CIPRC-ITEM-YTD < 50.00
               MOVE 50.00 TO CIPRC-ITEM-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIPRC-ITEM-YTD > 500.00
               MOVE 500.00 TO CIPRC-ITEM-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2150-DESCRIBE-STATUS.
           EVALUATE CIPRC-STATUS-CD
               WHEN 'AJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'YW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2160-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CIPRC-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIPRC-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO CIPRC-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2170-LIMIT-ITEM.
           IF CIPRC-ITEM-AMT < 25.00
               MOVE 25.00 TO CIPRC-ITEM-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIPRC-ITEM-AMT > 10000.00
               MOVE 10000.00 TO CIPRC-ITEM-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETRIEVE FEE FEE ON THE RETRIEVE FEE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2180-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CIPRC-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CIPRC-RETRIEVE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIPRC-SUSPEND-FLAG = 'Y'
                   COMPUTE CIPRC-RETRIEVE-FEE-AMT ROUNDED =
                       CIPRC-RETRIEVE-FEE-YTD * CIPRC-MARGIN / 200
               WHEN CIPRC-STATUS-CD = 'AJ'
                   MOVE 5.00 TO CIPRC-RETRIEVE-FEE-AMT
               WHEN CIPRC-STATUS-CD = 'JY'
                   MOVE 20.00 TO CIPRC-RETRIEVE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIPRC-RETRIEVE-FEE-AMT ROUNDED =
                       CIPRC-RETRIEVE-FEE-YTD * CIPRC-MARGIN / 100
           END-EVALUATE
           IF CIPRC-RETRIEVE-FEE-AMT > 0 AND CIPRC-RETRIEVE-FEE-AMT <
               2.00
               MOVE 2.00 TO CIPRC-RETRIEVE-FEE-AMT
           END-IF
           IF CIPRC-RETRIEVE-FEE-AMT > 50.00
               MOVE 50.00 TO CIPRC-RETRIEVE-FEE-AMT
           END-IF
           ADD CIPRC-RETRIEVE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2190-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E574 REF NO REQUIRED
      * E488 STORAGE AMT NEGATIVE
      * E053 STORAGE AMT OVER 25000
      * E011 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       2200-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIPRC-REF-NO = SPACES
                   MOVE 'E574' TO WS-REJECT-CODE
               WHEN CIPRC-STORAGE-AMT < 0
                   MOVE 'E488' TO WS-REJECT-CODE
               WHEN CIPRC-STORAGE-AMT > 25000.00
                   MOVE 'E053' TO WS-REJECT-CODE
               WHEN CIPRC-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E011' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2210-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E132 REF NO REQUIRED
      * E367 IMAGE FEE AMT NEGATIVE
      * E813 IMAGE FEE AMT OVER 50000
      * E910 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2220-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIPRC-REF-NO = SPACES
                   MOVE 'E132' TO WS-REJECT-CODE
               WHEN CIPRC-IMAGE-FEE-AMT < 0
                   MOVE 'E367' TO WS-REJECT-CODE
               WHEN CIPRC-IMAGE-FEE-AMT > 50000.00
                   MOVE 'E813' TO WS-REJECT-CODE
               WHEN CIPRC-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E910' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2230-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CIPRC-NOTICE-DATE > 0 AND CIPRC-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIPRC-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2240-SPLIT-RETRIEVE-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE CIPRC-RETRIEVE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RETRIEVE FEE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2250-SPLIT-RETRIEVE-FEE.
           COMPUTE WS-WORK-AMT = CIPRC-RETRIEVE-FEE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CIPRC-RETRIEVE-FEE-YTD - (WS-WORK-AMT
               * 4)
           COMPUTE CIPRC-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2260-COUNT-NOTICE.
           IF CIPRC-NOTICE-CNT < 9999999
               ADD 1 TO CIPRC-NOTICE-CNT
           END-IF
           IF CIPRC-NOTICE-CNT NOT < 12
               MOVE 'Y' TO CIPRC-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E294 ADDR REQUIRED
      * E082 WORK AMT 2 NEGATIVE
      * E055 WORK AMT 2 OVER 50000
      * E964 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2270-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIPRC-ADDR = SPACES
                   MOVE 'E294' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E082' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E055' TO WS-REJECT-CODE
               WHEN CIPRC-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E964' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * CW -> FB WHEN NEXT DUE DATE IS PAST
      * FB -> HN WHEN NEXT DUE DATE IS PAST
      * HN -> KX WHEN NEXT DUE DATE IS PAST
      * KX -> CW WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2280-SET-ITEM-TYPE.
           EVALUATE CIPRC-ITEM-TYPE-CD
               WHEN 'CW'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO CIPRC-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HN' TO CIPRC-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HN'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO CIPRC-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KX'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CW' TO CIPRC-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ITEM YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETRIEVE FEE YTD.
      *----------------------------------------------------------------*
       2290-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIPRC-RETRIEVE-FEE-YTD ROUNDED =
                   CIPRC-ITEM-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIPRC-RETRIEVE-FEE-YTD
           END-IF
           ADD CIPRC-RETRIEVE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2300-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CIPRC-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIPRC-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO CIPRC-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2310-FLAG-ITEM.
           IF CIPRC-ITEM-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIPRC-ITEM-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2320-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CIPRC-MATURITY-DATE > 0 AND CIPRC-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIPRC-MATURITY-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2330-FLAG-ITEM.
           IF CIPRC-ITEM-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIPRC-ITEM-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FW -> KL WHEN NEXT DUE DATE IS PAST
      * KL -> NB WHEN NEXT DUE DATE IS PAST
      * NB -> TP WHEN NEXT DUE DATE IS PAST
      * TP -> ZZ WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2340-SET-CHANNEL.
           EVALUATE CIPRC-CHANNEL-CD
               WHEN 'FW'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KL' TO CIPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KL'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NB' TO CIPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NB'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TP' TO CIPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TP'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZZ' TO CIPRC-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RETRIEVE FEE YTD.
      *----------------------------------------------------------------*
       2350-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIPRC-RETRIEVE-FEE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIPRC-RETRIEVE-FEE-YTD
           END-IF
           ADD CIPRC-RETRIEVE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       2360-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CIPRC-MATURITY-DATE > 0 AND CIPRC-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIPRC-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIPRC-ITEM-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIPRC-ITEM-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CIPRC-ITEM-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD CIPRC-ITEM-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIPRC-ITEM-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RETRIEVE BAND TIER FOR THE IMAGE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2370-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RETRIEVE-BAND-RATE(1) TO CIPRC-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CIPRC-IMAGE-FEE-AMT NOT < WS-RETRIEVE-BAND-LOW(WS-TX)
                   MOVE WS-RETRIEVE-BAND-RATE(WS-TX) TO
                       CIPRC-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIPRC-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIPRC-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STORAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETRIEVE FEE AMT.
      *----------------------------------------------------------------*
       2380-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIPRC-RETRIEVE-FEE-AMT ROUNDED =
                   CIPRC-STORAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIPRC-RETRIEVE-FEE-AMT
           END-IF
           ADD CIPRC-RETRIEVE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2390-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CIPRC-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIPRC-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO CIPRC-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E531 ADDR REQUIRED
      * E135 WORK AMT NEGATIVE
      * E921 WORK AMT OVER 5000
      * E719 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2400-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIPRC-ADDR = SPACES
                   MOVE 'E531' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E135' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E921' TO WS-REJECT-CODE
               WHEN CIPRC-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E719' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2410-COUNT-RETRY.
           IF CIPRC-RETRY-CNT < 9999999
               ADD 1 TO CIPRC-RETRY-CNT
           END-IF
           IF CIPRC-RETRY-CNT NOT < 3
               MOVE 'Y' TO CIPRC-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       2420-COUNT-NOTICE.
           IF CIPRC-NOTICE-CNT < 9999999
               ADD 1 TO CIPRC-NOTICE-CNT
           END-IF
           IF CIPRC-NOTICE-CNT NOT < 10
               MOVE 'Y' TO CIPRC-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       2430-COUNT-RETRY.
           IF CIPRC-RETRY-CNT < 9999999
               ADD 1 TO CIPRC-RETRY-CNT
           END-IF
           IF CIPRC-RETRY-CNT NOT < 3
               MOVE 'Y' TO CIPRC-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       2440-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIPRC-IMAGE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E069 REF NO REQUIRED
      * E250 ITEM AMT NEGATIVE
      * E050 ITEM AMT OVER 5000
      * E227 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2450-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIPRC-REF-NO = SPACES
                   MOVE 'E069' TO WS-REJECT-CODE
               WHEN CIPRC-ITEM-AMT < 0
                   MOVE 'E250' TO WS-REJECT-CODE
               WHEN CIPRC-ITEM-AMT > 5000.00
                   MOVE 'E050' TO WS-REJECT-CODE
               WHEN CIPRC-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E227' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E865 REF NO REQUIRED
      * E726 WORK AMT NEGATIVE
      * E223 WORK AMT OVER 5000
      * E904 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2460-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIPRC-REF-NO = SPACES
                   MOVE 'E865' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E726' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E223' TO WS-REJECT-CODE
               WHEN CIPRC-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E904' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2470-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CIPRC-NOTICE-DATE > 0 AND CIPRC-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIPRC-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIPRC-ITEM-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIPRC-ITEM-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CIPRC-ITEM-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIPRC-ITEM-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIPRC-ITEM-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E198 ADDR REQUIRED
      * E775 IMAGE FEE AMT NEGATIVE
      * E554 IMAGE FEE AMT OVER 50000
      * E759 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       2480-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIPRC-ADDR = SPACES
                   MOVE 'E198' TO WS-REJECT-CODE
               WHEN CIPRC-IMAGE-FEE-AMT < 0
                   MOVE 'E775' TO WS-REJECT-CODE
               WHEN CIPRC-IMAGE-FEE-AMT > 50000.00
                   MOVE 'E554' TO WS-REJECT-CODE
               WHEN CIPRC-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E759' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ITEM YTD.
      *----------------------------------------------------------------*
       2490-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIPRC-ITEM-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIPRC-ITEM-YTD
           END-IF
           ADD CIPRC-ITEM-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETRIEVE FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2500-ACCRUE-RETRIEVE-FEE.
           IF CIPRC-RETRIEVE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIPRC-RETRIEVE-FEE-AMT * CIPRC-PENALTY-PCT / 100 /
                       365
               ADD WS-WORK-AMT TO CIPRC-ITEM-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E477 REF NO REQUIRED
      * E647 RETRIEVE FEE YTD NEGATIVE
      * E749 RETRIEVE FEE YTD OVER 25000
      * E539 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2510-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIPRC-REF-NO = SPACES
                   MOVE 'E477' TO WS-REJECT-CODE
               WHEN CIPRC-RETRIEVE-FEE-YTD < 0
                   MOVE 'E647' TO WS-REJECT-CODE
               WHEN CIPRC-RETRIEVE-FEE-YTD > 25000.00
                   MOVE 'E749' TO WS-REJECT-CODE
               WHEN CIPRC-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E539' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CAPTURE SITE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       2520-ACCUM-CAPTURE-SITE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIPRC-CAPTURE-SITE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIPRC-CAPTURE-SITE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2530-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CIPRC-MATURITY-DATE > 0 AND CIPRC-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIPRC-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIPRC-ITEM-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CIPRC-ITEM-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CIPRC-ITEM-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CIPRC-ITEM-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIPRC-ITEM-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * CW -> FB WHEN NEXT DUE DATE IS PAST
      * FB -> HN WHEN NEXT DUE DATE IS PAST
      * HN -> KX WHEN NEXT DUE DATE IS PAST
      * KX -> CW WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2540-SET-ITEM-TYPE.
           EVALUATE CIPRC-ITEM-TYPE-CD
               WHEN 'CW'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO CIPRC-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HN' TO CIPRC-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HN'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO CIPRC-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KX'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CW' TO CIPRC-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETRIEVE FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2550-ACCRUE-RETRIEVE-FEE.
           IF CIPRC-RETRIEVE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIPRC-RETRIEVE-FEE-AMT * CIPRC-PENALTY-PCT / 100 /
                       360
               ADD WS-WORK-AMT TO CIPRC-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2560-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CIPRC-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIPRC-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO CIPRC-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2570-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CIPRC-MATURITY-DATE > 0 AND CIPRC-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIPRC-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIPRC-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIPRC-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CIPRC-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIPRC-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIPRC-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CAPTURE SITE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2580-DESCRIBE-CAPTURE-SITE.
           EVALUATE CIPRC-CAPTURE-SITE-CD
               WHEN 'BB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * CW -> FB WHEN NEXT DUE DATE IS PAST
      * FB -> HN WHEN NEXT DUE DATE IS PAST
      * HN -> KX WHEN NEXT DUE DATE IS PAST
      * KX -> CW WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2590-SET-ITEM-TYPE.
           EVALUATE CIPRC-ITEM-TYPE-CD
               WHEN 'CW'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO CIPRC-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HN' TO CIPRC-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HN'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO CIPRC-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KX'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CW' TO CIPRC-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2600-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE CIPRC-ITEM-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ITEM AMT BY CAPTURE SITE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2610-ACCUM-CAPTURE-SITE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIPRC-CAPTURE-SITE-CD
                   ADD CIPRC-ITEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIPRC-CAPTURE-SITE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIPRC-ITEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2620-FLAG-ITEM.
           IF CIPRC-ITEM-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIPRC-ITEM-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2630-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CIPRC-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIPRC-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO CIPRC-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2640-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIPRC-CHANNEL-CD
                   ADD CIPRC-STORAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIPRC-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIPRC-STORAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2650-ACCRUE-STORAGE.
           IF CIPRC-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIPRC-STORAGE-AMT * CIPRC-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO CIPRC-RETRIEVE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2660-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2670-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CIPRC-NOTICE-DATE > 0 AND CIPRC-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIPRC-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIPRC-RETRIEVE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIPRC-RETRIEVE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CIPRC-RETRIEVE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CIPRC-RETRIEVE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIPRC-RETRIEVE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * CW -> FB WHEN NEXT DUE DATE IS PAST
      * FB -> HN WHEN NEXT DUE DATE IS PAST
      * HN -> KX WHEN NEXT DUE DATE IS PAST
      * KX -> CW WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2680-SET-ITEM-TYPE.
           EVALUATE CIPRC-ITEM-TYPE-CD
               WHEN 'CW'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO CIPRC-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HN' TO CIPRC-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HN'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO CIPRC-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KX'
                   IF CIPRC-NEXT-DUE-DATE > 0 AND CIPRC-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CW' TO CIPRC-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2690-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RETRIEVE BAND TIER FOR THE ITEM YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2700-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RETRIEVE-BAND-RATE(1) TO CIPRC-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CIPRC-ITEM-YTD NOT < WS-RETRIEVE-BAND-LOW(WS-TX)
                   MOVE WS-RETRIEVE-BAND-RATE(WS-TX) TO CIPRC-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIPRC-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIPRC-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E058 REF NO REQUIRED
      * E660 WORK AMT 2 NEGATIVE
      * E871 WORK AMT 2 OVER 5000
      * E652 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       2710-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIPRC-REF-NO = SPACES
                   MOVE 'E058' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E660' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E871' TO WS-REJECT-CODE
               WHEN CIPRC-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E652' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2720-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-BAND-RATE(1) TO CIPRC-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT-2 NOT < WS-STORAGE-BAND-LOW(WS-TX)
                   MOVE WS-STORAGE-BAND-RATE(WS-TX) TO CIPRC-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIPRC-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIPRC-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AJ -> JY WHEN MATURITY DATE IS PAST
      * JY -> RU WHEN MATURITY DATE IS PAST
      * RU -> UR WHEN MATURITY DATE IS PAST
      * UR -> YW WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2730-SET-STATUS.
           EVALUATE CIPRC-STATUS-CD
               WHEN 'AJ'
                   IF CIPRC-MATURITY-DATE > 0 AND CIPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JY' TO CIPRC-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JY'
                   IF CIPRC-MATURITY-DATE > 0 AND CIPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RU' TO CIPRC-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF CIPRC-MATURITY-DATE > 0 AND CIPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO CIPRC-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF CIPRC-MATURITY-DATE > 0 AND CIPRC-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YW' TO CIPRC-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO CIPRC-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO CIPRC-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO CIPRC-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'CIPRC EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
