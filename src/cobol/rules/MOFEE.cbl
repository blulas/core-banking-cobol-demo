      *================================================================*
      * PROGRAM  : MOFEE                                               *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : MONEY ORDERS RULE - PRICING. CALLED BY THE MONEY    *
      *            ORDERS BATCH AND ONLINE PROGRAMS.                   *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-10-05 EWS  ORIGINAL                                       *
      * 1992-04-01 GAF  FACE TABLE EXPANDED REQ6930                    *
      * 1994-11-17 DPS  PERFORMANCE - OUTSTAND READ AHEAD INC0651      *
      * 1995-02-11 CRP  PAID NOTICE TEXT PER LEGAL CHG0685             *
      * 1996-06-07 EWS  ESCHEAT EXCEPTIONS TO OPS QUEUE CHG5182        *
      * 1997-02-25 RJT  VOIDED STATUS CODES ADDED REQ1066              *
      * 2000-04-10 JLM  ESCHEAT TABLE EXPANDED CHG0513                 *
      * 2004-08-13 LMB  OUTSTAND STATUS CODES ADDED INC8523            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOFEE.
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
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 30.
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
      * WS-VOIDED-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOIDED-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000551000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000690500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000650000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000018000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006600000000433000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011600000000204500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011850000000016000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012100000000267500'.
       01  WS-VOIDED-BAND-TBL REDEFINES WS-VOIDED-BAND-VALUES.
           05  WS-VOIDED-BAND-ROW OCCURS 8 TIMES.
               10  WS-VOIDED-BAND-LOW      PIC 9(09)V99.
               10  WS-VOIDED-BAND-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ESCHEAT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ESCHEAT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000099500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000159500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000406500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000400000000040500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001400000000255000'.
       01  WS-ESCHEAT-SCHED-TBL REDEFINES WS-ESCHEAT-SCHED-VALUES.
           05  WS-ESCHEAT-SCHED-ROW OCCURS 5 TIMES.
               10  WS-ESCHEAT-SCHED-LOW    PIC 9(09)V99.
               10  WS-ESCHEAT-SCHED-RATE   PIC 9(03)V9(06).
       COPY DATEPARM.
       COPY BANKCTL.

       LINKAGE SECTION.
       COPY MOFEEP.

       PROCEDURE DIVISION USING MOFEE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-AGE-LAST-ACT
           PERFORM 2010-FLAG-FACE
           PERFORM 2020-EDIT-REF-NO
           PERFORM 2030-EDIT-REF-NO
           PERFORM 2040-SELECT-FEE-PCT
           PERFORM 2050-SPLIT-FEE
           PERFORM 2060-ACCUM-ITEM-TYPE
           PERFORM 2070-SPLIT-FACE
           PERFORM 2080-SELECT-DISCOUNT
           PERFORM 2090-SET-REASON
           PERFORM 2100-ACCUM-REASON
           PERFORM 2110-SPLIT-OUTSTAND
           PERFORM 2120-EDIT-REF-NO
           PERFORM 2130-DESCRIBE-CHANNEL
           PERFORM 2140-COUNT-ITEM
           PERFORM 2150-AGE-LAST-ACT
           PERFORM 2160-EDIT-NAME
           PERFORM 2170-ACCRUE-PAID
           PERFORM 2180-PRORATE-FACE
           PERFORM 2190-CHECK-LAST-STMT
           PERFORM 2200-ASSESS-OVERRIDE
           PERFORM 2210-SET-REASON
           PERFORM 2220-ASSESS-OVERRIDE
           PERFORM 2230-ACCUM-ITEM-TYPE
           PERFORM 2240-PRORATE-PAID
           PERFORM 2250-CHECK-LAST-STMT
           PERFORM 2260-AGE-EFFECTIVE
           PERFORM 2270-DESCRIBE-ITEM-TYPE
           PERFORM 2280-ASSESS-OVERRIDE
           PERFORM 2290-AGE-LAST-STMT
           PERFORM 2300-DESCRIBE-ITEM-TYPE
           PERFORM 2310-SELECT-DISCOUNT
           PERFORM 2320-COUNT-RETRY
           PERFORM 2330-EDIT-NAME
           PERFORM 2340-COUNT-ITEM
           PERFORM 2350-DESCRIBE-REASON
           PERFORM 2360-ASSESS-OVERRIDE
           PERFORM 2370-PRORATE-FACE
           PERFORM 2380-SELECT-FEE-PCT
           PERFORM 2390-LIMIT-FEE
           PERFORM 2400-ACCRUE-FEE
           PERFORM 2410-COUNT-RETRY
           PERFORM 2420-SET-CHANNEL
           PERFORM 2430-AGE-EFFECTIVE
           PERFORM 2440-ACCUM-REASON
           PERFORM 2450-SELECT-FEE-PCT
           PERFORM 2460-SELECT-DISCOUNT
           PERFORM 2470-SELECT-DISCOUNT
           PERFORM 2480-PRORATE-ESCHEAT
           PERFORM 2490-ACCRUE-OUTSTAND
           PERFORM 2500-SPLIT-FACE
           PERFORM 2510-SET-CHANNEL
           PERFORM 2520-LIMIT-PAID
           PERFORM 2530-FLAG-FACE
           PERFORM 2540-PRORATE-FEE
           PERFORM 2550-SET-ITEM-TYPE
           PERFORM 2560-SPLIT-OUTSTAND
           PERFORM 2570-CHECK-LAST-ACT
           PERFORM 2580-ACCRUE-PAID
           PERFORM 2590-SELECT-DISCOUNT
           PERFORM 2600-PRORATE-PAID
           PERFORM 2610-PRORATE-OUTSTAND
           PERFORM 2620-EDIT-NAME
           PERFORM 2630-COUNT-RETRY
           PERFORM 2640-COUNT-ITEM
           PERFORM 2650-ASSESS-PREFERRED
           PERFORM 2660-ACCRUE-FACE
           PERFORM 2670-ACCRUE-FACE
           PERFORM 2680-SELECT-DISCOUNT
           PERFORM 2690-ACCUM-PAYEE-TYPE
           PERFORM 2700-ACCRUE-OUTSTAND
           PERFORM 2710-CHECK-LAST-PAY
           PERFORM 2720-FLAG-ITEM
           PERFORM 2730-SELECT-FEE-PCT
           PERFORM 2740-CHECK-LAST-STMT
           PERFORM 2750-SET-ITEM-TYPE
           PERFORM 2760-ACCRUE-FEE-PCT
           PERFORM 2770-FLAG-OUTSTAND
           PERFORM 2780-SPLIT-ESCHEAT
           PERFORM 2790-ACCUM-ITEM-TYPE
           PERFORM 2800-ACCUM-ITEM-TYPE
           PERFORM 2810-LIMIT-FACE
           PERFORM 2820-SELECT-DISCOUNT
           PERFORM 2830-COUNT-ITEM
           PERFORM 2840-ASSESS-STOP
           PERFORM 2850-AGE-LAST-ACT
           PERFORM 2860-ASSESS-OVERRIDE
           PERFORM 9000-SET-RETURN-CODE
           GOBACK.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE ZERO TO MOFEE-RETURN-CODE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2000-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF MOFEE-LAST-ACT-DATE > 0 AND MOFEE-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MOFEE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FACE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2010-FLAG-FACE.
           IF MOFEE-FACE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MOFEE-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E187 REF NO REQUIRED
      * E551 ESCHEAT AMT NEGATIVE
      * E485 ESCHEAT AMT OVER 10000
      * E015 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2020-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MOFEE-REF-NO = SPACES
                   MOVE 'E187' TO WS-REJECT-CODE
               WHEN MOFEE-ESCHEAT-AMT < 0
                   MOVE 'E551' TO WS-REJECT-CODE
               WHEN MOFEE-ESCHEAT-AMT > 10000.00
                   MOVE 'E485' TO WS-REJECT-CODE
               WHEN MOFEE-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E015' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E862 REF NO REQUIRED
      * E398 OUTSTAND AMT NEGATIVE
      * E964 OUTSTAND AMT OVER 5000
      * E403 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       2030-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MOFEE-REF-NO = SPACES
                   MOVE 'E862' TO WS-REJECT-CODE
               WHEN MOFEE-OUTSTAND-AMT < 0
                   MOVE 'E398' TO WS-REJECT-CODE
               WHEN MOFEE-OUTSTAND-AMT > 5000.00
                   MOVE 'E964' TO WS-REJECT-CODE
               WHEN MOFEE-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E403' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ESCHEAT SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2040-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ESCHEAT-SCHED-RATE(1) TO MOFEE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-ESCHEAT-SCHED-LOW(WS-TX)
                   MOVE WS-ESCHEAT-SCHED-RATE(WS-TX) TO MOFEE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MOFEE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MOFEE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2050-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MOFEE-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = MOFEE-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE MOFEE-ESCHEAT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ESCHEAT AMT BY ITEM TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2060-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MOFEE-ITEM-TYPE-CD
                   ADD MOFEE-ESCHEAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MOFEE-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MOFEE-ESCHEAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FACE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2070-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = MOFEE-FACE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = MOFEE-FACE-AMT - (WS-WORK-AMT * 3)
           COMPUTE MOFEE-ESCHEAT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ESCHEAT SCHED TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2080-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ESCHEAT-SCHED-RATE(1) TO MOFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MOFEE-PAID-AMT NOT < WS-ESCHEAT-SCHED-LOW(WS-TX)
                   MOVE WS-ESCHEAT-SCHED-RATE(WS-TX) TO MOFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MOFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MOFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> L WHEN LAST ACT DATE IS PAST
      * L -> P WHEN LAST ACT DATE IS PAST
      * P -> S WHEN LAST ACT DATE IS PAST
      * S -> T WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2090-SET-REASON.
           EVALUATE MOFEE-REASON-CD
               WHEN 'B'
                   IF MOFEE-LAST-ACT-DATE > 0 AND MOFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO MOFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF MOFEE-LAST-ACT-DATE > 0 AND MOFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO MOFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF MOFEE-LAST-ACT-DATE > 0 AND MOFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MOFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MOFEE-LAST-ACT-DATE > 0 AND MOFEE-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MOFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2100-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MOFEE-REASON-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MOFEE-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2110-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = MOFEE-OUTSTAND-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MOFEE-OUTSTAND-AMT - (WS-WORK-AMT *
               4)
           COMPUTE MOFEE-PAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E826 REF NO REQUIRED
      * E524 FEE AMT NEGATIVE
      * E375 FEE AMT OVER 50000
      * E449 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       2120-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MOFEE-REF-NO = SPACES
                   MOVE 'E826' TO WS-REJECT-CODE
               WHEN MOFEE-FEE-AMT < 0
                   MOVE 'E524' TO WS-REJECT-CODE
               WHEN MOFEE-FEE-AMT > 50000.00
                   MOVE 'E375' TO WS-REJECT-CODE
               WHEN MOFEE-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E449' TO WS-REJECT-CODE
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
       2130-DESCRIBE-CHANNEL.
           EVALUATE MOFEE-CHANNEL-CD
               WHEN 'DR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'DX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2140-COUNT-ITEM.
           IF MOFEE-ITEM-CNT < 9999999
               ADD 1 TO MOFEE-ITEM-CNT
           END-IF
           IF MOFEE-ITEM-CNT NOT < 12
               MOVE 'Y' TO MOFEE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2150-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF MOFEE-LAST-ACT-DATE > 0 AND MOFEE-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MOFEE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MOFEE-PAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MOFEE-PAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MOFEE-PAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MOFEE-PAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MOFEE-PAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E702 NAME REQUIRED
      * E108 WORK AMT NEGATIVE
      * E274 WORK AMT OVER 10000
      * E078 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2160-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MOFEE-NAME = SPACES
                   MOVE 'E702' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E108' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E274' TO WS-REJECT-CODE
               WHEN MOFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E078' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2170-ACCRUE-PAID.
           IF MOFEE-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MOFEE-PAID-AMT * MOFEE-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MOFEE-PAID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FACE AMT.
      *----------------------------------------------------------------*
       2180-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MOFEE-FACE-AMT ROUNDED =
                   MOFEE-PAID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MOFEE-FACE-AMT
           END-IF
           ADD MOFEE-FACE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2190-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF MOFEE-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MOFEE-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO MOFEE-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ESCHEAT FEE ON THE FACE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2200-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN MOFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO MOFEE-ESCHEAT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MOFEE-STOP-FLAG = 'Y'
                   COMPUTE MOFEE-ESCHEAT-AMT ROUNDED = MOFEE-FACE-AMT *
                       MOFEE-DISCOUNT / 200
               WHEN MOFEE-ITEM-TYPE-CD = 'B'
                   MOVE 15.00 TO MOFEE-ESCHEAT-AMT
               WHEN MOFEE-ITEM-TYPE-CD = 'H'
                   MOVE 10.00 TO MOFEE-ESCHEAT-AMT
               WHEN OTHER
                   COMPUTE MOFEE-ESCHEAT-AMT ROUNDED = MOFEE-FACE-AMT *
                       MOFEE-DISCOUNT / 100
           END-EVALUATE
           IF MOFEE-ESCHEAT-AMT > 0 AND MOFEE-ESCHEAT-AMT < 2.00
               MOVE 2.00 TO MOFEE-ESCHEAT-AMT
           END-IF
           IF MOFEE-ESCHEAT-AMT > 35.00
               MOVE 35.00 TO MOFEE-ESCHEAT-AMT
           END-IF
           ADD MOFEE-ESCHEAT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> L WHEN LAST PAY DATE IS PAST
      * L -> P WHEN LAST PAY DATE IS PAST
      * P -> S WHEN LAST PAY DATE IS PAST
      * S -> T WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2210-SET-REASON.
           EVALUATE MOFEE-REASON-CD
               WHEN 'B'
                   IF MOFEE-LAST-PAY-DATE > 0 AND MOFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO MOFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF MOFEE-LAST-PAY-DATE > 0 AND MOFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO MOFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF MOFEE-LAST-PAY-DATE > 0 AND MOFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MOFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MOFEE-LAST-PAY-DATE > 0 AND MOFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MOFEE-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE OUTSTAND AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       2220-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN MOFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MOFEE-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = MOFEE-OUTSTAND-AMT *
                       MOFEE-DISCOUNT / 200
               WHEN MOFEE-CHANNEL-CD = 'DR'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN MOFEE-CHANNEL-CD = 'DX'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = MOFEE-OUTSTAND-AMT *
                       MOFEE-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE ESCHEAT AMT BY ITEM TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2230-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MOFEE-ITEM-TYPE-CD
                   ADD MOFEE-ESCHEAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MOFEE-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MOFEE-ESCHEAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       2240-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MOFEE-PAID-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MOFEE-PAID-AMT
           END-IF
           ADD MOFEE-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2250-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF MOFEE-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MOFEE-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO MOFEE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       2260-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF MOFEE-EFFECTIVE-DATE > 0 AND MOFEE-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MOFEE-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MOFEE-FACE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MOFEE-FACE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MOFEE-FACE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MOFEE-FACE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MOFEE-FACE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2270-DESCRIBE-ITEM-TYPE.
           EVALUATE MOFEE-ITEM-TYPE-CD
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE FACE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2280-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN MOFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO MOFEE-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MOFEE-STOP-FLAG = 'Y'
                   COMPUTE MOFEE-PAID-AMT ROUNDED = MOFEE-FACE-AMT *
                       MOFEE-DISCOUNT / 200
               WHEN MOFEE-PAYEE-TYPE-CD = 'CJ'
                   MOVE 15.00 TO MOFEE-PAID-AMT
               WHEN MOFEE-PAYEE-TYPE-CD = 'FY'
                   MOVE 10.00 TO MOFEE-PAID-AMT
               WHEN OTHER
                   COMPUTE MOFEE-PAID-AMT ROUNDED = MOFEE-FACE-AMT *
                       MOFEE-DISCOUNT / 100
           END-EVALUATE
           IF MOFEE-PAID-AMT > 0 AND MOFEE-PAID-AMT < 2.00
               MOVE 2.00 TO MOFEE-PAID-AMT
           END-IF
           IF MOFEE-PAID-AMT > 35.00
               MOVE 35.00 TO MOFEE-PAID-AMT
           END-IF
           ADD MOFEE-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       2290-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF MOFEE-LAST-STMT-DATE > 0 AND MOFEE-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MOFEE-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MOFEE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD MOFEE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MOFEE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MOFEE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MOFEE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2300-DESCRIBE-ITEM-TYPE.
           EVALUATE MOFEE-ITEM-TYPE-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'S'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ESCHEAT SCHED TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2310-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ESCHEAT-SCHED-RATE(1) TO MOFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MOFEE-FEE-AMT NOT < WS-ESCHEAT-SCHED-LOW(WS-TX)
                   MOVE WS-ESCHEAT-SCHED-RATE(WS-TX) TO MOFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MOFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MOFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2320-COUNT-RETRY.
           IF MOFEE-RETRY-CNT < 9999999
               ADD 1 TO MOFEE-RETRY-CNT
           END-IF
           IF MOFEE-RETRY-CNT NOT < 12
               MOVE 'Y' TO MOFEE-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E514 NAME REQUIRED
      * E669 OUTSTAND AMT NEGATIVE
      * E898 OUTSTAND AMT OVER 1000000
      * E139 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       2330-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MOFEE-NAME = SPACES
                   MOVE 'E514' TO WS-REJECT-CODE
               WHEN MOFEE-OUTSTAND-AMT < 0
                   MOVE 'E669' TO WS-REJECT-CODE
               WHEN MOFEE-OUTSTAND-AMT > 1000000.00
                   MOVE 'E898' TO WS-REJECT-CODE
               WHEN MOFEE-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E139' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2340-COUNT-ITEM.
           IF MOFEE-ITEM-CNT < 9999999
               ADD 1 TO MOFEE-ITEM-CNT
           END-IF
           IF MOFEE-ITEM-CNT NOT < 5
               MOVE 'Y' TO MOFEE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       2350-DESCRIBE-REASON.
           EVALUATE MOFEE-REASON-CD
               WHEN 'B'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       2360-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN MOFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO MOFEE-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MOFEE-STOP-FLAG = 'Y'
                   COMPUTE MOFEE-PAID-AMT ROUNDED = WS-WORK-AMT-2 *
                       MOFEE-FEE-PCT / 200
               WHEN MOFEE-PAYEE-TYPE-CD = 'CJ'
                   MOVE 3.00 TO MOFEE-PAID-AMT
               WHEN MOFEE-PAYEE-TYPE-CD = 'FY'
                   MOVE 12.00 TO MOFEE-PAID-AMT
               WHEN OTHER
                   COMPUTE MOFEE-PAID-AMT ROUNDED = WS-WORK-AMT-2 *
                       MOFEE-FEE-PCT / 100
           END-EVALUATE
           IF MOFEE-PAID-AMT > 0 AND MOFEE-PAID-AMT < 10.00
               MOVE 10.00 TO MOFEE-PAID-AMT
           END-IF
           IF MOFEE-PAID-AMT > 35.00
               MOVE 35.00 TO MOFEE-PAID-AMT
           END-IF
           ADD MOFEE-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FACE AMT.
      *----------------------------------------------------------------*
       2370-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MOFEE-FACE-AMT ROUNDED =
                   MOFEE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MOFEE-FACE-AMT
           END-IF
           ADD MOFEE-FACE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE ESCHEAT SCHED TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2380-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ESCHEAT-SCHED-RATE(1) TO MOFEE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MOFEE-PAID-AMT NOT < WS-ESCHEAT-SCHED-LOW(WS-TX)
                   MOVE WS-ESCHEAT-SCHED-RATE(WS-TX) TO MOFEE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MOFEE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MOFEE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       2390-LIMIT-FEE.
           IF MOFEE-FEE-AMT < 25.00
               MOVE 25.00 TO MOFEE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MOFEE-FEE-AMT > 10000.00
               MOVE 10000.00 TO MOFEE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2400-ACCRUE-FEE.
           IF MOFEE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MOFEE-FEE-AMT * MOFEE-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MOFEE-PAID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       2410-COUNT-RETRY.
           IF MOFEE-RETRY-CNT < 9999999
               ADD 1 TO MOFEE-RETRY-CNT
           END-IF
           IF MOFEE-RETRY-CNT NOT < 10
               MOVE 'Y' TO MOFEE-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * DR -> DX WHEN LAST PAY DATE IS PAST
      * DX -> PT WHEN LAST PAY DATE IS PAST
      * PT -> RN WHEN LAST PAY DATE IS PAST
      * RN -> YD WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2420-SET-CHANNEL.
           EVALUATE MOFEE-CHANNEL-CD
               WHEN 'DR'
                   IF MOFEE-LAST-PAY-DATE > 0 AND MOFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO MOFEE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DX'
                   IF MOFEE-LAST-PAY-DATE > 0 AND MOFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO MOFEE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF MOFEE-LAST-PAY-DATE > 0 AND MOFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RN' TO MOFEE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RN'
                   IF MOFEE-LAST-PAY-DATE > 0 AND MOFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YD' TO MOFEE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2430-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF MOFEE-EFFECTIVE-DATE > 0 AND MOFEE-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MOFEE-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MOFEE-PAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD MOFEE-PAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MOFEE-PAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MOFEE-PAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MOFEE-PAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE OUTSTAND AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2440-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MOFEE-REASON-CD
                   ADD MOFEE-OUTSTAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MOFEE-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MOFEE-OUTSTAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ESCHEAT SCHED TIER FOR THE OUTSTAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2450-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ESCHEAT-SCHED-RATE(1) TO MOFEE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MOFEE-OUTSTAND-AMT NOT < WS-ESCHEAT-SCHED-LOW(WS-TX)
                   MOVE WS-ESCHEAT-SCHED-RATE(WS-TX) TO MOFEE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MOFEE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MOFEE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ESCHEAT SCHED TIER FOR THE FACE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2460-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ESCHEAT-SCHED-RATE(1) TO MOFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MOFEE-FACE-AMT NOT < WS-ESCHEAT-SCHED-LOW(WS-TX)
                   MOVE WS-ESCHEAT-SCHED-RATE(WS-TX) TO MOFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MOFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MOFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED BAND TIER FOR THE OUTSTAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2470-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-BAND-RATE(1) TO MOFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF MOFEE-OUTSTAND-AMT NOT < WS-VOIDED-BAND-LOW(WS-TX)
                   MOVE WS-VOIDED-BAND-RATE(WS-TX) TO MOFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MOFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MOFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ESCHEAT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2480-PRORATE-ESCHEAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MOFEE-ESCHEAT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2490-ACCRUE-OUTSTAND.
           IF MOFEE-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MOFEE-OUTSTAND-AMT * MOFEE-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MOFEE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FACE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2500-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = MOFEE-FACE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MOFEE-FACE-AMT - (WS-WORK-AMT * 12)
           COMPUTE MOFEE-OUTSTAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * DR -> DX WHEN LAST PAY DATE IS PAST
      * DX -> PT WHEN LAST PAY DATE IS PAST
      * PT -> RN WHEN LAST PAY DATE IS PAST
      * RN -> YD WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2510-SET-CHANNEL.
           EVALUATE MOFEE-CHANNEL-CD
               WHEN 'DR'
                   IF MOFEE-LAST-PAY-DATE > 0 AND MOFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO MOFEE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DX'
                   IF MOFEE-LAST-PAY-DATE > 0 AND MOFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO MOFEE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF MOFEE-LAST-PAY-DATE > 0 AND MOFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RN' TO MOFEE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RN'
                   IF MOFEE-LAST-PAY-DATE > 0 AND MOFEE-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YD' TO MOFEE-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAID YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       2520-LIMIT-PAID.
           IF MOFEE-PAID-YTD < 50.00
               MOVE 50.00 TO MOFEE-PAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MOFEE-PAID-YTD > 2500.00
               MOVE 2500.00 TO MOFEE-PAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       2530-FLAG-FACE.
           IF MOFEE-FACE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MOFEE-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       2540-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MOFEE-FEE-AMT ROUNDED =
                   MOFEE-PAID-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MOFEE-FEE-AMT
           END-IF
           ADD MOFEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * B -> H WHEN LAST STMT DATE IS PAST
      * H -> S WHEN LAST STMT DATE IS PAST
      * S -> T WHEN LAST STMT DATE IS PAST
      * T -> B WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2550-SET-ITEM-TYPE.
           EVALUATE MOFEE-ITEM-TYPE-CD
               WHEN 'B'
                   IF MOFEE-LAST-STMT-DATE > 0 AND MOFEE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'H' TO MOFEE-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF MOFEE-LAST-STMT-DATE > 0 AND MOFEE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'S' TO MOFEE-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MOFEE-LAST-STMT-DATE > 0 AND MOFEE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'T' TO MOFEE-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF MOFEE-LAST-STMT-DATE > 0 AND MOFEE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'B' TO MOFEE-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2560-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE MOFEE-OUTSTAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2570-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF MOFEE-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MOFEE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO MOFEE-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2580-ACCRUE-PAID.
           IF MOFEE-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MOFEE-PAID-AMT * MOFEE-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ESCHEAT SCHED TIER FOR THE PAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2590-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ESCHEAT-SCHED-RATE(1) TO MOFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MOFEE-PAID-YTD NOT < WS-ESCHEAT-SCHED-LOW(WS-TX)
                   MOVE WS-ESCHEAT-SCHED-RATE(WS-TX) TO MOFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MOFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MOFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       2600-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MOFEE-PAID-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OUTSTAND AMT.
      *----------------------------------------------------------------*
       2610-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MOFEE-OUTSTAND-AMT ROUNDED =
                   MOFEE-PAID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MOFEE-OUTSTAND-AMT
           END-IF
           ADD MOFEE-OUTSTAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E418 NAME REQUIRED
      * E212 WORK AMT 2 NEGATIVE
      * E186 WORK AMT 2 OVER 250000
      * E857 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       2620-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MOFEE-NAME = SPACES
                   MOVE 'E418' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E212' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E186' TO WS-REJECT-CODE
               WHEN MOFEE-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E857' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2630-COUNT-RETRY.
           IF MOFEE-RETRY-CNT < 9999999
               ADD 1 TO MOFEE-RETRY-CNT
           END-IF
           IF MOFEE-RETRY-CNT NOT < 12
               MOVE 'Y' TO MOFEE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       2640-COUNT-ITEM.
           IF MOFEE-ITEM-CNT < 9999999
               ADD 1 TO MOFEE-ITEM-CNT
           END-IF
           IF MOFEE-ITEM-CNT NOT < 6
               MOVE 'Y' TO MOFEE-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FACE FEE ON THE PAID AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       2650-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN MOFEE-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO MOFEE-FACE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MOFEE-OVERRIDE-FLAG = 'Y'
                   COMPUTE MOFEE-FACE-AMT ROUNDED = MOFEE-PAID-AMT *
                       MOFEE-DISCOUNT / 200
               WHEN MOFEE-REASON-CD = 'B'
                   MOVE 7.00 TO MOFEE-FACE-AMT
               WHEN MOFEE-REASON-CD = 'L'
                   MOVE 20.00 TO MOFEE-FACE-AMT
               WHEN OTHER
                   COMPUTE MOFEE-FACE-AMT ROUNDED = MOFEE-PAID-AMT *
                       MOFEE-DISCOUNT / 100
           END-EVALUATE
           IF MOFEE-FACE-AMT > 0 AND MOFEE-FACE-AMT < 2.00
               MOVE 2.00 TO MOFEE-FACE-AMT
           END-IF
           IF MOFEE-FACE-AMT > 25.00
               MOVE 25.00 TO MOFEE-FACE-AMT
           END-IF
           ADD MOFEE-FACE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2660-ACCRUE-FACE.
           IF MOFEE-FACE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MOFEE-FACE-AMT * MOFEE-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2670-ACCRUE-FACE.
           IF MOFEE-FACE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MOFEE-FACE-AMT * MOFEE-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO MOFEE-OUTSTAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED BAND TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2680-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-BAND-RATE(1) TO MOFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF MOFEE-PAID-AMT NOT < WS-VOIDED-BAND-LOW(WS-TX)
                   MOVE WS-VOIDED-BAND-RATE(WS-TX) TO MOFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MOFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MOFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY PAYEE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2690-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MOFEE-PAYEE-TYPE-CD
                   ADD MOFEE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MOFEE-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MOFEE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2700-ACCRUE-OUTSTAND.
           IF MOFEE-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MOFEE-OUTSTAND-AMT * MOFEE-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MOFEE-PAID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2710-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MOFEE-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MOFEE-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO MOFEE-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       2720-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ESCHEAT SCHED TIER FOR THE OUTSTAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2730-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ESCHEAT-SCHED-RATE(1) TO MOFEE-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MOFEE-OUTSTAND-AMT NOT < WS-ESCHEAT-SCHED-LOW(WS-TX)
                   MOVE WS-ESCHEAT-SCHED-RATE(WS-TX) TO MOFEE-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MOFEE-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MOFEE-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       2740-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF MOFEE-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MOFEE-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO MOFEE-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * B -> H WHEN LAST STMT DATE IS PAST
      * H -> S WHEN LAST STMT DATE IS PAST
      * S -> T WHEN LAST STMT DATE IS PAST
      * T -> B WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       2750-SET-ITEM-TYPE.
           EVALUATE MOFEE-ITEM-TYPE-CD
               WHEN 'B'
                   IF MOFEE-LAST-STMT-DATE > 0 AND MOFEE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'H' TO MOFEE-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF MOFEE-LAST-STMT-DATE > 0 AND MOFEE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'S' TO MOFEE-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MOFEE-LAST-STMT-DATE > 0 AND MOFEE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'T' TO MOFEE-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF MOFEE-LAST-STMT-DATE > 0 AND MOFEE-LAST-STMT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'B' TO MOFEE-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       2760-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * MOFEE-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO MOFEE-ESCHEAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       2770-FLAG-OUTSTAND.
           IF MOFEE-OUTSTAND-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MOFEE-OUTSTAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ESCHEAT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       2780-SPLIT-ESCHEAT.
           COMPUTE WS-WORK-AMT = MOFEE-ESCHEAT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MOFEE-ESCHEAT-AMT - (WS-WORK-AMT * 4)
           COMPUTE MOFEE-PAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID YTD BY ITEM TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2790-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MOFEE-ITEM-TYPE-CD
                   ADD MOFEE-PAID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MOFEE-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MOFEE-PAID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY ITEM TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       2800-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MOFEE-ITEM-TYPE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MOFEE-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       2810-LIMIT-FACE.
           IF MOFEE-FACE-AMT < 0.00
               MOVE 0.00 TO MOFEE-FACE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MOFEE-FACE-AMT > 50000.00
               MOVE 50000.00 TO MOFEE-FACE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ESCHEAT SCHED TIER FOR THE PAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       2820-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ESCHEAT-SCHED-RATE(1) TO MOFEE-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MOFEE-PAID-YTD NOT < WS-ESCHEAT-SCHED-LOW(WS-TX)
                   MOVE WS-ESCHEAT-SCHED-RATE(WS-TX) TO MOFEE-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MOFEE-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MOFEE-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       2830-COUNT-ITEM.
           IF MOFEE-ITEM-CNT < 9999999
               ADD 1 TO MOFEE-ITEM-CNT
           END-IF
           IF MOFEE-ITEM-CNT NOT < 12
               MOVE 'Y' TO MOFEE-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND FEE ON THE FEE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       2840-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MOFEE-STOP-FLAG = 'Y'
                   MOVE ZERO TO MOFEE-OUTSTAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MOFEE-OVERRIDE-FLAG = 'Y'
                   COMPUTE MOFEE-OUTSTAND-AMT ROUNDED = MOFEE-FEE-AMT *
                       MOFEE-FEE-PCT / 200
               WHEN MOFEE-PAYEE-TYPE-CD = 'CJ'
                   MOVE 5.00 TO MOFEE-OUTSTAND-AMT
               WHEN MOFEE-PAYEE-TYPE-CD = 'FY'
                   MOVE 7.00 TO MOFEE-OUTSTAND-AMT
               WHEN OTHER
                   COMPUTE MOFEE-OUTSTAND-AMT ROUNDED = MOFEE-FEE-AMT *
                       MOFEE-FEE-PCT / 100
           END-EVALUATE
           IF MOFEE-OUTSTAND-AMT > 0 AND MOFEE-OUTSTAND-AMT < 10.00
               MOVE 10.00 TO MOFEE-OUTSTAND-AMT
           END-IF
           IF MOFEE-OUTSTAND-AMT > 150.00
               MOVE 150.00 TO MOFEE-OUTSTAND-AMT
           END-IF
           ADD MOFEE-OUTSTAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       2850-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF MOFEE-LAST-ACT-DATE > 0 AND MOFEE-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MOFEE-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MOFEE-PAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MOFEE-PAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MOFEE-PAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MOFEE-PAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MOFEE-PAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE PAID YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       2860-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN MOFEE-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MOFEE-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = MOFEE-PAID-YTD *
                       MOFEE-FEE-PCT / 200
               WHEN MOFEE-REASON-CD = 'B'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN MOFEE-REASON-CD = 'L'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = MOFEE-PAID-YTD *
                       MOFEE-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

       9000-SET-RETURN-CODE.
           EVALUATE TRUE
               WHEN WS-REJECT-FLAG = 'Y'
                   MOVE 08 TO MOFEE-RETURN-CODE
               WHEN WS-EXCEPT-COUNT > 0 OR WS-CAPPED-COUNT > 0
                   MOVE 04 TO MOFEE-RETURN-CODE
               WHEN OTHER
                   MOVE 00 TO MOFEE-RETURN-CODE
           END-EVALUATE.

       8100-WRITE-EXCEPTION.
           DISPLAY 'MOFEE EXCEPTION ' WS-EXC-SEVERITY ' ' WS-EXC-AMOUNT.
