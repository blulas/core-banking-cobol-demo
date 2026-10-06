      *================================================================*
      * PROGRAM  : CNOSETL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : COIN AND CURRENCY - SETTLEMENT. READS THE CCO       *
      *            TRANSACTIONS, UPDATES THE CCO MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. BRANCH COIN AND CURRENCY ORDERS *
      *            AND ARMORED CARRIER SHIPMENT                        *
      * INPUT    : CCOMST (KSDS), CCOTRN, BANKCTL                      *
      * OUTPUT   : CCOOUT, CNOSERPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CNCAL, CNFEE, CNPEN     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-11-15 PQR  ORIGINAL                                       *
      * 1997-07-27 MKL  AUDIT FINDING - SHIPMENT CONTROL TOTALS REQ5846*
      * 1998-07-04 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-10-12 CRP  NEW FEE REPORT COLUMNS PRB1238                 *
      * 2001-04-20 EWS  CURRENCY NOTICE TEXT PER LEGAL INC2425         *
      * 2001-10-23 CRP  PERFORMANCE - VARIANCE READ AHEAD REQ6534      *
      * 2002-12-15 GAF  VARIANCE LIMIT REVISED INC1425                 *
      * 2002-07-12 TWH  VARIANCE LIMIT REVISED PRB0261                 *
      * 2002-01-07 EWS  STRAP NOTICE TEXT PER LEGAL PRB4319            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CNOSETL.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "CCOMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CCOM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "CCOTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "CCOOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "CNOSERPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY CCOMST.
       FD  TRAN-FILE.
       COPY CCOTRN.
       FD  OUT-FILE.
       01  OUT-REC                         PIC X(200).
       FD  REPORT-FILE.
       01  REPORT-REC                      PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-MST-STATUS               PIC X(02).
           05  WS-TRN-STATUS               PIC X(02).
           05  WS-OUT-STATUS               PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                             VALUE 'Y'.
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
      * WS-VARIANCE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VARIANCE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000398000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000363000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000610500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015050000000843500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017550000000120500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018050000000850000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018550000000844000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028550000000727500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00038550000000337500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00048550000000081000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00049550000000593000'.
       01  WS-VARIANCE-TIER-TBL REDEFINES WS-VARIANCE-TIER-VALUES.
           05  WS-VARIANCE-TIER-ROW OCCURS 11 TIMES.
               10  WS-VARIANCE-TIER-LOW    PIC 9(09)V99.
               10  WS-VARIANCE-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COIN-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COIN-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000838000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000578000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000751500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000106500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010800000000717500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015800000000421500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015850000000649000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016850000000467000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016900000000881500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017000000000062000'.
       01  WS-COIN-SCHED-TBL REDEFINES WS-COIN-SCHED-VALUES.
           05  WS-COIN-SCHED-ROW OCCURS 10 TIMES.
               10  WS-COIN-SCHED-LOW       PIC 9(09)V99.
               10  WS-COIN-SCHED-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CNOSETL  COIN AND CURRENCY'.
           05  FILLER                      PIC X(16)
                                           VALUE ' BUSINESS DATE '.
           05  RH-DATE                     PIC 9(08).
           05  FILLER                      PIC X(68) VALUE SPACES.
       01  WS-RPT-DETAIL.
           05  RD-KEY                      PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-CODE                     PIC X(02).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-AMT                      PIC -(11)9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-REJECT                   PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-DESC                     PIC X(24).
           05  FILLER                      PIC X(68) VALUE SPACES.
       01  WS-RPT-TOTAL.
           05  RT-LABEL                    PIC X(40).
           05  RT-COUNT                    PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  RT-AMT                      PIC -(13)9.99.
           05  FILLER                      PIC X(65) VALUE SPACES.
       COPY DATEPARM.
       COPY BANKCTL.
       COPY ABNDPARM.
       COPY CNCALP.
       COPY CNFEEP.
       COPY CNPENP.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-READ-TRAN
           PERFORM UNTIL WS-EOF
               PERFORM 2100-PROCESS-ONE
               PERFORM 2000-READ-TRAN
           END-PERFORM
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           OPEN I-O MASTER-FILE
           IF WS-MST-STATUS NOT = '00'
               MOVE 'CCOMST' TO ABND-FILE-NAME
               MOVE WS-MST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           OPEN INPUT TRAN-FILE
           OPEN OUTPUT OUT-FILE REPORT-FILE
           MOVE CTL-BUSINESS-DATE TO RH-DATE
           WRITE REPORT-REC FROM WS-RPT-HEAD.

       2000-READ-TRAN.
           READ TRAN-FILE
               AT END SET WS-EOF TO TRUE
               NOT AT END ADD 1 TO WS-READ-COUNT
           END-READ.

       2100-PROCESS-ONE.
           MOVE SPACES TO WS-REJECT-CODE
           MOVE 'N' TO WS-REJECT-FLAG
           MOVE CCOT-KEY TO CCOM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SET-CHANNEL
               PERFORM 3010-ASSESS-REVERSAL
               PERFORM 3020-ACCRUE-SHIPMENT
               PERFORM 3030-ACCUM-CHANNEL
               PERFORM 3040-AGE-NEXT-DUE
               PERFORM 3050-SPLIT-VARIANCE
               PERFORM 3060-COUNT-SEQ
               PERFORM 3070-AGE-POST
               PERFORM 3080-ACCUM-DENOM
               PERFORM 3090-LIMIT-COIN
               PERFORM 3100-CHECK-NOTICE
               PERFORM 3110-FLAG-CURRENCY
               PERFORM 3120-SELECT-RATE
               PERFORM 3130-LIMIT-ITEM
               PERFORM 3140-ACCRUE-VARIANCE
               PERFORM 3150-FLAG-VARIANCE
               PERFORM 3160-AGE-LAST-PAY
               PERFORM 3170-SELECT-FEE-PCT
               PERFORM 3180-AGE-EFFECTIVE
               PERFORM 3190-ACCUM-DENOM
               PERFORM 3200-SET-CHANNEL
               PERFORM 3210-SPLIT-COIN
               PERFORM 3220-COUNT-RETRY
               PERFORM 3230-SET-CHANNEL
               PERFORM 3240-DESCRIBE-REASON
               PERFORM 3250-ASSESS-HOLD
               PERFORM 3260-ACCUM-SOURCE
               PERFORM 3270-ACCUM-DENOM
               PERFORM 3280-SET-CHANNEL
               PERFORM 3290-LIMIT-CURRENCY
               PERFORM 3300-SET-CARRIER
               PERFORM 3310-SET-CHANNEL
               PERFORM 3320-COUNT-CYCLE
               PERFORM 3330-AGE-LAST-PAY
               PERFORM 3340-DESCRIBE-CARRIER
               PERFORM 3350-AGE-OPEN
               PERFORM 3360-COUNT-CYCLE
               PERFORM 3370-SELECT-PENALTY-PCT
               PERFORM 3380-CHECK-EFFECTIVE
               PERFORM 3390-ACCRUE-FEE
               PERFORM 3400-SET-SOURCE
               PERFORM 3410-COUNT-CYCLE
               PERFORM 3420-PRORATE-CURRENCY
               PERFORM 3430-CHECK-NEXT-REVIEW
               PERFORM 3440-AGE-MATURITY
               PERFORM 3450-ASSESS-EXEMPT
               PERFORM 3460-FLAG-COIN
               PERFORM 3470-PRORATE-COIN
               PERFORM 3480-ASSESS-REVERSAL
               PERFORM 3490-CHECK-OPEN
               PERFORM 3500-AGE-LAST-ACT
               PERFORM 3510-ACCUM-SOURCE
               PERFORM 3520-COUNT-NOTICE
               PERFORM 3530-PRORATE-VARIANCE
               PERFORM 3540-PRORATE-FEE
               PERFORM 3550-ACCUM-CARRIER
               PERFORM 3560-CHECK-NOTICE
               PERFORM 3570-ACCRUE-FEE
               PERFORM 3580-EDIT-ADDR
               PERFORM 3590-AGE-NOTICE
               PERFORM 3600-EDIT-CUST-ID
               PERFORM 3610-ACCRUE-COIN
               PERFORM 3620-COUNT-ITEM
               PERFORM 3630-COUNT-NOTICE
               PERFORM 3640-SET-DENOM
               PERFORM 3650-FLAG-CURRENCY
               PERFORM 3660-DESCRIBE-CHANNEL
               PERFORM 3670-PRORATE-VARIANCE
               PERFORM 3680-EDIT-ADDR
               PERFORM 3690-SELECT-FEE-PCT
               PERFORM 3700-ASSESS-AUTO-RENEW
               PERFORM 3710-DESCRIBE-REASON
               PERFORM 3720-COUNT-CYCLE
               PERFORM 3730-FLAG-FEE
               PERFORM 3740-EDIT-CUST-ID
               PERFORM 3750-DESCRIBE-DENOM
               PERFORM 3760-COUNT-NOTICE
               PERFORM 3770-PRORATE-CURRENCY
               PERFORM 3780-LIMIT-STRAP
               PERFORM 3790-LIMIT-SHIPMENT
               PERFORM 3800-AGE-LAST-PAY
               PERFORM 3810-COUNT-SEQ
               PERFORM 3820-ASSESS-AUTO-RENEW
               PERFORM 3830-COUNT-CYCLE
               PERFORM 3840-CHECK-OPEN
               PERFORM 3850-EDIT-NAME
               PERFORM 3860-SPLIT-SHIPMENT
               PERFORM 3870-AGE-POST
               PERFORM 3880-LIMIT-STRAP
               PERFORM 3890-LIMIT-VARIANCE
               PERFORM 3900-COUNT-CYCLE
               PERFORM 3910-DESCRIBE-CARRIER
               PERFORM 3920-SELECT-RATE
               PERFORM 3930-ASSESS-EXEMPT
               PERFORM 3940-ACCUM-SOURCE
               PERFORM 3950-ACCUM-DENOM
               PERFORM 3960-SET-STATUS
               PERFORM 3970-SELECT-FEE-PCT
               PERFORM 3980-SELECT-PENALTY-PCT
               PERFORM 3990-ACCUM-SOURCE
               PERFORM 4000-EDIT-CONTACT
               PERFORM 4010-ACCRUE-FEE-PCT
               PERFORM 4020-PRORATE-COIN
               PERFORM 4030-ACCUM-CARRIER
               PERFORM 4040-SELECT-FEE-PCT
               PERFORM 4050-LIMIT-COIN
               PERFORM 4060-COUNT-CYCLE
               PERFORM 7020-CALL-CNCAL
               PERFORM 7030-CALL-CNFEE
               PERFORM 7010-CALL-CNPEN
               REWRITE CCO-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'CCOMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM CCO-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN MATURITY DATE IS PAST
      * BAT -> MAL WHEN MATURITY DATE IS PAST
      * MAL -> TEL WHEN MATURITY DATE IS PAST
      * TEL -> ONL WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3000-SET-CHANNEL.
           EVALUATE CCOT-CHANNEL-CD
               WHEN 'BRN'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPMENT FEE ON THE SHIPMENT YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3010-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CCOM-SHIPMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-STOP-FLAG = 'Y'
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = CCOM-SHIPMENT-YTD
                       * CCOM-RATE / 200
               WHEN CCOT-TRAN-CD = 'OR'
                   MOVE 10.00 TO CCOM-SHIPMENT-AMT
               WHEN CCOT-TRAN-CD = 'SH'
                   MOVE 15.00 TO CCOM-SHIPMENT-AMT
               WHEN OTHER
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = CCOM-SHIPMENT-YTD
                       * CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-SHIPMENT-AMT > 0 AND CCOM-SHIPMENT-AMT < 2.00
               MOVE 2.00 TO CCOM-SHIPMENT-AMT
           END-IF
           IF CCOM-SHIPMENT-AMT > 75.00
               MOVE 75.00 TO CCOM-SHIPMENT-AMT
           END-IF
           ADD CCOM-SHIPMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPMENT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3020-ACCRUE-SHIPMENT.
           IF CCOT-SHIPMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-SHIPMENT-AMT * CCOM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CCOM-STRAP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3030-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOT-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3040-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VARIANCE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3050-SPLIT-VARIANCE.
           COMPUTE WS-WORK-AMT = CCOM-VARIANCE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CCOM-VARIANCE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE CCOT-VARIANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3060-COUNT-SEQ.
           IF CCOT-SEQ < 9999999
               ADD 1 TO CCOT-SEQ
           END-IF
           IF CCOT-SEQ NOT < 12
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3070-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF CCOT-POST-DATE > 0 AND CCOT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SHIPMENT AMT BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3080-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-DENOM-CD
                   ADD CCOM-SHIPMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-SHIPMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * COIN AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3090-LIMIT-COIN.
           IF CCOT-COIN-AMT < 100.00
               MOVE 100.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-COIN-AMT > 2500.00
               MOVE 2500.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CURRENCY AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3110-FLAG-CURRENCY.
           IF CCOM-CURRENCY-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CCOM-CURRENCY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VARIANCE TIER TIER FOR THE STRAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VARIANCE-TIER-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CCOM-STRAP-AMT NOT < WS-VARIANCE-TIER-LOW(WS-TX)
                   MOVE WS-VARIANCE-TIER-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3130-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VARIANCE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3140-ACCRUE-VARIANCE.
           IF CCOM-VARIANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-VARIANCE-YTD * CCOM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-CURRENCY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VARIANCE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3150-FLAG-VARIANCE.
           IF CCOM-VARIANCE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CCOM-VARIANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3160-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-CURRENCY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD CCOM-CURRENCY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CCOM-CURRENCY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CCOM-CURRENCY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-CURRENCY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE VARIANCE TIER TIER FOR THE STRAP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3170-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VARIANCE-TIER-RATE(1) TO CCOM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CCOM-STRAP-YTD NOT < WS-VARIANCE-TIER-LOW(WS-TX)
                   MOVE WS-VARIANCE-TIER-RATE(WS-TX) TO CCOM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3180-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-STRAP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CCOM-STRAP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CCOM-STRAP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOM-STRAP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-STRAP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3190-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-DENOM-CD
                   ADD CCOM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AW -> DW WHEN LAST PAY DATE IS PAST
      * DW -> GN WHEN LAST PAY DATE IS PAST
      * GN -> RS WHEN LAST PAY DATE IS PAST
      * RS -> UZ WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3200-SET-CHANNEL.
           EVALUATE CCOM-CHANNEL-CD
               WHEN 'AW'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DW' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DW'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GN' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GN'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RS' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RS'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UZ' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT COIN AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3210-SPLIT-COIN.
           COMPUTE WS-WORK-AMT = CCOM-COIN-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CCOM-COIN-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3220-COUNT-RETRY.
           IF CCOM-RETRY-CNT < 9999999
               ADD 1 TO CCOM-RETRY-CNT
           END-IF
           IF CCOM-RETRY-CNT NOT < 6
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AW -> DW WHEN NEXT DUE DATE IS PAST
      * DW -> GN WHEN NEXT DUE DATE IS PAST
      * GN -> RS WHEN NEXT DUE DATE IS PAST
      * RS -> UZ WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3230-SET-CHANNEL.
           EVALUATE CCOM-CHANNEL-CD
               WHEN 'AW'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DW' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DW'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GN' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GN'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RS' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RS'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UZ' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3240-DESCRIBE-REASON.
           EVALUATE CCOM-REASON-CD
               WHEN 'JL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VARIANCE FEE ON THE SHIPMENT YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3250-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CCOM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CCOT-VARIANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   COMPUTE CCOT-VARIANCE-AMT ROUNDED = CCOM-SHIPMENT-YTD
                       * CCOM-RATE / 200
               WHEN CCOM-DENOM-CD = 'B'
                   MOVE 5.00 TO CCOT-VARIANCE-AMT
               WHEN CCOM-DENOM-CD = 'D'
                   MOVE 7.00 TO CCOT-VARIANCE-AMT
               WHEN OTHER
                   COMPUTE CCOT-VARIANCE-AMT ROUNDED = CCOM-SHIPMENT-YTD
                       * CCOM-RATE / 100
           END-EVALUATE
           IF CCOT-VARIANCE-AMT > 0 AND CCOT-VARIANCE-AMT < 10.00
               MOVE 10.00 TO CCOT-VARIANCE-AMT
           END-IF
           IF CCOT-VARIANCE-AMT > 100.00
               MOVE 100.00 TO CCOT-VARIANCE-AMT
           END-IF
           ADD CCOT-VARIANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE COIN YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3260-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-SOURCE-CD
                   ADD CCOM-COIN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-COIN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VARIANCE YTD BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3270-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-DENOM-CD
                   ADD CCOM-VARIANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-VARIANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST ACT DATE IS PAST
      * BAT -> MAL WHEN LAST ACT DATE IS PAST
      * MAL -> TEL WHEN LAST ACT DATE IS PAST
      * TEL -> ONL WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3280-SET-CHANNEL.
           EVALUATE CCOT-CHANNEL-CD
               WHEN 'BRN'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CURRENCY AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3290-LIMIT-CURRENCY.
           IF CCOM-CURRENCY-AMT < 25.00
               MOVE 25.00 TO CCOM-CURRENCY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-CURRENCY-AMT > 5000.00
               MOVE 5000.00 TO CCOM-CURRENCY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARRIER CD:
      * J -> L WHEN NEXT DUE DATE IS PAST
      * L -> M WHEN NEXT DUE DATE IS PAST
      * M -> X WHEN NEXT DUE DATE IS PAST
      * X -> J WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3300-SET-CARRIER.
           EVALUATE CCOM-CARRIER-CD
               WHEN 'J'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST PAY DATE IS PAST
      * BAT -> MAL WHEN LAST PAY DATE IS PAST
      * MAL -> TEL WHEN LAST PAY DATE IS PAST
      * TEL -> ONL WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3310-SET-CHANNEL.
           EVALUATE CCOT-CHANNEL-CD
               WHEN 'BRN'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3320-COUNT-CYCLE.
           IF CCOM-CYCLE-CNT < 9999999
               ADD 1 TO CCOM-CYCLE-CNT
           END-IF
           IF CCOM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO CCOM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3330-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOT-SHIPMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CCOT-SHIPMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CCOT-SHIPMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOT-SHIPMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOT-SHIPMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CARRIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3340-DESCRIBE-CARRIER.
           EVALUATE CCOM-CARRIER-CD
               WHEN 'J'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3350-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CCOM-OPEN-DATE > 0 AND CCOM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-SHIPMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD CCOM-SHIPMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CCOM-SHIPMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOM-SHIPMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-SHIPMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3360-COUNT-CYCLE.
           IF CCOM-CYCLE-CNT < 9999999
               ADD 1 TO CCOM-CYCLE-CNT
           END-IF
           IF CCOM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO CCOM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COIN SCHED TIER FOR THE STRAP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3370-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-SCHED-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CCOM-STRAP-YTD NOT < WS-COIN-SCHED-LOW(WS-TX)
                   MOVE WS-COIN-SCHED-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3380-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3390-ACCRUE-FEE.
           IF CCOM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-FEE-YTD * CCOM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CCOM-VARIANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * CFR -> DTX WHEN LAST PAY DATE IS PAST
      * DTX -> EEA WHEN LAST PAY DATE IS PAST
      * EEA -> EUA WHEN LAST PAY DATE IS PAST
      * EUA -> TPB WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3400-SET-SOURCE.
           EVALUATE CCOM-SOURCE-CD
               WHEN 'CFR'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DTX' TO CCOM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DTX'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EEA' TO CCOM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EEA'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EUA' TO CCOM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EUA'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TPB' TO CCOM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3410-COUNT-CYCLE.
           IF CCOM-CYCLE-CNT < 9999999
               ADD 1 TO CCOM-CYCLE-CNT
           END-IF
           IF CCOM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO CCOM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STRAP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CURRENCY YTD.
      *----------------------------------------------------------------*
       3420-PRORATE-CURRENCY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-CURRENCY-YTD ROUNDED =
                   CCOM-STRAP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-CURRENCY-YTD
           END-IF
           ADD CCOM-CURRENCY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3430-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3440-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-VARIANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CCOM-VARIANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CCOM-VARIANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD CCOM-VARIANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-VARIANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE VARIANCE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3450-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CCOM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-STOP-FLAG = 'Y'
                   COMPUTE CCOM-FEE-YTD ROUNDED = CCOM-VARIANCE-YTD *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOM-CARRIER-CD = 'J'
                   MOVE 15.00 TO CCOM-FEE-YTD
               WHEN CCOM-CARRIER-CD = 'L'
                   MOVE 20.00 TO CCOM-FEE-YTD
               WHEN OTHER
                   COMPUTE CCOM-FEE-YTD ROUNDED = CCOM-VARIANCE-YTD *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-FEE-YTD > 0 AND CCOM-FEE-YTD < 1.00
               MOVE 1.00 TO CCOM-FEE-YTD
           END-IF
           IF CCOM-FEE-YTD > 25.00
               MOVE 25.00 TO CCOM-FEE-YTD
           END-IF
           ADD CCOM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * COIN YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3460-FLAG-COIN.
           IF CCOM-COIN-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CCOM-COIN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHIPMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COIN AMT.
      *----------------------------------------------------------------*
       3470-PRORATE-COIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOT-COIN-AMT ROUNDED =
                   CCOT-SHIPMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOT-COIN-AMT
           END-IF
           ADD CCOT-COIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE COIN FEE ON THE STRAP AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3480-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CCOM-COIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   COMPUTE CCOM-COIN-AMT ROUNDED = CCOM-STRAP-AMT *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO CCOM-COIN-AMT
               WHEN CCOT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO CCOM-COIN-AMT
               WHEN OTHER
                   COMPUTE CCOM-COIN-AMT ROUNDED = CCOM-STRAP-AMT *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-COIN-AMT > 0 AND CCOM-COIN-AMT < 1.00
               MOVE 1.00 TO CCOM-COIN-AMT
           END-IF
           IF CCOM-COIN-AMT > 50.00
               MOVE 50.00 TO CCOM-COIN-AMT
           END-IF
           ADD CCOM-COIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3490-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CCOM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3500-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE STRAP YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3510-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-SOURCE-CD
                   ADD CCOM-STRAP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-STRAP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3520-COUNT-NOTICE.
           IF CCOM-NOTICE-CNT < 9999999
               ADD 1 TO CCOM-NOTICE-CNT
           END-IF
           IF CCOM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHIPMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VARIANCE YTD.
      *----------------------------------------------------------------*
       3530-PRORATE-VARIANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-VARIANCE-YTD ROUNDED =
                   CCOT-SHIPMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOM-VARIANCE-YTD
           END-IF
           ADD CCOM-VARIANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3540-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-FEE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOM-FEE-AMT
           END-IF
           ADD CCOM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE CURRENCY AMT BY CARRIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3550-ACCUM-CARRIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-CARRIER-CD
                   ADD CCOM-CURRENCY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-CARRIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-CURRENCY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3560-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3570-ACCRUE-FEE.
           IF CCOM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-FEE-YTD * CCOM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CCOM-STRAP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E783 ADDR REQUIRED
      * E960 CURRENCY YTD NEGATIVE
      * E625 CURRENCY YTD OVER 10000
      * E565 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3580-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-ADDR = SPACES
                   MOVE 'E783' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-YTD < 0
                   MOVE 'E960' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-YTD > 10000.00
                   MOVE 'E625' TO WS-REJECT-CODE
               WHEN CCOM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E565' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NOTICE-DATE > 0 AND CCOM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOT-STRAP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CCOT-STRAP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CCOT-STRAP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CCOT-STRAP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOT-STRAP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E260 CUST ID REQUIRED
      * E700 FEE YTD NEGATIVE
      * E707 FEE YTD OVER 99999
      * E197 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3600-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CUST-ID = SPACES
                   MOVE 'E260' TO WS-REJECT-CODE
               WHEN CCOM-FEE-YTD < 0
                   MOVE 'E700' TO WS-REJECT-CODE
               WHEN CCOM-FEE-YTD > 99999.00
                   MOVE 'E707' TO WS-REJECT-CODE
               WHEN CCOM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E197' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COIN AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3610-ACCRUE-COIN.
           IF CCOM-COIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-COIN-AMT * CCOM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CCOM-COIN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3620-COUNT-ITEM.
           IF CCOM-ITEM-CNT < 9999999
               ADD 1 TO CCOM-ITEM-CNT
           END-IF
           IF CCOM-ITEM-CNT NOT < 5
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3630-COUNT-NOTICE.
           IF CCOM-NOTICE-CNT < 9999999
               ADD 1 TO CCOM-NOTICE-CNT
           END-IF
           IF CCOM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DENOM CD:
      * B -> D WHEN EXPIRY DATE IS PAST
      * D -> P WHEN EXPIRY DATE IS PAST
      * P -> S WHEN EXPIRY DATE IS PAST
      * S -> W WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3640-SET-DENOM.
           EVALUATE CCOM-DENOM-CD
               WHEN 'B'
                   IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CURRENCY YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3650-FLAG-CURRENCY.
           IF CCOM-CURRENCY-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-CURRENCY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3660-DESCRIBE-CHANNEL.
           EVALUATE CCOM-CHANNEL-CD
               WHEN 'AW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VARIANCE AMT.
      *----------------------------------------------------------------*
       3670-PRORATE-VARIANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOT-VARIANCE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOT-VARIANCE-AMT
           END-IF
           ADD CCOT-VARIANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E954 ADDR REQUIRED
      * E060 STRAP AMT NEGATIVE
      * E166 STRAP AMT OVER 5000
      * E045 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3680-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-ADDR = SPACES
                   MOVE 'E954' TO WS-REJECT-CODE
               WHEN CCOT-STRAP-AMT < 0
                   MOVE 'E060' TO WS-REJECT-CODE
               WHEN CCOT-STRAP-AMT > 5000.00
                   MOVE 'E166' TO WS-REJECT-CODE
               WHEN CCOM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E045' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VARIANCE TIER TIER FOR THE STRAP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3690-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VARIANCE-TIER-RATE(1) TO CCOM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CCOM-STRAP-YTD NOT < WS-VARIANCE-TIER-LOW(WS-TX)
                   MOVE WS-VARIANCE-TIER-RATE(WS-TX) TO CCOM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CURRENCY FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3700-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CCOM-CURRENCY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   COMPUTE CCOM-CURRENCY-AMT ROUNDED = WS-WORK-AMT-2 *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO CCOM-CURRENCY-AMT
               WHEN CCOT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO CCOM-CURRENCY-AMT
               WHEN OTHER
                   COMPUTE CCOM-CURRENCY-AMT ROUNDED = WS-WORK-AMT-2 *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-CURRENCY-AMT > 0 AND CCOM-CURRENCY-AMT < 10.00
               MOVE 10.00 TO CCOM-CURRENCY-AMT
           END-IF
           IF CCOM-CURRENCY-AMT > 150.00
               MOVE 150.00 TO CCOM-CURRENCY-AMT
           END-IF
           ADD CCOM-CURRENCY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3710-DESCRIBE-REASON.
           EVALUATE CCOM-REASON-CD
               WHEN 'JL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3720-COUNT-CYCLE.
           IF CCOM-CYCLE-CNT < 9999999
               ADD 1 TO CCOM-CYCLE-CNT
           END-IF
           IF CCOM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3730-FLAG-FEE.
           IF CCOM-FEE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CCOM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E757 CUST ID REQUIRED
      * E477 COIN AMT NEGATIVE
      * E189 COIN AMT OVER 99999
      * E980 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3740-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CUST-ID = SPACES
                   MOVE 'E757' TO WS-REJECT-CODE
               WHEN CCOT-COIN-AMT < 0
                   MOVE 'E477' TO WS-REJECT-CODE
               WHEN CCOT-COIN-AMT > 99999.00
                   MOVE 'E189' TO WS-REJECT-CODE
               WHEN CCOT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E980' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DENOM CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3750-DESCRIBE-DENOM.
           EVALUATE CCOM-DENOM-CD
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'D'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'P'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'W'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3760-COUNT-NOTICE.
           IF CCOM-NOTICE-CNT < 9999999
               ADD 1 TO CCOM-NOTICE-CNT
           END-IF
           IF CCOM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO CCOM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VARIANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CURRENCY AMT.
      *----------------------------------------------------------------*
       3770-PRORATE-CURRENCY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-CURRENCY-AMT ROUNDED =
                   CCOM-VARIANCE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-CURRENCY-AMT
           END-IF
           ADD CCOM-CURRENCY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STRAP YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3780-LIMIT-STRAP.
           IF CCOM-STRAP-YTD < 0.00
               MOVE 0.00 TO CCOM-STRAP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-STRAP-YTD > 1000.00
               MOVE 1000.00 TO CCOM-STRAP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHIPMENT AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3790-LIMIT-SHIPMENT.
           IF CCOM-SHIPMENT-AMT < 0.00
               MOVE 0.00 TO CCOM-SHIPMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-SHIPMENT-AMT > 10000.00
               MOVE 10000.00 TO CCOM-SHIPMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3800-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3810-COUNT-SEQ.
           IF CCOT-SEQ < 9999999
               ADD 1 TO CCOT-SEQ
           END-IF
           IF CCOT-SEQ NOT < 12
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VARIANCE FEE ON THE COIN YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3820-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CCOM-VARIANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   COMPUTE CCOM-VARIANCE-AMT ROUNDED = CCOM-COIN-YTD *
                       CCOM-FEE-PCT / 200
               WHEN CCOM-SOURCE-CD = 'CFR'
                   MOVE 20.00 TO CCOM-VARIANCE-AMT
               WHEN CCOM-SOURCE-CD = 'DTX'
                   MOVE 12.00 TO CCOM-VARIANCE-AMT
               WHEN OTHER
                   COMPUTE CCOM-VARIANCE-AMT ROUNDED = CCOM-COIN-YTD *
                       CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOM-VARIANCE-AMT > 0 AND CCOM-VARIANCE-AMT < 1.00
               MOVE 1.00 TO CCOM-VARIANCE-AMT
           END-IF
           IF CCOM-VARIANCE-AMT > 35.00
               MOVE 35.00 TO CCOM-VARIANCE-AMT
           END-IF
           ADD CCOM-VARIANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3830-COUNT-CYCLE.
           IF CCOM-CYCLE-CNT < 9999999
               ADD 1 TO CCOM-CYCLE-CNT
           END-IF
           IF CCOM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO CCOT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3840-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CCOM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO CCOM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E109 NAME REQUIRED
      * E272 WORK AMT NEGATIVE
      * E307 WORK AMT OVER 250000
      * E130 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3850-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-NAME = SPACES
                   MOVE 'E109' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E272' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E307' TO WS-REJECT-CODE
               WHEN CCOT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E130' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3860-SPLIT-SHIPMENT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE CCOM-SHIPMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3870-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF CCOT-POST-DATE > 0 AND CCOT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STRAP AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3880-LIMIT-STRAP.
           IF CCOM-STRAP-AMT < 10.00
               MOVE 10.00 TO CCOM-STRAP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-STRAP-AMT > 2500.00
               MOVE 2500.00 TO CCOM-STRAP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VARIANCE YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3890-LIMIT-VARIANCE.
           IF CCOM-VARIANCE-YTD < 50.00
               MOVE 50.00 TO CCOM-VARIANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-VARIANCE-YTD > 1000.00
               MOVE 1000.00 TO CCOM-VARIANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3900-COUNT-CYCLE.
           IF CCOM-CYCLE-CNT < 9999999
               ADD 1 TO CCOM-CYCLE-CNT
           END-IF
           IF CCOM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO CCOT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARRIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3910-DESCRIBE-CARRIER.
           EVALUATE CCOM-CARRIER-CD
               WHEN 'J'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'X'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COIN SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3920-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-SCHED-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-COIN-SCHED-LOW(WS-TX)
                   MOVE WS-COIN-SCHED-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPMENT FEE ON THE VARIANCE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3930-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CCOM-SHIPMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = CCOM-VARIANCE-YTD
                       * CCOM-PENALTY-PCT / 200
               WHEN CCOM-STATUS-CD = 'ADH'
                   MOVE 12.00 TO CCOM-SHIPMENT-AMT
               WHEN CCOM-STATUS-CD = 'FVV'
                   MOVE 12.00 TO CCOM-SHIPMENT-AMT
               WHEN OTHER
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = CCOM-VARIANCE-YTD
                       * CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-SHIPMENT-AMT > 0 AND CCOM-SHIPMENT-AMT < 10.00
               MOVE 10.00 TO CCOM-SHIPMENT-AMT
           END-IF
           IF CCOM-SHIPMENT-AMT > 250.00
               MOVE 250.00 TO CCOM-SHIPMENT-AMT
           END-IF
           ADD CCOM-SHIPMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3940-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-SOURCE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHIPMENT AMT BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3950-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-DENOM-CD
                   ADD CCOT-SHIPMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOT-SHIPMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * ADH -> FVV WHEN EXPIRY DATE IS PAST
      * FVV -> GCB WHEN EXPIRY DATE IS PAST
      * GCB -> UZP WHEN EXPIRY DATE IS PAST
      * UZP -> VXB WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3960-SET-STATUS.
           EVALUATE CCOM-STATUS-CD
               WHEN 'ADH'
                   IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FVV' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FVV'
                   IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GCB' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GCB'
                   IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UZP' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UZP'
                   IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VXB' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COIN SCHED TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-SCHED-RATE(1) TO CCOM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CCOM-FEE-AMT NOT < WS-COIN-SCHED-LOW(WS-TX)
                   MOVE WS-COIN-SCHED-RATE(WS-TX) TO CCOM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COIN SCHED TIER FOR THE SHIPMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3980-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-SCHED-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CCOT-SHIPMENT-AMT NOT < WS-COIN-SCHED-LOW(WS-TX)
                   MOVE WS-COIN-SCHED-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3990-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-SOURCE-CD
                   ADD CCOM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E829 CONTACT REQUIRED
      * E196 FEE AMT NEGATIVE
      * E655 FEE AMT OVER 99999
      * E967 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4000-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CONTACT = SPACES
                   MOVE 'E829' TO WS-REJECT-CODE
               WHEN CCOM-FEE-AMT < 0
                   MOVE 'E196' TO WS-REJECT-CODE
               WHEN CCOM-FEE-AMT > 99999.00
                   MOVE 'E655' TO WS-REJECT-CODE
               WHEN CCOM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E967' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4010-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CCOM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4020-PRORATE-COIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-COIN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CARRIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4030-ACCUM-CARRIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-CARRIER-CD
                   ADD CCOM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-CARRIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COIN SCHED TIER FOR THE CURRENCY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4040-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-SCHED-RATE(1) TO CCOM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CCOM-CURRENCY-YTD NOT < WS-COIN-SCHED-LOW(WS-TX)
                   MOVE WS-COIN-SCHED-RATE(WS-TX) TO CCOM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COIN YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4050-LIMIT-COIN.
           IF CCOM-COIN-YTD < 50.00
               MOVE 50.00 TO CCOM-COIN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-COIN-YTD > 50000.00
               MOVE 50000.00 TO CCOM-COIN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4060-COUNT-CYCLE.
           IF CCOM-CYCLE-CNT < 9999999
               ADD 1 TO CCOM-CYCLE-CNT
           END-IF
           IF CCOM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE CCO FIELDS TO CNCAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-CNCAL.
           INITIALIZE CNCAL-PARMS
           MOVE CCOM-STRAP-YTD
             TO CNCAL-STRAP-YTD
           MOVE CCOM-VARIANCE-AMT
             TO CNCAL-VARIANCE-AMT
           MOVE CCOM-FEE-YTD
             TO CNCAL-FEE-YTD
           MOVE CCOM-SHIPMENT-AMT
             TO CNCAL-SHIPMENT-AMT
           MOVE CCOM-SHIPMENT-YTD
             TO CNCAL-SHIPMENT-YTD
           MOVE CCOM-COIN-AMT
             TO CNCAL-COIN-AMT
           MOVE CCOM-EXPIRY-DATE
             TO CNCAL-EXPIRY-DATE
           MOVE CCOM-MATURITY-DATE
             TO CNCAL-MATURITY-DATE
           MOVE CCOM-OPEN-DATE
             TO CNCAL-OPEN-DATE
           MOVE CCOM-NEXT-REVIEW-DATE
             TO CNCAL-NEXT-REVIEW-DATE
           MOVE CCOM-STATUS-CD
             TO CNCAL-STATUS-CD
           MOVE CCOM-CARRIER-CD
             TO CNCAL-CARRIER-CD
           MOVE CCOM-SOURCE-CD
             TO CNCAL-SOURCE-CD
           MOVE CCOM-CHANNEL-CD
             TO CNCAL-CHANNEL-CD
           MOVE CCOM-OVERRIDE-FLAG
             TO CNCAL-OVERRIDE-FLAG
           MOVE CCOM-EXEMPT-FLAG
             TO CNCAL-EXEMPT-FLAG
           MOVE CCOM-NOTICE-SENT-FLAG
             TO CNCAL-NOTICE-SENT-FLAG
           MOVE CCOM-NOTICE-CNT
             TO CNCAL-NOTICE-CNT
           MOVE CCOM-RETRY-CNT
             TO CNCAL-RETRY-CNT
           MOVE CCOM-FEE-PCT
             TO CNCAL-FEE-PCT
           MOVE CCOM-RATE
             TO CNCAL-RATE
           MOVE CCOM-NAME
             TO CNCAL-NAME
           MOVE CCOM-DESC
             TO CNCAL-DESC
           CALL 'CNCAL' USING CNCAL-PARMS
           EVALUATE CNCAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CNCAL-STRAP-YTD
                     TO CCOM-STRAP-YTD
                   MOVE CNCAL-FEE-YTD
                     TO CCOM-FEE-YTD
                   MOVE CNCAL-SHIPMENT-AMT
                     TO CCOM-SHIPMENT-AMT
                   MOVE CNCAL-OVERRIDE-FLAG
                     TO CCOM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-CNCAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE CCO FIELDS TO CNFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-CNFEE.
           INITIALIZE CNFEE-PARMS
           MOVE CCOM-STRAP-YTD
             TO CNFEE-STRAP-YTD
           MOVE CCOM-CURRENCY-YTD
             TO CNFEE-CURRENCY-YTD
           MOVE CCOM-STRAP-AMT
             TO CNFEE-STRAP-AMT
           MOVE CCOM-FEE-AMT
             TO CNFEE-FEE-AMT
           MOVE CCOM-VARIANCE-YTD
             TO CNFEE-VARIANCE-YTD
           MOVE CCOM-COIN-YTD
             TO CNFEE-COIN-YTD
           MOVE CCOM-NEXT-REVIEW-DATE
             TO CNFEE-NEXT-REVIEW-DATE
           MOVE CCOM-LAST-PAY-DATE
             TO CNFEE-LAST-PAY-DATE
           MOVE CCOM-NOTICE-DATE
             TO CNFEE-NOTICE-DATE
           MOVE CCOM-EFFECTIVE-DATE
             TO CNFEE-EFFECTIVE-DATE
           MOVE CCOM-REASON-CD
             TO CNFEE-REASON-CD
           MOVE CCOM-SOURCE-CD
             TO CNFEE-SOURCE-CD
           MOVE CCOM-DENOM-CD
             TO CNFEE-DENOM-CD
           MOVE CCOM-CARRIER-CD
             TO CNFEE-CARRIER-CD
           MOVE CCOM-EXEMPT-FLAG
             TO CNFEE-EXEMPT-FLAG
           MOVE CCOM-AUTO-RENEW-FLAG
             TO CNFEE-AUTO-RENEW-FLAG
           MOVE CCOM-HOLD-FLAG
             TO CNFEE-HOLD-FLAG
           MOVE CCOM-CYCLE-CNT
             TO CNFEE-CYCLE-CNT
           MOVE CCOM-NOTICE-CNT
             TO CNFEE-NOTICE-CNT
           MOVE CCOM-RATE
             TO CNFEE-RATE
           MOVE CCOM-FEE-PCT
             TO CNFEE-FEE-PCT
           MOVE CCOM-DESC
             TO CNFEE-DESC
           MOVE CCOM-REF-NO
             TO CNFEE-REF-NO
           CALL 'CNFEE' USING CNFEE-PARMS
           EVALUATE CNFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CNFEE-CURRENCY-YTD
                     TO CCOM-CURRENCY-YTD
                   MOVE CNFEE-STRAP-AMT
                     TO CCOM-STRAP-AMT
                   MOVE CNFEE-FEE-AMT
                     TO CCOM-FEE-AMT
                   MOVE CNFEE-VARIANCE-YTD
                     TO CCOM-VARIANCE-YTD
                   MOVE CNFEE-COIN-YTD
                     TO CCOM-COIN-YTD
                   MOVE CNFEE-EXEMPT-FLAG
                     TO CCOM-EXEMPT-FLAG
                   MOVE CNFEE-AUTO-RENEW-FLAG
                     TO CCOM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-CNFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE CCO FIELDS TO CNPEN AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-CNPEN.
           INITIALIZE CNPEN-PARMS
           MOVE CCOM-SHIPMENT-AMT
             TO CNPEN-SHIPMENT-AMT
           MOVE CCOM-COIN-AMT
             TO CNPEN-COIN-AMT
           MOVE CCOM-STRAP-YTD
             TO CNPEN-STRAP-YTD
           MOVE CCOM-SHIPMENT-YTD
             TO CNPEN-SHIPMENT-YTD
           MOVE CCOM-CURRENCY-YTD
             TO CNPEN-CURRENCY-YTD
           MOVE CCOM-FEE-AMT
             TO CNPEN-FEE-AMT
           MOVE CCOM-NOTICE-DATE
             TO CNPEN-NOTICE-DATE
           MOVE CCOM-NEXT-REVIEW-DATE
             TO CNPEN-NEXT-REVIEW-DATE
           MOVE CCOM-LAST-PAY-DATE
             TO CNPEN-LAST-PAY-DATE
           MOVE CCOM-MATURITY-DATE
             TO CNPEN-MATURITY-DATE
           MOVE CCOM-CARRIER-CD
             TO CNPEN-CARRIER-CD
           MOVE CCOM-REASON-CD
             TO CNPEN-REASON-CD
           MOVE CCOM-SOURCE-CD
             TO CNPEN-SOURCE-CD
           MOVE CCOM-CHANNEL-CD
             TO CNPEN-CHANNEL-CD
           MOVE CCOM-STOP-FLAG
             TO CNPEN-STOP-FLAG
           MOVE CCOM-HOLD-FLAG
             TO CNPEN-HOLD-FLAG
           MOVE CCOM-AUTO-RENEW-FLAG
             TO CNPEN-AUTO-RENEW-FLAG
           MOVE CCOM-RETRY-CNT
             TO CNPEN-RETRY-CNT
           MOVE CCOM-ITEM-CNT
             TO CNPEN-ITEM-CNT
           MOVE CCOM-RATE
             TO CNPEN-RATE
           MOVE CCOM-PENALTY-PCT
             TO CNPEN-PENALTY-PCT
           MOVE CCOM-REF-NO
             TO CNPEN-REF-NO
           MOVE CCOM-CONTACT
             TO CNPEN-CONTACT
           CALL 'CNPEN' USING CNPEN-PARMS
           EVALUATE CNPEN-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CNPEN-STRAP-YTD
                     TO CCOM-STRAP-YTD
                   MOVE CNPEN-FEE-AMT
                     TO CCOM-FEE-AMT
                   MOVE CNPEN-STOP-FLAG
                     TO CCOM-STOP-FLAG
                   MOVE CNPEN-HOLD-FLAG
                     TO CCOM-HOLD-FLAG
                   MOVE CNPEN-AUTO-RENEW-FLAG
                     TO CCOM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-CNPEN' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE CCOT-KEY TO RD-KEY
           MOVE CCOT-TRAN-CD TO RD-CODE
           MOVE CCOT-COIN-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE CCOT-KEY TO RD-KEY
           MOVE WS-EXC-AMOUNT TO RD-AMT
           MOVE 'EXCP' TO RD-REJECT
           MOVE WS-EXC-SEVERITY TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       9000-TERMINATE.
           MOVE 'TRANSACTIONS READ' TO RT-LABEL
           MOVE WS-READ-COUNT TO RT-COUNT
           MOVE ZERO TO RT-AMT
           WRITE REPORT-REC FROM WS-RPT-TOTAL
           MOVE 'RECORDS WRITTEN' TO RT-LABEL
           MOVE WS-WRITE-COUNT TO RT-COUNT
           MOVE ZERO TO RT-AMT
           WRITE REPORT-REC FROM WS-RPT-TOTAL
           MOVE 'REJECTED' TO RT-LABEL
           MOVE WS-REJECT-COUNT TO RT-COUNT
           MOVE ZERO TO RT-AMT
           WRITE REPORT-REC FROM WS-RPT-TOTAL
           MOVE 'FEES ASSESSED' TO RT-LABEL
           MOVE WS-WAIVED-COUNT TO RT-COUNT
           MOVE WS-TOTAL-FEES TO RT-AMT
           WRITE REPORT-REC FROM WS-RPT-TOTAL
           MOVE 'INTEREST' TO RT-LABEL
           MOVE WS-NEGATIVE-COUNT TO RT-COUNT
           MOVE WS-TOTAL-INTEREST TO RT-AMT
           WRITE REPORT-REC FROM WS-RPT-TOTAL
           MOVE 'EXCEPTIONS' TO RT-LABEL
           MOVE WS-EXCEPT-COUNT TO RT-COUNT
           MOVE ZERO TO RT-AMT
           WRITE REPORT-REC FROM WS-RPT-TOTAL
           PERFORM VARYING WS-CX FROM 1 BY 1 UNTIL WS-CX > WS-CAT-USED
               MOVE WS-CAT-CODE(WS-CX) TO RT-LABEL
               MOVE WS-CAT-CNT(WS-CX) TO RT-COUNT
               MOVE WS-CAT-AMT(WS-CX) TO RT-AMT
               WRITE REPORT-REC FROM WS-RPT-TOTAL
           END-PERFORM
           CLOSE MASTER-FILE TRAN-FILE OUT-FILE REPORT-FILE
           DISPLAY 'CNOSETL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CNOSETL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
