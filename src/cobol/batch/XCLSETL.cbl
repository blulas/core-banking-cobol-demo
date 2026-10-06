      *================================================================*
      * PROGRAM  : XCLSETL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : CHRISTMAS CLUB SAVINGS - SETTLEMENT. READS THE XCL  *
      *            TRANSACTIONS, UPDATES THE XCL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. HOLIDAY CLUB SAVINGS PLANS WITH *
      *            WEEKLY DEPOSITS AND NOVEMBER PAYOUT                 *
      * INPUT    : XCLMST (KSDS), XCLTRN, BANKCTL                      *
      * OUTPUT   : XCLOUT, XCLSERPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, XCPEN, XCFEE, XCLIM     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-02-20 MKL  ORIGINAL                                       *
      * 1999-02-21 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-04-19 CRP  AUDIT FINDING - MISSED CONTROL TOTALS INC4919  *
      * 2003-11-09 MKL  PENALTY NOTICE TEXT PER LEGAL INC7938          *
      * 2004-03-04 EWS  MISSED TABLE EXPANDED REQ9443                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. XCLSETL.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "XCLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS XCLM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "XCLTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "XCLOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "XCLSERPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY XCLMST.
       FD  TRAN-FILE.
       COPY XCLTRN.
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
                                           VALUE 13.000000.
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
      * WS-MISSED-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MISSED-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000637000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000875000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000118000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002700000000075000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002950000000857500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003950000000447500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004450000000713000'.
       01  WS-MISSED-GRID-TBL REDEFINES WS-MISSED-GRID-VALUES.
           05  WS-MISSED-GRID-ROW OCCURS 7 TIMES.
               10  WS-MISSED-GRID-LOW      PIC 9(09)V99.
               10  WS-MISSED-GRID-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-GOAL-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-GOAL-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000890000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000856000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000420000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000711000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012000000000340000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012100000000660000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013100000000753500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013200000000174500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013700000000649000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014200000000257500'.
       01  WS-GOAL-TIER-TBL REDEFINES WS-GOAL-TIER-VALUES.
           05  WS-GOAL-TIER-ROW OCCURS 10 TIMES.
               10  WS-GOAL-TIER-LOW        PIC 9(09)V99.
               10  WS-GOAL-TIER-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PENALTY-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PENALTY-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000182000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000170000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000123500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007600000000099000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007650000000741500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008150000000811500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008250000000125500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018250000000671000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018350000000058000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020850000000395500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020950000000818000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021000000000099000'.
       01  WS-PENALTY-TIER-TBL REDEFINES WS-PENALTY-TIER-VALUES.
           05  WS-PENALTY-TIER-ROW OCCURS 12 TIMES.
               10  WS-PENALTY-TIER-LOW     PIC 9(09)V99.
               10  WS-PENALTY-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYOUT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYOUT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000328500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000744000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000815500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010750000000683500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013250000000187000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013300000000839000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014300000000337500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014400000000274000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016900000000118500'.
       01  WS-PAYOUT-SCHED-TBL REDEFINES WS-PAYOUT-SCHED-VALUES.
           05  WS-PAYOUT-SCHED-ROW OCCURS 9 TIMES.
               10  WS-PAYOUT-SCHED-LOW     PIC 9(09)V99.
               10  WS-PAYOUT-SCHED-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  XCLSETL  CHRISTMAS CLUB SAVINGS'.
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
       COPY XCPENP.
       COPY XCFEEP.
       COPY XCLIMP.

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
               MOVE 'XCLMST' TO ABND-FILE-NAME
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
           MOVE XCLT-KEY TO XCLM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-PRORATE-CONTRIB
               PERFORM 3010-SELECT-RATE
               PERFORM 3020-FLAG-ACCRUED
               PERFORM 3030-SPLIT-GOAL
               PERFORM 3040-SET-REASON
               PERFORM 3050-ASSESS-OVERRIDE
               PERFORM 3060-PRORATE-MISSED
               PERFORM 3070-AGE-MATURITY
               PERFORM 3080-COUNT-PAYMENT
               PERFORM 3090-PRORATE-ACCRUED
               PERFORM 3100-AGE-NEXT-DUE
               PERFORM 3110-PRORATE-WEEKLY
               PERFORM 3120-EDIT-CUST-ID
               PERFORM 3130-DESCRIBE-TRAN
               PERFORM 3140-DESCRIBE-CYCLE
               PERFORM 3150-AGE-LAST-PAY
               PERFORM 3160-SELECT-DISCOUNT
               PERFORM 3170-SET-CYCLE
               PERFORM 3180-DESCRIBE-REASON
               PERFORM 3190-ASSESS-EXEMPT
               PERFORM 3200-EDIT-MEMO
               PERFORM 3210-COUNT-RETRY
               PERFORM 3220-LIMIT-ACCRUED
               PERFORM 3230-ACCRUE-ACCRUED
               PERFORM 3240-SPLIT-PENALTY
               PERFORM 3250-DESCRIBE-TYPE
               PERFORM 3260-SET-TRAN
               PERFORM 3270-AGE-EFF
               PERFORM 3280-ACCUM-CYCLE
               PERFORM 3290-FLAG-GOAL
               PERFORM 3300-CHECK-LAST-PAY
               PERFORM 3310-DESCRIBE-REASON
               PERFORM 3320-SELECT-FEE-PCT
               PERFORM 3330-SET-CYCLE
               PERFORM 3340-FLAG-ITEM
               PERFORM 3350-ACCUM-TYPE
               PERFORM 3360-ASSESS-PREFERRED
               PERFORM 3370-ACCRUE-PAYOUT
               PERFORM 3380-PRORATE-PENALTY
               PERFORM 3390-ASSESS-STOP
               PERFORM 3400-SELECT-DISCOUNT
               PERFORM 3410-DESCRIBE-CYCLE
               PERFORM 3420-LIMIT-MISSED
               PERFORM 3430-ACCRUE-WEEKLY
               PERFORM 3440-SET-CYCLE
               PERFORM 3450-PRORATE-WEEKLY
               PERFORM 3460-CHECK-NEXT-DUE
               PERFORM 3470-FLAG-ACCRUED
               PERFORM 3480-SELECT-RATE
               PERFORM 3490-FLAG-GOAL
               PERFORM 3500-ACCUM-PAYOUT-METHOD
               PERFORM 3510-SET-REGION
               PERFORM 3520-ACCRUE-CONTRIB
               PERFORM 3530-ACCRUE-WEEKLY
               PERFORM 3540-COUNT-RETRY
               PERFORM 3550-ACCRUE-CONTRIB
               PERFORM 3560-SELECT-FEE-PCT
               PERFORM 3570-EDIT-OPERATOR
               PERFORM 3580-EDIT-MEMO
               PERFORM 3590-PRORATE-CONTRIB
               PERFORM 3600-ASSESS-EXEMPT
               PERFORM 3610-EDIT-CONTACT
               PERFORM 3620-ASSESS-AUTO-RENEW
               PERFORM 3630-FLAG-ITEM
               PERFORM 3640-SELECT-RATE
               PERFORM 3650-SPLIT-BONUS
               PERFORM 3660-SET-TRAN
               PERFORM 3670-ACCUM-TRAN
               PERFORM 3680-PRORATE-GOAL
               PERFORM 3690-SET-TYPE
               PERFORM 3700-SPLIT-BONUS
               PERFORM 3710-ASSESS-STOP
               PERFORM 3720-ACCRUE-MISSED
               PERFORM 3730-ASSESS-AUTO-RENEW
               PERFORM 3740-FLAG-ITEM
               PERFORM 3750-AGE-NEXT-REVIEW
               PERFORM 3760-SELECT-RATE
               PERFORM 3770-ASSESS-EXEMPT
               PERFORM 3780-PRORATE-CONTRIB
               PERFORM 3790-EDIT-CONTACT
               PERFORM 3800-ACCUM-PLAN
               PERFORM 3810-DESCRIBE-PAYOUT-METHOD
               PERFORM 3820-SELECT-FEE-PCT
               PERFORM 3830-DESCRIBE-PAYOUT-METHOD
               PERFORM 3840-DESCRIBE-REGION
               PERFORM 3850-ACCRUE-BONUS
               PERFORM 3860-AGE-POST
               PERFORM 3870-FLAG-WEEKLY
               PERFORM 3880-SET-PAYOUT-METHOD
               PERFORM 3890-LIMIT-ACCRUED
               PERFORM 3900-PRORATE-BONUS
               PERFORM 3910-CHECK-OPEN
               PERFORM 3920-EDIT-REJECT
               PERFORM 3930-FLAG-GOAL
               PERFORM 3940-ACCUM-PAYOUT-METHOD
               PERFORM 3950-CHECK-NOTICE
               PERFORM 3960-EDIT-DESC
               PERFORM 3970-ACCRUE-CONTRIB
               PERFORM 3980-SELECT-FEE-PCT
               PERFORM 3990-ACCRUE-CONTRIB
               PERFORM 4000-PRORATE-PENALTY
               PERFORM 4010-LIMIT-GOAL
               PERFORM 4020-DESCRIBE-PAYOUT-METHOD
               PERFORM 4030-SELECT-FEE-PCT
               PERFORM 4040-SET-CYCLE
               PERFORM 4050-COUNT-RETRY
               PERFORM 4060-PRORATE-WEEKLY
               PERFORM 4070-SELECT-FEE-PCT
               PERFORM 4080-PRORATE-GOAL
               PERFORM 4090-LIMIT-WEEKLY
               PERFORM 4100-DESCRIBE-CYCLE
               PERFORM 4110-SET-TYPE
               PERFORM 4120-SELECT-RATE
               PERFORM 4130-ACCRUE-CONTRIB
               PERFORM 4140-LIMIT-GOAL
               PERFORM 4150-SET-PAYOUT-METHOD
               PERFORM 4160-CHECK-EFF
               PERFORM 4170-PRORATE-CONTRIB
               PERFORM 4180-LIMIT-MISSED
               PERFORM 4190-ACCRUE-MISSED
               PERFORM 4200-FLAG-CONTRIB
               PERFORM 4210-DESCRIBE-TYPE
               PERFORM 7030-CALL-XCPEN
               PERFORM 7020-CALL-XCFEE
               PERFORM 7010-CALL-XCLIM
               REWRITE XCL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'XCLMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM XCL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * PRORATE GOAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTRIB YTD.
      *----------------------------------------------------------------*
       3000-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-CONTRIB-YTD ROUNDED =
                   XCLM-GOAL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-CONTRIB-YTD
           END-IF
           ADD XCLM-CONTRIB-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT SCHED TIER FOR THE CONTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3010-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-SCHED-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF XCLM-CONTRIB-AMT NOT < WS-PAYOUT-SCHED-LOW(WS-TX)
                   MOVE WS-PAYOUT-SCHED-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUED AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3020-FLAG-ACCRUED.
           IF XCLM-ACCRUED-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLM-ACCRUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3030-SPLIT-GOAL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE XCLT-GOAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AL -> BT WHEN LAST PAY DATE IS PAST
      * BT -> CY WHEN LAST PAY DATE IS PAST
      * CY -> KM WHEN LAST PAY DATE IS PAST
      * KM -> MV WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3040-SET-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BT'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CY' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CY'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KM' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KM'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MV' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE MISSED FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3050-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO XCLM-MISSED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   COMPUTE XCLM-MISSED-AMT ROUNDED = WS-WORK-AMT-2 *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-REGION-CD = 'FLM'
                   MOVE 15.00 TO XCLM-MISSED-AMT
               WHEN XCLM-REGION-CD = 'HLS'
                   MOVE 7.00 TO XCLM-MISSED-AMT
               WHEN OTHER
                   COMPUTE XCLM-MISSED-AMT ROUNDED = WS-WORK-AMT-2 *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-MISSED-AMT > 0 AND XCLM-MISSED-AMT < 2.00
               MOVE 2.00 TO XCLM-MISSED-AMT
           END-IF
           IF XCLM-MISSED-AMT > 250.00
               MOVE 250.00 TO XCLM-MISSED-AMT
           END-IF
           ADD XCLM-MISSED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MISSED AMT.
      *----------------------------------------------------------------*
       3060-PRORATE-MISSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-MISSED-AMT ROUNDED =
                   XCLM-CONTRIB-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-MISSED-AMT
           END-IF
           ADD XCLM-MISSED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3070-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF XCLM-MATURITY-DATE > 0 AND XCLM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3080-COUNT-PAYMENT.
           IF XCLM-PAYMENT-CNT < 9999999
               ADD 1 TO XCLM-PAYMENT-CNT
           END-IF
           IF XCLM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GOAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ACCRUED AMT.
      *----------------------------------------------------------------*
       3090-PRORATE-ACCRUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-ACCRUED-AMT ROUNDED =
                   XCLT-GOAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-ACCRUED-AMT
           END-IF
           ADD XCLM-ACCRUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3100-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLT-CONTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD XCLT-CONTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCLT-CONTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLT-CONTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLT-CONTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WEEKLY AMT.
      *----------------------------------------------------------------*
       3110-PRORATE-WEEKLY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-WEEKLY-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-WEEKLY-AMT
           END-IF
           ADD XCLM-WEEKLY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E289 CUST ID REQUIRED
      * E227 CONTRIB YTD NEGATIVE
      * E942 CONTRIB YTD OVER 50000
      * E134 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3120-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-CUST-ID = SPACES
                   MOVE 'E289' TO WS-REJECT-CODE
               WHEN XCLM-CONTRIB-YTD < 0
                   MOVE 'E227' TO WS-REJECT-CODE
               WHEN XCLM-CONTRIB-YTD > 50000.00
                   MOVE 'E942' TO WS-REJECT-CODE
               WHEN XCLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E134' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3130-DESCRIBE-TRAN.
           EVALUATE XCLT-TRAN-CD
               WHEN 'WD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PO'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3140-DESCRIBE-CYCLE.
           EVALUATE XCLM-CYCLE-CD
               WHEN 'EM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3150-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-MISSED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD XCLM-MISSED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-MISSED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-MISSED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-MISSED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT SCHED TIER FOR THE BONUS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3160-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-SCHED-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF XCLM-BONUS-YTD NOT < WS-PAYOUT-SCHED-LOW(WS-TX)
                   MOVE WS-PAYOUT-SCHED-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EM -> FE WHEN NEXT DUE DATE IS PAST
      * FE -> PL WHEN NEXT DUE DATE IS PAST
      * PL -> RU WHEN NEXT DUE DATE IS PAST
      * RU -> SU WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3170-SET-CYCLE.
           EVALUATE XCLM-CYCLE-CD
               WHEN 'EM'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FE' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FE'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PL' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RU' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SU' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3180-DESCRIBE-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE ACCRUED YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3190-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = XCLM-ACCRUED-YTD *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = XCLM-ACCRUED-YTD *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E389 MEMO REQUIRED
      * E202 BONUS AMT NEGATIVE
      * E075 BONUS AMT OVER 1000000
      * E243 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3200-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-MEMO = SPACES
                   MOVE 'E389' TO WS-REJECT-CODE
               WHEN XCLT-BONUS-AMT < 0
                   MOVE 'E202' TO WS-REJECT-CODE
               WHEN XCLT-BONUS-AMT > 1000000.00
                   MOVE 'E075' TO WS-REJECT-CODE
               WHEN XCLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E243' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-RETRY.
           IF XCLM-RETRY-CNT < 9999999
               ADD 1 TO XCLM-RETRY-CNT
           END-IF
           IF XCLM-RETRY-CNT NOT < 6
               MOVE 'Y' TO XCLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUED YTD IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3220-LIMIT-ACCRUED.
           IF XCLM-ACCRUED-YTD < 10.00
               MOVE 10.00 TO XCLM-ACCRUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-ACCRUED-YTD > 500.00
               MOVE 500.00 TO XCLM-ACCRUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ACCRUED AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3230-ACCRUE-ACCRUED.
           IF XCLM-ACCRUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-ACCRUED-AMT * XCLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-MISSED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PENALTY YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3240-SPLIT-PENALTY.
           COMPUTE WS-WORK-AMT = XCLM-PENALTY-YTD / 12
           COMPUTE WS-WORK-AMT-2 = XCLM-PENALTY-YTD - (WS-WORK-AMT * 12)
           COMPUTE XCLM-PAYOUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3250-DESCRIBE-TYPE.
           EVALUATE XCLM-TYPE-CD
               WHEN 'A'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'D'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'E'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'W'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * WD -> DP WHEN LAST ACT DATE IS PAST
      * DP -> PO WHEN LAST ACT DATE IS PAST
      * PO -> BN WHEN LAST ACT DATE IS PAST
      * BN -> PN WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3260-SET-TRAN.
           EVALUATE XCLT-TRAN-CD
               WHEN 'WD'
                   IF XCLM-LAST-ACT-DATE > 0 AND XCLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF XCLM-LAST-ACT-DATE > 0 AND XCLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PO' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PO'
                   IF XCLM-LAST-ACT-DATE > 0 AND XCLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BN' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BN'
                   IF XCLM-LAST-ACT-DATE > 0 AND XCLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PN' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3270-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE BONUS AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3280-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-CYCLE-CD
                   ADD XCLM-BONUS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-BONUS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * GOAL YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3290-FLAG-GOAL.
           IF XCLM-GOAL-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLM-GOAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3300-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF XCLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO XCLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3310-DESCRIBE-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'CY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE GOAL TIER TIER FOR THE GOAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3320-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GOAL-TIER-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF XCLM-GOAL-AMT NOT < WS-GOAL-TIER-LOW(WS-TX)
                   MOVE WS-GOAL-TIER-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EM -> FE WHEN OPEN DATE IS PAST
      * FE -> PL WHEN OPEN DATE IS PAST
      * PL -> RU WHEN OPEN DATE IS PAST
      * RU -> SU WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3330-SET-CYCLE.
           EVALUATE XCLM-CYCLE-CD
               WHEN 'EM'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FE' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FE'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PL' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RU' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SU' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3340-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BONUS AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3350-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-TYPE-CD
                   ADD XCLT-BONUS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLT-BONUS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WEEKLY FEE ON THE GOAL YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3360-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO XCLT-WEEKLY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE XCLT-WEEKLY-AMT ROUNDED = XCLM-GOAL-YTD *
                       XCLM-FEE-PCT / 200
               WHEN XCLT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO XCLT-WEEKLY-AMT
               WHEN XCLT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO XCLT-WEEKLY-AMT
               WHEN OTHER
                   COMPUTE XCLT-WEEKLY-AMT ROUNDED = XCLM-GOAL-YTD *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLT-WEEKLY-AMT > 0 AND XCLT-WEEKLY-AMT < 5.00
               MOVE 5.00 TO XCLT-WEEKLY-AMT
           END-IF
           IF XCLT-WEEKLY-AMT > 250.00
               MOVE 250.00 TO XCLT-WEEKLY-AMT
           END-IF
           ADD XCLT-WEEKLY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYOUT YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3370-ACCRUE-PAYOUT.
           IF XCLM-PAYOUT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-PAYOUT-YTD * XCLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-MISSED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MISSED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PENALTY YTD.
      *----------------------------------------------------------------*
       3380-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-PENALTY-YTD ROUNDED =
                   XCLM-MISSED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-PENALTY-YTD
           END-IF
           ADD XCLM-PENALTY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB FEE ON THE WEEKLY AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3390-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCLM-CONTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   COMPUTE XCLM-CONTRIB-AMT ROUNDED = XCLM-WEEKLY-AMT *
                       XCLM-RATE / 200
               WHEN XCLM-PAYOUT-METHOD-CD = 'B'
                   MOVE 12.00 TO XCLM-CONTRIB-AMT
               WHEN XCLM-PAYOUT-METHOD-CD = 'H'
                   MOVE 12.00 TO XCLM-CONTRIB-AMT
               WHEN OTHER
                   COMPUTE XCLM-CONTRIB-AMT ROUNDED = XCLM-WEEKLY-AMT *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-CONTRIB-AMT > 0 AND XCLM-CONTRIB-AMT < 5.00
               MOVE 5.00 TO XCLM-CONTRIB-AMT
           END-IF
           IF XCLM-CONTRIB-AMT > 50.00
               MOVE 50.00 TO XCLM-CONTRIB-AMT
           END-IF
           ADD XCLM-CONTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE MISSED GRID TIER FOR THE ACCRUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3400-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MISSED-GRID-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF XCLM-ACCRUED-AMT NOT < WS-MISSED-GRID-LOW(WS-TX)
                   MOVE WS-MISSED-GRID-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3410-DESCRIBE-CYCLE.
           EVALUATE XCLM-CYCLE-CD
               WHEN 'EM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MISSED YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3420-LIMIT-MISSED.
           IF XCLM-MISSED-YTD < 100.00
               MOVE 100.00 TO XCLM-MISSED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-MISSED-YTD > 1000.00
               MOVE 1000.00 TO XCLM-MISSED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WEEKLY YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3430-ACCRUE-WEEKLY.
           IF XCLM-WEEKLY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-WEEKLY-YTD * XCLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-CONTRIB-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EM -> FE WHEN NOTICE DATE IS PAST
      * FE -> PL WHEN NOTICE DATE IS PAST
      * PL -> RU WHEN NOTICE DATE IS PAST
      * RU -> SU WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3440-SET-CYCLE.
           EVALUATE XCLM-CYCLE-CD
               WHEN 'EM'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FE' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FE'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PL' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RU' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SU' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WEEKLY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WEEKLY YTD.
      *----------------------------------------------------------------*
       3450-PRORATE-WEEKLY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-WEEKLY-YTD ROUNDED =
                   XCLM-WEEKLY-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-WEEKLY-YTD
           END-IF
           ADD XCLM-WEEKLY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3460-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF XCLM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUED AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3470-FLAG-ACCRUED.
           IF XCLM-ACCRUED-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-ACCRUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE CONTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3480-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF XCLT-CONTRIB-AMT NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * GOAL AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3490-FLAG-GOAL.
           IF XCLM-GOAL-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLM-GOAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ACCRUED AMT BY PAYOUT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3500-ACCUM-PAYOUT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-PAYOUT-METHOD-CD
                   ADD XCLM-ACCRUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-PAYOUT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-ACCRUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * FLM -> HLS WHEN EFF DATE IS PAST
      * HLS -> KMJ WHEN EFF DATE IS PAST
      * KMJ -> PDA WHEN EFF DATE IS PAST
      * PDA -> RVG WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3510-SET-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HLS' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HLS'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KMJ' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KMJ'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PDA' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PDA'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RVG' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3520-ACCRUE-CONTRIB.
           IF XCLM-CONTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-CONTRIB-AMT * XCLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-CONTRIB-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WEEKLY YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3530-ACCRUE-WEEKLY.
           IF XCLM-WEEKLY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-WEEKLY-YTD * XCLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO XCLT-BONUS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3540-COUNT-RETRY.
           IF XCLM-RETRY-CNT < 9999999
               ADD 1 TO XCLM-RETRY-CNT
           END-IF
           IF XCLM-RETRY-CNT NOT < 10
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3550-ACCRUE-CONTRIB.
           IF XCLM-CONTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-CONTRIB-AMT * XCLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO XCLT-CONTRIB-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GOAL TIER TIER FOR THE BONUS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3560-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GOAL-TIER-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF XCLT-BONUS-AMT NOT < WS-GOAL-TIER-LOW(WS-TX)
                   MOVE WS-GOAL-TIER-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E684 OPERATOR REQUIRED
      * E077 PAYOUT YTD NEGATIVE
      * E831 PAYOUT YTD OVER 5000
      * E560 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3570-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-OPERATOR = SPACES
                   MOVE 'E684' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-YTD < 0
                   MOVE 'E077' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-YTD > 5000.00
                   MOVE 'E831' TO WS-REJECT-CODE
               WHEN XCLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E560' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E518 MEMO REQUIRED
      * E025 PENALTY AMT NEGATIVE
      * E268 PENALTY AMT OVER 1000000
      * E637 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3580-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-MEMO = SPACES
                   MOVE 'E518' TO WS-REJECT-CODE
               WHEN XCLM-PENALTY-AMT < 0
                   MOVE 'E025' TO WS-REJECT-CODE
               WHEN XCLM-PENALTY-AMT > 1000000.00
                   MOVE 'E268' TO WS-REJECT-CODE
               WHEN XCLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E637' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BONUS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTRIB YTD.
      *----------------------------------------------------------------*
       3590-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-CONTRIB-YTD ROUNDED =
                   XCLM-BONUS-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-CONTRIB-YTD
           END-IF
           ADD XCLM-CONTRIB-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE MISSED YTD FEE ON THE BONUS AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3600-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO XCLM-MISSED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE XCLM-MISSED-YTD ROUNDED = XCLT-BONUS-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 12.00 TO XCLM-MISSED-YTD
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 5.00 TO XCLM-MISSED-YTD
               WHEN OTHER
                   COMPUTE XCLM-MISSED-YTD ROUNDED = XCLT-BONUS-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-MISSED-YTD > 0 AND XCLM-MISSED-YTD < 1.00
               MOVE 1.00 TO XCLM-MISSED-YTD
           END-IF
           IF XCLM-MISSED-YTD > 25.00
               MOVE 25.00 TO XCLM-MISSED-YTD
           END-IF
           ADD XCLM-MISSED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E235 CONTACT REQUIRED
      * E094 WEEKLY AMT NEGATIVE
      * E544 WEEKLY AMT OVER 5000
      * E013 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3610-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-CONTACT = SPACES
                   MOVE 'E235' TO WS-REJECT-CODE
               WHEN XCLM-WEEKLY-AMT < 0
                   MOVE 'E094' TO WS-REJECT-CODE
               WHEN XCLM-WEEKLY-AMT > 5000.00
                   MOVE 'E544' TO WS-REJECT-CODE
               WHEN XCLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E013' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE WEEKLY AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3620-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = XCLM-WEEKLY-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN XCLT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = XCLM-WEEKLY-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3630-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE MISSED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3640-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF XCLM-MISSED-AMT NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BONUS AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3650-SPLIT-BONUS.
           COMPUTE WS-WORK-AMT = XCLT-BONUS-AMT / 4
           COMPUTE WS-WORK-AMT-2 = XCLT-BONUS-AMT - (WS-WORK-AMT * 4)
           COMPUTE XCLM-WEEKLY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * WD -> DP WHEN NEXT REVIEW DATE IS PAST
      * DP -> PO WHEN NEXT REVIEW DATE IS PAST
      * PO -> BN WHEN NEXT REVIEW DATE IS PAST
      * BN -> PN WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3660-SET-TRAN.
           EVALUATE XCLT-TRAN-CD
               WHEN 'WD'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DP' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PO' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PO'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BN' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BN'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PN' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3670-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLT-TRAN-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MISSED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO GOAL AMT.
      *----------------------------------------------------------------*
       3680-PRORATE-GOAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-GOAL-AMT ROUNDED =
                   XCLM-MISSED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-GOAL-AMT
           END-IF
           ADD XCLM-GOAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * A -> B WHEN EFF DATE IS PAST
      * B -> D WHEN EFF DATE IS PAST
      * D -> E WHEN EFF DATE IS PAST
      * E -> T WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3690-SET-TYPE.
           EVALUATE XCLM-TYPE-CD
               WHEN 'A'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BONUS AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3700-SPLIT-BONUS.
           COMPUTE WS-WORK-AMT = XCLT-BONUS-AMT / 3
           COMPUTE WS-WORK-AMT-2 = XCLT-BONUS-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MISSED YTD FEE ON THE BONUS AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3710-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCLM-MISSED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   COMPUTE XCLM-MISSED-YTD ROUNDED = XCLT-BONUS-AMT *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 20.00 TO XCLM-MISSED-YTD
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 10.00 TO XCLM-MISSED-YTD
               WHEN OTHER
                   COMPUTE XCLM-MISSED-YTD ROUNDED = XCLT-BONUS-AMT *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-MISSED-YTD > 0 AND XCLM-MISSED-YTD < 2.00
               MOVE 2.00 TO XCLM-MISSED-YTD
           END-IF
           IF XCLM-MISSED-YTD > 75.00
               MOVE 75.00 TO XCLM-MISSED-YTD
           END-IF
           ADD XCLM-MISSED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MISSED YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3720-ACCRUE-MISSED.
           IF XCLM-MISSED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-MISSED-YTD * XCLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-MISSED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY FEE ON THE WEEKLY AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3730-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO XCLM-PENALTY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   COMPUTE XCLM-PENALTY-AMT ROUNDED = XCLM-WEEKLY-AMT *
                       XCLM-RATE / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 12.00 TO XCLM-PENALTY-AMT
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 15.00 TO XCLM-PENALTY-AMT
               WHEN OTHER
                   COMPUTE XCLM-PENALTY-AMT ROUNDED = XCLM-WEEKLY-AMT *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-PENALTY-AMT > 0 AND XCLM-PENALTY-AMT < 5.00
               MOVE 5.00 TO XCLM-PENALTY-AMT
           END-IF
           IF XCLM-PENALTY-AMT > 150.00
               MOVE 150.00 TO XCLM-PENALTY-AMT
           END-IF
           ADD XCLM-PENALTY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3740-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3750-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF XCLM-NEXT-REVIEW-DATE > 0 AND XCLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3760-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYOUT YTD FEE ON THE WEEKLY YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3770-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO XCLM-PAYOUT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE XCLM-PAYOUT-YTD ROUNDED = XCLM-WEEKLY-YTD *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-PAYOUT-METHOD-CD = 'B'
                   MOVE 15.00 TO XCLM-PAYOUT-YTD
               WHEN XCLM-PAYOUT-METHOD-CD = 'H'
                   MOVE 5.00 TO XCLM-PAYOUT-YTD
               WHEN OTHER
                   COMPUTE XCLM-PAYOUT-YTD ROUNDED = XCLM-WEEKLY-YTD *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-PAYOUT-YTD > 0 AND XCLM-PAYOUT-YTD < 5.00
               MOVE 5.00 TO XCLM-PAYOUT-YTD
           END-IF
           IF XCLM-PAYOUT-YTD > 75.00
               MOVE 75.00 TO XCLM-PAYOUT-YTD
           END-IF
           ADD XCLM-PAYOUT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WEEKLY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       3780-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-CONTRIB-AMT ROUNDED =
                   XCLM-WEEKLY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-CONTRIB-AMT
           END-IF
           ADD XCLM-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E480 CONTACT REQUIRED
      * E907 ACCRUED YTD NEGATIVE
      * E641 ACCRUED YTD OVER 99999
      * E871 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3790-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-CONTACT = SPACES
                   MOVE 'E480' TO WS-REJECT-CODE
               WHEN XCLM-ACCRUED-YTD < 0
                   MOVE 'E907' TO WS-REJECT-CODE
               WHEN XCLM-ACCRUED-YTD > 99999.00
                   MOVE 'E641' TO WS-REJECT-CODE
               WHEN XCLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E871' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BONUS AMT BY PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3800-ACCUM-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-PLAN-CD
                   ADD XCLM-BONUS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-BONUS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYOUT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3810-DESCRIBE-PAYOUT-METHOD.
           EVALUATE XCLM-PAYOUT-METHOD-CD
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE GOAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3820-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF XCLM-GOAL-AMT NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYOUT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3830-DESCRIBE-PAYOUT-METHOD.
           EVALUATE XCLM-PAYOUT-METHOD-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'U'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'W'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3840-DESCRIBE-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BONUS AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3850-ACCRUE-BONUS.
           IF XCLT-BONUS-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLT-BONUS-AMT * XCLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO XCLT-CONTRIB-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3860-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLT-BONUS-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD XCLT-BONUS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD XCLT-BONUS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLT-BONUS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLT-BONUS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WEEKLY AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3870-FLAG-WEEKLY.
           IF XCLT-WEEKLY-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLT-WEEKLY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYOUT METHOD CD:
      * B -> H WHEN EXPIRY DATE IS PAST
      * H -> U WHEN EXPIRY DATE IS PAST
      * U -> W WHEN EXPIRY DATE IS PAST
      * W -> B WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3880-SET-PAYOUT-METHOD.
           EVALUATE XCLM-PAYOUT-METHOD-CD
               WHEN 'B'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCRUED AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3890-LIMIT-ACCRUED.
           IF XCLM-ACCRUED-AMT < 25.00
               MOVE 25.00 TO XCLM-ACCRUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-ACCRUED-AMT > 10000.00
               MOVE 10000.00 TO XCLM-ACCRUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BONUS YTD.
      *----------------------------------------------------------------*
       3900-PRORATE-BONUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-BONUS-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-BONUS-YTD
           END-IF
           ADD XCLM-BONUS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3910-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF XCLM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E460 REJECT CD REQUIRED
      * E988 BONUS YTD NEGATIVE
      * E519 BONUS YTD OVER 5000
      * E678 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3920-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-REJECT-CD = SPACES
                   MOVE 'E460' TO WS-REJECT-CODE
               WHEN XCLM-BONUS-YTD < 0
                   MOVE 'E988' TO WS-REJECT-CODE
               WHEN XCLM-BONUS-YTD > 5000.00
                   MOVE 'E519' TO WS-REJECT-CODE
               WHEN XCLM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E678' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * GOAL YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3930-FLAG-GOAL.
           IF XCLM-GOAL-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-GOAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTRIB AMT BY PAYOUT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3940-ACCUM-PAYOUT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-PAYOUT-METHOD-CD
                   ADD XCLT-CONTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-PAYOUT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLT-CONTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCLM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO XCLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E693 DESC REQUIRED
      * E948 CONTRIB YTD NEGATIVE
      * E703 CONTRIB YTD OVER 10000
      * E371 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-DESC = SPACES
                   MOVE 'E693' TO WS-REJECT-CODE
               WHEN XCLM-CONTRIB-YTD < 0
                   MOVE 'E948' TO WS-REJECT-CODE
               WHEN XCLM-CONTRIB-YTD > 10000.00
                   MOVE 'E703' TO WS-REJECT-CODE
               WHEN XCLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E371' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3970-ACCRUE-CONTRIB.
           IF XCLM-CONTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-CONTRIB-AMT * XCLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE CONTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3980-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF XCLT-CONTRIB-AMT NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3990-ACCRUE-CONTRIB.
           IF XCLT-CONTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLT-CONTRIB-AMT * XCLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-CONTRIB-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PENALTY AMT.
      *----------------------------------------------------------------*
       4000-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-PENALTY-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-PENALTY-AMT
           END-IF
           ADD XCLM-PENALTY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * GOAL YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4010-LIMIT-GOAL.
           IF XCLM-GOAL-YTD < 10.00
               MOVE 10.00 TO XCLM-GOAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-GOAL-YTD > 5000.00
               MOVE 5000.00 TO XCLM-GOAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYOUT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4020-DESCRIBE-PAYOUT-METHOD.
           EVALUATE XCLM-PAYOUT-METHOD-CD
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'U'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE GOAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4030-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF XCLM-GOAL-YTD NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EM -> FE WHEN LAST PAY DATE IS PAST
      * FE -> PL WHEN LAST PAY DATE IS PAST
      * PL -> RU WHEN LAST PAY DATE IS PAST
      * RU -> SU WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4040-SET-CYCLE.
           EVALUATE XCLM-CYCLE-CD
               WHEN 'EM'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FE' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FE'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PL' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RU' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SU' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4050-COUNT-RETRY.
           IF XCLM-RETRY-CNT < 9999999
               ADD 1 TO XCLM-RETRY-CNT
           END-IF
           IF XCLM-RETRY-CNT NOT < 3
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WEEKLY AMT.
      *----------------------------------------------------------------*
       4060-PRORATE-WEEKLY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-WEEKLY-AMT ROUNDED =
                   XCLT-CONTRIB-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-WEEKLY-AMT
           END-IF
           ADD XCLM-WEEKLY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE PAYOUT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4070-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF XCLM-PAYOUT-AMT NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO GOAL AMT.
      *----------------------------------------------------------------*
       4080-PRORATE-GOAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-GOAL-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLT-GOAL-AMT
           END-IF
           ADD XCLT-GOAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WEEKLY YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4090-LIMIT-WEEKLY.
           IF XCLM-WEEKLY-YTD < 10.00
               MOVE 10.00 TO XCLM-WEEKLY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-WEEKLY-YTD > 50000.00
               MOVE 50000.00 TO XCLM-WEEKLY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4100-DESCRIBE-CYCLE.
           EVALUATE XCLM-CYCLE-CD
               WHEN 'EM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * A -> B WHEN LAST PAY DATE IS PAST
      * B -> D WHEN LAST PAY DATE IS PAST
      * D -> E WHEN LAST PAY DATE IS PAST
      * E -> T WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4110-SET-TYPE.
           EVALUATE XCLM-TYPE-CD
               WHEN 'A'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE MISSED GRID TIER FOR THE BONUS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4120-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-MISSED-GRID-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF XCLT-BONUS-AMT NOT < WS-MISSED-GRID-LOW(WS-TX)
                   MOVE WS-MISSED-GRID-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4130-ACCRUE-CONTRIB.
           IF XCLM-CONTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-CONTRIB-YTD * XCLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-ACCRUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * GOAL YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4140-LIMIT-GOAL.
           IF XCLM-GOAL-YTD < 0.00
               MOVE 0.00 TO XCLM-GOAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-GOAL-YTD > 2500.00
               MOVE 2500.00 TO XCLM-GOAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYOUT METHOD CD:
      * B -> H WHEN NEXT DUE DATE IS PAST
      * H -> U WHEN NEXT DUE DATE IS PAST
      * U -> W WHEN NEXT DUE DATE IS PAST
      * W -> B WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4150-SET-PAYOUT-METHOD.
           EVALUATE XCLM-PAYOUT-METHOD-CD
               WHEN 'B'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4160-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF XCLT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ACCRUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTRIB YTD.
      *----------------------------------------------------------------*
       4170-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-CONTRIB-YTD ROUNDED =
                   XCLM-ACCRUED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-CONTRIB-YTD
           END-IF
           ADD XCLM-CONTRIB-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * MISSED YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4180-LIMIT-MISSED.
           IF XCLM-MISSED-YTD < 100.00
               MOVE 100.00 TO XCLM-MISSED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-MISSED-YTD > 10000.00
               MOVE 10000.00 TO XCLM-MISSED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MISSED YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4190-ACCRUE-MISSED.
           IF XCLM-MISSED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-MISSED-YTD * XCLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-PENALTY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTRIB AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4200-FLAG-CONTRIB.
           IF XCLT-CONTRIB-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLT-CONTRIB-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4210-DESCRIBE-TYPE.
           EVALUATE XCLM-TYPE-CD
               WHEN 'A'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'D'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'E'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE XCL FIELDS TO XCPEN AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-XCPEN.
           INITIALIZE XCPEN-PARMS
           MOVE XCLM-MISSED-YTD
             TO XCPEN-MISSED-YTD
           MOVE XCLM-GOAL-AMT
             TO XCPEN-GOAL-AMT
           MOVE XCLM-WEEKLY-AMT
             TO XCPEN-WEEKLY-AMT
           MOVE XCLM-GOAL-YTD
             TO XCPEN-GOAL-YTD
           MOVE XCLM-BONUS-YTD
             TO XCPEN-BONUS-YTD
           MOVE XCLM-CONTRIB-AMT
             TO XCPEN-CONTRIB-AMT
           MOVE XCLM-LAST-PAY-DATE
             TO XCPEN-LAST-PAY-DATE
           MOVE XCLM-NOTICE-DATE
             TO XCPEN-NOTICE-DATE
           MOVE XCLM-OPEN-DATE
             TO XCPEN-OPEN-DATE
           MOVE XCLM-MATURITY-DATE
             TO XCPEN-MATURITY-DATE
           MOVE XCLM-PAYOUT-METHOD-CD
             TO XCPEN-PAYOUT-METHOD-CD
           MOVE XCLM-PLAN-CD
             TO XCPEN-PLAN-CD
           MOVE XCLM-REGION-CD
             TO XCPEN-REGION-CD
           MOVE XCLM-TYPE-CD
             TO XCPEN-TYPE-CD
           MOVE XCLM-OVERRIDE-FLAG
             TO XCPEN-OVERRIDE-FLAG
           MOVE XCLM-PREFERRED-FLAG
             TO XCPEN-PREFERRED-FLAG
           MOVE XCLM-STOP-FLAG
             TO XCPEN-STOP-FLAG
           MOVE XCLM-CYCLE-CNT
             TO XCPEN-CYCLE-CNT
           MOVE XCLM-NOTICE-CNT
             TO XCPEN-NOTICE-CNT
           MOVE XCLM-RATE
             TO XCPEN-RATE
           MOVE XCLM-FEE-PCT
             TO XCPEN-FEE-PCT
           MOVE XCLM-CUST-ID
             TO XCPEN-CUST-ID
           MOVE XCLM-CONTACT
             TO XCPEN-CONTACT
           CALL 'XCPEN' USING XCPEN-PARMS
           EVALUATE XCPEN-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE XCPEN-MISSED-YTD
                     TO XCLM-MISSED-YTD
                   MOVE XCPEN-BONUS-YTD
                     TO XCLM-BONUS-YTD
                   MOVE XCPEN-STOP-FLAG
                     TO XCLM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-XCPEN' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE XCL FIELDS TO XCFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-XCFEE.
           INITIALIZE XCFEE-PARMS
           MOVE XCLM-BONUS-YTD
             TO XCFEE-BONUS-YTD
           MOVE XCLM-MISSED-YTD
             TO XCFEE-MISSED-YTD
           MOVE XCLM-GOAL-YTD
             TO XCFEE-GOAL-YTD
           MOVE XCLM-WEEKLY-AMT
             TO XCFEE-WEEKLY-AMT
           MOVE XCLM-GOAL-AMT
             TO XCFEE-GOAL-AMT
           MOVE XCLM-CONTRIB-AMT
             TO XCFEE-CONTRIB-AMT
           MOVE XCLM-MATURITY-DATE
             TO XCFEE-MATURITY-DATE
           MOVE XCLM-NOTICE-DATE
             TO XCFEE-NOTICE-DATE
           MOVE XCLM-LAST-PAY-DATE
             TO XCFEE-LAST-PAY-DATE
           MOVE XCLM-NEXT-DUE-DATE
             TO XCFEE-NEXT-DUE-DATE
           MOVE XCLM-REGION-CD
             TO XCFEE-REGION-CD
           MOVE XCLM-REASON-CD
             TO XCFEE-REASON-CD
           MOVE XCLM-CYCLE-CD
             TO XCFEE-CYCLE-CD
           MOVE XCLM-TYPE-CD
             TO XCFEE-TYPE-CD
           MOVE XCLM-AUTO-RENEW-FLAG
             TO XCFEE-AUTO-RENEW-FLAG
           MOVE XCLM-EXEMPT-FLAG
             TO XCFEE-EXEMPT-FLAG
           MOVE XCLM-STOP-FLAG
             TO XCFEE-STOP-FLAG
           MOVE XCLM-NOTICE-CNT
             TO XCFEE-NOTICE-CNT
           MOVE XCLM-CYCLE-CNT
             TO XCFEE-CYCLE-CNT
           MOVE XCLM-DISCOUNT
             TO XCFEE-DISCOUNT
           MOVE XCLM-RATE
             TO XCFEE-RATE
           MOVE XCLM-DESC
             TO XCFEE-DESC
           MOVE XCLM-REF-NO
             TO XCFEE-REF-NO
           CALL 'XCFEE' USING XCFEE-PARMS
           EVALUATE XCFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE XCFEE-GOAL-YTD
                     TO XCLM-GOAL-YTD
                   MOVE XCFEE-WEEKLY-AMT
                     TO XCLM-WEEKLY-AMT
                   MOVE XCFEE-GOAL-AMT
                     TO XCLM-GOAL-AMT
                   MOVE XCFEE-AUTO-RENEW-FLAG
                     TO XCLM-AUTO-RENEW-FLAG
                   MOVE XCFEE-EXEMPT-FLAG
                     TO XCLM-EXEMPT-FLAG
                   MOVE XCFEE-STOP-FLAG
                     TO XCLM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-XCFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE XCL FIELDS TO XCLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-XCLIM.
           INITIALIZE XCLIM-PARMS
           MOVE XCLM-PAYOUT-YTD
             TO XCLIM-PAYOUT-YTD
           MOVE XCLM-ACCRUED-AMT
             TO XCLIM-ACCRUED-AMT
           MOVE XCLM-GOAL-AMT
             TO XCLIM-GOAL-AMT
           MOVE XCLM-PAYOUT-AMT
             TO XCLIM-PAYOUT-AMT
           MOVE XCLM-WEEKLY-AMT
             TO XCLIM-WEEKLY-AMT
           MOVE XCLM-PENALTY-YTD
             TO XCLIM-PENALTY-YTD
           MOVE XCLM-NOTICE-DATE
             TO XCLIM-NOTICE-DATE
           MOVE XCLM-OPEN-DATE
             TO XCLIM-OPEN-DATE
           MOVE XCLM-LAST-PAY-DATE
             TO XCLIM-LAST-PAY-DATE
           MOVE XCLM-LAST-STMT-DATE
             TO XCLIM-LAST-STMT-DATE
           MOVE XCLM-REGION-CD
             TO XCLIM-REGION-CD
           MOVE XCLM-PLAN-CD
             TO XCLIM-PLAN-CD
           MOVE XCLM-CYCLE-CD
             TO XCLIM-CYCLE-CD
           MOVE XCLM-REASON-CD
             TO XCLIM-REASON-CD
           MOVE XCLM-SUSPEND-FLAG
             TO XCLIM-SUSPEND-FLAG
           MOVE XCLM-STOP-FLAG
             TO XCLIM-STOP-FLAG
           MOVE XCLM-PREFERRED-FLAG
             TO XCLIM-PREFERRED-FLAG
           MOVE XCLM-PAYMENT-CNT
             TO XCLIM-PAYMENT-CNT
           MOVE XCLM-CYCLE-CNT
             TO XCLIM-CYCLE-CNT
           MOVE XCLM-DISCOUNT
             TO XCLIM-DISCOUNT
           MOVE XCLM-FEE-PCT
             TO XCLIM-FEE-PCT
           MOVE XCLM-REF-NO
             TO XCLIM-REF-NO
           MOVE XCLM-ADDR
             TO XCLIM-ADDR
           CALL 'XCLIM' USING XCLIM-PARMS
           EVALUATE XCLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE XCLIM-ACCRUED-AMT
                     TO XCLM-ACCRUED-AMT
                   MOVE XCLIM-PAYOUT-AMT
                     TO XCLM-PAYOUT-AMT
                   MOVE XCLIM-WEEKLY-AMT
                     TO XCLM-WEEKLY-AMT
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-XCLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE XCLT-KEY TO RD-KEY
           MOVE XCLT-TRAN-CD TO RD-CODE
           MOVE XCLT-BONUS-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE XCLT-KEY TO RD-KEY
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
           DISPLAY 'XCLSETL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'XCLSETL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
