      *================================================================*
      * PROGRAM  : XCLPOST                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : CHRISTMAS CLUB SAVINGS - DAILY POSTING. READS THE   *
      *            XCL TRANSACTIONS, UPDATES THE XCL MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. HOLIDAY CLUB SAVINGS   *
      *            PLANS WITH WEEKLY DEPOSITS AND NOVEMBER PAYOUT      *
      * INPUT    : XCLMST (KSDS), XCLTRN, BANKCTL                      *
      * OUTPUT   : XCLOUT, XCLPORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, XCLIM, XCFEE, XCCAL     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-09-23 SKN  ORIGINAL                                       *
      * 1994-10-05 JLM  PENALTY CUTOFF MOVED INC7292                   *
      * 1994-02-02 BHS  ACCRUED CUTOFF MOVED PRB5500                   *
      * 1994-10-02 TWH  WEEKLY FEE SCHEDULE UPDATE PRB8983             *
      * 1999-11-05 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-04-01 SKN  PERFORMANCE - CONTRIB READ AHEAD PRB2601       *
      * 2003-06-09 SKN  PAYOUT EXCEPTIONS TO OPS QUEUE INC3807         *
      * 2003-06-25 CRP  WEEKLY ROUNDING - HALF UP REQ6308              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. XCLPOST.
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
           SELECT REPORT-FILE ASSIGN TO "XCLPORPT"
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
                                           VALUE 9.000000.
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
      * WS-ACCRUED-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ACCRUED-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000884500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000354500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000615000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000821500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018500000000160000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019500000000696500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022000000000889500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024500000000406000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024750000000173500'.
       01  WS-ACCRUED-SCHED-TBL REDEFINES WS-ACCRUED-SCHED-VALUES.
           05  WS-ACCRUED-SCHED-ROW OCCURS 9 TIMES.
               10  WS-ACCRUED-SCHED-LOW    PIC 9(09)V99.
               10  WS-ACCRUED-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PENALTY-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PENALTY-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000331000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000123000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000166000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001750000000173000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011750000000306000'.
       01  WS-PENALTY-TIER-TBL REDEFINES WS-PENALTY-TIER-VALUES.
           05  WS-PENALTY-TIER-ROW OCCURS 5 TIMES.
               10  WS-PENALTY-TIER-LOW     PIC 9(09)V99.
               10  WS-PENALTY-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PENALTY-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PENALTY-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000331000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000518000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000559000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015050000000454000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015550000000185500'.
       01  WS-PENALTY-SCHED-TBL REDEFINES WS-PENALTY-SCHED-VALUES.
           05  WS-PENALTY-SCHED-ROW OCCURS 5 TIMES.
               10  WS-PENALTY-SCHED-LOW    PIC 9(09)V99.
               10  WS-PENALTY-SCHED-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  XCLPOST  CHRISTMAS CLUB SAVINGS'.
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
       COPY XCLIMP.
       COPY XCFEEP.
       COPY XCCALP.

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
               PERFORM 3000-ACCUM-PAYOUT-METHOD
               PERFORM 3010-CHECK-LAST-PAY
               PERFORM 3020-AGE-OPEN
               PERFORM 3030-PRORATE-MISSED
               PERFORM 3040-LIMIT-ITEM
               PERFORM 3050-PRORATE-ACCRUED
               PERFORM 3060-FLAG-BONUS
               PERFORM 3070-FLAG-PENALTY
               PERFORM 3080-COUNT-CYCLE
               PERFORM 3090-DESCRIBE-PAYOUT-METHOD
               PERFORM 3100-ACCUM-REASON
               PERFORM 3110-SELECT-DISCOUNT
               PERFORM 3120-ACCUM-CHANNEL
               PERFORM 3130-FLAG-CONTRIB
               PERFORM 3140-PRORATE-PAYOUT
               PERFORM 3150-COUNT-RETRY
               PERFORM 3160-SET-PLAN
               PERFORM 3170-SPLIT-MISSED
               PERFORM 3180-SPLIT-CONTRIB
               PERFORM 3190-SET-PLAN
               PERFORM 3200-LIMIT-ACCRUED
               PERFORM 3210-COUNT-CYCLE
               PERFORM 3220-ASSESS-SUSPEND
               PERFORM 3230-COUNT-CYCLE
               PERFORM 3240-COUNT-SEQ
               PERFORM 3250-ACCUM-PLAN
               PERFORM 3260-COUNT-SEQ
               PERFORM 3270-EDIT-CONTACT
               PERFORM 3280-SELECT-DISCOUNT
               PERFORM 3290-ACCRUE-RATE
               PERFORM 3300-LIMIT-BONUS
               PERFORM 3310-SET-PAYOUT-METHOD
               PERFORM 3320-FLAG-CONTRIB
               PERFORM 3330-ACCRUE-PAYOUT
               PERFORM 3340-FLAG-CONTRIB
               PERFORM 3350-ACCUM-REASON
               PERFORM 3360-ASSESS-STOP
               PERFORM 3370-ASSESS-STOP
               PERFORM 3380-PRORATE-CONTRIB
               PERFORM 3390-SPLIT-BONUS
               PERFORM 3400-SPLIT-PENALTY
               PERFORM 3410-SELECT-DISCOUNT
               PERFORM 3420-EDIT-NAME
               PERFORM 3430-SET-REGION
               PERFORM 3440-AGE-NOTICE
               PERFORM 3450-ASSESS-OVERRIDE
               PERFORM 3460-ACCRUE-ACCRUED
               PERFORM 3470-LIMIT-BONUS
               PERFORM 3480-FLAG-ACCRUED
               PERFORM 3490-ASSESS-SUSPEND
               PERFORM 3500-SPLIT-BONUS
               PERFORM 3510-EDIT-MEMO
               PERFORM 3520-COUNT-CYCLE
               PERFORM 3530-DESCRIBE-CYCLE
               PERFORM 3540-SET-CHANNEL
               PERFORM 3550-FLAG-BONUS
               PERFORM 3560-ASSESS-STOP
               PERFORM 3570-FLAG-ACCRUED
               PERFORM 3580-COUNT-RETRY
               PERFORM 3590-SPLIT-BONUS
               PERFORM 3600-LIMIT-CONTRIB
               PERFORM 3610-EDIT-NAME
               PERFORM 3620-ACCRUE-WEEKLY
               PERFORM 3630-AGE-MATURITY
               PERFORM 3640-LIMIT-WEEKLY
               PERFORM 3650-ACCRUE-CONTRIB
               PERFORM 3660-SPLIT-CONTRIB
               PERFORM 3670-COUNT-SEQ
               PERFORM 3680-SET-CHANNEL
               PERFORM 3690-COUNT-RETRY
               PERFORM 3700-ACCRUE-PENALTY
               PERFORM 3710-SPLIT-ACCRUED
               PERFORM 3720-EDIT-NAME
               PERFORM 3730-SPLIT-CONTRIB
               PERFORM 3740-ASSESS-STOP
               PERFORM 3750-SELECT-FEE-PCT
               PERFORM 3760-SPLIT-CONTRIB
               PERFORM 3770-SPLIT-CONTRIB
               PERFORM 3780-FLAG-BONUS
               PERFORM 3790-SET-REASON
               PERFORM 3800-SELECT-FEE-PCT
               PERFORM 3810-ACCUM-TRAN
               PERFORM 3820-CHECK-LAST-ACT
               PERFORM 3830-COUNT-CYCLE
               PERFORM 3840-CHECK-POST
               PERFORM 3850-FLAG-WEEKLY
               PERFORM 3860-PRORATE-PAYOUT
               PERFORM 3870-ASSESS-AUTO-RENEW
               PERFORM 3880-AGE-LAST-STMT
               PERFORM 3890-SELECT-DISCOUNT
               PERFORM 3900-ACCRUE-PENALTY
               PERFORM 3910-PRORATE-GOAL
               PERFORM 3920-SPLIT-GOAL
               PERFORM 3930-SELECT-DISCOUNT
               PERFORM 3940-EDIT-CUST-ID
               PERFORM 3950-SET-CHANNEL
               PERFORM 3960-ACCRUE-MISSED
               PERFORM 3970-PRORATE-CONTRIB
               PERFORM 3980-DESCRIBE-PLAN
               PERFORM 3990-EDIT-CUST-ID
               PERFORM 4000-ACCRUE-GOAL
               PERFORM 4010-CHECK-EFF
               PERFORM 4020-EDIT-REJECT
               PERFORM 4030-DESCRIBE-CHANNEL
               PERFORM 4040-COUNT-RETRY
               PERFORM 4050-SELECT-FEE-PCT
               PERFORM 4060-AGE-NOTICE
               PERFORM 4070-PRORATE-MISSED
               PERFORM 4080-SET-CYCLE
               PERFORM 4090-COUNT-CYCLE
               PERFORM 4100-ACCRUE-CONTRIB
               PERFORM 4110-CHECK-LAST-STMT
               PERFORM 4120-SPLIT-CONTRIB
               PERFORM 4130-SPLIT-WEEKLY
               PERFORM 4140-ACCUM-PAYOUT-METHOD
               PERFORM 4150-SPLIT-CONTRIB
               PERFORM 4160-ACCRUE-GOAL
               PERFORM 4170-PRORATE-WEEKLY
               PERFORM 4180-CHECK-EFF
               PERFORM 4190-SELECT-RATE
               PERFORM 4200-SPLIT-BONUS
               PERFORM 4210-ASSESS-EXEMPT
               PERFORM 4220-DESCRIBE-TRAN
               PERFORM 4230-LIMIT-WEEKLY
               PERFORM 4240-ASSESS-PREFERRED
               PERFORM 4250-ACCUM-PLAN
               PERFORM 4260-CHECK-NOTICE
               PERFORM 4270-SET-REASON
               PERFORM 4280-COUNT-PAYMENT
               PERFORM 4290-COUNT-CYCLE
               PERFORM 4300-ACCUM-CYCLE
               PERFORM 4310-CHECK-EFF
               PERFORM 4320-SET-PLAN
               PERFORM 4330-DESCRIBE-CHANNEL
               PERFORM 4340-ACCRUE-MISSED
               PERFORM 4350-SET-REGION
               PERFORM 4360-ASSESS-EXEMPT
               PERFORM 4370-SELECT-DISCOUNT
               PERFORM 4380-CHECK-LAST-ACT
               PERFORM 4390-DESCRIBE-PAYOUT-METHOD
               PERFORM 4400-EDIT-REJECT
               PERFORM 4410-SET-TYPE
               PERFORM 4420-ASSESS-PREFERRED
               PERFORM 4430-AGE-OPEN
               PERFORM 4440-EDIT-DESC
               PERFORM 4450-SPLIT-CONTRIB
               PERFORM 4460-PRORATE-PENALTY
               PERFORM 4470-SPLIT-BONUS
               PERFORM 4480-LIMIT-CONTRIB
               PERFORM 4490-AGE-LAST-ACT
               PERFORM 4500-AGE-MATURITY
               PERFORM 4510-DESCRIBE-CHANNEL
               PERFORM 4520-FLAG-ACCRUED
               PERFORM 4530-ACCRUE-WEEKLY
               PERFORM 4540-ACCRUE-MISSED
               PERFORM 4550-COUNT-CYCLE
               PERFORM 4560-ACCUM-PAYOUT-METHOD
               PERFORM 4570-SET-PLAN
               PERFORM 4580-FLAG-GOAL
               PERFORM 4590-AGE-LAST-ACT
               PERFORM 4600-ACCUM-PAYOUT-METHOD
               PERFORM 4610-LIMIT-PAYOUT
               PERFORM 4620-LIMIT-WEEKLY
               PERFORM 4630-SPLIT-CONTRIB
               PERFORM 4640-PRORATE-CONTRIB
               PERFORM 4650-FLAG-ITEM
               PERFORM 4660-ASSESS-EXEMPT
               PERFORM 4670-LIMIT-GOAL
               PERFORM 4680-COUNT-CYCLE
               PERFORM 4690-SPLIT-CONTRIB
               PERFORM 4700-ASSESS-SUSPEND
               PERFORM 4710-SELECT-FEE-PCT
               PERFORM 4720-AGE-MATURITY
               PERFORM 4730-AGE-EXPIRY
               PERFORM 4740-LIMIT-BONUS
               PERFORM 4750-DESCRIBE-PLAN
               PERFORM 4760-ACCRUE-CONTRIB
               PERFORM 4770-PRORATE-BONUS
               PERFORM 4780-ACCRUE-BONUS
               PERFORM 4790-AGE-OPEN
               PERFORM 4800-DESCRIBE-REGION
               PERFORM 4810-SET-REGION
               PERFORM 4820-FLAG-PAYOUT
               PERFORM 4830-ASSESS-OVERRIDE
               PERFORM 4840-EDIT-MEMO
               PERFORM 4850-DESCRIBE-REGION
               PERFORM 7010-CALL-XCLIM
               PERFORM 7020-CALL-XCFEE
               PERFORM 7040-CALL-XCCAL
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
      * ACCUMULATE CONTRIB YTD BY PAYOUT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3000-ACCUM-PAYOUT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-PAYOUT-METHOD-CD
                   ADD XCLM-CONTRIB-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-PAYOUT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-CONTRIB-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3010-CHECK-LAST-PAY.
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
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3020-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ACCRUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MISSED AMT.
      *----------------------------------------------------------------*
       3030-PRORATE-MISSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-MISSED-AMT ROUNDED =
                   XCLM-ACCRUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-MISSED-AMT
           END-IF
           ADD XCLM-MISSED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3040-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BONUS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ACCRUED YTD.
      *----------------------------------------------------------------*
       3050-PRORATE-ACCRUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-ACCRUED-YTD ROUNDED =
                   XCLM-BONUS-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-ACCRUED-YTD
           END-IF
           ADD XCLM-ACCRUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * BONUS AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3060-FLAG-BONUS.
           IF XCLT-BONUS-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCLT-BONUS-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3070-FLAG-PENALTY.
           IF XCLM-PENALTY-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCLM-PENALTY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3080-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYOUT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3090-DESCRIBE-PAYOUT-METHOD.
           EVALUATE XCLM-PAYOUT-METHOD-CD
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE BONUS YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-REASON-CD
                   ADD XCLM-BONUS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-BONUS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY SCHED TIER FOR THE BONUS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3110-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-SCHED-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF XCLT-BONUS-AMT NOT < WS-PENALTY-SCHED-LOW(WS-TX)
                   MOVE WS-PENALTY-SCHED-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GOAL YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3120-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLT-CHANNEL-CD
                   ADD XCLM-GOAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-GOAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CONTRIB YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3130-FLAG-CONTRIB.
           IF XCLM-CONTRIB-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-CONTRIB-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYOUT AMT.
      *----------------------------------------------------------------*
       3140-PRORATE-PAYOUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-PAYOUT-AMT ROUNDED =
                   XCLT-CONTRIB-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-PAYOUT-AMT
           END-IF
           ADD XCLM-PAYOUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3150-COUNT-RETRY.
           IF XCLM-RETRY-CNT < 9999999
               ADD 1 TO XCLM-RETRY-CNT
           END-IF
           IF XCLM-RETRY-CNT NOT < 3
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN CD:
      * GN -> JT WHEN LAST STMT DATE IS PAST
      * JT -> KC WHEN LAST STMT DATE IS PAST
      * KC -> KW WHEN LAST STMT DATE IS PAST
      * KW -> MX WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3160-SET-PLAN.
           EVALUATE XCLM-PLAN-CD
               WHEN 'GN'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JT'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KC' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KC'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MX' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT MISSED YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-MISSED.
           COMPUTE WS-WORK-AMT = XCLM-MISSED-YTD / 4
           COMPUTE WS-WORK-AMT-2 = XCLM-MISSED-YTD - (WS-WORK-AMT * 4)
           COMPUTE XCLM-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3180-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCLM-CONTRIB-AMT / 2
           COMPUTE WS-WORK-AMT-2 = XCLM-CONTRIB-AMT - (WS-WORK-AMT * 2)
           COMPUTE XCLM-PAYOUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN CD:
      * GN -> JT WHEN NEXT REVIEW DATE IS PAST
      * JT -> KC WHEN NEXT REVIEW DATE IS PAST
      * KC -> KW WHEN NEXT REVIEW DATE IS PAST
      * KW -> MX WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3190-SET-PLAN.
           EVALUATE XCLM-PLAN-CD
               WHEN 'GN'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JT' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JT'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KC' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KC'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KW' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MX' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCRUED AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3200-LIMIT-ACCRUED.
           IF XCLM-ACCRUED-AMT < 100.00
               MOVE 100.00 TO XCLM-ACCRUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-ACCRUED-AMT > 5000.00
               MOVE 5000.00 TO XCLM-ACCRUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB YTD FEE ON THE PENALTY YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3220-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO XCLM-CONTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   COMPUTE XCLM-CONTRIB-YTD ROUNDED = XCLM-PENALTY-YTD *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-CYCLE-CD = 'EM'
                   MOVE 20.00 TO XCLM-CONTRIB-YTD
               WHEN XCLM-CYCLE-CD = 'FE'
                   MOVE 7.00 TO XCLM-CONTRIB-YTD
               WHEN OTHER
                   COMPUTE XCLM-CONTRIB-YTD ROUNDED = XCLM-PENALTY-YTD *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-CONTRIB-YTD > 0 AND XCLM-CONTRIB-YTD < 10.00
               MOVE 10.00 TO XCLM-CONTRIB-YTD
           END-IF
           IF XCLM-CONTRIB-YTD > 75.00
               MOVE 75.00 TO XCLM-CONTRIB-YTD
           END-IF
           ADD XCLM-CONTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3230-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3240-COUNT-SEQ.
           IF XCLT-SEQ < 9999999
               ADD 1 TO XCLT-SEQ
           END-IF
           IF XCLT-SEQ NOT < 12
               MOVE 'Y' TO XCLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BONUS AMT BY PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3250-ACCUM-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-PLAN-CD
                   ADD XCLT-BONUS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLT-BONUS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3260-COUNT-SEQ.
           IF XCLT-SEQ < 9999999
               ADD 1 TO XCLT-SEQ
           END-IF
           IF XCLT-SEQ NOT < 5
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E951 CONTACT REQUIRED
      * E858 ACCRUED YTD NEGATIVE
      * E908 ACCRUED YTD OVER 25000
      * E316 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3270-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-CONTACT = SPACES
                   MOVE 'E951' TO WS-REJECT-CODE
               WHEN XCLM-ACCRUED-YTD < 0
                   MOVE 'E858' TO WS-REJECT-CODE
               WHEN XCLM-ACCRUED-YTD > 25000.00
                   MOVE 'E908' TO WS-REJECT-CODE
               WHEN XCLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E316' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY SCHED TIER FOR THE WEEKLY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3280-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-SCHED-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF XCLT-WEEKLY-AMT NOT < WS-PENALTY-SCHED-LOW(WS-TX)
                   MOVE WS-PENALTY-SCHED-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3290-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCLT-BONUS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BONUS YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3300-LIMIT-BONUS.
           IF XCLM-BONUS-YTD < 100.00
               MOVE 100.00 TO XCLM-BONUS-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-BONUS-YTD > 500.00
               MOVE 500.00 TO XCLM-BONUS-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYOUT METHOD CD:
      * B -> H WHEN MATURITY DATE IS PAST
      * H -> U WHEN MATURITY DATE IS PAST
      * U -> W WHEN MATURITY DATE IS PAST
      * W -> B WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3310-SET-PAYOUT-METHOD.
           EVALUATE XCLM-PAYOUT-METHOD-CD
               WHEN 'B'
                   IF XCLM-MATURITY-DATE > 0 AND XCLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF XCLM-MATURITY-DATE > 0 AND XCLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF XCLM-MATURITY-DATE > 0 AND XCLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF XCLM-MATURITY-DATE > 0 AND XCLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONTRIB AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3320-FLAG-CONTRIB.
           IF XCLT-CONTRIB-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCLT-CONTRIB-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYOUT YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3330-ACCRUE-PAYOUT.
           IF XCLM-PAYOUT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-PAYOUT-YTD * XCLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-BONUS-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTRIB YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3340-FLAG-CONTRIB.
           IF XCLM-CONTRIB-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLM-CONTRIB-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MISSED AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3350-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-REASON-CD
                   ADD XCLM-MISSED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-MISSED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GOAL FEE ON THE BONUS AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3360-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCLT-GOAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   COMPUTE XCLT-GOAL-AMT ROUNDED = XCLM-BONUS-AMT *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 15.00 TO XCLT-GOAL-AMT
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 5.00 TO XCLT-GOAL-AMT
               WHEN OTHER
                   COMPUTE XCLT-GOAL-AMT ROUNDED = XCLM-BONUS-AMT *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLT-GOAL-AMT > 0 AND XCLT-GOAL-AMT < 1.00
               MOVE 1.00 TO XCLT-GOAL-AMT
           END-IF
           IF XCLT-GOAL-AMT > 100.00
               MOVE 100.00 TO XCLT-GOAL-AMT
           END-IF
           ADD XCLT-GOAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUED YTD FEE ON THE WEEKLY AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3370-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCLM-ACCRUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   COMPUTE XCLM-ACCRUED-YTD ROUNDED = XCLT-WEEKLY-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-REGION-CD = 'FLM'
                   MOVE 15.00 TO XCLM-ACCRUED-YTD
               WHEN XCLM-REGION-CD = 'HLS'
                   MOVE 15.00 TO XCLM-ACCRUED-YTD
               WHEN OTHER
                   COMPUTE XCLM-ACCRUED-YTD ROUNDED = XCLT-WEEKLY-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-ACCRUED-YTD > 0 AND XCLM-ACCRUED-YTD < 1.00
               MOVE 1.00 TO XCLM-ACCRUED-YTD
           END-IF
           IF XCLM-ACCRUED-YTD > 250.00
               MOVE 250.00 TO XCLM-ACCRUED-YTD
           END-IF
           ADD XCLM-ACCRUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE BONUS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       3380-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-CONTRIB-AMT ROUNDED =
                   XCLT-BONUS-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-CONTRIB-AMT
           END-IF
           ADD XCLM-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT BONUS AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3390-SPLIT-BONUS.
           COMPUTE WS-WORK-AMT = XCLT-BONUS-AMT / 2
           COMPUTE WS-WORK-AMT-2 = XCLT-BONUS-AMT - (WS-WORK-AMT * 2)
           COMPUTE XCLT-GOAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PENALTY YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3400-SPLIT-PENALTY.
           COMPUTE WS-WORK-AMT = XCLM-PENALTY-YTD / 4
           COMPUTE WS-WORK-AMT-2 = XCLM-PENALTY-YTD - (WS-WORK-AMT * 4)
           COMPUTE XCLM-MISSED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE PAYOUT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3410-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF XCLM-PAYOUT-YTD NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E870 NAME REQUIRED
      * E714 CONTRIB AMT NEGATIVE
      * E858 CONTRIB AMT OVER 25000
      * E473 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3420-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-NAME = SPACES
                   MOVE 'E870' TO WS-REJECT-CODE
               WHEN XCLT-CONTRIB-AMT < 0
                   MOVE 'E714' TO WS-REJECT-CODE
               WHEN XCLT-CONTRIB-AMT > 25000.00
                   MOVE 'E858' TO WS-REJECT-CODE
               WHEN XCLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E473' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * FLM -> HLS WHEN LAST ACT DATE IS PAST
      * HLS -> KMJ WHEN LAST ACT DATE IS PAST
      * KMJ -> PDA WHEN LAST ACT DATE IS PAST
      * PDA -> RVG WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3430-SET-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   IF XCLM-LAST-ACT-DATE > 0 AND XCLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HLS' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HLS'
                   IF XCLM-LAST-ACT-DATE > 0 AND XCLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KMJ' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KMJ'
                   IF XCLM-LAST-ACT-DATE > 0 AND XCLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PDA' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PDA'
                   IF XCLM-LAST-ACT-DATE > 0 AND XCLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RVG' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3440-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYOUT FEE ON THE CONTRIB AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3450-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO XCLM-PAYOUT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   COMPUTE XCLM-PAYOUT-AMT ROUNDED = XCLT-CONTRIB-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 20.00 TO XCLM-PAYOUT-AMT
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 15.00 TO XCLM-PAYOUT-AMT
               WHEN OTHER
                   COMPUTE XCLM-PAYOUT-AMT ROUNDED = XCLT-CONTRIB-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-PAYOUT-AMT > 0 AND XCLM-PAYOUT-AMT < 10.00
               MOVE 10.00 TO XCLM-PAYOUT-AMT
           END-IF
           IF XCLM-PAYOUT-AMT > 25.00
               MOVE 25.00 TO XCLM-PAYOUT-AMT
           END-IF
           ADD XCLM-PAYOUT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ACCRUED AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3460-ACCRUE-ACCRUED.
           IF XCLM-ACCRUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-ACCRUED-AMT * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCLT-WEEKLY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BONUS AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3470-LIMIT-BONUS.
           IF XCLM-BONUS-AMT < 10.00
               MOVE 10.00 TO XCLM-BONUS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-BONUS-AMT > 500.00
               MOVE 500.00 TO XCLM-BONUS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUED AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3480-FLAG-ACCRUED.
           IF XCLM-ACCRUED-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-ACCRUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GOAL FEE ON THE BONUS YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3490-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO XCLM-GOAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   COMPUTE XCLM-GOAL-AMT ROUNDED = XCLM-BONUS-YTD *
                       XCLM-RATE / 200
               WHEN XCLM-CYCLE-CD = 'EM'
                   MOVE 3.00 TO XCLM-GOAL-AMT
               WHEN XCLM-CYCLE-CD = 'FE'
                   MOVE 12.00 TO XCLM-GOAL-AMT
               WHEN OTHER
                   COMPUTE XCLM-GOAL-AMT ROUNDED = XCLM-BONUS-YTD *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-GOAL-AMT > 0 AND XCLM-GOAL-AMT < 5.00
               MOVE 5.00 TO XCLM-GOAL-AMT
           END-IF
           IF XCLM-GOAL-AMT > 35.00
               MOVE 35.00 TO XCLM-GOAL-AMT
           END-IF
           ADD XCLM-GOAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT BONUS YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3500-SPLIT-BONUS.
           COMPUTE WS-WORK-AMT = XCLM-BONUS-YTD / 3
           COMPUTE WS-WORK-AMT-2 = XCLM-BONUS-YTD - (WS-WORK-AMT * 3)
           COMPUTE XCLM-MISSED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E055 MEMO REQUIRED
      * E829 MISSED AMT NEGATIVE
      * E682 MISSED AMT OVER 5000
      * E825 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3510-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-MEMO = SPACES
                   MOVE 'E055' TO WS-REJECT-CODE
               WHEN XCLM-MISSED-AMT < 0
                   MOVE 'E829' TO WS-REJECT-CODE
               WHEN XCLM-MISSED-AMT > 5000.00
                   MOVE 'E682' TO WS-REJECT-CODE
               WHEN XCLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E825' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3520-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3530-DESCRIBE-CYCLE.
           EVALUATE XCLM-CYCLE-CD
               WHEN 'EM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN MATURITY DATE IS PAST
      * BAT -> MAL WHEN MATURITY DATE IS PAST
      * MAL -> TEL WHEN MATURITY DATE IS PAST
      * TEL -> ONL WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3540-SET-CHANNEL.
           EVALUATE XCLT-CHANNEL-CD
               WHEN 'BRN'
                   IF XCLM-MATURITY-DATE > 0 AND XCLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF XCLM-MATURITY-DATE > 0 AND XCLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF XCLM-MATURITY-DATE > 0 AND XCLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF XCLM-MATURITY-DATE > 0 AND XCLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BONUS YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3550-FLAG-BONUS.
           IF XCLM-BONUS-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCLM-BONUS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GOAL FEE ON THE CONTRIB AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3560-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCLM-GOAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   COMPUTE XCLM-GOAL-AMT ROUNDED = XCLT-CONTRIB-AMT *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-CYCLE-CD = 'EM'
                   MOVE 12.00 TO XCLM-GOAL-AMT
               WHEN XCLM-CYCLE-CD = 'FE'
                   MOVE 15.00 TO XCLM-GOAL-AMT
               WHEN OTHER
                   COMPUTE XCLM-GOAL-AMT ROUNDED = XCLT-CONTRIB-AMT *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-GOAL-AMT > 0 AND XCLM-GOAL-AMT < 5.00
               MOVE 5.00 TO XCLM-GOAL-AMT
           END-IF
           IF XCLM-GOAL-AMT > 35.00
               MOVE 35.00 TO XCLM-GOAL-AMT
           END-IF
           ADD XCLM-GOAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCRUED AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3570-FLAG-ACCRUED.
           IF XCLM-ACCRUED-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-ACCRUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3580-COUNT-RETRY.
           IF XCLM-RETRY-CNT < 9999999
               ADD 1 TO XCLM-RETRY-CNT
           END-IF
           IF XCLM-RETRY-CNT NOT < 6
               MOVE 'Y' TO XCLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BONUS AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3590-SPLIT-BONUS.
           COMPUTE WS-WORK-AMT = XCLT-BONUS-AMT / 12
           COMPUTE WS-WORK-AMT-2 = XCLT-BONUS-AMT - (WS-WORK-AMT * 12)
           COMPUTE XCLM-WEEKLY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTRIB YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3600-LIMIT-CONTRIB.
           IF XCLM-CONTRIB-YTD < 50.00
               MOVE 50.00 TO XCLM-CONTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-CONTRIB-YTD > 5000.00
               MOVE 5000.00 TO XCLM-CONTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E896 NAME REQUIRED
      * E356 PENALTY AMT NEGATIVE
      * E027 PENALTY AMT OVER 5000
      * E987 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3610-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-NAME = SPACES
                   MOVE 'E896' TO WS-REJECT-CODE
               WHEN XCLM-PENALTY-AMT < 0
                   MOVE 'E356' TO WS-REJECT-CODE
               WHEN XCLM-PENALTY-AMT > 5000.00
                   MOVE 'E027' TO WS-REJECT-CODE
               WHEN XCLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E987' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WEEKLY YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3620-ACCRUE-WEEKLY.
           IF XCLM-WEEKLY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-WEEKLY-YTD * XCLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-BONUS-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3630-AGE-MATURITY.
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
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WEEKLY AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3640-LIMIT-WEEKLY.
           IF XCLT-WEEKLY-AMT < 10.00
               MOVE 10.00 TO XCLT-WEEKLY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLT-WEEKLY-AMT > 10000.00
               MOVE 10000.00 TO XCLT-WEEKLY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3650-ACCRUE-CONTRIB.
           IF XCLM-CONTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-CONTRIB-YTD * XCLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-MISSED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3660-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCLT-CONTRIB-AMT / 3
           COMPUTE WS-WORK-AMT-2 = XCLT-CONTRIB-AMT - (WS-WORK-AMT * 3)
           COMPUTE XCLM-CONTRIB-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-SEQ.
           IF XCLT-SEQ < 9999999
               ADD 1 TO XCLT-SEQ
           END-IF
           IF XCLT-SEQ NOT < 12
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT REVIEW DATE IS PAST
      * BAT -> MAL WHEN NEXT REVIEW DATE IS PAST
      * MAL -> TEL WHEN NEXT REVIEW DATE IS PAST
      * TEL -> ONL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3680-SET-CHANNEL.
           EVALUATE XCLT-CHANNEL-CD
               WHEN 'BRN'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3690-COUNT-RETRY.
           IF XCLM-RETRY-CNT < 9999999
               ADD 1 TO XCLM-RETRY-CNT
           END-IF
           IF XCLM-RETRY-CNT NOT < 6
               MOVE 'Y' TO XCLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PENALTY AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3700-ACCRUE-PENALTY.
           IF XCLM-PENALTY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-PENALTY-AMT * XCLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-ACCRUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ACCRUED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3710-SPLIT-ACCRUED.
           COMPUTE WS-WORK-AMT = XCLM-ACCRUED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = XCLM-ACCRUED-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E571 NAME REQUIRED
      * E319 ACCRUED YTD NEGATIVE
      * E252 ACCRUED YTD OVER 25000
      * E219 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3720-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-NAME = SPACES
                   MOVE 'E571' TO WS-REJECT-CODE
               WHEN XCLM-ACCRUED-YTD < 0
                   MOVE 'E319' TO WS-REJECT-CODE
               WHEN XCLM-ACCRUED-YTD > 25000.00
                   MOVE 'E252' TO WS-REJECT-CODE
               WHEN XCLM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E219' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3730-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCLM-CONTRIB-YTD / 3
           COMPUTE WS-WORK-AMT-2 = XCLM-CONTRIB-YTD - (WS-WORK-AMT * 3)
           COMPUTE XCLM-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYOUT YTD FEE ON THE CONTRIB YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3740-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCLM-PAYOUT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   COMPUTE XCLM-PAYOUT-YTD ROUNDED = XCLM-CONTRIB-YTD *
                       XCLM-DISCOUNT / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 12.00 TO XCLM-PAYOUT-YTD
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 7.00 TO XCLM-PAYOUT-YTD
               WHEN OTHER
                   COMPUTE XCLM-PAYOUT-YTD ROUNDED = XCLM-CONTRIB-YTD *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-PAYOUT-YTD > 0 AND XCLM-PAYOUT-YTD < 1.00
               MOVE 1.00 TO XCLM-PAYOUT-YTD
           END-IF
           IF XCLM-PAYOUT-YTD > 250.00
               MOVE 250.00 TO XCLM-PAYOUT-YTD
           END-IF
           ADD XCLM-PAYOUT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3750-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-SCHED-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-ACCRUED-SCHED-LOW(WS-TX)
                   MOVE WS-ACCRUED-SCHED-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3760-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCLM-CONTRIB-YTD / 2
           COMPUTE WS-WORK-AMT-2 = XCLM-CONTRIB-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3770-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCLM-CONTRIB-YTD / 2
           COMPUTE WS-WORK-AMT-2 = XCLM-CONTRIB-YTD - (WS-WORK-AMT * 2)
           COMPUTE XCLM-PAYOUT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BONUS AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3780-FLAG-BONUS.
           IF XCLM-BONUS-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLM-BONUS-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AL -> BT WHEN OPEN DATE IS PAST
      * BT -> CY WHEN OPEN DATE IS PAST
      * CY -> KM WHEN OPEN DATE IS PAST
      * KM -> MV WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BT'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CY' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CY'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KM' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KM'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MV' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED SCHED TIER FOR THE CONTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3800-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-SCHED-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF XCLM-CONTRIB-AMT NOT < WS-ACCRUED-SCHED-LOW(WS-TX)
                   MOVE WS-ACCRUED-SCHED-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WEEKLY AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3810-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLT-TRAN-CD
                   ADD XCLM-WEEKLY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-WEEKLY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3820-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF XCLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO XCLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3830-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3840-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF XCLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO XCLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WEEKLY YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3850-FLAG-WEEKLY.
           IF XCLM-WEEKLY-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLM-WEEKLY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYOUT AMT.
      *----------------------------------------------------------------*
       3860-PRORATE-PAYOUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-PAYOUT-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-PAYOUT-AMT
           END-IF
           ADD XCLM-PAYOUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUED YTD FEE ON THE MISSED YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3870-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO XCLM-ACCRUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   COMPUTE XCLM-ACCRUED-YTD ROUNDED = XCLM-MISSED-YTD *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 5.00 TO XCLM-ACCRUED-YTD
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 3.00 TO XCLM-ACCRUED-YTD
               WHEN OTHER
                   COMPUTE XCLM-ACCRUED-YTD ROUNDED = XCLM-MISSED-YTD *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-ACCRUED-YTD > 0 AND XCLM-ACCRUED-YTD < 2.00
               MOVE 2.00 TO XCLM-ACCRUED-YTD
           END-IF
           IF XCLM-ACCRUED-YTD > 150.00
               MOVE 150.00 TO XCLM-ACCRUED-YTD
           END-IF
           ADD XCLM-ACCRUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3880-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED SCHED TIER FOR THE CONTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3890-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-SCHED-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF XCLM-CONTRIB-AMT NOT < WS-ACCRUED-SCHED-LOW(WS-TX)
                   MOVE WS-ACCRUED-SCHED-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PENALTY AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-PENALTY.
           IF XCLM-PENALTY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-PENALTY-AMT * XCLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-PAYOUT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WEEKLY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO GOAL AMT.
      *----------------------------------------------------------------*
       3910-PRORATE-GOAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-GOAL-AMT ROUNDED =
                   XCLT-WEEKLY-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-GOAL-AMT
           END-IF
           ADD XCLM-GOAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT GOAL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3920-SPLIT-GOAL.
           COMPUTE WS-WORK-AMT = XCLT-GOAL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = XCLT-GOAL-AMT - (WS-WORK-AMT * 3)
           COMPUTE XCLT-WEEKLY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY SCHED TIER FOR THE PAYOUT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-SCHED-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF XCLM-PAYOUT-YTD NOT < WS-PENALTY-SCHED-LOW(WS-TX)
                   MOVE WS-PENALTY-SCHED-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E492 CUST ID REQUIRED
      * E197 PENALTY YTD NEGATIVE
      * E185 PENALTY YTD OVER 5000
      * E373 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3940-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-CUST-ID = SPACES
                   MOVE 'E492' TO WS-REJECT-CODE
               WHEN XCLM-PENALTY-YTD < 0
                   MOVE 'E197' TO WS-REJECT-CODE
               WHEN XCLM-PENALTY-YTD > 5000.00
                   MOVE 'E185' TO WS-REJECT-CODE
               WHEN XCLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E373' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST PAY DATE IS PAST
      * BAT -> MAL WHEN LAST PAY DATE IS PAST
      * MAL -> TEL WHEN LAST PAY DATE IS PAST
      * TEL -> ONL WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3950-SET-CHANNEL.
           EVALUATE XCLT-CHANNEL-CD
               WHEN 'BRN'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MISSED YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3960-ACCRUE-MISSED.
           IF XCLM-MISSED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-MISSED-YTD * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCLM-BONUS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BONUS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       3970-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-CONTRIB-AMT ROUNDED =
                   XCLM-BONUS-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-CONTRIB-AMT
           END-IF
           ADD XCLM-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3980-DESCRIBE-PLAN.
           EVALUATE XCLM-PLAN-CD
               WHEN 'GN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E606 CUST ID REQUIRED
      * E136 BONUS AMT NEGATIVE
      * E881 BONUS AMT OVER 99999
      * E933 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3990-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-CUST-ID = SPACES
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN XCLM-BONUS-AMT < 0
                   MOVE 'E136' TO WS-REJECT-CODE
               WHEN XCLM-BONUS-AMT > 99999.00
                   MOVE 'E881' TO WS-REJECT-CODE
               WHEN XCLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E933' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GOAL AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4000-ACCRUE-GOAL.
           IF XCLT-GOAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLT-GOAL-AMT * XCLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO XCLT-WEEKLY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4010-CHECK-EFF.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO XCLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E559 REJECT CD REQUIRED
      * E804 ACCRUED YTD NEGATIVE
      * E013 ACCRUED YTD OVER 25000
      * E569 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4020-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-REJECT-CD = SPACES
                   MOVE 'E559' TO WS-REJECT-CODE
               WHEN XCLM-ACCRUED-YTD < 0
                   MOVE 'E804' TO WS-REJECT-CODE
               WHEN XCLM-ACCRUED-YTD > 25000.00
                   MOVE 'E013' TO WS-REJECT-CODE
               WHEN XCLM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E569' TO WS-REJECT-CODE
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
       4030-DESCRIBE-CHANNEL.
           EVALUATE XCLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4040-COUNT-RETRY.
           IF XCLM-RETRY-CNT < 9999999
               ADD 1 TO XCLM-RETRY-CNT
           END-IF
           IF XCLM-RETRY-CNT NOT < 12
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY SCHED TIER FOR THE PAYOUT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4050-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-SCHED-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF XCLM-PAYOUT-YTD NOT < WS-PENALTY-SCHED-LOW(WS-TX)
                   MOVE WS-PENALTY-SCHED-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4060-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAYOUT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MISSED YTD.
      *----------------------------------------------------------------*
       4070-PRORATE-MISSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-MISSED-YTD ROUNDED =
                   XCLM-PAYOUT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-MISSED-YTD
           END-IF
           ADD XCLM-MISSED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EM -> FE WHEN LAST STMT DATE IS PAST
      * FE -> PL WHEN LAST STMT DATE IS PAST
      * PL -> RU WHEN LAST STMT DATE IS PAST
      * RU -> SU WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4080-SET-CYCLE.
           EVALUATE XCLM-CYCLE-CD
               WHEN 'EM'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FE' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FE'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PL' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RU' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SU' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4090-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO XCLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4100-ACCRUE-CONTRIB.
           IF XCLM-CONTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-CONTRIB-AMT * XCLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-MISSED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4110-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF XCLM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO XCLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4120-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCLT-CONTRIB-AMT / 3
           COMPUTE WS-WORK-AMT-2 = XCLT-CONTRIB-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WEEKLY AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4130-SPLIT-WEEKLY.
           COMPUTE WS-WORK-AMT = XCLT-WEEKLY-AMT / 2
           COMPUTE WS-WORK-AMT-2 = XCLT-WEEKLY-AMT - (WS-WORK-AMT * 2)
           COMPUTE XCLM-GOAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ACCRUED YTD BY PAYOUT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4140-ACCUM-PAYOUT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-PAYOUT-METHOD-CD
                   ADD XCLM-ACCRUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-PAYOUT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-ACCRUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4150-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCLT-CONTRIB-AMT / 4
           COMPUTE WS-WORK-AMT-2 = XCLT-CONTRIB-AMT - (WS-WORK-AMT * 4)
           COMPUTE XCLT-WEEKLY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GOAL AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4160-ACCRUE-GOAL.
           IF XCLM-GOAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-GOAL-AMT * XCLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-ACCRUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GOAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WEEKLY AMT.
      *----------------------------------------------------------------*
       4170-PRORATE-WEEKLY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-WEEKLY-AMT ROUNDED =
                   XCLM-GOAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLT-WEEKLY-AMT
           END-IF
           ADD XCLT-WEEKLY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4180-CHECK-EFF.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO XCLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4190-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-SCHED-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT NOT < WS-PENALTY-SCHED-LOW(WS-TX)
                   MOVE WS-PENALTY-SCHED-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BONUS AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4200-SPLIT-BONUS.
           COMPUTE WS-WORK-AMT = XCLT-BONUS-AMT / 12
           COMPUTE WS-WORK-AMT-2 = XCLT-BONUS-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GOAL YTD FEE ON THE PAYOUT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4210-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO XCLM-GOAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE XCLM-GOAL-YTD ROUNDED = XCLM-PAYOUT-AMT *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-TYPE-CD = 'A'
                   MOVE 3.00 TO XCLM-GOAL-YTD
               WHEN XCLM-TYPE-CD = 'B'
                   MOVE 12.00 TO XCLM-GOAL-YTD
               WHEN OTHER
                   COMPUTE XCLM-GOAL-YTD ROUNDED = XCLM-PAYOUT-AMT *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-GOAL-YTD > 0 AND XCLM-GOAL-YTD < 10.00
               MOVE 10.00 TO XCLM-GOAL-YTD
           END-IF
           IF XCLM-GOAL-YTD > 50.00
               MOVE 50.00 TO XCLM-GOAL-YTD
           END-IF
           ADD XCLM-GOAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4220-DESCRIBE-TRAN.
           EVALUATE XCLT-TRAN-CD
               WHEN 'WD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'DP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PO'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WEEKLY AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4230-LIMIT-WEEKLY.
           IF XCLT-WEEKLY-AMT < 100.00
               MOVE 100.00 TO XCLT-WEEKLY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLT-WEEKLY-AMT > 50000.00
               MOVE 50000.00 TO XCLT-WEEKLY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB YTD FEE ON THE GOAL YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4240-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO XCLM-CONTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-STOP-FLAG = 'Y'
                   COMPUTE XCLM-CONTRIB-YTD ROUNDED = XCLM-GOAL-YTD *
                       XCLM-DISCOUNT / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 7.00 TO XCLM-CONTRIB-YTD
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 12.00 TO XCLM-CONTRIB-YTD
               WHEN OTHER
                   COMPUTE XCLM-CONTRIB-YTD ROUNDED = XCLM-GOAL-YTD *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-CONTRIB-YTD > 0 AND XCLM-CONTRIB-YTD < 10.00
               MOVE 10.00 TO XCLM-CONTRIB-YTD
           END-IF
           IF XCLM-CONTRIB-YTD > 100.00
               MOVE 100.00 TO XCLM-CONTRIB-YTD
           END-IF
           ADD XCLM-CONTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE MISSED AMT BY PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4250-ACCUM-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-PLAN-CD
                   ADD XCLM-MISSED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-MISSED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4260-CHECK-NOTICE.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AL -> BT WHEN EFF DATE IS PAST
      * BT -> CY WHEN EFF DATE IS PAST
      * CY -> KM WHEN EFF DATE IS PAST
      * KM -> MV WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4270-SET-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BT'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CY' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CY'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KM' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KM'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MV' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4280-COUNT-PAYMENT.
           IF XCLM-PAYMENT-CNT < 9999999
               ADD 1 TO XCLM-PAYMENT-CNT
           END-IF
           IF XCLM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4290-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO XCLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4300-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-CYCLE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4310-CHECK-EFF.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO XCLM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN CD:
      * GN -> JT WHEN EXPIRY DATE IS PAST
      * JT -> KC WHEN EXPIRY DATE IS PAST
      * KC -> KW WHEN EXPIRY DATE IS PAST
      * KW -> MX WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4320-SET-PLAN.
           EVALUATE XCLM-PLAN-CD
               WHEN 'GN'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JT'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KC' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KC'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MX' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4330-DESCRIBE-CHANNEL.
           EVALUATE XCLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MISSED AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4340-ACCRUE-MISSED.
           IF XCLM-MISSED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-MISSED-AMT * XCLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-WEEKLY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * FLM -> HLS WHEN NEXT REVIEW DATE IS PAST
      * HLS -> KMJ WHEN NEXT REVIEW DATE IS PAST
      * KMJ -> PDA WHEN NEXT REVIEW DATE IS PAST
      * PDA -> RVG WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4350-SET-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HLS' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HLS'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KMJ' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KMJ'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PDA' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PDA'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'RVG' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BONUS FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4360-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO XCLT-BONUS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   COMPUTE XCLT-BONUS-AMT ROUNDED = WS-WORK-AMT-2 *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-REGION-CD = 'FLM'
                   MOVE 15.00 TO XCLT-BONUS-AMT
               WHEN XCLM-REGION-CD = 'HLS'
                   MOVE 12.00 TO XCLT-BONUS-AMT
               WHEN OTHER
                   COMPUTE XCLT-BONUS-AMT ROUNDED = WS-WORK-AMT-2 *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLT-BONUS-AMT > 0 AND XCLT-BONUS-AMT < 10.00
               MOVE 10.00 TO XCLT-BONUS-AMT
           END-IF
           IF XCLT-BONUS-AMT > 35.00
               MOVE 35.00 TO XCLT-BONUS-AMT
           END-IF
           ADD XCLT-BONUS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE ACCRUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4370-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF XCLM-ACCRUED-AMT NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4380-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF XCLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO XCLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYOUT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4390-DESCRIBE-PAYOUT-METHOD.
           EVALUATE XCLM-PAYOUT-METHOD-CD
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'U'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E041 REJECT CD REQUIRED
      * E407 GOAL AMT NEGATIVE
      * E619 GOAL AMT OVER 99999
      * E443 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4400-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-REJECT-CD = SPACES
                   MOVE 'E041' TO WS-REJECT-CODE
               WHEN XCLM-GOAL-AMT < 0
                   MOVE 'E407' TO WS-REJECT-CODE
               WHEN XCLM-GOAL-AMT > 99999.00
                   MOVE 'E619' TO WS-REJECT-CODE
               WHEN XCLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E443' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * A -> B WHEN NEXT REVIEW DATE IS PAST
      * B -> D WHEN NEXT REVIEW DATE IS PAST
      * D -> E WHEN NEXT REVIEW DATE IS PAST
      * E -> T WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4410-SET-TYPE.
           EVALUATE XCLM-TYPE-CD
               WHEN 'A'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'B' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'D' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'E' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'T' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE MISSED AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4420-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = XCLM-MISSED-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = XCLM-MISSED-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4430-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E723 DESC REQUIRED
      * E556 ACCRUED AMT NEGATIVE
      * E486 ACCRUED AMT OVER 99999
      * E896 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4440-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-DESC = SPACES
                   MOVE 'E723' TO WS-REJECT-CODE
               WHEN XCLM-ACCRUED-AMT < 0
                   MOVE 'E556' TO WS-REJECT-CODE
               WHEN XCLM-ACCRUED-AMT > 99999.00
                   MOVE 'E486' TO WS-REJECT-CODE
               WHEN XCLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E896' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4450-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCLM-CONTRIB-AMT / 3
           COMPUTE WS-WORK-AMT-2 = XCLM-CONTRIB-AMT - (WS-WORK-AMT * 3)
           COMPUTE XCLT-GOAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ACCRUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PENALTY YTD.
      *----------------------------------------------------------------*
       4460-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-PENALTY-YTD ROUNDED =
                   XCLM-ACCRUED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-PENALTY-YTD
           END-IF
           ADD XCLM-PENALTY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT BONUS YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4470-SPLIT-BONUS.
           COMPUTE WS-WORK-AMT = XCLM-BONUS-YTD / 4
           COMPUTE WS-WORK-AMT-2 = XCLM-BONUS-YTD - (WS-WORK-AMT * 4)
           COMPUTE XCLM-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTRIB AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4480-LIMIT-CONTRIB.
           IF XCLM-CONTRIB-AMT < 25.00
               MOVE 25.00 TO XCLM-CONTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-CONTRIB-AMT > 5000.00
               MOVE 5000.00 TO XCLM-CONTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4490-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF XCLM-LAST-ACT-DATE > 0 AND XCLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4500-AGE-MATURITY.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4510-DESCRIBE-CHANNEL.
           EVALUATE XCLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCRUED YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4520-FLAG-ACCRUED.
           IF XCLM-ACCRUED-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCLM-ACCRUED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WEEKLY AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4530-ACCRUE-WEEKLY.
           IF XCLM-WEEKLY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-WEEKLY-AMT * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MISSED AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4540-ACCRUE-MISSED.
           IF XCLM-MISSED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-MISSED-AMT * XCLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO XCLM-WEEKLY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4550-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BONUS AMT BY PAYOUT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4560-ACCUM-PAYOUT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-PAYOUT-METHOD-CD
                   ADD XCLM-BONUS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-PAYOUT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-BONUS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN CD:
      * GN -> JT WHEN LAST PAY DATE IS PAST
      * JT -> KC WHEN LAST PAY DATE IS PAST
      * KC -> KW WHEN LAST PAY DATE IS PAST
      * KW -> MX WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4570-SET-PLAN.
           EVALUATE XCLM-PLAN-CD
               WHEN 'GN'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JT'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KC' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KC'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MX' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * GOAL AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4580-FLAG-GOAL.
           IF XCLM-GOAL-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCLM-GOAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4590-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF XCLM-LAST-ACT-DATE > 0 AND XCLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYOUT YTD BY PAYOUT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4600-ACCUM-PAYOUT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-PAYOUT-METHOD-CD
                   ADD XCLM-PAYOUT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-PAYOUT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-PAYOUT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYOUT YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4610-LIMIT-PAYOUT.
           IF XCLM-PAYOUT-YTD < 25.00
               MOVE 25.00 TO XCLM-PAYOUT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-PAYOUT-YTD > 5000.00
               MOVE 5000.00 TO XCLM-PAYOUT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WEEKLY AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4620-LIMIT-WEEKLY.
           IF XCLT-WEEKLY-AMT < 0.00
               MOVE 0.00 TO XCLT-WEEKLY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLT-WEEKLY-AMT > 5000.00
               MOVE 5000.00 TO XCLT-WEEKLY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4630-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCLT-CONTRIB-AMT / 12
           COMPUTE WS-WORK-AMT-2 = XCLT-CONTRIB-AMT - (WS-WORK-AMT * 12)
           COMPUTE XCLM-GOAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MISSED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       4640-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-CONTRIB-AMT ROUNDED =
                   XCLM-MISSED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-CONTRIB-AMT
           END-IF
           ADD XCLM-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4650-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WEEKLY YTD FEE ON THE PENALTY AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4660-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO XCLM-WEEKLY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-STOP-FLAG = 'Y'
                   COMPUTE XCLM-WEEKLY-YTD ROUNDED = XCLM-PENALTY-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 10.00 TO XCLM-WEEKLY-YTD
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 10.00 TO XCLM-WEEKLY-YTD
               WHEN OTHER
                   COMPUTE XCLM-WEEKLY-YTD ROUNDED = XCLM-PENALTY-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-WEEKLY-YTD > 0 AND XCLM-WEEKLY-YTD < 5.00
               MOVE 5.00 TO XCLM-WEEKLY-YTD
           END-IF
           IF XCLM-WEEKLY-YTD > 75.00
               MOVE 75.00 TO XCLM-WEEKLY-YTD
           END-IF
           ADD XCLM-WEEKLY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * GOAL AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4670-LIMIT-GOAL.
           IF XCLM-GOAL-AMT < 50.00
               MOVE 50.00 TO XCLM-GOAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-GOAL-AMT > 2500.00
               MOVE 2500.00 TO XCLM-GOAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4680-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO XCLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4690-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCLM-CONTRIB-YTD / 2
           COMPUTE WS-WORK-AMT-2 = XCLM-CONTRIB-YTD - (WS-WORK-AMT * 2)
           COMPUTE XCLT-GOAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUED FEE ON THE WEEKLY AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4700-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO XCLM-ACCRUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   COMPUTE XCLM-ACCRUED-AMT ROUNDED = XCLM-WEEKLY-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-PAYOUT-METHOD-CD = 'B'
                   MOVE 15.00 TO XCLM-ACCRUED-AMT
               WHEN XCLM-PAYOUT-METHOD-CD = 'H'
                   MOVE 7.00 TO XCLM-ACCRUED-AMT
               WHEN OTHER
                   COMPUTE XCLM-ACCRUED-AMT ROUNDED = XCLM-WEEKLY-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-ACCRUED-AMT > 0 AND XCLM-ACCRUED-AMT < 2.00
               MOVE 2.00 TO XCLM-ACCRUED-AMT
           END-IF
           IF XCLM-ACCRUED-AMT > 75.00
               MOVE 75.00 TO XCLM-ACCRUED-AMT
           END-IF
           ADD XCLM-ACCRUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY SCHED TIER FOR THE WEEKLY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4710-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-SCHED-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF XCLM-WEEKLY-AMT NOT < WS-PENALTY-SCHED-LOW(WS-TX)
                   MOVE WS-PENALTY-SCHED-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4720-AGE-MATURITY.
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
                   ADD XCLM-PAYOUT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD XCLM-PAYOUT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-PAYOUT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-PAYOUT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-PAYOUT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4730-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BONUS AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4740-LIMIT-BONUS.
           IF XCLT-BONUS-AMT < 0.00
               MOVE 0.00 TO XCLT-BONUS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLT-BONUS-AMT > 1000.00
               MOVE 1000.00 TO XCLT-BONUS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4750-DESCRIBE-PLAN.
           EVALUATE XCLM-PLAN-CD
               WHEN 'GN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4760-ACCRUE-CONTRIB.
           IF XCLT-CONTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLT-CONTRIB-AMT * XCLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO XCLT-WEEKLY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BONUS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4770-PRORATE-BONUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-BONUS-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BONUS YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4780-ACCRUE-BONUS.
           IF XCLM-BONUS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-BONUS-YTD * XCLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO XCLM-ACCRUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4790-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-BONUS-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD XCLM-BONUS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-BONUS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-BONUS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-BONUS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4800-DESCRIBE-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * FLM -> HLS WHEN LAST PAY DATE IS PAST
      * HLS -> KMJ WHEN LAST PAY DATE IS PAST
      * KMJ -> PDA WHEN LAST PAY DATE IS PAST
      * PDA -> RVG WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4810-SET-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HLS' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HLS'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KMJ' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KMJ'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PDA' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PDA'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RVG' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYOUT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4820-FLAG-PAYOUT.
           IF XCLM-PAYOUT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCLM-PAYOUT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BONUS FEE ON THE CONTRIB AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4830-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO XCLM-BONUS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-STOP-FLAG = 'Y'
                   COMPUTE XCLM-BONUS-AMT ROUNDED = XCLM-CONTRIB-AMT *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 3.00 TO XCLM-BONUS-AMT
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 3.00 TO XCLM-BONUS-AMT
               WHEN OTHER
                   COMPUTE XCLM-BONUS-AMT ROUNDED = XCLM-CONTRIB-AMT *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-BONUS-AMT > 0 AND XCLM-BONUS-AMT < 10.00
               MOVE 10.00 TO XCLM-BONUS-AMT
           END-IF
           IF XCLM-BONUS-AMT > 35.00
               MOVE 35.00 TO XCLM-BONUS-AMT
           END-IF
           ADD XCLM-BONUS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E630 MEMO REQUIRED
      * E268 BONUS AMT NEGATIVE
      * E700 BONUS AMT OVER 50000
      * E915 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4840-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-MEMO = SPACES
                   MOVE 'E630' TO WS-REJECT-CODE
               WHEN XCLT-BONUS-AMT < 0
                   MOVE 'E268' TO WS-REJECT-CODE
               WHEN XCLT-BONUS-AMT > 50000.00
                   MOVE 'E700' TO WS-REJECT-CODE
               WHEN XCLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E915' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4850-DESCRIBE-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
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
                   MOVE XCLIM-PAYOUT-YTD
                     TO XCLM-PAYOUT-YTD
                   MOVE XCLIM-ACCRUED-AMT
                     TO XCLM-ACCRUED-AMT
                   MOVE XCLIM-GOAL-AMT
                     TO XCLM-GOAL-AMT
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
                   MOVE XCFEE-CONTRIB-AMT
                     TO XCLM-CONTRIB-AMT
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
      * PASS THE XCL FIELDS TO XCCAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-XCCAL.
           INITIALIZE XCCAL-PARMS
           MOVE XCLM-GOAL-YTD
             TO XCCAL-GOAL-YTD
           MOVE XCLM-PAYOUT-AMT
             TO XCCAL-PAYOUT-AMT
           MOVE XCLM-WEEKLY-YTD
             TO XCCAL-WEEKLY-YTD
           MOVE XCLM-CONTRIB-YTD
             TO XCCAL-CONTRIB-YTD
           MOVE XCLM-MISSED-AMT
             TO XCCAL-MISSED-AMT
           MOVE XCLM-PENALTY-YTD
             TO XCCAL-PENALTY-YTD
           MOVE XCLM-LAST-STMT-DATE
             TO XCCAL-LAST-STMT-DATE
           MOVE XCLM-NOTICE-DATE
             TO XCCAL-NOTICE-DATE
           MOVE XCLM-LAST-ACT-DATE
             TO XCCAL-LAST-ACT-DATE
           MOVE XCLM-MATURITY-DATE
             TO XCCAL-MATURITY-DATE
           MOVE XCLM-PLAN-CD
             TO XCCAL-PLAN-CD
           MOVE XCLM-CYCLE-CD
             TO XCCAL-CYCLE-CD
           MOVE XCLM-REGION-CD
             TO XCCAL-REGION-CD
           MOVE XCLM-REASON-CD
             TO XCCAL-REASON-CD
           MOVE XCLM-PREFERRED-FLAG
             TO XCCAL-PREFERRED-FLAG
           MOVE XCLM-OVERRIDE-FLAG
             TO XCCAL-OVERRIDE-FLAG
           MOVE XCLM-STOP-FLAG
             TO XCCAL-STOP-FLAG
           MOVE XCLM-RETRY-CNT
             TO XCCAL-RETRY-CNT
           MOVE XCLM-CYCLE-CNT
             TO XCCAL-CYCLE-CNT
           MOVE XCLM-FEE-PCT
             TO XCCAL-FEE-PCT
           MOVE XCLM-DISCOUNT
             TO XCCAL-DISCOUNT
           MOVE XCLM-REF-NO
             TO XCCAL-REF-NO
           MOVE XCLM-CUST-ID
             TO XCCAL-CUST-ID
           CALL 'XCCAL' USING XCCAL-PARMS
           EVALUATE XCCAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE XCCAL-PAYOUT-AMT
                     TO XCLM-PAYOUT-AMT
                   MOVE XCCAL-WEEKLY-YTD
                     TO XCLM-WEEKLY-YTD
                   MOVE XCCAL-CONTRIB-YTD
                     TO XCLM-CONTRIB-YTD
                   MOVE XCCAL-PENALTY-YTD
                     TO XCLM-PENALTY-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-XCCAL' TO ABND-PARAGRAPH
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
           DISPLAY 'XCLPOST READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'XCLPOST' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
