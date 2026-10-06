      *================================================================*
      * PROGRAM  : XCLAGE                                              *
      * TYPE     : BATCH                                               *
      * PURPOSE  : CHRISTMAS CLUB SAVINGS - AGING. READS THE XCL       *
      *            TRANSACTIONS, UPDATES THE XCL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. HOLIDAY CLUB SAVINGS PLANS WITH *
      *            WEEKLY DEPOSITS AND NOVEMBER PAYOUT                 *
      * INPUT    : XCLMST (KSDS), XCLTRN, BANKCTL                      *
      * OUTPUT   : XCLOUT, XCLAGRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, XCTIER, XCCAL, XCPEN    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-01-25 TWH  ORIGINAL                                       *
      * 1992-12-18 CRP  MISSED NOTICE TEXT PER LEGAL PRB3599           *
      * 2000-08-05 EWS  CONTRIB FEE SCHEDULE UPDATE CHG3436            *
      * 2001-02-10 BHS  PERFORMANCE - WEEKLY READ AHEAD INC3354        *
      * 2003-01-04 EWS  ACCRUED CUTOFF MOVED REQ4503                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. XCLAGE.
       AUTHOR. J L MARSH.

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
           SELECT REPORT-FILE ASSIGN TO "XCLAGRPT"
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
                                           VALUE 17.000000.
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
      * WS-MISSED-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MISSED-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000142500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000755500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000842000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000400000000165000'.
       01  WS-MISSED-TIER-TBL REDEFINES WS-MISSED-TIER-VALUES.
           05  WS-MISSED-TIER-ROW OCCURS 4 TIMES.
               10  WS-MISSED-TIER-LOW      PIC 9(09)V99.
               10  WS-MISSED-TIER-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYOUT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYOUT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000263000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000072000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000594000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000856500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000415000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014000000000014000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014100000000856000'.
       01  WS-PAYOUT-GRID-TBL REDEFINES WS-PAYOUT-GRID-VALUES.
           05  WS-PAYOUT-GRID-ROW OCCURS 7 TIMES.
               10  WS-PAYOUT-GRID-LOW      PIC 9(09)V99.
               10  WS-PAYOUT-GRID-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  XCLAGE  CHRISTMAS CLUB SAVINGS'.
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
       COPY XCTIERP.
       COPY XCCALP.
       COPY XCPENP.

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
               PERFORM 3000-ACCUM-PLAN
               PERFORM 3010-ACCRUE-CONTRIB
               PERFORM 3020-SELECT-RATE
               PERFORM 3030-DESCRIBE-REGION
               PERFORM 3040-CHECK-OPEN
               PERFORM 3050-EDIT-NAME
               PERFORM 3060-ACCUM-CHANNEL
               PERFORM 3070-COUNT-CYCLE
               PERFORM 3080-EDIT-REF-NO
               PERFORM 3090-SELECT-FEE-PCT
               PERFORM 3100-SET-TRAN
               PERFORM 3110-EDIT-ADDR
               PERFORM 3120-FLAG-MISSED
               PERFORM 3130-COUNT-PAYMENT
               PERFORM 3140-DESCRIBE-CHANNEL
               PERFORM 3150-CHECK-EXPIRY
               PERFORM 3160-CHECK-NEXT-REVIEW
               PERFORM 3170-SELECT-DISCOUNT
               PERFORM 3180-SPLIT-MISSED
               PERFORM 3190-CHECK-LAST-ACT
               PERFORM 3200-ACCRUE-GOAL
               PERFORM 3210-DESCRIBE-PAYOUT-METHOD
               PERFORM 3220-CHECK-NEXT-DUE
               PERFORM 3230-PRORATE-GOAL
               PERFORM 3240-COUNT-RETRY
               PERFORM 3250-AGE-NEXT-REVIEW
               PERFORM 3260-ACCRUE-BONUS
               PERFORM 3270-ASSESS-AUTO-RENEW
               PERFORM 3280-COUNT-CYCLE
               PERFORM 3290-ACCRUE-ACCRUED
               PERFORM 3300-AGE-EFF
               PERFORM 3310-ACCRUE-WEEKLY
               PERFORM 3320-PRORATE-GOAL
               PERFORM 3330-CHECK-POST
               PERFORM 3340-DESCRIBE-TYPE
               PERFORM 3350-CHECK-LAST-STMT
               PERFORM 3360-LIMIT-ACCRUED
               PERFORM 3370-PRORATE-MISSED
               PERFORM 3380-AGE-LAST-STMT
               PERFORM 3390-AGE-NEXT-REVIEW
               PERFORM 3400-ACCUM-CYCLE
               PERFORM 3410-SET-TYPE
               PERFORM 3420-FLAG-ITEM
               PERFORM 3430-ACCUM-REGION
               PERFORM 3440-FLAG-GOAL
               PERFORM 3450-ACCRUE-WEEKLY
               PERFORM 3460-SET-REGION
               PERFORM 3470-ASSESS-OVERRIDE
               PERFORM 3480-PRORATE-PENALTY
               PERFORM 3490-COUNT-PAYMENT
               PERFORM 3500-SPLIT-CONTRIB
               PERFORM 3510-DESCRIBE-TYPE
               PERFORM 3520-DESCRIBE-CHANNEL
               PERFORM 3530-SPLIT-WEEKLY
               PERFORM 3540-ASSESS-PREFERRED
               PERFORM 3550-AGE-NEXT-DUE
               PERFORM 3560-LIMIT-ITEM
               PERFORM 3570-SET-CHANNEL
               PERFORM 3580-EDIT-REF-NO
               PERFORM 3590-EDIT-DESC
               PERFORM 3600-SPLIT-PENALTY
               PERFORM 3610-SET-CYCLE
               PERFORM 3620-EDIT-DESC
               PERFORM 3630-CHECK-OPEN
               PERFORM 3640-AGE-LAST-ACT
               PERFORM 3650-LIMIT-BONUS
               PERFORM 3660-AGE-MATURITY
               PERFORM 3670-ASSESS-PREFERRED
               PERFORM 3680-AGE-MATURITY
               PERFORM 3690-EDIT-REF-NO
               PERFORM 3700-DESCRIBE-CYCLE
               PERFORM 3710-FLAG-CONTRIB
               PERFORM 3720-AGE-LAST-STMT
               PERFORM 3730-AGE-LAST-ACT
               PERFORM 3740-AGE-MATURITY
               PERFORM 3750-SET-CHANNEL
               PERFORM 3760-EDIT-REF-NO
               PERFORM 3770-CHECK-MATURITY
               PERFORM 3780-DESCRIBE-CYCLE
               PERFORM 3790-EDIT-ADDR
               PERFORM 3800-COUNT-RETRY
               PERFORM 3810-ASSESS-SUSPEND
               PERFORM 3820-SET-REASON
               PERFORM 3830-CHECK-NEXT-REVIEW
               PERFORM 3840-SET-PLAN
               PERFORM 3850-EDIT-OPERATOR
               PERFORM 3860-DESCRIBE-REGION
               PERFORM 3870-AGE-MATURITY
               PERFORM 3880-SPLIT-CONTRIB
               PERFORM 3890-ACCUM-CHANNEL
               PERFORM 3900-ASSESS-STOP
               PERFORM 3910-CHECK-MATURITY
               PERFORM 3920-ASSESS-SUSPEND
               PERFORM 3930-SELECT-FEE-PCT
               PERFORM 3940-ACCUM-PAYOUT-METHOD
               PERFORM 3950-CHECK-LAST-ACT
               PERFORM 3960-DESCRIBE-CHANNEL
               PERFORM 3970-PRORATE-MISSED
               PERFORM 3980-SELECT-DISCOUNT
               PERFORM 3990-FLAG-WEEKLY
               PERFORM 4000-DESCRIBE-TRAN
               PERFORM 4010-PRORATE-BONUS
               PERFORM 4020-ACCRUE-ACCRUED
               PERFORM 4030-ACCUM-REASON
               PERFORM 4040-ASSESS-OVERRIDE
               PERFORM 4050-LIMIT-WEEKLY
               PERFORM 4060-PRORATE-WEEKLY
               PERFORM 4070-SET-PAYOUT-METHOD
               PERFORM 4080-FLAG-WEEKLY
               PERFORM 4090-ACCUM-PAYOUT-METHOD
               PERFORM 4100-COUNT-RETRY
               PERFORM 4110-DESCRIBE-CYCLE
               PERFORM 4120-LIMIT-WEEKLY
               PERFORM 4130-SPLIT-MISSED
               PERFORM 4140-DESCRIBE-REASON
               PERFORM 4150-SET-CHANNEL
               PERFORM 4160-ACCUM-REGION
               PERFORM 4170-FLAG-ITEM
               PERFORM 4180-ASSESS-OVERRIDE
               PERFORM 4190-ASSESS-SUSPEND
               PERFORM 4200-LIMIT-BONUS
               PERFORM 4210-CHECK-LAST-PAY
               PERFORM 4220-ASSESS-PREFERRED
               PERFORM 4230-DESCRIBE-REGION
               PERFORM 4240-ACCUM-CHANNEL
               PERFORM 4250-ACCUM-TYPE
               PERFORM 4260-ASSESS-REVERSAL
               PERFORM 4270-COUNT-RETRY
               PERFORM 4280-EDIT-CONTACT
               PERFORM 4290-ASSESS-EXEMPT
               PERFORM 4300-ACCRUE-WEEKLY
               PERFORM 4310-SELECT-FEE-PCT
               PERFORM 4320-COUNT-SEQ
               PERFORM 4330-SET-PAYOUT-METHOD
               PERFORM 4340-SET-CYCLE
               PERFORM 4350-EDIT-MEMO
               PERFORM 4360-SPLIT-PENALTY
               PERFORM 4370-SELECT-FEE-PCT
               PERFORM 4380-COUNT-PAYMENT
               PERFORM 4390-ACCUM-CYCLE
               PERFORM 4400-ASSESS-STOP
               PERFORM 4410-SELECT-FEE-PCT
               PERFORM 4420-EDIT-REF-NO
               PERFORM 4430-ACCRUE-PENALTY
               PERFORM 4440-AGE-POST
               PERFORM 4450-AGE-LAST-STMT
               PERFORM 4460-FLAG-BONUS
               PERFORM 4470-ACCRUE-WEEKLY
               PERFORM 4480-DESCRIBE-PLAN
               PERFORM 4490-PRORATE-BONUS
               PERFORM 4500-EDIT-NAME
               PERFORM 4510-COUNT-CYCLE
               PERFORM 4520-SELECT-FEE-PCT
               PERFORM 4530-AGE-LAST-ACT
               PERFORM 4540-ACCRUE-PAYOUT
               PERFORM 4550-ACCUM-PAYOUT-METHOD
               PERFORM 4560-PRORATE-MISSED
               PERFORM 4570-ACCRUE-WEEKLY
               PERFORM 4580-CHECK-EFF
               PERFORM 4590-LIMIT-ITEM
               PERFORM 4600-COUNT-NOTICE
               PERFORM 4610-FLAG-ITEM
               PERFORM 4620-ASSESS-REVERSAL
               PERFORM 4630-ASSESS-STOP
               PERFORM 4640-FLAG-GOAL
               PERFORM 4650-EDIT-CONTACT
               PERFORM 4660-ACCUM-TYPE
               PERFORM 4670-PRORATE-CONTRIB
               PERFORM 4680-FLAG-PENALTY
               PERFORM 7000-CALL-XCTIER
               PERFORM 7040-CALL-XCCAL
               PERFORM 7030-CALL-XCPEN
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
      * ACCUMULATE GOAL AMT BY PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3000-ACCUM-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-PLAN-CD
                   ADD XCLT-GOAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLT-GOAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-CONTRIB.
           IF XCLM-CONTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-CONTRIB-YTD * XCLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-PAYOUT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT GRID TIER FOR THE GOAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3020-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-GRID-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF XCLM-GOAL-AMT NOT < WS-PAYOUT-GRID-LOW(WS-TX)
                   MOVE WS-PAYOUT-GRID-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3030-DESCRIBE-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3040-CHECK-OPEN.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E618 NAME REQUIRED
      * E390 WORK AMT NEGATIVE
      * E280 WORK AMT OVER 5000
      * E258 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3050-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-NAME = SPACES
                   MOVE 'E618' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E390' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E280' TO WS-REJECT-CODE
               WHEN XCLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E258' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GOAL AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3060-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLT-CHANNEL-CD
                   ADD XCLT-GOAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLT-GOAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3070-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E605 REF NO REQUIRED
      * E145 BONUS YTD NEGATIVE
      * E637 BONUS YTD OVER 250000
      * E959 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-REF-NO = SPACES
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN XCLM-BONUS-YTD < 0
                   MOVE 'E145' TO WS-REJECT-CODE
               WHEN XCLM-BONUS-YTD > 250000.00
                   MOVE 'E637' TO WS-REJECT-CODE
               WHEN XCLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E959' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT GRID TIER FOR THE CONTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3090-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-GRID-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF XCLT-CONTRIB-AMT NOT < WS-PAYOUT-GRID-LOW(WS-TX)
                   MOVE WS-PAYOUT-GRID-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * WD -> DP WHEN LAST STMT DATE IS PAST
      * DP -> PO WHEN LAST STMT DATE IS PAST
      * PO -> BN WHEN LAST STMT DATE IS PAST
      * BN -> PN WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3100-SET-TRAN.
           EVALUATE XCLT-TRAN-CD
               WHEN 'WD'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PO' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PO'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BN' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BN'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PN' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E429 ADDR REQUIRED
      * E485 PAYOUT AMT NEGATIVE
      * E901 PAYOUT AMT OVER 1000000
      * E447 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3110-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-ADDR = SPACES
                   MOVE 'E429' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-AMT < 0
                   MOVE 'E485' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-AMT > 1000000.00
                   MOVE 'E901' TO WS-REJECT-CODE
               WHEN XCLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * MISSED YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3120-FLAG-MISSED.
           IF XCLM-MISSED-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-MISSED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3130-COUNT-PAYMENT.
           IF XCLM-PAYMENT-CNT < 9999999
               ADD 1 TO XCLM-PAYMENT-CNT
           END-IF
           IF XCLM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3140-DESCRIBE-CHANNEL.
           EVALUATE XCLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF XCLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-EXPIRY-DATE TO DTU-DATE-1
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
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3160-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF XCLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MISSED TIER TIER FOR THE MISSED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3170-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MISSED-TIER-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCLM-MISSED-AMT NOT < WS-MISSED-TIER-LOW(WS-TX)
                   MOVE WS-MISSED-TIER-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MISSED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3180-SPLIT-MISSED.
           COMPUTE WS-WORK-AMT = XCLM-MISSED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = XCLM-MISSED-AMT - (WS-WORK-AMT * 12)
           COMPUTE XCLM-PAYOUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-LAST-ACT.
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
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GOAL YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3200-ACCRUE-GOAL.
           IF XCLM-GOAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-GOAL-YTD * XCLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-CONTRIB-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYOUT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3210-DESCRIBE-PAYOUT-METHOD.
           EVALUATE XCLM-PAYOUT-METHOD-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'H'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'U'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3220-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO GOAL AMT.
      *----------------------------------------------------------------*
       3230-PRORATE-GOAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-GOAL-AMT ROUNDED =
                   XCLT-CONTRIB-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-GOAL-AMT
           END-IF
           ADD XCLM-GOAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3240-COUNT-RETRY.
           IF XCLM-RETRY-CNT < 9999999
               ADD 1 TO XCLM-RETRY-CNT
           END-IF
           IF XCLM-RETRY-CNT NOT < 10
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3250-AGE-NEXT-REVIEW.
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
               WHEN WS-DAYS NOT > 30
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BONUS YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3260-ACCRUE-BONUS.
           IF XCLM-BONUS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-BONUS-YTD * XCLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-WEEKLY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WEEKLY FEE ON THE WORK AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3270-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO XCLM-WEEKLY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-STOP-FLAG = 'Y'
                   COMPUTE XCLM-WEEKLY-AMT ROUNDED = WS-WORK-AMT *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-TYPE-CD = 'A'
                   MOVE 5.00 TO XCLM-WEEKLY-AMT
               WHEN XCLM-TYPE-CD = 'B'
                   MOVE 10.00 TO XCLM-WEEKLY-AMT
               WHEN OTHER
                   COMPUTE XCLM-WEEKLY-AMT ROUNDED = WS-WORK-AMT *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-WEEKLY-AMT > 0 AND XCLM-WEEKLY-AMT < 1.00
               MOVE 1.00 TO XCLM-WEEKLY-AMT
           END-IF
           IF XCLM-WEEKLY-AMT > 35.00
               MOVE 35.00 TO XCLM-WEEKLY-AMT
           END-IF
           ADD XCLM-WEEKLY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3280-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ACCRUED YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3290-ACCRUE-ACCRUED.
           IF XCLM-ACCRUED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-ACCRUED-YTD * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCLM-CONTRIB-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3300-AGE-EFF.
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
                   ADD XCLM-MISSED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD XCLM-MISSED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-MISSED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-MISSED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-MISSED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WEEKLY AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3310-ACCRUE-WEEKLY.
           IF XCLM-WEEKLY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-WEEKLY-AMT * XCLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-BONUS-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GOAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO GOAL YTD.
      *----------------------------------------------------------------*
       3320-PRORATE-GOAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-GOAL-YTD ROUNDED =
                   XCLM-GOAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-GOAL-YTD
           END-IF
           ADD XCLM-GOAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3330-CHECK-POST.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3340-DESCRIBE-TYPE.
           EVALUATE XCLM-TYPE-CD
               WHEN 'A'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'D'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'E'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'W'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3350-CHECK-LAST-STMT.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUED YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3360-LIMIT-ACCRUED.
           IF XCLM-ACCRUED-YTD < 10.00
               MOVE 10.00 TO XCLM-ACCRUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-ACCRUED-YTD > 2500.00
               MOVE 2500.00 TO XCLM-ACCRUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MISSED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3370-PRORATE-MISSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   XCLM-MISSED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3380-AGE-LAST-STMT.
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
                   ADD XCLM-WEEKLY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD XCLM-WEEKLY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-WEEKLY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-WEEKLY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-WEEKLY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3390-AGE-NEXT-REVIEW.
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
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE GOAL AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3400-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-CYCLE-CD
                   ADD XCLT-GOAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLT-GOAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * A -> B WHEN OPEN DATE IS PAST
      * B -> D WHEN OPEN DATE IS PAST
      * D -> E WHEN OPEN DATE IS PAST
      * E -> T WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3410-SET-TYPE.
           EVALUATE XCLM-TYPE-CD
               WHEN 'A'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3420-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYOUT YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-REGION-CD
                   ADD XCLM-PAYOUT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-PAYOUT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * GOAL AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3440-FLAG-GOAL.
           IF XCLM-GOAL-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLM-GOAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WEEKLY YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3450-ACCRUE-WEEKLY.
           IF XCLM-WEEKLY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-WEEKLY-YTD * XCLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO XCLT-WEEKLY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * FLM -> HLS WHEN LAST PAY DATE IS PAST
      * HLS -> KMJ WHEN LAST PAY DATE IS PAST
      * KMJ -> PDA WHEN LAST PAY DATE IS PAST
      * PDA -> RVG WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3460-SET-REGION.
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
      * ASSESS THE GOAL FEE ON THE BONUS YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3470-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO XCLM-GOAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   COMPUTE XCLM-GOAL-AMT ROUNDED = XCLM-BONUS-YTD *
                       XCLM-DISCOUNT / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 20.00 TO XCLM-GOAL-AMT
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 20.00 TO XCLM-GOAL-AMT
               WHEN OTHER
                   COMPUTE XCLM-GOAL-AMT ROUNDED = XCLM-BONUS-YTD *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-GOAL-AMT > 0 AND XCLM-GOAL-AMT < 1.00
               MOVE 1.00 TO XCLM-GOAL-AMT
           END-IF
           IF XCLM-GOAL-AMT > 75.00
               MOVE 75.00 TO XCLM-GOAL-AMT
           END-IF
           ADD XCLM-GOAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PENALTY YTD.
      *----------------------------------------------------------------*
       3480-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-PENALTY-YTD ROUNDED =
                   XCLM-CONTRIB-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-PENALTY-YTD
           END-IF
           ADD XCLM-PENALTY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3490-COUNT-PAYMENT.
           IF XCLM-PAYMENT-CNT < 9999999
               ADD 1 TO XCLM-PAYMENT-CNT
           END-IF
           IF XCLM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO XCLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3500-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCLM-CONTRIB-YTD / 4
           COMPUTE WS-WORK-AMT-2 = XCLM-CONTRIB-YTD - (WS-WORK-AMT * 4)
           COMPUTE XCLT-BONUS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3510-DESCRIBE-TYPE.
           EVALUATE XCLM-TYPE-CD
               WHEN 'A'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'D'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'E'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'W'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3520-DESCRIBE-CHANNEL.
           EVALUATE XCLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WEEKLY YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3530-SPLIT-WEEKLY.
           COMPUTE WS-WORK-AMT = XCLM-WEEKLY-YTD / 3
           COMPUTE WS-WORK-AMT-2 = XCLM-WEEKLY-YTD - (WS-WORK-AMT * 3)
           COMPUTE XCLM-PENALTY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB YTD FEE ON THE BONUS AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3540-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO XCLM-CONTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE XCLM-CONTRIB-YTD ROUNDED = XCLT-BONUS-AMT *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 5.00 TO XCLM-CONTRIB-YTD
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 7.00 TO XCLM-CONTRIB-YTD
               WHEN OTHER
                   COMPUTE XCLM-CONTRIB-YTD ROUNDED = XCLT-BONUS-AMT *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-CONTRIB-YTD > 0 AND XCLM-CONTRIB-YTD < 10.00
               MOVE 10.00 TO XCLM-CONTRIB-YTD
           END-IF
           IF XCLM-CONTRIB-YTD > 100.00
               MOVE 100.00 TO XCLM-CONTRIB-YTD
           END-IF
           ADD XCLM-CONTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3550-AGE-NEXT-DUE.
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
                   ADD XCLM-BONUS-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD XCLM-BONUS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD XCLM-BONUS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-BONUS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-BONUS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3560-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT REVIEW DATE IS PAST
      * BAT -> MAL WHEN NEXT REVIEW DATE IS PAST
      * MAL -> TEL WHEN NEXT REVIEW DATE IS PAST
      * TEL -> ONL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3570-SET-CHANNEL.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E060 REF NO REQUIRED
      * E328 PENALTY YTD NEGATIVE
      * E484 PENALTY YTD OVER 10000
      * E549 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3580-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-REF-NO = SPACES
                   MOVE 'E060' TO WS-REJECT-CODE
               WHEN XCLM-PENALTY-YTD < 0
                   MOVE 'E328' TO WS-REJECT-CODE
               WHEN XCLM-PENALTY-YTD > 10000.00
                   MOVE 'E484' TO WS-REJECT-CODE
               WHEN XCLM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E549' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E448 DESC REQUIRED
      * E990 WEEKLY AMT NEGATIVE
      * E114 WEEKLY AMT OVER 25000
      * E467 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3590-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-DESC = SPACES
                   MOVE 'E448' TO WS-REJECT-CODE
               WHEN XCLT-WEEKLY-AMT < 0
                   MOVE 'E990' TO WS-REJECT-CODE
               WHEN XCLT-WEEKLY-AMT > 25000.00
                   MOVE 'E114' TO WS-REJECT-CODE
               WHEN XCLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E467' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PENALTY YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3600-SPLIT-PENALTY.
           COMPUTE WS-WORK-AMT = XCLM-PENALTY-YTD / 2
           COMPUTE WS-WORK-AMT-2 = XCLM-PENALTY-YTD - (WS-WORK-AMT * 2)
           COMPUTE XCLM-CONTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EM -> FE WHEN LAST PAY DATE IS PAST
      * FE -> PL WHEN LAST PAY DATE IS PAST
      * PL -> RU WHEN LAST PAY DATE IS PAST
      * RU -> SU WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3610-SET-CYCLE.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E065 DESC REQUIRED
      * E541 GOAL AMT NEGATIVE
      * E326 GOAL AMT OVER 10000
      * E594 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3620-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-DESC = SPACES
                   MOVE 'E065' TO WS-REJECT-CODE
               WHEN XCLT-GOAL-AMT < 0
                   MOVE 'E541' TO WS-REJECT-CODE
               WHEN XCLT-GOAL-AMT > 10000.00
                   MOVE 'E326' TO WS-REJECT-CODE
               WHEN XCLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E594' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3630-CHECK-OPEN.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO XCLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3640-AGE-LAST-ACT.
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
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BONUS AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3650-LIMIT-BONUS.
           IF XCLT-BONUS-AMT < 0.00
               MOVE 0.00 TO XCLT-BONUS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLT-BONUS-AMT > 10000.00
               MOVE 10000.00 TO XCLT-BONUS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3660-AGE-MATURITY.
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
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BONUS FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3670-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO XCLT-BONUS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   COMPUTE XCLT-BONUS-AMT ROUNDED = WS-WORK-AMT-2 *
                       XCLM-DISCOUNT / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 5.00 TO XCLT-BONUS-AMT
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 7.00 TO XCLT-BONUS-AMT
               WHEN OTHER
                   COMPUTE XCLT-BONUS-AMT ROUNDED = WS-WORK-AMT-2 *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLT-BONUS-AMT > 0 AND XCLT-BONUS-AMT < 5.00
               MOVE 5.00 TO XCLT-BONUS-AMT
           END-IF
           IF XCLT-BONUS-AMT > 75.00
               MOVE 75.00 TO XCLT-BONUS-AMT
           END-IF
           ADD XCLT-BONUS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3680-AGE-MATURITY.
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
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E019 REF NO REQUIRED
      * E095 PAYOUT AMT NEGATIVE
      * E208 PAYOUT AMT OVER 50000
      * E935 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3690-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-REF-NO = SPACES
                   MOVE 'E019' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-AMT < 0
                   MOVE 'E095' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-AMT > 50000.00
                   MOVE 'E208' TO WS-REJECT-CODE
               WHEN XCLM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E935' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3700-DESCRIBE-CYCLE.
           EVALUATE XCLM-CYCLE-CD
               WHEN 'EM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONTRIB YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3710-FLAG-CONTRIB.
           IF XCLM-CONTRIB-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-CONTRIB-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3720-AGE-LAST-STMT.
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
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3730-AGE-LAST-ACT.
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
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3740-AGE-MATURITY.
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
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-CHANNEL.
           EVALUATE XCLT-CHANNEL-CD
               WHEN 'BRN'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E627 REF NO REQUIRED
      * E701 PAYOUT YTD NEGATIVE
      * E806 PAYOUT YTD OVER 99999
      * E805 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3760-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-REF-NO = SPACES
                   MOVE 'E627' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-YTD < 0
                   MOVE 'E701' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-YTD > 99999.00
                   MOVE 'E806' TO WS-REJECT-CODE
               WHEN XCLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E805' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3770-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF XCLM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO XCLM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3780-DESCRIBE-CYCLE.
           EVALUATE XCLM-CYCLE-CD
               WHEN 'EM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E176 ADDR REQUIRED
      * E273 WEEKLY AMT NEGATIVE
      * E427 WEEKLY AMT OVER 25000
      * E495 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3790-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-ADDR = SPACES
                   MOVE 'E176' TO WS-REJECT-CODE
               WHEN XCLT-WEEKLY-AMT < 0
                   MOVE 'E273' TO WS-REJECT-CODE
               WHEN XCLT-WEEKLY-AMT > 25000.00
                   MOVE 'E427' TO WS-REJECT-CODE
               WHEN XCLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E495' TO WS-REJECT-CODE
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
       3800-COUNT-RETRY.
           IF XCLM-RETRY-CNT < 9999999
               ADD 1 TO XCLM-RETRY-CNT
           END-IF
           IF XCLM-RETRY-CNT NOT < 12
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GOAL YTD FEE ON THE ACCRUED YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3810-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO XCLM-GOAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   COMPUTE XCLM-GOAL-YTD ROUNDED = XCLM-ACCRUED-YTD *
                       XCLM-DISCOUNT / 200
               WHEN XCLT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO XCLM-GOAL-YTD
               WHEN XCLT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO XCLM-GOAL-YTD
               WHEN OTHER
                   COMPUTE XCLM-GOAL-YTD ROUNDED = XCLM-ACCRUED-YTD *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-GOAL-YTD > 0 AND XCLM-GOAL-YTD < 1.00
               MOVE 1.00 TO XCLM-GOAL-YTD
           END-IF
           IF XCLM-GOAL-YTD > 25.00
               MOVE 25.00 TO XCLM-GOAL-YTD
           END-IF
           ADD XCLM-GOAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AL -> BT WHEN EFF DATE IS PAST
      * BT -> CY WHEN EFF DATE IS PAST
      * CY -> KM WHEN EFF DATE IS PAST
      * KM -> MV WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3820-SET-REASON.
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
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3830-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF XCLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN CD:
      * GN -> JT WHEN NOTICE DATE IS PAST
      * JT -> KC WHEN NOTICE DATE IS PAST
      * KC -> KW WHEN NOTICE DATE IS PAST
      * KW -> MX WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3840-SET-PLAN.
           EVALUATE XCLM-PLAN-CD
               WHEN 'GN'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JT'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KC' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KC'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MX' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E215 OPERATOR REQUIRED
      * E103 WORK AMT NEGATIVE
      * E679 WORK AMT OVER 99999
      * E342 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3850-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-OPERATOR = SPACES
                   MOVE 'E215' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E103' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E679' TO WS-REJECT-CODE
               WHEN XCLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E342' TO WS-REJECT-CODE
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
       3860-DESCRIBE-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3870-AGE-MATURITY.
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
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-PAYOUT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-PAYOUT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-PAYOUT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-PAYOUT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3880-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCLM-CONTRIB-YTD / 12
           COMPUTE WS-WORK-AMT-2 = XCLM-CONTRIB-YTD - (WS-WORK-AMT * 12)
           COMPUTE XCLM-PAYOUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MISSED YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLT-CHANNEL-CD
                   ADD XCLM-MISSED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-MISSED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BONUS YTD FEE ON THE BONUS AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3900-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCLM-BONUS-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   COMPUTE XCLM-BONUS-YTD ROUNDED = XCLM-BONUS-AMT *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 20.00 TO XCLM-BONUS-YTD
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 5.00 TO XCLM-BONUS-YTD
               WHEN OTHER
                   COMPUTE XCLM-BONUS-YTD ROUNDED = XCLM-BONUS-AMT *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-BONUS-YTD > 0 AND XCLM-BONUS-YTD < 5.00
               MOVE 5.00 TO XCLM-BONUS-YTD
           END-IF
           IF XCLM-BONUS-YTD > 150.00
               MOVE 150.00 TO XCLM-BONUS-YTD
           END-IF
           ADD XCLM-BONUS-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3910-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF XCLM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO XCLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WEEKLY FEE ON THE WORK AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3920-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO XCLM-WEEKLY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE XCLM-WEEKLY-AMT ROUNDED = WS-WORK-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO XCLM-WEEKLY-AMT
               WHEN XCLT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO XCLM-WEEKLY-AMT
               WHEN OTHER
                   COMPUTE XCLM-WEEKLY-AMT ROUNDED = WS-WORK-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-WEEKLY-AMT > 0 AND XCLM-WEEKLY-AMT < 5.00
               MOVE 5.00 TO XCLM-WEEKLY-AMT
           END-IF
           IF XCLM-WEEKLY-AMT > 250.00
               MOVE 250.00 TO XCLM-WEEKLY-AMT
           END-IF
           ADD XCLM-WEEKLY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT GRID TIER FOR THE WEEKLY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-GRID-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF XCLM-WEEKLY-AMT NOT < WS-PAYOUT-GRID-LOW(WS-TX)
                   MOVE WS-PAYOUT-GRID-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GOAL YTD BY PAYOUT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3940-ACCUM-PAYOUT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-PAYOUT-METHOD-CD
                   ADD XCLM-GOAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-PAYOUT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-GOAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-LAST-ACT.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3960-DESCRIBE-CHANNEL.
           EVALUATE XCLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE GOAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MISSED AMT.
      *----------------------------------------------------------------*
       3970-PRORATE-MISSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-MISSED-AMT ROUNDED =
                   XCLM-GOAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-MISSED-AMT
           END-IF
           ADD XCLM-MISSED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT GRID TIER FOR THE ACCRUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3980-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-GRID-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF XCLM-ACCRUED-YTD NOT < WS-PAYOUT-GRID-LOW(WS-TX)
                   MOVE WS-PAYOUT-GRID-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WEEKLY AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3990-FLAG-WEEKLY.
           IF XCLM-WEEKLY-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-WEEKLY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4000-DESCRIBE-TRAN.
           EVALUATE XCLT-TRAN-CD
               WHEN 'WD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'DP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PO'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PENALTY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BONUS AMT.
      *----------------------------------------------------------------*
       4010-PRORATE-BONUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-BONUS-AMT ROUNDED =
                   XCLM-PENALTY-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLT-BONUS-AMT
           END-IF
           ADD XCLT-BONUS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ACCRUED YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4020-ACCRUE-ACCRUED.
           IF XCLM-ACCRUED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-ACCRUED-YTD * XCLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-ACCRUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTRIB YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4030-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-REASON-CD
                   ADD XCLM-CONTRIB-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-CONTRIB-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WEEKLY FEE ON THE PAYOUT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4040-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO XCLM-WEEKLY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-STOP-FLAG = 'Y'
                   COMPUTE XCLM-WEEKLY-AMT ROUNDED = XCLM-PAYOUT-AMT *
                       XCLM-FEE-PCT / 200
               WHEN XCLT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO XCLM-WEEKLY-AMT
               WHEN XCLT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO XCLM-WEEKLY-AMT
               WHEN OTHER
                   COMPUTE XCLM-WEEKLY-AMT ROUNDED = XCLM-PAYOUT-AMT *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-WEEKLY-AMT > 0 AND XCLM-WEEKLY-AMT < 5.00
               MOVE 5.00 TO XCLM-WEEKLY-AMT
           END-IF
           IF XCLM-WEEKLY-AMT > 100.00
               MOVE 100.00 TO XCLM-WEEKLY-AMT
           END-IF
           ADD XCLM-WEEKLY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WEEKLY YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4050-LIMIT-WEEKLY.
           IF XCLM-WEEKLY-YTD < 25.00
               MOVE 25.00 TO XCLM-WEEKLY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-WEEKLY-YTD > 10000.00
               MOVE 10000.00 TO XCLM-WEEKLY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MISSED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WEEKLY AMT.
      *----------------------------------------------------------------*
       4060-PRORATE-WEEKLY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-WEEKLY-AMT ROUNDED =
                   XCLM-MISSED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLT-WEEKLY-AMT
           END-IF
           ADD XCLT-WEEKLY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYOUT METHOD CD:
      * B -> H WHEN MATURITY DATE IS PAST
      * H -> U WHEN MATURITY DATE IS PAST
      * U -> W WHEN MATURITY DATE IS PAST
      * W -> B WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4070-SET-PAYOUT-METHOD.
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
      * WEEKLY AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4080-FLAG-WEEKLY.
           IF XCLT-WEEKLY-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCLT-WEEKLY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYOUT AMT BY PAYOUT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-PAYOUT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-PAYOUT-METHOD-CD
                   ADD XCLM-PAYOUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-PAYOUT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-PAYOUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4100-COUNT-RETRY.
           IF XCLM-RETRY-CNT < 9999999
               ADD 1 TO XCLM-RETRY-CNT
           END-IF
           IF XCLM-RETRY-CNT NOT < 3
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4110-DESCRIBE-CYCLE.
           EVALUATE XCLM-CYCLE-CD
               WHEN 'EM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WEEKLY AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4120-LIMIT-WEEKLY.
           IF XCLT-WEEKLY-AMT < 25.00
               MOVE 25.00 TO XCLT-WEEKLY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLT-WEEKLY-AMT > 2500.00
               MOVE 2500.00 TO XCLT-WEEKLY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MISSED YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4130-SPLIT-MISSED.
           COMPUTE WS-WORK-AMT = XCLM-MISSED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = XCLM-MISSED-YTD - (WS-WORK-AMT * 3)
           COMPUTE XCLM-WEEKLY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4140-DESCRIBE-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4150-SET-CHANNEL.
           EVALUATE XCLT-CHANNEL-CD
               WHEN 'BRN'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4160-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4170-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MISSED YTD FEE ON THE BONUS AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4180-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO XCLM-MISSED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   COMPUTE XCLM-MISSED-YTD ROUNDED = XCLT-BONUS-AMT *
                       XCLM-RATE / 200
               WHEN XCLM-CYCLE-CD = 'EM'
                   MOVE 7.00 TO XCLM-MISSED-YTD
               WHEN XCLM-CYCLE-CD = 'FE'
                   MOVE 3.00 TO XCLM-MISSED-YTD
               WHEN OTHER
                   COMPUTE XCLM-MISSED-YTD ROUNDED = XCLT-BONUS-AMT *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-MISSED-YTD > 0 AND XCLM-MISSED-YTD < 1.00
               MOVE 1.00 TO XCLM-MISSED-YTD
           END-IF
           IF XCLM-MISSED-YTD > 150.00
               MOVE 150.00 TO XCLM-MISSED-YTD
           END-IF
           ADD XCLM-MISSED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE BONUS YTD FEE ON THE ACCRUED YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4190-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO XCLM-BONUS-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE XCLM-BONUS-YTD ROUNDED = XCLM-ACCRUED-YTD *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-TYPE-CD = 'A'
                   MOVE 3.00 TO XCLM-BONUS-YTD
               WHEN XCLM-TYPE-CD = 'B'
                   MOVE 5.00 TO XCLM-BONUS-YTD
               WHEN OTHER
                   COMPUTE XCLM-BONUS-YTD ROUNDED = XCLM-ACCRUED-YTD *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-BONUS-YTD > 0 AND XCLM-BONUS-YTD < 5.00
               MOVE 5.00 TO XCLM-BONUS-YTD
           END-IF
           IF XCLM-BONUS-YTD > 25.00
               MOVE 25.00 TO XCLM-BONUS-YTD
           END-IF
           ADD XCLM-BONUS-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * BONUS YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4200-LIMIT-BONUS.
           IF XCLM-BONUS-YTD < 0.00
               MOVE 0.00 TO XCLM-BONUS-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-BONUS-YTD > 10000.00
               MOVE 10000.00 TO XCLM-BONUS-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4210-CHECK-LAST-PAY.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUED FEE ON THE PENALTY AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4220-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO XCLM-ACCRUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE XCLM-ACCRUED-AMT ROUNDED = XCLM-PENALTY-AMT *
                       XCLM-FEE-PCT / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 3.00 TO XCLM-ACCRUED-AMT
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 15.00 TO XCLM-ACCRUED-AMT
               WHEN OTHER
                   COMPUTE XCLM-ACCRUED-AMT ROUNDED = XCLM-PENALTY-AMT *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-ACCRUED-AMT > 0 AND XCLM-ACCRUED-AMT < 10.00
               MOVE 10.00 TO XCLM-ACCRUED-AMT
           END-IF
           IF XCLM-ACCRUED-AMT > 50.00
               MOVE 50.00 TO XCLM-ACCRUED-AMT
           END-IF
           ADD XCLM-ACCRUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4230-DESCRIBE-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ACCRUED YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4240-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLT-CHANNEL-CD
                   ADD XCLM-ACCRUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-ACCRUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WEEKLY AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4250-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-TYPE-CD
                   ADD XCLT-WEEKLY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLT-WEEKLY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WEEKLY FEE ON THE WEEKLY AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4260-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO XCLT-WEEKLY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   COMPUTE XCLT-WEEKLY-AMT ROUNDED = XCLM-WEEKLY-AMT *
                       XCLM-RATE / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 5.00 TO XCLT-WEEKLY-AMT
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 20.00 TO XCLT-WEEKLY-AMT
               WHEN OTHER
                   COMPUTE XCLT-WEEKLY-AMT ROUNDED = XCLM-WEEKLY-AMT *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLT-WEEKLY-AMT > 0 AND XCLT-WEEKLY-AMT < 2.00
               MOVE 2.00 TO XCLT-WEEKLY-AMT
           END-IF
           IF XCLT-WEEKLY-AMT > 150.00
               MOVE 150.00 TO XCLT-WEEKLY-AMT
           END-IF
           ADD XCLT-WEEKLY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4270-COUNT-RETRY.
           IF XCLM-RETRY-CNT < 9999999
               ADD 1 TO XCLM-RETRY-CNT
           END-IF
           IF XCLM-RETRY-CNT NOT < 3
               MOVE 'Y' TO XCLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E121 CONTACT REQUIRED
      * E031 WEEKLY AMT NEGATIVE
      * E290 WEEKLY AMT OVER 10000
      * E505 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4280-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-CONTACT = SPACES
                   MOVE 'E121' TO WS-REJECT-CODE
               WHEN XCLT-WEEKLY-AMT < 0
                   MOVE 'E031' TO WS-REJECT-CODE
               WHEN XCLT-WEEKLY-AMT > 10000.00
                   MOVE 'E290' TO WS-REJECT-CODE
               WHEN XCLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GOAL FEE ON THE BONUS YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4290-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO XCLT-GOAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   COMPUTE XCLT-GOAL-AMT ROUNDED = XCLM-BONUS-YTD *
                       XCLM-FEE-PCT / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 7.00 TO XCLT-GOAL-AMT
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 20.00 TO XCLT-GOAL-AMT
               WHEN OTHER
                   COMPUTE XCLT-GOAL-AMT ROUNDED = XCLM-BONUS-YTD *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLT-GOAL-AMT > 0 AND XCLT-GOAL-AMT < 1.00
               MOVE 1.00 TO XCLT-GOAL-AMT
           END-IF
           IF XCLT-GOAL-AMT > 75.00
               MOVE 75.00 TO XCLT-GOAL-AMT
           END-IF
           ADD XCLT-GOAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WEEKLY YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4300-ACCRUE-WEEKLY.
           IF XCLM-WEEKLY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-WEEKLY-YTD * XCLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO XCLT-WEEKLY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MISSED TIER TIER FOR THE BONUS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4310-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MISSED-TIER-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCLT-BONUS-AMT NOT < WS-MISSED-TIER-LOW(WS-TX)
                   MOVE WS-MISSED-TIER-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4320-COUNT-SEQ.
           IF XCLT-SEQ < 9999999
               ADD 1 TO XCLT-SEQ
           END-IF
           IF XCLT-SEQ NOT < 3
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYOUT METHOD CD:
      * B -> H WHEN LAST PAY DATE IS PAST
      * H -> U WHEN LAST PAY DATE IS PAST
      * U -> W WHEN LAST PAY DATE IS PAST
      * W -> B WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4330-SET-PAYOUT-METHOD.
           EVALUATE XCLM-PAYOUT-METHOD-CD
               WHEN 'B'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EM -> FE WHEN NOTICE DATE IS PAST
      * FE -> PL WHEN NOTICE DATE IS PAST
      * PL -> RU WHEN NOTICE DATE IS PAST
      * RU -> SU WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4340-SET-CYCLE.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E648 MEMO REQUIRED
      * E406 WEEKLY AMT NEGATIVE
      * E459 WEEKLY AMT OVER 5000
      * E146 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4350-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-MEMO = SPACES
                   MOVE 'E648' TO WS-REJECT-CODE
               WHEN XCLM-WEEKLY-AMT < 0
                   MOVE 'E406' TO WS-REJECT-CODE
               WHEN XCLM-WEEKLY-AMT > 5000.00
                   MOVE 'E459' TO WS-REJECT-CODE
               WHEN XCLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E146' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PENALTY YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4360-SPLIT-PENALTY.
           COMPUTE WS-WORK-AMT = XCLM-PENALTY-YTD / 2
           COMPUTE WS-WORK-AMT-2 = XCLM-PENALTY-YTD - (WS-WORK-AMT * 2)
           COMPUTE XCLM-BONUS-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT GRID TIER FOR THE ACCRUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4370-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-GRID-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF XCLM-ACCRUED-YTD NOT < WS-PAYOUT-GRID-LOW(WS-TX)
                   MOVE WS-PAYOUT-GRID-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4380-COUNT-PAYMENT.
           IF XCLM-PAYMENT-CNT < 9999999
               ADD 1 TO XCLM-PAYMENT-CNT
           END-IF
           IF XCLM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO XCLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MISSED AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4390-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-CYCLE-CD
                   ADD XCLM-MISSED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-MISSED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUED YTD FEE ON THE BONUS YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4400-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCLM-ACCRUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   COMPUTE XCLM-ACCRUED-YTD ROUNDED = XCLM-BONUS-YTD *
                       XCLM-RATE / 200
               WHEN XCLM-PLAN-CD = 'GN'
                   MOVE 12.00 TO XCLM-ACCRUED-YTD
               WHEN XCLM-PLAN-CD = 'JT'
                   MOVE 7.00 TO XCLM-ACCRUED-YTD
               WHEN OTHER
                   COMPUTE XCLM-ACCRUED-YTD ROUNDED = XCLM-BONUS-YTD *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-ACCRUED-YTD > 0 AND XCLM-ACCRUED-YTD < 10.00
               MOVE 10.00 TO XCLM-ACCRUED-YTD
           END-IF
           IF XCLM-ACCRUED-YTD > 150.00
               MOVE 150.00 TO XCLM-ACCRUED-YTD
           END-IF
           ADD XCLM-ACCRUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT GRID TIER FOR THE CONTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4410-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-GRID-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF XCLM-CONTRIB-AMT NOT < WS-PAYOUT-GRID-LOW(WS-TX)
                   MOVE WS-PAYOUT-GRID-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E424 REF NO REQUIRED
      * E889 WEEKLY AMT NEGATIVE
      * E298 WEEKLY AMT OVER 25000
      * E913 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4420-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-REF-NO = SPACES
                   MOVE 'E424' TO WS-REJECT-CODE
               WHEN XCLM-WEEKLY-AMT < 0
                   MOVE 'E889' TO WS-REJECT-CODE
               WHEN XCLM-WEEKLY-AMT > 25000.00
                   MOVE 'E298' TO WS-REJECT-CODE
               WHEN XCLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E913' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PENALTY YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4430-ACCRUE-PENALTY.
           IF XCLM-PENALTY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-PENALTY-YTD * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCLM-MISSED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4440-AGE-POST.
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
                   ADD XCLM-PAYOUT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLM-PAYOUT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-PAYOUT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-PAYOUT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-PAYOUT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4450-AGE-LAST-STMT.
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
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BONUS YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4460-FLAG-BONUS.
           IF XCLM-BONUS-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-BONUS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WEEKLY AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4470-ACCRUE-WEEKLY.
           IF XCLT-WEEKLY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLT-WEEKLY-AMT * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCLM-GOAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4480-DESCRIBE-PLAN.
           EVALUATE XCLM-PLAN-CD
               WHEN 'GN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BONUS AMT.
      *----------------------------------------------------------------*
       4490-PRORATE-BONUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-BONUS-AMT ROUNDED =
                   XCLM-CONTRIB-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLT-BONUS-AMT
           END-IF
           ADD XCLT-BONUS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E013 NAME REQUIRED
      * E043 CONTRIB AMT NEGATIVE
      * E733 CONTRIB AMT OVER 5000
      * E811 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4500-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-NAME = SPACES
                   MOVE 'E013' TO WS-REJECT-CODE
               WHEN XCLM-CONTRIB-AMT < 0
                   MOVE 'E043' TO WS-REJECT-CODE
               WHEN XCLM-CONTRIB-AMT > 5000.00
                   MOVE 'E733' TO WS-REJECT-CODE
               WHEN XCLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E811' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4510-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO XCLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MISSED TIER TIER FOR THE ACCRUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4520-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MISSED-TIER-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCLM-ACCRUED-AMT NOT < WS-MISSED-TIER-LOW(WS-TX)
                   MOVE WS-MISSED-TIER-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4530-AGE-LAST-ACT.
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
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYOUT YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4540-ACCRUE-PAYOUT.
           IF XCLM-PAYOUT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-PAYOUT-YTD * XCLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-WEEKLY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GOAL AMT BY PAYOUT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4550-ACCUM-PAYOUT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-PAYOUT-METHOD-CD
                   ADD XCLM-GOAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-PAYOUT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-GOAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GOAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MISSED YTD.
      *----------------------------------------------------------------*
       4560-PRORATE-MISSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-MISSED-YTD ROUNDED =
                   XCLM-GOAL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-MISSED-YTD
           END-IF
           ADD XCLM-MISSED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WEEKLY AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4570-ACCRUE-WEEKLY.
           IF XCLT-WEEKLY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLT-WEEKLY-AMT * XCLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4580-CHECK-EFF.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4590-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4600-COUNT-NOTICE.
           IF XCLM-NOTICE-CNT < 9999999
               ADD 1 TO XCLM-NOTICE-CNT
           END-IF
           IF XCLM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4610-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WEEKLY FEE ON THE PAYOUT AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4620-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO XCLT-WEEKLY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   COMPUTE XCLT-WEEKLY-AMT ROUNDED = XCLM-PAYOUT-AMT *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 10.00 TO XCLT-WEEKLY-AMT
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 12.00 TO XCLT-WEEKLY-AMT
               WHEN OTHER
                   COMPUTE XCLT-WEEKLY-AMT ROUNDED = XCLM-PAYOUT-AMT *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLT-WEEKLY-AMT > 0 AND XCLT-WEEKLY-AMT < 10.00
               MOVE 10.00 TO XCLT-WEEKLY-AMT
           END-IF
           IF XCLT-WEEKLY-AMT > 150.00
               MOVE 150.00 TO XCLT-WEEKLY-AMT
           END-IF
           ADD XCLT-WEEKLY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE GOAL FEE ON THE BONUS AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4630-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCLM-GOAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   COMPUTE XCLM-GOAL-AMT ROUNDED = XCLM-BONUS-AMT *
                       XCLM-RATE / 200
               WHEN XCLM-TYPE-CD = 'A'
                   MOVE 15.00 TO XCLM-GOAL-AMT
               WHEN XCLM-TYPE-CD = 'B'
                   MOVE 15.00 TO XCLM-GOAL-AMT
               WHEN OTHER
                   COMPUTE XCLM-GOAL-AMT ROUNDED = XCLM-BONUS-AMT *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-GOAL-AMT > 0 AND XCLM-GOAL-AMT < 1.00
               MOVE 1.00 TO XCLM-GOAL-AMT
           END-IF
           IF XCLM-GOAL-AMT > 150.00
               MOVE 150.00 TO XCLM-GOAL-AMT
           END-IF
           ADD XCLM-GOAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * GOAL AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4640-FLAG-GOAL.
           IF XCLM-GOAL-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-GOAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E306 CONTACT REQUIRED
      * E364 GOAL AMT NEGATIVE
      * E496 GOAL AMT OVER 50000
      * E270 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4650-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-CONTACT = SPACES
                   MOVE 'E306' TO WS-REJECT-CODE
               WHEN XCLM-GOAL-AMT < 0
                   MOVE 'E364' TO WS-REJECT-CODE
               WHEN XCLM-GOAL-AMT > 50000.00
                   MOVE 'E496' TO WS-REJECT-CODE
               WHEN XCLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E270' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GOAL YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4660-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-TYPE-CD
                   ADD XCLM-GOAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-GOAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BONUS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       4670-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-CONTRIB-AMT ROUNDED =
                   XCLM-BONUS-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLT-CONTRIB-AMT
           END-IF
           ADD XCLT-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PENALTY AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4680-FLAG-PENALTY.
           IF XCLM-PENALTY-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-PENALTY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE XCL FIELDS TO XCTIER AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-XCTIER.
           INITIALIZE XCTIER-PARMS
           MOVE XCLM-BONUS-YTD
             TO XCTIER-BONUS-YTD
           MOVE XCLM-ACCRUED-AMT
             TO XCTIER-ACCRUED-AMT
           MOVE XCLM-PAYOUT-AMT
             TO XCTIER-PAYOUT-AMT
           MOVE XCLM-ACCRUED-YTD
             TO XCTIER-ACCRUED-YTD
           MOVE XCLM-MISSED-AMT
             TO XCTIER-MISSED-AMT
           MOVE XCLM-GOAL-AMT
             TO XCTIER-GOAL-AMT
           MOVE XCLM-NOTICE-DATE
             TO XCTIER-NOTICE-DATE
           MOVE XCLM-LAST-PAY-DATE
             TO XCTIER-LAST-PAY-DATE
           MOVE XCLM-LAST-STMT-DATE
             TO XCTIER-LAST-STMT-DATE
           MOVE XCLM-EXPIRY-DATE
             TO XCTIER-EXPIRY-DATE
           MOVE XCLM-PAYOUT-METHOD-CD
             TO XCTIER-PAYOUT-METHOD-CD
           MOVE XCLM-REGION-CD
             TO XCTIER-REGION-CD
           MOVE XCLM-PLAN-CD
             TO XCTIER-PLAN-CD
           MOVE XCLM-REASON-CD
             TO XCTIER-REASON-CD
           MOVE XCLM-OVERRIDE-FLAG
             TO XCTIER-OVERRIDE-FLAG
           MOVE XCLM-SUSPEND-FLAG
             TO XCTIER-SUSPEND-FLAG
           MOVE XCLM-PREFERRED-FLAG
             TO XCTIER-PREFERRED-FLAG
           MOVE XCLM-RETRY-CNT
             TO XCTIER-RETRY-CNT
           MOVE XCLM-CYCLE-CNT
             TO XCTIER-CYCLE-CNT
           MOVE XCLM-RATE
             TO XCTIER-RATE
           MOVE XCLM-DISCOUNT
             TO XCTIER-DISCOUNT
           MOVE XCLM-NAME
             TO XCTIER-NAME
           MOVE XCLM-CONTACT
             TO XCTIER-CONTACT
           CALL 'XCTIER' USING XCTIER-PARMS
           EVALUATE XCTIER-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE XCTIER-BONUS-YTD
                     TO XCLM-BONUS-YTD
                   MOVE XCTIER-ACCRUED-AMT
                     TO XCLM-ACCRUED-AMT
                   MOVE XCTIER-PAYOUT-AMT
                     TO XCLM-PAYOUT-AMT
                   MOVE XCTIER-MISSED-AMT
                     TO XCLM-MISSED-AMT
                   MOVE XCTIER-OVERRIDE-FLAG
                     TO XCLM-OVERRIDE-FLAG
                   MOVE XCTIER-SUSPEND-FLAG
                     TO XCLM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RIER' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-XCTIER' TO ABND-PARAGRAPH
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
                   MOVE XCCAL-GOAL-YTD
                     TO XCLM-GOAL-YTD
                   MOVE XCCAL-PAYOUT-AMT
                     TO XCLM-PAYOUT-AMT
                   MOVE XCCAL-WEEKLY-YTD
                     TO XCLM-WEEKLY-YTD
                   MOVE XCCAL-CONTRIB-YTD
                     TO XCLM-CONTRIB-YTD
                   MOVE XCCAL-MISSED-AMT
                     TO XCLM-MISSED-AMT
                   MOVE XCCAL-PREFERRED-FLAG
                     TO XCLM-PREFERRED-FLAG
                   MOVE XCCAL-STOP-FLAG
                     TO XCLM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-XCCAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
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
                   MOVE XCPEN-GOAL-AMT
                     TO XCLM-GOAL-AMT
                   MOVE XCPEN-WEEKLY-AMT
                     TO XCLM-WEEKLY-AMT
                   MOVE XCPEN-PREFERRED-FLAG
                     TO XCLM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-XCPEN' TO ABND-PARAGRAPH
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
           DISPLAY 'XCLAGE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'XCLAGE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
