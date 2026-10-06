      *================================================================*
      * PROGRAM  : XCLSTMT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : CHRISTMAS CLUB SAVINGS - STATEMENT PREPARATION.     *
      *            READS THE XCL TRANSACTIONS, UPDATES THE XCL MASTER  *
      *            AND PRODUCES THE CONTROL REPORT. HOLIDAY CLUB       *
      *            SAVINGS PLANS WITH WEEKLY DEPOSITS AND NOVEMBER     *
      *            PAYOUT                                              *
      * INPUT    : XCLMST (KSDS), XCLTRN, BANKCTL                      *
      * OUTPUT   : XCLOUT, XCLSTRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, XCTIER, XCCAL, XCLIM    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-09-10 RJT  ORIGINAL                                       *
      * 1992-08-10 PQR  BONUS RERUN PROTECTION PRB3932                 *
      * 1993-01-13 BHS  ACCRUED ROUNDING - HALF UP REQ2251             *
      * 1997-12-12 SKN  MISSED STATUS CODES ADDED PRB7759              *
      * 1998-04-19 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. XCLSTMT.
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
           SELECT REPORT-FILE ASSIGN TO "XCLSTRPT"
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
                                           VALUE 12.000000.
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
      * WS-ACCRUED-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ACCRUED-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000448000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000768500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000773500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011050000000532000'.
       01  WS-ACCRUED-TIER-TBL REDEFINES WS-ACCRUED-TIER-VALUES.
           05  WS-ACCRUED-TIER-ROW OCCURS 4 TIMES.
               10  WS-ACCRUED-TIER-LOW     PIC 9(09)V99.
               10  WS-ACCRUED-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-GOAL-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-GOAL-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000423500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000529000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000843500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007750000000867000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008250000000887500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013250000000843500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023250000000905500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025750000000712500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026000000000780000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026500000000264000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027000000000418500'.
       01  WS-GOAL-TIER-TBL REDEFINES WS-GOAL-TIER-VALUES.
           05  WS-GOAL-TIER-ROW OCCURS 11 TIMES.
               10  WS-GOAL-TIER-LOW        PIC 9(09)V99.
               10  WS-GOAL-TIER-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYOUT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYOUT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000251500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000450000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000032000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015050000000548500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015300000000762500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017800000000433500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022800000000893000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023300000000585000'.
       01  WS-PAYOUT-GRID-TBL REDEFINES WS-PAYOUT-GRID-VALUES.
           05  WS-PAYOUT-GRID-ROW OCCURS 8 TIMES.
               10  WS-PAYOUT-GRID-LOW      PIC 9(09)V99.
               10  WS-PAYOUT-GRID-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  XCLSTMT  CHRISTMAS CLUB SAVINGS'.
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
               PERFORM 3000-SELECT-FEE-PCT
               PERFORM 3010-DESCRIBE-TYPE
               PERFORM 3020-SET-CYCLE
               PERFORM 3030-ACCRUE-DISCOUNT
               PERFORM 3040-ACCRUE-RATE
               PERFORM 3050-SPLIT-PAYOUT
               PERFORM 3060-CHECK-NEXT-REVIEW
               PERFORM 3070-SET-REGION
               PERFORM 3080-DESCRIBE-REGION
               PERFORM 3090-FLAG-WEEKLY
               PERFORM 3100-ASSESS-STOP
               PERFORM 3110-CHECK-LAST-STMT
               PERFORM 3120-ACCUM-CHANNEL
               PERFORM 3130-LIMIT-WEEKLY
               PERFORM 3140-ASSESS-SUSPEND
               PERFORM 3150-FLAG-ITEM
               PERFORM 3160-DESCRIBE-TRAN
               PERFORM 3170-SELECT-DISCOUNT
               PERFORM 3180-CHECK-NOTICE
               PERFORM 3190-CHECK-POST
               PERFORM 3200-AGE-POST
               PERFORM 3210-ACCUM-REGION
               PERFORM 3220-AGE-NOTICE
               PERFORM 3230-EDIT-REF-NO
               PERFORM 3240-ACCUM-TYPE
               PERFORM 3250-DESCRIBE-REASON
               PERFORM 3260-EDIT-MEMO
               PERFORM 3270-SELECT-FEE-PCT
               PERFORM 3280-ASSESS-AUTO-RENEW
               PERFORM 3290-DESCRIBE-CHANNEL
               PERFORM 3300-SELECT-DISCOUNT
               PERFORM 3310-DESCRIBE-REGION
               PERFORM 3320-COUNT-SEQ
               PERFORM 3330-SELECT-DISCOUNT
               PERFORM 3340-DESCRIBE-TYPE
               PERFORM 3350-COUNT-NOTICE
               PERFORM 3360-EDIT-ADDR
               PERFORM 3370-COUNT-RETRY
               PERFORM 3380-ACCRUE-CONTRIB
               PERFORM 3390-AGE-NEXT-DUE
               PERFORM 3400-SELECT-DISCOUNT
               PERFORM 3410-ASSESS-AUTO-RENEW
               PERFORM 3420-ACCRUE-PENALTY
               PERFORM 3430-ASSESS-EXEMPT
               PERFORM 3440-ASSESS-EXEMPT
               PERFORM 3450-SPLIT-WEEKLY
               PERFORM 3460-EDIT-DESC
               PERFORM 3470-COUNT-PAYMENT
               PERFORM 3480-COUNT-SEQ
               PERFORM 3490-ASSESS-AUTO-RENEW
               PERFORM 3500-SELECT-DISCOUNT
               PERFORM 3510-AGE-LAST-ACT
               PERFORM 3520-FLAG-BONUS
               PERFORM 3530-COUNT-PAYMENT
               PERFORM 3540-LIMIT-GOAL
               PERFORM 3550-SELECT-RATE
               PERFORM 3560-FLAG-ITEM
               PERFORM 3570-AGE-LAST-STMT
               PERFORM 3580-CHECK-LAST-STMT
               PERFORM 3590-LIMIT-ITEM
               PERFORM 3600-COUNT-CYCLE
               PERFORM 3610-FLAG-ITEM
               PERFORM 3620-ACCRUE-CONTRIB
               PERFORM 3630-SPLIT-PAYOUT
               PERFORM 3640-PRORATE-PENALTY
               PERFORM 3650-LIMIT-PENALTY
               PERFORM 3660-SELECT-DISCOUNT
               PERFORM 3670-CHECK-NEXT-DUE
               PERFORM 3680-PRORATE-PAYOUT
               PERFORM 3690-LIMIT-BONUS
               PERFORM 3700-ACCRUE-GOAL
               PERFORM 3710-ASSESS-STOP
               PERFORM 3720-ASSESS-SUSPEND
               PERFORM 3730-EDIT-CUST-ID
               PERFORM 3740-LIMIT-ITEM
               PERFORM 3750-SET-REASON
               PERFORM 3760-ACCUM-REGION
               PERFORM 3770-SET-REASON
               PERFORM 3780-ACCRUE-MISSED
               PERFORM 3790-ASSESS-REVERSAL
               PERFORM 3800-LIMIT-CONTRIB
               PERFORM 3810-ACCRUE-DISCOUNT
               PERFORM 3820-ACCUM-PLAN
               PERFORM 3830-SELECT-FEE-PCT
               PERFORM 3840-ACCRUE-BONUS
               PERFORM 3850-CHECK-MATURITY
               PERFORM 3860-EDIT-OPERATOR
               PERFORM 3870-DESCRIBE-TYPE
               PERFORM 3880-SPLIT-WEEKLY
               PERFORM 3890-SELECT-DISCOUNT
               PERFORM 3900-ASSESS-PREFERRED
               PERFORM 3910-CHECK-NOTICE
               PERFORM 3920-CHECK-NEXT-DUE
               PERFORM 3930-PRORATE-PENALTY
               PERFORM 3940-COUNT-NOTICE
               PERFORM 3950-AGE-NEXT-REVIEW
               PERFORM 3960-PRORATE-ACCRUED
               PERFORM 3970-EDIT-CONTACT
               PERFORM 3980-SPLIT-GOAL
               PERFORM 3990-COUNT-NOTICE
               PERFORM 4000-ACCRUE-ACCRUED
               PERFORM 4010-SPLIT-BONUS
               PERFORM 4020-DESCRIBE-TYPE
               PERFORM 4030-PRORATE-BONUS
               PERFORM 4040-ASSESS-AUTO-RENEW
               PERFORM 4050-EDIT-CONTACT
               PERFORM 4060-EDIT-NAME
               PERFORM 4070-COUNT-SEQ
               PERFORM 4080-AGE-LAST-PAY
               PERFORM 4090-ASSESS-PREFERRED
               PERFORM 4100-SET-REGION
               PERFORM 4110-SELECT-RATE
               PERFORM 4120-SET-TRAN
               PERFORM 4130-LIMIT-PENALTY
               PERFORM 4140-AGE-POST
               PERFORM 4150-SET-REASON
               PERFORM 4160-ACCUM-CYCLE
               PERFORM 4170-ACCUM-TRAN
               PERFORM 4180-FLAG-ACCRUED
               PERFORM 4190-ASSESS-AUTO-RENEW
               PERFORM 4200-ACCUM-CHANNEL
               PERFORM 4210-ACCUM-REASON
               PERFORM 4220-DESCRIBE-TYPE
               PERFORM 4230-SELECT-RATE
               PERFORM 4240-CHECK-POST
               PERFORM 4250-SELECT-DISCOUNT
               PERFORM 4260-DESCRIBE-REGION
               PERFORM 4270-SET-TYPE
               PERFORM 4280-ACCUM-PAYOUT-METHOD
               PERFORM 4290-SPLIT-GOAL
               PERFORM 4300-LIMIT-PAYOUT
               PERFORM 4310-CHECK-NEXT-REVIEW
               PERFORM 4320-LIMIT-ITEM
               PERFORM 4330-PRORATE-ACCRUED
               PERFORM 4340-SELECT-RATE
               PERFORM 4350-DESCRIBE-TYPE
               PERFORM 4360-ASSESS-STOP
               PERFORM 4370-ACCUM-CHANNEL
               PERFORM 4380-SET-CYCLE
               PERFORM 4390-ASSESS-AUTO-RENEW
               PERFORM 4400-LIMIT-ACCRUED
               PERFORM 4410-EDIT-REJECT
               PERFORM 4420-CHECK-LAST-PAY
               PERFORM 4430-EDIT-ADDR
               PERFORM 4440-FLAG-CONTRIB
               PERFORM 4450-DESCRIBE-TYPE
               PERFORM 4460-AGE-NOTICE
               PERFORM 4470-PRORATE-WEEKLY
               PERFORM 4480-SET-TRAN
               PERFORM 4490-PRORATE-WEEKLY
               PERFORM 4500-ASSESS-REVERSAL
               PERFORM 4510-ACCRUE-GOAL
               PERFORM 4520-EDIT-NAME
               PERFORM 4530-COUNT-SEQ
               PERFORM 4540-FLAG-ITEM
               PERFORM 4550-AGE-POST
               PERFORM 4560-ASSESS-REVERSAL
               PERFORM 4570-EDIT-CONTACT
               PERFORM 4580-FLAG-ITEM
               PERFORM 4590-CHECK-NEXT-DUE
               PERFORM 4600-LIMIT-PENALTY
               PERFORM 4610-LIMIT-BONUS
               PERFORM 4620-SET-REASON
               PERFORM 4630-CHECK-EXPIRY
               PERFORM 4640-CHECK-EXPIRY
               PERFORM 4650-FLAG-ACCRUED
               PERFORM 4660-COUNT-SEQ
               PERFORM 4670-SELECT-DISCOUNT
               PERFORM 4680-SPLIT-GOAL
               PERFORM 4690-EDIT-OPERATOR
               PERFORM 4700-EDIT-NAME
               PERFORM 4710-SELECT-DISCOUNT
               PERFORM 4720-LIMIT-WEEKLY
               PERFORM 4730-ACCRUE-GOAL
               PERFORM 4740-AGE-MATURITY
               PERFORM 4750-SPLIT-CONTRIB
               PERFORM 7000-CALL-XCTIER
               PERFORM 7040-CALL-XCCAL
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
      * SELECT THE PAYOUT GRID TIER FOR THE ACCRUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3000-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-GRID-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF XCLM-ACCRUED-AMT NOT < WS-PAYOUT-GRID-LOW(WS-TX)
                   MOVE WS-PAYOUT-GRID-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3010-DESCRIBE-TYPE.
           EVALUATE XCLM-TYPE-CD
               WHEN 'A'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'D'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'E'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'W'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EM -> FE WHEN LAST PAY DATE IS PAST
      * FE -> PL WHEN LAST PAY DATE IS PAST
      * PL -> RU WHEN LAST PAY DATE IS PAST
      * RU -> SU WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3020-SET-CYCLE.
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
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3030-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * XCLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-GOAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3040-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCLM-GOAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYOUT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3050-SPLIT-PAYOUT.
           COMPUTE WS-WORK-AMT = XCLM-PAYOUT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = XCLM-PAYOUT-YTD - (WS-WORK-AMT * 3)
           COMPUTE XCLM-BONUS-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3060-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * FLM -> HLS WHEN NOTICE DATE IS PAST
      * HLS -> KMJ WHEN NOTICE DATE IS PAST
      * KMJ -> PDA WHEN NOTICE DATE IS PAST
      * PDA -> RVG WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3070-SET-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HLS' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HLS'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KMJ' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KMJ'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PDA' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PDA'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RVG' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3080-DESCRIBE-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WEEKLY AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3090-FLAG-WEEKLY.
           IF XCLM-WEEKLY-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-WEEKLY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY FEE ON THE CONTRIB AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3100-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCLM-PENALTY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   COMPUTE XCLM-PENALTY-AMT ROUNDED = XCLT-CONTRIB-AMT *
                       XCLM-RATE / 200
               WHEN XCLM-TYPE-CD = 'A'
                   MOVE 20.00 TO XCLM-PENALTY-AMT
               WHEN XCLM-TYPE-CD = 'B'
                   MOVE 5.00 TO XCLM-PENALTY-AMT
               WHEN OTHER
                   COMPUTE XCLM-PENALTY-AMT ROUNDED = XCLT-CONTRIB-AMT *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-PENALTY-AMT > 0 AND XCLM-PENALTY-AMT < 2.00
               MOVE 2.00 TO XCLM-PENALTY-AMT
           END-IF
           IF XCLM-PENALTY-AMT > 50.00
               MOVE 50.00 TO XCLM-PENALTY-AMT
           END-IF
           ADD XCLM-PENALTY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3110-CHECK-LAST-STMT.
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
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BONUS AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3120-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLT-CHANNEL-CD
                   ADD XCLM-BONUS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-BONUS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WEEKLY YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3130-LIMIT-WEEKLY.
           IF XCLM-WEEKLY-YTD < 0.00
               MOVE 0.00 TO XCLM-WEEKLY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-WEEKLY-YTD > 5000.00
               MOVE 5000.00 TO XCLM-WEEKLY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB FEE ON THE MISSED YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3140-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO XCLM-CONTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   COMPUTE XCLM-CONTRIB-AMT ROUNDED = XCLM-MISSED-YTD *
                       XCLM-RATE / 200
               WHEN XCLM-TYPE-CD = 'A'
                   MOVE 5.00 TO XCLM-CONTRIB-AMT
               WHEN XCLM-TYPE-CD = 'B'
                   MOVE 5.00 TO XCLM-CONTRIB-AMT
               WHEN OTHER
                   COMPUTE XCLM-CONTRIB-AMT ROUNDED = XCLM-MISSED-YTD *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-CONTRIB-AMT > 0 AND XCLM-CONTRIB-AMT < 10.00
               MOVE 10.00 TO XCLM-CONTRIB-AMT
           END-IF
           IF XCLM-CONTRIB-AMT > 35.00
               MOVE 35.00 TO XCLM-CONTRIB-AMT
           END-IF
           ADD XCLM-CONTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3150-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3160-DESCRIBE-TRAN.
           EVALUATE XCLT-TRAN-CD
               WHEN 'WD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PO'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE GOAL TIER TIER FOR THE BONUS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3170-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-GOAL-TIER-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF XCLT-BONUS-AMT NOT < WS-GOAL-TIER-LOW(WS-TX)
                   MOVE WS-GOAL-TIER-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3180-CHECK-NOTICE.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO XCLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-POST.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3200-AGE-POST.
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
                   ADD XCLM-PAYOUT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLM-PAYOUT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-PAYOUT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-PAYOUT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-PAYOUT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE GOAL YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3210-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-REGION-CD
                   ADD XCLM-GOAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-GOAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3220-AGE-NOTICE.
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
               WHEN WS-DAYS NOT > 30
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E217 REF NO REQUIRED
      * E642 GOAL YTD NEGATIVE
      * E258 GOAL YTD OVER 25000
      * E934 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3230-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-REF-NO = SPACES
                   MOVE 'E217' TO WS-REJECT-CODE
               WHEN XCLM-GOAL-YTD < 0
                   MOVE 'E642' TO WS-REJECT-CODE
               WHEN XCLM-GOAL-YTD > 25000.00
                   MOVE 'E258' TO WS-REJECT-CODE
               WHEN XCLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E934' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BONUS AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3240-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-TYPE-CD
                   ADD XCLM-BONUS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-BONUS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3250-DESCRIBE-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E310 MEMO REQUIRED
      * E361 WEEKLY YTD NEGATIVE
      * E554 WEEKLY YTD OVER 1000000
      * E040 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3260-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-MEMO = SPACES
                   MOVE 'E310' TO WS-REJECT-CODE
               WHEN XCLM-WEEKLY-YTD < 0
                   MOVE 'E361' TO WS-REJECT-CODE
               WHEN XCLM-WEEKLY-YTD > 1000000.00
                   MOVE 'E554' TO WS-REJECT-CODE
               WHEN XCLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E040' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GOAL TIER TIER FOR THE ACCRUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GOAL-TIER-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF XCLM-ACCRUED-YTD NOT < WS-GOAL-TIER-LOW(WS-TX)
                   MOVE WS-GOAL-TIER-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYOUT YTD FEE ON THE MISSED AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3280-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO XCLM-PAYOUT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   COMPUTE XCLM-PAYOUT-YTD ROUNDED = XCLM-MISSED-AMT *
                       XCLM-RATE / 200
               WHEN XCLT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO XCLM-PAYOUT-YTD
               WHEN XCLT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO XCLM-PAYOUT-YTD
               WHEN OTHER
                   COMPUTE XCLM-PAYOUT-YTD ROUNDED = XCLM-MISSED-AMT *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-PAYOUT-YTD > 0 AND XCLM-PAYOUT-YTD < 1.00
               MOVE 1.00 TO XCLM-PAYOUT-YTD
           END-IF
           IF XCLM-PAYOUT-YTD > 250.00
               MOVE 250.00 TO XCLM-PAYOUT-YTD
           END-IF
           ADD XCLM-PAYOUT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3290-DESCRIBE-CHANNEL.
           EVALUATE XCLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE GOAL TIER TIER FOR THE BONUS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3300-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-GOAL-TIER-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF XCLT-BONUS-AMT NOT < WS-GOAL-TIER-LOW(WS-TX)
                   MOVE WS-GOAL-TIER-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3310-DESCRIBE-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3320-COUNT-SEQ.
           IF XCLT-SEQ < 9999999
               ADD 1 TO XCLT-SEQ
           END-IF
           IF XCLT-SEQ NOT < 6
               MOVE 'Y' TO XCLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GOAL TIER TIER FOR THE GOAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-GOAL-TIER-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF XCLT-GOAL-AMT NOT < WS-GOAL-TIER-LOW(WS-TX)
                   MOVE WS-GOAL-TIER-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3340-DESCRIBE-TYPE.
           EVALUATE XCLM-TYPE-CD
               WHEN 'A'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'B'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'D'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'E'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'W'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3350-COUNT-NOTICE.
           IF XCLM-NOTICE-CNT < 9999999
               ADD 1 TO XCLM-NOTICE-CNT
           END-IF
           IF XCLM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO XCLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E923 ADDR REQUIRED
      * E742 WORK AMT NEGATIVE
      * E282 WORK AMT OVER 99999
      * E271 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-ADDR = SPACES
                   MOVE 'E923' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E742' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E282' TO WS-REJECT-CODE
               WHEN XCLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E271' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3370-COUNT-RETRY.
           IF XCLM-RETRY-CNT < 9999999
               ADD 1 TO XCLM-RETRY-CNT
           END-IF
           IF XCLM-RETRY-CNT NOT < 6
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3380-ACCRUE-CONTRIB.
           IF XCLM-CONTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-CONTRIB-AMT * XCLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-WEEKLY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3390-AGE-NEXT-DUE.
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
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED TIER TIER FOR THE PENALTY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3400-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-TIER-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCLM-PENALTY-AMT NOT < WS-ACCRUED-TIER-LOW(WS-TX)
                   MOVE WS-ACCRUED-TIER-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB YTD FEE ON THE PAYOUT YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3410-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO XCLM-CONTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-STOP-FLAG = 'Y'
                   COMPUTE XCLM-CONTRIB-YTD ROUNDED = XCLM-PAYOUT-YTD *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 15.00 TO XCLM-CONTRIB-YTD
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 3.00 TO XCLM-CONTRIB-YTD
               WHEN OTHER
                   COMPUTE XCLM-CONTRIB-YTD ROUNDED = XCLM-PAYOUT-YTD *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-CONTRIB-YTD > 0 AND XCLM-CONTRIB-YTD < 1.00
               MOVE 1.00 TO XCLM-CONTRIB-YTD
           END-IF
           IF XCLM-CONTRIB-YTD > 25.00
               MOVE 25.00 TO XCLM-CONTRIB-YTD
           END-IF
           ADD XCLM-CONTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PENALTY AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3420-ACCRUE-PENALTY.
           IF XCLM-PENALTY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-PENALTY-AMT * XCLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-CONTRIB-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUED FEE ON THE CONTRIB YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3430-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO XCLM-ACCRUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   COMPUTE XCLM-ACCRUED-AMT ROUNDED = XCLM-CONTRIB-YTD *
                       XCLM-RATE / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 7.00 TO XCLM-ACCRUED-AMT
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 12.00 TO XCLM-ACCRUED-AMT
               WHEN OTHER
                   COMPUTE XCLM-ACCRUED-AMT ROUNDED = XCLM-CONTRIB-YTD *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-ACCRUED-AMT > 0 AND XCLM-ACCRUED-AMT < 10.00
               MOVE 10.00 TO XCLM-ACCRUED-AMT
           END-IF
           IF XCLM-ACCRUED-AMT > 100.00
               MOVE 100.00 TO XCLM-ACCRUED-AMT
           END-IF
           ADD XCLM-ACCRUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE GOAL YTD FEE ON THE CONTRIB YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3440-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO XCLM-GOAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   COMPUTE XCLM-GOAL-YTD ROUNDED = XCLM-CONTRIB-YTD *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-PAYOUT-METHOD-CD = 'B'
                   MOVE 15.00 TO XCLM-GOAL-YTD
               WHEN XCLM-PAYOUT-METHOD-CD = 'H'
                   MOVE 15.00 TO XCLM-GOAL-YTD
               WHEN OTHER
                   COMPUTE XCLM-GOAL-YTD ROUNDED = XCLM-CONTRIB-YTD *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-GOAL-YTD > 0 AND XCLM-GOAL-YTD < 2.00
               MOVE 2.00 TO XCLM-GOAL-YTD
           END-IF
           IF XCLM-GOAL-YTD > 250.00
               MOVE 250.00 TO XCLM-GOAL-YTD
           END-IF
           ADD XCLM-GOAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3450-SPLIT-WEEKLY.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE XCLM-WEEKLY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E488 DESC REQUIRED
      * E298 BONUS AMT NEGATIVE
      * E140 BONUS AMT OVER 1000000
      * E875 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3460-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-DESC = SPACES
                   MOVE 'E488' TO WS-REJECT-CODE
               WHEN XCLT-BONUS-AMT < 0
                   MOVE 'E298' TO WS-REJECT-CODE
               WHEN XCLT-BONUS-AMT > 1000000.00
                   MOVE 'E140' TO WS-REJECT-CODE
               WHEN XCLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E875' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3470-COUNT-PAYMENT.
           IF XCLM-PAYMENT-CNT < 9999999
               ADD 1 TO XCLM-PAYMENT-CNT
           END-IF
           IF XCLM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3480-COUNT-SEQ.
           IF XCLT-SEQ < 9999999
               ADD 1 TO XCLT-SEQ
           END-IF
           IF XCLT-SEQ NOT < 3
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE GOAL YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3490-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = XCLM-GOAL-YTD *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-CYCLE-CD = 'EM'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN XCLM-CYCLE-CD = 'FE'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = XCLM-GOAL-YTD *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED TIER TIER FOR THE GOAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3500-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-TIER-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCLT-GOAL-AMT NOT < WS-ACCRUED-TIER-LOW(WS-TX)
                   MOVE WS-ACCRUED-TIER-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3510-AGE-LAST-ACT.
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
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BONUS YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3520-FLAG-BONUS.
           IF XCLM-BONUS-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-BONUS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3530-COUNT-PAYMENT.
           IF XCLM-PAYMENT-CNT < 9999999
               ADD 1 TO XCLM-PAYMENT-CNT
           END-IF
           IF XCLM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * GOAL AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3540-LIMIT-GOAL.
           IF XCLT-GOAL-AMT < 25.00
               MOVE 25.00 TO XCLT-GOAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLT-GOAL-AMT > 10000.00
               MOVE 10000.00 TO XCLT-GOAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT GRID TIER FOR THE ACCRUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3550-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-GRID-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF XCLM-ACCRUED-YTD NOT < WS-PAYOUT-GRID-LOW(WS-TX)
                   MOVE WS-PAYOUT-GRID-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3560-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3570-AGE-LAST-STMT.
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
                   ADD XCLM-PAYOUT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLM-PAYOUT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-PAYOUT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-PAYOUT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-PAYOUT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3580-CHECK-LAST-STMT.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3590-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3600-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3610-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3620-ACCRUE-CONTRIB.
           IF XCLT-CONTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLT-CONTRIB-AMT * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCLM-BONUS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYOUT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3630-SPLIT-PAYOUT.
           COMPUTE WS-WORK-AMT = XCLM-PAYOUT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = XCLM-PAYOUT-YTD - (WS-WORK-AMT * 12)
           COMPUTE XCLT-BONUS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BONUS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PENALTY AMT.
      *----------------------------------------------------------------*
       3640-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-PENALTY-AMT ROUNDED =
                   XCLM-BONUS-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-PENALTY-AMT
           END-IF
           ADD XCLM-PENALTY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PENALTY YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3650-LIMIT-PENALTY.
           IF XCLM-PENALTY-YTD < 10.00
               MOVE 10.00 TO XCLM-PENALTY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-PENALTY-YTD > 10000.00
               MOVE 10000.00 TO XCLM-PENALTY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED TIER TIER FOR THE MISSED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3660-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-TIER-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCLM-MISSED-AMT NOT < WS-ACCRUED-TIER-LOW(WS-TX)
                   MOVE WS-ACCRUED-TIER-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3670-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYOUT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3680-PRORATE-PAYOUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   XCLM-PAYOUT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * BONUS AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3690-LIMIT-BONUS.
           IF XCLT-BONUS-AMT < 0.00
               MOVE 0.00 TO XCLT-BONUS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLT-BONUS-AMT > 10000.00
               MOVE 10000.00 TO XCLT-BONUS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GOAL AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3700-ACCRUE-GOAL.
           IF XCLT-GOAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLT-GOAL-AMT * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCLM-PAYOUT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GOAL YTD FEE ON THE ACCRUED AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3710-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCLM-GOAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE XCLM-GOAL-YTD ROUNDED = XCLM-ACCRUED-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-REGION-CD = 'FLM'
                   MOVE 3.00 TO XCLM-GOAL-YTD
               WHEN XCLM-REGION-CD = 'HLS'
                   MOVE 12.00 TO XCLM-GOAL-YTD
               WHEN OTHER
                   COMPUTE XCLM-GOAL-YTD ROUNDED = XCLM-ACCRUED-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-GOAL-YTD > 0 AND XCLM-GOAL-YTD < 2.00
               MOVE 2.00 TO XCLM-GOAL-YTD
           END-IF
           IF XCLM-GOAL-YTD > 150.00
               MOVE 150.00 TO XCLM-GOAL-YTD
           END-IF
           ADD XCLM-GOAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY FEE ON THE MISSED AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3720-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO XCLM-PENALTY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE XCLM-PENALTY-AMT ROUNDED = XCLM-MISSED-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 20.00 TO XCLM-PENALTY-AMT
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 15.00 TO XCLM-PENALTY-AMT
               WHEN OTHER
                   COMPUTE XCLM-PENALTY-AMT ROUNDED = XCLM-MISSED-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-PENALTY-AMT > 0 AND XCLM-PENALTY-AMT < 1.00
               MOVE 1.00 TO XCLM-PENALTY-AMT
           END-IF
           IF XCLM-PENALTY-AMT > 100.00
               MOVE 100.00 TO XCLM-PENALTY-AMT
           END-IF
           ADD XCLM-PENALTY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E215 CUST ID REQUIRED
      * E688 PENALTY AMT NEGATIVE
      * E488 PENALTY AMT OVER 99999
      * E087 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3730-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-CUST-ID = SPACES
                   MOVE 'E215' TO WS-REJECT-CODE
               WHEN XCLM-PENALTY-AMT < 0
                   MOVE 'E688' TO WS-REJECT-CODE
               WHEN XCLM-PENALTY-AMT > 99999.00
                   MOVE 'E488' TO WS-REJECT-CODE
               WHEN XCLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E087' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3740-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AL -> BT WHEN LAST ACT DATE IS PAST
      * BT -> CY WHEN LAST ACT DATE IS PAST
      * CY -> KM WHEN LAST ACT DATE IS PAST
      * KM -> MV WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   IF XCLM-LAST-ACT-DATE > 0 AND XCLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BT'
                   IF XCLM-LAST-ACT-DATE > 0 AND XCLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CY' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CY'
                   IF XCLM-LAST-ACT-DATE > 0 AND XCLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KM' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KM'
                   IF XCLM-LAST-ACT-DATE > 0 AND XCLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MV' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ACCRUED YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3760-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-REGION-CD
                   ADD XCLM-ACCRUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-ACCRUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AL -> BT WHEN EXPIRY DATE IS PAST
      * BT -> CY WHEN EXPIRY DATE IS PAST
      * CY -> KM WHEN EXPIRY DATE IS PAST
      * KM -> MV WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3770-SET-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BT'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CY' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CY'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KM' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KM'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MV' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MISSED AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3780-ACCRUE-MISSED.
           IF XCLM-MISSED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-MISSED-AMT * XCLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO XCLM-WEEKLY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MISSED YTD FEE ON THE PENALTY AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3790-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO XCLM-MISSED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE XCLM-MISSED-YTD ROUNDED = XCLM-PENALTY-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-CYCLE-CD = 'EM'
                   MOVE 12.00 TO XCLM-MISSED-YTD
               WHEN XCLM-CYCLE-CD = 'FE'
                   MOVE 20.00 TO XCLM-MISSED-YTD
               WHEN OTHER
                   COMPUTE XCLM-MISSED-YTD ROUNDED = XCLM-PENALTY-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-MISSED-YTD > 0 AND XCLM-MISSED-YTD < 2.00
               MOVE 2.00 TO XCLM-MISSED-YTD
           END-IF
           IF XCLM-MISSED-YTD > 250.00
               MOVE 250.00 TO XCLM-MISSED-YTD
           END-IF
           ADD XCLM-MISSED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CONTRIB YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3800-LIMIT-CONTRIB.
           IF XCLM-CONTRIB-YTD < 0.00
               MOVE 0.00 TO XCLM-CONTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-CONTRIB-YTD > 10000.00
               MOVE 10000.00 TO XCLM-CONTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3810-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * XCLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WEEKLY AMT BY PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3820-ACCUM-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-PLAN-CD
                   ADD XCLM-WEEKLY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-WEEKLY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GOAL TIER TIER FOR THE CONTRIB YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3830-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GOAL-TIER-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF XCLM-CONTRIB-YTD NOT < WS-GOAL-TIER-LOW(WS-TX)
                   MOVE WS-GOAL-TIER-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BONUS YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3840-ACCRUE-BONUS.
           IF XCLM-BONUS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-BONUS-YTD * XCLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-PAYOUT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-MATURITY.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E084 OPERATOR REQUIRED
      * E773 MISSED AMT NEGATIVE
      * E153 MISSED AMT OVER 5000
      * E141 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3860-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-OPERATOR = SPACES
                   MOVE 'E084' TO WS-REJECT-CODE
               WHEN XCLM-MISSED-AMT < 0
                   MOVE 'E773' TO WS-REJECT-CODE
               WHEN XCLM-MISSED-AMT > 5000.00
                   MOVE 'E153' TO WS-REJECT-CODE
               WHEN XCLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E141' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3870-DESCRIBE-TYPE.
           EVALUATE XCLM-TYPE-CD
               WHEN 'A'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'D'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'E'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'W'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WEEKLY YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3880-SPLIT-WEEKLY.
           COMPUTE WS-WORK-AMT = XCLM-WEEKLY-YTD / 4
           COMPUTE WS-WORK-AMT-2 = XCLM-WEEKLY-YTD - (WS-WORK-AMT * 4)
           COMPUTE XCLM-BONUS-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT GRID TIER FOR THE PAYOUT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3890-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-GRID-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF XCLM-PAYOUT-YTD NOT < WS-PAYOUT-GRID-LOW(WS-TX)
                   MOVE WS-PAYOUT-GRID-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB FEE ON THE PAYOUT YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3900-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO XCLM-CONTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE XCLM-CONTRIB-AMT ROUNDED = XCLM-PAYOUT-YTD *
                       XCLM-RATE / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 3.00 TO XCLM-CONTRIB-AMT
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 5.00 TO XCLM-CONTRIB-AMT
               WHEN OTHER
                   COMPUTE XCLM-CONTRIB-AMT ROUNDED = XCLM-PAYOUT-YTD *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-CONTRIB-AMT > 0 AND XCLM-CONTRIB-AMT < 2.00
               MOVE 2.00 TO XCLM-CONTRIB-AMT
           END-IF
           IF XCLM-CONTRIB-AMT > 25.00
               MOVE 25.00 TO XCLM-CONTRIB-AMT
           END-IF
           ADD XCLM-CONTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3910-CHECK-NOTICE.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3920-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO XCLM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PENALTY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PENALTY AMT.
      *----------------------------------------------------------------*
       3930-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-PENALTY-AMT ROUNDED =
                   XCLM-PENALTY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-PENALTY-AMT
           END-IF
           ADD XCLM-PENALTY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3940-COUNT-NOTICE.
           IF XCLM-NOTICE-CNT < 9999999
               ADD 1 TO XCLM-NOTICE-CNT
           END-IF
           IF XCLM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3950-AGE-NEXT-REVIEW.
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
                   ADD XCLM-BONUS-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD XCLM-BONUS-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-BONUS-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-BONUS-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-BONUS-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ACCRUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3960-PRORATE-ACCRUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   XCLM-ACCRUED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E241 CONTACT REQUIRED
      * E477 WEEKLY AMT NEGATIVE
      * E913 WEEKLY AMT OVER 250000
      * E486 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3970-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-CONTACT = SPACES
                   MOVE 'E241' TO WS-REJECT-CODE
               WHEN XCLT-WEEKLY-AMT < 0
                   MOVE 'E477' TO WS-REJECT-CODE
               WHEN XCLT-WEEKLY-AMT > 250000.00
                   MOVE 'E913' TO WS-REJECT-CODE
               WHEN XCLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E486' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GOAL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3980-SPLIT-GOAL.
           COMPUTE WS-WORK-AMT = XCLT-GOAL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = XCLT-GOAL-AMT - (WS-WORK-AMT * 2)
           COMPUTE XCLM-PAYOUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3990-COUNT-NOTICE.
           IF XCLM-NOTICE-CNT < 9999999
               ADD 1 TO XCLM-NOTICE-CNT
           END-IF
           IF XCLM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO XCLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ACCRUED AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4000-ACCRUE-ACCRUED.
           IF XCLM-ACCRUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-ACCRUED-AMT * XCLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-PAYOUT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BONUS YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4010-SPLIT-BONUS.
           COMPUTE WS-WORK-AMT = XCLM-BONUS-YTD / 2
           COMPUTE WS-WORK-AMT-2 = XCLM-BONUS-YTD - (WS-WORK-AMT * 2)
           COMPUTE XCLT-BONUS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4020-DESCRIBE-TYPE.
           EVALUATE XCLM-TYPE-CD
               WHEN 'A'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'D'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'E'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WEEKLY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BONUS AMT.
      *----------------------------------------------------------------*
       4030-PRORATE-BONUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-BONUS-AMT ROUNDED =
                   XCLT-WEEKLY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLT-BONUS-AMT
           END-IF
           ADD XCLT-BONUS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE BONUS FEE ON THE GOAL AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4040-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO XCLT-BONUS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   COMPUTE XCLT-BONUS-AMT ROUNDED = XCLT-GOAL-AMT *
                       XCLM-RATE / 200
               WHEN XCLM-PLAN-CD = 'GN'
                   MOVE 3.00 TO XCLT-BONUS-AMT
               WHEN XCLM-PLAN-CD = 'JT'
                   MOVE 5.00 TO XCLT-BONUS-AMT
               WHEN OTHER
                   COMPUTE XCLT-BONUS-AMT ROUNDED = XCLT-GOAL-AMT *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLT-BONUS-AMT > 0 AND XCLT-BONUS-AMT < 10.00
               MOVE 10.00 TO XCLT-BONUS-AMT
           END-IF
           IF XCLT-BONUS-AMT > 35.00
               MOVE 35.00 TO XCLT-BONUS-AMT
           END-IF
           ADD XCLT-BONUS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E754 CONTACT REQUIRED
      * E469 WEEKLY AMT NEGATIVE
      * E846 WEEKLY AMT OVER 25000
      * E355 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4050-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-CONTACT = SPACES
                   MOVE 'E754' TO WS-REJECT-CODE
               WHEN XCLT-WEEKLY-AMT < 0
                   MOVE 'E469' TO WS-REJECT-CODE
               WHEN XCLT-WEEKLY-AMT > 25000.00
                   MOVE 'E846' TO WS-REJECT-CODE
               WHEN XCLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E355' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E745 NAME REQUIRED
      * E325 PAYOUT YTD NEGATIVE
      * E112 PAYOUT YTD OVER 250000
      * E056 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4060-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-NAME = SPACES
                   MOVE 'E745' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-YTD < 0
                   MOVE 'E325' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-YTD > 250000.00
                   MOVE 'E112' TO WS-REJECT-CODE
               WHEN XCLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E056' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4070-COUNT-SEQ.
           IF XCLT-SEQ < 9999999
               ADD 1 TO XCLT-SEQ
           END-IF
           IF XCLT-SEQ NOT < 10
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4080-AGE-LAST-PAY.
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
                   ADD XCLM-BONUS-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-BONUS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-BONUS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-BONUS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-BONUS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BONUS YTD FEE ON THE CONTRIB AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4090-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO XCLM-BONUS-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE XCLM-BONUS-YTD ROUNDED = XCLM-CONTRIB-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO XCLM-BONUS-YTD
               WHEN XCLT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO XCLM-BONUS-YTD
               WHEN OTHER
                   COMPUTE XCLM-BONUS-YTD ROUNDED = XCLM-CONTRIB-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-BONUS-YTD > 0 AND XCLM-BONUS-YTD < 1.00
               MOVE 1.00 TO XCLM-BONUS-YTD
           END-IF
           IF XCLM-BONUS-YTD > 100.00
               MOVE 100.00 TO XCLM-BONUS-YTD
           END-IF
           ADD XCLM-BONUS-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * FLM -> HLS WHEN NEXT REVIEW DATE IS PAST
      * HLS -> KMJ WHEN NEXT REVIEW DATE IS PAST
      * KMJ -> PDA WHEN NEXT REVIEW DATE IS PAST
      * PDA -> RVG WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4100-SET-REGION.
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
      * SELECT THE PAYOUT GRID TIER FOR THE MISSED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4110-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-GRID-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF XCLM-MISSED-AMT NOT < WS-PAYOUT-GRID-LOW(WS-TX)
                   MOVE WS-PAYOUT-GRID-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * WD -> DP WHEN NEXT REVIEW DATE IS PAST
      * DP -> PO WHEN NEXT REVIEW DATE IS PAST
      * PO -> BN WHEN NEXT REVIEW DATE IS PAST
      * BN -> PN WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4120-SET-TRAN.
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
      * PENALTY YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4130-LIMIT-PENALTY.
           IF XCLM-PENALTY-YTD < 50.00
               MOVE 50.00 TO XCLM-PENALTY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-PENALTY-YTD > 1000.00
               MOVE 1000.00 TO XCLM-PENALTY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4140-AGE-POST.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AL -> BT WHEN OPEN DATE IS PAST
      * BT -> CY WHEN OPEN DATE IS PAST
      * CY -> KM WHEN OPEN DATE IS PAST
      * KM -> MV WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4150-SET-REASON.
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
      * ACCUMULATE GOAL AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4160-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-CYCLE-CD
                   ADD XCLM-GOAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-GOAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTRIB AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4170-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLT-TRAN-CD
                   ADD XCLT-CONTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLT-CONTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUED AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4180-FLAG-ACCRUED.
           IF XCLM-ACCRUED-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-ACCRUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GOAL YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4190-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO XCLM-GOAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   COMPUTE XCLM-GOAL-YTD ROUNDED = WS-WORK-AMT-2 *
                       XCLM-FEE-PCT / 200
               WHEN XCLT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO XCLM-GOAL-YTD
               WHEN XCLT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO XCLM-GOAL-YTD
               WHEN OTHER
                   COMPUTE XCLM-GOAL-YTD ROUNDED = WS-WORK-AMT-2 *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-GOAL-YTD > 0 AND XCLM-GOAL-YTD < 10.00
               MOVE 10.00 TO XCLM-GOAL-YTD
           END-IF
           IF XCLM-GOAL-YTD > 100.00
               MOVE 100.00 TO XCLM-GOAL-YTD
           END-IF
           ADD XCLM-GOAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYOUT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4200-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLT-CHANNEL-CD
                   ADD XCLM-PAYOUT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-PAYOUT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4210-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-REASON-CD
                   ADD XCLM-PENALTY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-PENALTY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4220-DESCRIBE-TYPE.
           EVALUATE XCLM-TYPE-CD
               WHEN 'A'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'E'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE GOAL TIER TIER FOR THE CONTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4230-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-GOAL-TIER-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF XCLM-CONTRIB-AMT NOT < WS-GOAL-TIER-LOW(WS-TX)
                   MOVE WS-GOAL-TIER-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4240-CHECK-POST.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYOUT GRID TIER FOR THE PAYOUT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4250-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYOUT-GRID-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF XCLM-PAYOUT-AMT NOT < WS-PAYOUT-GRID-LOW(WS-TX)
                   MOVE WS-PAYOUT-GRID-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4260-DESCRIBE-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * A -> B WHEN LAST PAY DATE IS PAST
      * B -> D WHEN LAST PAY DATE IS PAST
      * D -> E WHEN LAST PAY DATE IS PAST
      * E -> T WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4270-SET-TYPE.
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
      * ACCUMULATE WEEKLY AMT BY PAYOUT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4280-ACCUM-PAYOUT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-PAYOUT-METHOD-CD
                   ADD XCLT-WEEKLY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-PAYOUT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLT-WEEKLY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GOAL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4290-SPLIT-GOAL.
           COMPUTE WS-WORK-AMT = XCLM-GOAL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = XCLM-GOAL-AMT - (WS-WORK-AMT * 4)
           COMPUTE XCLT-GOAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYOUT YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4300-LIMIT-PAYOUT.
           IF XCLM-PAYOUT-YTD < 100.00
               MOVE 100.00 TO XCLM-PAYOUT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-PAYOUT-YTD > 5000.00
               MOVE 5000.00 TO XCLM-PAYOUT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4310-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4320-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BONUS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ACCRUED YTD.
      *----------------------------------------------------------------*
       4330-PRORATE-ACCRUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-ACCRUED-YTD ROUNDED =
                   XCLT-BONUS-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-ACCRUED-YTD
           END-IF
           ADD XCLM-ACCRUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED TIER TIER FOR THE PENALTY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4340-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-TIER-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCLM-PENALTY-YTD NOT < WS-ACCRUED-TIER-LOW(WS-TX)
                   MOVE WS-ACCRUED-TIER-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4350-DESCRIBE-TYPE.
           EVALUATE XCLM-TYPE-CD
               WHEN 'A'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'B'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'D'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'E'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'W'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BONUS FEE ON THE CONTRIB AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4360-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCLM-BONUS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   COMPUTE XCLM-BONUS-AMT ROUNDED = XCLM-CONTRIB-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 20.00 TO XCLM-BONUS-AMT
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 7.00 TO XCLM-BONUS-AMT
               WHEN OTHER
                   COMPUTE XCLM-BONUS-AMT ROUNDED = XCLM-CONTRIB-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-BONUS-AMT > 0 AND XCLM-BONUS-AMT < 2.00
               MOVE 2.00 TO XCLM-BONUS-AMT
           END-IF
           IF XCLM-BONUS-AMT > 75.00
               MOVE 75.00 TO XCLM-BONUS-AMT
           END-IF
           ADD XCLM-BONUS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTRIB AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4370-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLT-CHANNEL-CD
                   ADD XCLT-CONTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLT-CONTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EM -> FE WHEN NOTICE DATE IS PAST
      * FE -> PL WHEN NOTICE DATE IS PAST
      * PL -> RU WHEN NOTICE DATE IS PAST
      * RU -> SU WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4380-SET-CYCLE.
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
      * ASSESS THE CONTRIB YTD FEE ON THE PENALTY YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4390-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO XCLM-CONTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE XCLM-CONTRIB-YTD ROUNDED = XCLM-PENALTY-YTD *
                       XCLM-RATE / 200
               WHEN XCLM-PLAN-CD = 'GN'
                   MOVE 20.00 TO XCLM-CONTRIB-YTD
               WHEN XCLM-PLAN-CD = 'JT'
                   MOVE 5.00 TO XCLM-CONTRIB-YTD
               WHEN OTHER
                   COMPUTE XCLM-CONTRIB-YTD ROUNDED = XCLM-PENALTY-YTD *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-CONTRIB-YTD > 0 AND XCLM-CONTRIB-YTD < 5.00
               MOVE 5.00 TO XCLM-CONTRIB-YTD
           END-IF
           IF XCLM-CONTRIB-YTD > 35.00
               MOVE 35.00 TO XCLM-CONTRIB-YTD
           END-IF
           ADD XCLM-CONTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCRUED YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4400-LIMIT-ACCRUED.
           IF XCLM-ACCRUED-YTD < 0.00
               MOVE 0.00 TO XCLM-ACCRUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-ACCRUED-YTD > 2500.00
               MOVE 2500.00 TO XCLM-ACCRUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E991 REJECT CD REQUIRED
      * E093 WEEKLY YTD NEGATIVE
      * E529 WEEKLY YTD OVER 10000
      * E023 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4410-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-REJECT-CD = SPACES
                   MOVE 'E991' TO WS-REJECT-CODE
               WHEN XCLM-WEEKLY-YTD < 0
                   MOVE 'E093' TO WS-REJECT-CODE
               WHEN XCLM-WEEKLY-YTD > 10000.00
                   MOVE 'E529' TO WS-REJECT-CODE
               WHEN XCLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E023' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4420-CHECK-LAST-PAY.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO XCLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E110 ADDR REQUIRED
      * E141 CONTRIB AMT NEGATIVE
      * E949 CONTRIB AMT OVER 99999
      * E753 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4430-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-ADDR = SPACES
                   MOVE 'E110' TO WS-REJECT-CODE
               WHEN XCLM-CONTRIB-AMT < 0
                   MOVE 'E141' TO WS-REJECT-CODE
               WHEN XCLM-CONTRIB-AMT > 99999.00
                   MOVE 'E949' TO WS-REJECT-CODE
               WHEN XCLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E753' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CONTRIB AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4440-FLAG-CONTRIB.
           IF XCLT-CONTRIB-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCLT-CONTRIB-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4450-DESCRIBE-TYPE.
           EVALUATE XCLM-TYPE-CD
               WHEN 'A'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'D'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'E'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4460-AGE-NOTICE.
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
                   ADD XCLT-BONUS-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLT-BONUS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD XCLT-BONUS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLT-BONUS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLT-BONUS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WEEKLY AMT.
      *----------------------------------------------------------------*
       4470-PRORATE-WEEKLY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-WEEKLY-AMT ROUNDED =
                   XCLT-CONTRIB-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLT-WEEKLY-AMT
           END-IF
           ADD XCLT-WEEKLY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * WD -> DP WHEN EFF DATE IS PAST
      * DP -> PO WHEN EFF DATE IS PAST
      * PO -> BN WHEN EFF DATE IS PAST
      * BN -> PN WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4480-SET-TRAN.
           EVALUATE XCLT-TRAN-CD
               WHEN 'WD'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PO' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PO'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BN' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BN'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PN' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE GOAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WEEKLY AMT.
      *----------------------------------------------------------------*
       4490-PRORATE-WEEKLY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-WEEKLY-AMT ROUNDED =
                   XCLM-GOAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-WEEKLY-AMT
           END-IF
           ADD XCLM-WEEKLY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE ACCRUED AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4500-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = XCLM-ACCRUED-AMT *
                       XCLM-RATE / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = XCLM-ACCRUED-AMT *
                       XCLM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GOAL AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4510-ACCRUE-GOAL.
           IF XCLT-GOAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLT-GOAL-AMT * XCLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-PAYOUT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E291 NAME REQUIRED
      * E516 ACCRUED AMT NEGATIVE
      * E639 ACCRUED AMT OVER 1000000
      * E125 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4520-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-NAME = SPACES
                   MOVE 'E291' TO WS-REJECT-CODE
               WHEN XCLM-ACCRUED-AMT < 0
                   MOVE 'E516' TO WS-REJECT-CODE
               WHEN XCLM-ACCRUED-AMT > 1000000.00
                   MOVE 'E639' TO WS-REJECT-CODE
               WHEN XCLM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E125' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4530-COUNT-SEQ.
           IF XCLT-SEQ < 9999999
               ADD 1 TO XCLT-SEQ
           END-IF
           IF XCLT-SEQ NOT < 6
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4540-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4550-AGE-POST.
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
               WHEN WS-DAYS NOT > 15
                   ADD XCLT-BONUS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD XCLT-BONUS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLT-BONUS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLT-BONUS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB FEE ON THE BONUS AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4560-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO XCLM-CONTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   COMPUTE XCLM-CONTRIB-AMT ROUNDED = XCLT-BONUS-AMT *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-TYPE-CD = 'A'
                   MOVE 12.00 TO XCLM-CONTRIB-AMT
               WHEN XCLM-TYPE-CD = 'B'
                   MOVE 3.00 TO XCLM-CONTRIB-AMT
               WHEN OTHER
                   COMPUTE XCLM-CONTRIB-AMT ROUNDED = XCLT-BONUS-AMT *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-CONTRIB-AMT > 0 AND XCLM-CONTRIB-AMT < 1.00
               MOVE 1.00 TO XCLM-CONTRIB-AMT
           END-IF
           IF XCLM-CONTRIB-AMT > 50.00
               MOVE 50.00 TO XCLM-CONTRIB-AMT
           END-IF
           ADD XCLM-CONTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E442 CONTACT REQUIRED
      * E670 WORK AMT 2 NEGATIVE
      * E683 WORK AMT 2 OVER 99999
      * E384 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4570-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-CONTACT = SPACES
                   MOVE 'E442' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E670' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E683' TO WS-REJECT-CODE
               WHEN XCLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E384' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4580-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4590-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4600-LIMIT-PENALTY.
           IF XCLM-PENALTY-AMT < 100.00
               MOVE 100.00 TO XCLM-PENALTY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-PENALTY-AMT > 5000.00
               MOVE 5000.00 TO XCLM-PENALTY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BONUS AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4610-LIMIT-BONUS.
           IF XCLM-BONUS-AMT < 25.00
               MOVE 25.00 TO XCLM-BONUS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-BONUS-AMT > 1000.00
               MOVE 1000.00 TO XCLM-BONUS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AL -> BT WHEN LAST STMT DATE IS PAST
      * BT -> CY WHEN LAST STMT DATE IS PAST
      * CY -> KM WHEN LAST STMT DATE IS PAST
      * KM -> MV WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4620-SET-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BT'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CY' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CY'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KM' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KM'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MV' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4630-CHECK-EXPIRY.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4640-CHECK-EXPIRY.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUED AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4650-FLAG-ACCRUED.
           IF XCLM-ACCRUED-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCLM-ACCRUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4660-COUNT-SEQ.
           IF XCLT-SEQ < 9999999
               ADD 1 TO XCLT-SEQ
           END-IF
           IF XCLT-SEQ NOT < 5
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4670-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-TIER-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-ACCRUED-TIER-LOW(WS-TX)
                   MOVE WS-ACCRUED-TIER-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GOAL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4680-SPLIT-GOAL.
           COMPUTE WS-WORK-AMT = XCLM-GOAL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = XCLM-GOAL-AMT - (WS-WORK-AMT * 3)
           COMPUTE XCLM-CONTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E951 OPERATOR REQUIRED
      * E907 PAYOUT YTD NEGATIVE
      * E607 PAYOUT YTD OVER 25000
      * E494 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4690-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-OPERATOR = SPACES
                   MOVE 'E951' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-YTD < 0
                   MOVE 'E907' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-YTD > 25000.00
                   MOVE 'E607' TO WS-REJECT-CODE
               WHEN XCLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E494' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E807 NAME REQUIRED
      * E686 CONTRIB YTD NEGATIVE
      * E292 CONTRIB YTD OVER 1000000
      * E710 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4700-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-NAME = SPACES
                   MOVE 'E807' TO WS-REJECT-CODE
               WHEN XCLM-CONTRIB-YTD < 0
                   MOVE 'E686' TO WS-REJECT-CODE
               WHEN XCLM-CONTRIB-YTD > 1000000.00
                   MOVE 'E292' TO WS-REJECT-CODE
               WHEN XCLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E710' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GOAL TIER TIER FOR THE CONTRIB YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4710-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-GOAL-TIER-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF XCLM-CONTRIB-YTD NOT < WS-GOAL-TIER-LOW(WS-TX)
                   MOVE WS-GOAL-TIER-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WEEKLY AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4720-LIMIT-WEEKLY.
           IF XCLT-WEEKLY-AMT < 25.00
               MOVE 25.00 TO XCLT-WEEKLY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLT-WEEKLY-AMT > 1000.00
               MOVE 1000.00 TO XCLT-WEEKLY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GOAL YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4730-ACCRUE-GOAL.
           IF XCLM-GOAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-GOAL-YTD * XCLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-ACCRUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4740-AGE-MATURITY.
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
               WHEN WS-DAYS NOT > 45
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4750-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCLM-CONTRIB-YTD / 4
           COMPUTE WS-WORK-AMT-2 = XCLM-CONTRIB-YTD - (WS-WORK-AMT * 4)
           COMPUTE XCLM-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
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
                   MOVE XCTIER-ACCRUED-YTD
                     TO XCLM-ACCRUED-YTD
                   MOVE XCTIER-GOAL-AMT
                     TO XCLM-GOAL-AMT
                   MOVE XCTIER-OVERRIDE-FLAG
                     TO XCLM-OVERRIDE-FLAG
                   MOVE XCTIER-PREFERRED-FLAG
                     TO XCLM-PREFERRED-FLAG
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
                   MOVE XCCAL-WEEKLY-YTD
                     TO XCLM-WEEKLY-YTD
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
                   MOVE XCLIM-GOAL-AMT
                     TO XCLM-GOAL-AMT
                   MOVE XCLIM-WEEKLY-AMT
                     TO XCLM-WEEKLY-AMT
                   MOVE XCLIM-PENALTY-YTD
                     TO XCLM-PENALTY-YTD
                   MOVE XCLIM-SUSPEND-FLAG
                     TO XCLM-SUSPEND-FLAG
                   MOVE XCLIM-STOP-FLAG
                     TO XCLM-STOP-FLAG
                   MOVE XCLIM-PREFERRED-FLAG
                     TO XCLM-PREFERRED-FLAG
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
           DISPLAY 'XCLSTMT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'XCLSTMT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
