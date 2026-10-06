      *================================================================*
      * PROGRAM  : XCLRNEW                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : CHRISTMAS CLUB SAVINGS - RENEWAL. READS THE XCL     *
      *            TRANSACTIONS, UPDATES THE XCL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. HOLIDAY CLUB SAVINGS PLANS WITH *
      *            WEEKLY DEPOSITS AND NOVEMBER PAYOUT                 *
      * INPUT    : XCLMST (KSDS), XCLTRN, BANKCTL                      *
      * OUTPUT   : XCLOUT, XCLRNRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, XCFEE, XCCAL            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-01-06 LMB  ORIGINAL                                       *
      * 1992-02-20 GAF  CONTRIB RERUN PROTECTION REQ2631               *
      * 1993-08-01 SKN  NEW GOAL REPORT COLUMNS INC6801                *
      * 1993-01-27 MKL  NEW MISSED REPORT COLUMNS REQ2817              *
      * 1993-11-28 BHS  AUDIT FINDING - ACCRUED CONTROL TOTALS CHG0340 *
      * 1994-04-02 GAF  ACCRUED LIMIT REVISED INC2058                  *
      * 1997-07-04 BHS  AUDIT FINDING - CONTRIB CONTROL TOTALS REQ1099 *
      * 1999-05-22 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-08-14 RJT  WEEKLY TABLE EXPANDED INC2448                  *
      * 2001-03-08 GAF  CONTRIB RERUN PROTECTION CHG3721               *
      * 2004-09-23 EWS  PERFORMANCE - GOAL READ AHEAD INC1690          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. XCLRNEW.
       AUTHOR. C R PATEL.

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
           SELECT REPORT-FILE ASSIGN TO "XCLRNRPT"
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
                                           VALUE 11.000000.
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
                                           VALUE '00000000000000712000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000483500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000581000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000651000'.
       01  WS-ACCRUED-SCHED-TBL REDEFINES WS-ACCRUED-SCHED-VALUES.
           05  WS-ACCRUED-SCHED-ROW OCCURS 4 TIMES.
               10  WS-ACCRUED-SCHED-LOW    PIC 9(09)V99.
               10  WS-ACCRUED-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-GOAL-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-GOAL-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000651500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000701500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000867500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001350000000530500'.
       01  WS-GOAL-BAND-TBL REDEFINES WS-GOAL-BAND-VALUES.
           05  WS-GOAL-BAND-ROW OCCURS 4 TIMES.
               10  WS-GOAL-BAND-LOW        PIC 9(09)V99.
               10  WS-GOAL-BAND-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ACCRUED-SCHEDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ACCRUED-SCHEDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000791000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000531500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000906500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001550000000898000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001600000000268000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006600000000150000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016600000000138500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017100000000812500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017350000000247000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019850000000409000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022350000000506500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032350000000593000'.
       01  WS-ACCRUED-SCHEDX-TBL REDEFINES WS-ACCRUED-SCHEDX-VALUES.
           05  WS-ACCRUED-SCHEDX-ROW OCCURS 12 TIMES.
               10  WS-ACCRUED-SCHEDX-LOW   PIC 9(09)V99.
               10  WS-ACCRUED-SCHEDX-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PENALTY-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PENALTY-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000709000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000207000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000064000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010750000000729000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010800000000035000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011300000000564000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016300000000119000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026300000000318000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026800000000503000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027800000000624000'.
       01  WS-PENALTY-TIER-TBL REDEFINES WS-PENALTY-TIER-VALUES.
           05  WS-PENALTY-TIER-ROW OCCURS 10 TIMES.
               10  WS-PENALTY-TIER-LOW     PIC 9(09)V99.
               10  WS-PENALTY-TIER-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  XCLRNEW  CHRISTMAS CLUB SAVINGS'.
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
               PERFORM 3000-EDIT-REF-NO
               PERFORM 3010-EDIT-ADDR
               PERFORM 3020-SELECT-DISCOUNT
               PERFORM 3030-EDIT-DESC
               PERFORM 3040-COUNT-SEQ
               PERFORM 3050-AGE-POST
               PERFORM 3060-SELECT-RATE
               PERFORM 3070-SET-CHANNEL
               PERFORM 3080-PRORATE-PENALTY
               PERFORM 3090-SET-TRAN
               PERFORM 3100-AGE-LAST-STMT
               PERFORM 3110-FLAG-WEEKLY
               PERFORM 3120-PRORATE-GOAL
               PERFORM 3130-CHECK-LAST-STMT
               PERFORM 3140-SPLIT-PAYOUT
               PERFORM 3150-ACCRUE-GOAL
               PERFORM 3160-CHECK-LAST-STMT
               PERFORM 3170-ACCUM-CYCLE
               PERFORM 3180-DESCRIBE-REGION
               PERFORM 3190-ASSESS-AUTO-RENEW
               PERFORM 3200-ACCRUE-BONUS
               PERFORM 3210-SELECT-RATE
               PERFORM 3220-ASSESS-PREFERRED
               PERFORM 3230-CHECK-NEXT-DUE
               PERFORM 3240-ASSESS-SUSPEND
               PERFORM 3250-ASSESS-PREFERRED
               PERFORM 3260-AGE-LAST-ACT
               PERFORM 3270-CHECK-LAST-PAY
               PERFORM 3280-SPLIT-ACCRUED
               PERFORM 3290-COUNT-NOTICE
               PERFORM 3300-PRORATE-CONTRIB
               PERFORM 3310-COUNT-PAYMENT
               PERFORM 3320-COUNT-SEQ
               PERFORM 3330-ASSESS-STOP
               PERFORM 3340-CHECK-NEXT-DUE
               PERFORM 3350-AGE-EXPIRY
               PERFORM 3360-SPLIT-MISSED
               PERFORM 3370-PRORATE-WEEKLY
               PERFORM 3380-SET-REASON
               PERFORM 3390-CHECK-LAST-PAY
               PERFORM 3400-DESCRIBE-TRAN
               PERFORM 3410-ASSESS-REVERSAL
               PERFORM 3420-FLAG-WEEKLY
               PERFORM 3430-ACCRUE-BONUS
               PERFORM 3440-ASSESS-REVERSAL
               PERFORM 3450-SPLIT-BONUS
               PERFORM 3460-FLAG-PENALTY
               PERFORM 3470-ASSESS-SUSPEND
               PERFORM 3480-DESCRIBE-PLAN
               PERFORM 3490-PRORATE-CONTRIB
               PERFORM 3500-LIMIT-WEEKLY
               PERFORM 3510-SELECT-FEE-PCT
               PERFORM 3520-EDIT-ADDR
               PERFORM 3530-EDIT-NAME
               PERFORM 3540-SPLIT-CONTRIB
               PERFORM 3550-SET-TRAN
               PERFORM 3560-PRORATE-MISSED
               PERFORM 3570-SET-PLAN
               PERFORM 3580-ASSESS-STOP
               PERFORM 3590-DESCRIBE-TYPE
               PERFORM 3600-AGE-LAST-STMT
               PERFORM 3610-DESCRIBE-REASON
               PERFORM 3620-ACCRUE-GOAL
               PERFORM 3630-AGE-EFF
               PERFORM 3640-ACCUM-REGION
               PERFORM 3650-CHECK-EXPIRY
               PERFORM 3660-SET-PLAN
               PERFORM 3670-COUNT-CYCLE
               PERFORM 3680-EDIT-MEMO
               PERFORM 3690-ASSESS-STOP
               PERFORM 3700-PRORATE-PENALTY
               PERFORM 3710-ACCRUE-PENALTY
               PERFORM 3720-SPLIT-ACCRUED
               PERFORM 3730-SPLIT-PENALTY
               PERFORM 3740-COUNT-SEQ
               PERFORM 3750-SET-REGION
               PERFORM 3760-PRORATE-BONUS
               PERFORM 3770-SELECT-RATE
               PERFORM 3780-ASSESS-AUTO-RENEW
               PERFORM 3790-ACCRUE-CONTRIB
               PERFORM 3800-ACCUM-PLAN
               PERFORM 3810-SELECT-FEE-PCT
               PERFORM 3820-PRORATE-PAYOUT
               PERFORM 3830-FLAG-ACCRUED
               PERFORM 3840-COUNT-NOTICE
               PERFORM 3850-ACCUM-TYPE
               PERFORM 3860-PRORATE-ACCRUED
               PERFORM 3870-DESCRIBE-REASON
               PERFORM 3880-AGE-NOTICE
               PERFORM 3890-AGE-OPEN
               PERFORM 3900-COUNT-RETRY
               PERFORM 3910-CHECK-LAST-PAY
               PERFORM 3920-CHECK-EFF
               PERFORM 3930-SPLIT-PENALTY
               PERFORM 3940-PRORATE-CONTRIB
               PERFORM 3950-DESCRIBE-TRAN
               PERFORM 3960-SELECT-RATE
               PERFORM 3970-SELECT-FEE-PCT
               PERFORM 3980-EDIT-MEMO
               PERFORM 3990-FLAG-ACCRUED
               PERFORM 4000-PRORATE-BONUS
               PERFORM 4010-SELECT-DISCOUNT
               PERFORM 4020-ACCRUE-GOAL
               PERFORM 4030-LIMIT-BONUS
               PERFORM 4040-PRORATE-PENALTY
               PERFORM 4050-SET-REGION
               PERFORM 4060-DESCRIBE-REASON
               PERFORM 4070-ACCUM-REASON
               PERFORM 4080-AGE-POST
               PERFORM 4090-SPLIT-CONTRIB
               PERFORM 4100-PRORATE-CONTRIB
               PERFORM 4110-AGE-EXPIRY
               PERFORM 4120-SET-CYCLE
               PERFORM 4130-FLAG-ITEM
               PERFORM 4140-COUNT-SEQ
               PERFORM 4150-EDIT-NAME
               PERFORM 4160-FLAG-WEEKLY
               PERFORM 4170-SELECT-FEE-PCT
               PERFORM 4180-PRORATE-GOAL
               PERFORM 4190-ASSESS-OVERRIDE
               PERFORM 4200-ACCRUE-GOAL
               PERFORM 4210-FLAG-MISSED
               PERFORM 4220-ASSESS-AUTO-RENEW
               PERFORM 4230-EDIT-CONTACT
               PERFORM 4240-SELECT-RATE
               PERFORM 4250-EDIT-CONTACT
               PERFORM 4260-DESCRIBE-TRAN
               PERFORM 4270-SELECT-FEE-PCT
               PERFORM 4280-SPLIT-WEEKLY
               PERFORM 4290-COUNT-RETRY
               PERFORM 4300-EDIT-MEMO
               PERFORM 4310-EDIT-REF-NO
               PERFORM 4320-SELECT-RATE
               PERFORM 4330-LIMIT-ITEM
               PERFORM 4340-ASSESS-SUSPEND
               PERFORM 4350-DESCRIBE-REGION
               PERFORM 4360-ASSESS-PREFERRED
               PERFORM 4370-COUNT-CYCLE
               PERFORM 4380-ACCRUE-WEEKLY
               PERFORM 4390-PRORATE-PENALTY
               PERFORM 4400-SPLIT-CONTRIB
               PERFORM 4410-FLAG-PENALTY
               PERFORM 4420-PRORATE-CONTRIB
               PERFORM 4430-AGE-LAST-ACT
               PERFORM 4440-ACCRUE-MISSED
               PERFORM 4450-AGE-NEXT-DUE
               PERFORM 4460-FLAG-ITEM
               PERFORM 4470-LIMIT-PENALTY
               PERFORM 4480-ACCRUE-PAYOUT
               PERFORM 4490-SELECT-FEE-PCT
               PERFORM 4500-ACCRUE-BONUS
               PERFORM 4510-EDIT-NAME
               PERFORM 4520-SPLIT-ACCRUED
               PERFORM 4530-COUNT-NOTICE
               PERFORM 4540-SELECT-RATE
               PERFORM 4550-SELECT-DISCOUNT
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E524 REF NO REQUIRED
      * E648 WEEKLY YTD NEGATIVE
      * E687 WEEKLY YTD OVER 99999
      * E318 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3000-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-REF-NO = SPACES
                   MOVE 'E524' TO WS-REJECT-CODE
               WHEN XCLM-WEEKLY-YTD < 0
                   MOVE 'E648' TO WS-REJECT-CODE
               WHEN XCLM-WEEKLY-YTD > 99999.00
                   MOVE 'E687' TO WS-REJECT-CODE
               WHEN XCLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E318' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E266 ADDR REQUIRED
      * E489 PENALTY AMT NEGATIVE
      * E394 PENALTY AMT OVER 99999
      * E790 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3010-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-ADDR = SPACES
                   MOVE 'E266' TO WS-REJECT-CODE
               WHEN XCLM-PENALTY-AMT < 0
                   MOVE 'E489' TO WS-REJECT-CODE
               WHEN XCLM-PENALTY-AMT > 99999.00
                   MOVE 'E394' TO WS-REJECT-CODE
               WHEN XCLM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E790' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED SCHED TIER FOR THE GOAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3020-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-SCHED-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCLM-GOAL-YTD NOT < WS-ACCRUED-SCHED-LOW(WS-TX)
                   MOVE WS-ACCRUED-SCHED-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E128 DESC REQUIRED
      * E447 PAYOUT YTD NEGATIVE
      * E335 PAYOUT YTD OVER 5000
      * E069 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3030-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-DESC = SPACES
                   MOVE 'E128' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-YTD < 0
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-YTD > 5000.00
                   MOVE 'E335' TO WS-REJECT-CODE
               WHEN XCLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E069' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-SEQ.
           IF XCLT-SEQ < 9999999
               ADD 1 TO XCLT-SEQ
           END-IF
           IF XCLT-SEQ NOT < 6
               MOVE 'Y' TO XCLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3050-AGE-POST.
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
               WHEN WS-DAYS NOT > 45
                   ADD XCLM-PAYOUT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-PAYOUT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-PAYOUT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-PAYOUT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED SCHED TIER FOR THE PAYOUT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3060-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-SCHED-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCLM-PAYOUT-AMT NOT < WS-ACCRUED-SCHED-LOW(WS-TX)
                   MOVE WS-ACCRUED-SCHED-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3070-SET-CHANNEL.
           EVALUATE XCLT-CHANNEL-CD
               WHEN 'BRN'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PENALTY AMT.
      *----------------------------------------------------------------*
       3080-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-PENALTY-AMT ROUNDED =
                   XCLM-CONTRIB-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-PENALTY-AMT
           END-IF
           ADD XCLM-PENALTY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * WD -> DP WHEN NEXT DUE DATE IS PAST
      * DP -> PO WHEN NEXT DUE DATE IS PAST
      * PO -> BN WHEN NEXT DUE DATE IS PAST
      * BN -> PN WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3090-SET-TRAN.
           EVALUATE XCLT-TRAN-CD
               WHEN 'WD'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PO' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PO'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BN' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BN'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PN' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3100-AGE-LAST-STMT.
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
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-WEEKLY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-WEEKLY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-WEEKLY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WEEKLY YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3110-FLAG-WEEKLY.
           IF XCLM-WEEKLY-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCLM-WEEKLY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ACCRUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO GOAL AMT.
      *----------------------------------------------------------------*
       3120-PRORATE-GOAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-GOAL-AMT ROUNDED =
                   XCLM-ACCRUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLT-GOAL-AMT
           END-IF
           ADD XCLT-GOAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-LAST-STMT.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO XCLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYOUT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3140-SPLIT-PAYOUT.
           COMPUTE WS-WORK-AMT = XCLM-PAYOUT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = XCLM-PAYOUT-AMT - (WS-WORK-AMT * 12)
           COMPUTE XCLM-BONUS-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GOAL AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3150-ACCRUE-GOAL.
           IF XCLM-GOAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-GOAL-AMT * XCLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-PAYOUT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3160-CHECK-LAST-STMT.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO XCLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTRIB AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-CYCLE-CD
                   ADD XCLM-CONTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-CONTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3180-DESCRIBE-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WEEKLY YTD FEE ON THE CONTRIB AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3190-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO XCLM-WEEKLY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   COMPUTE XCLM-WEEKLY-YTD ROUNDED = XCLT-CONTRIB-AMT *
                       XCLM-RATE / 200
               WHEN XCLM-PAYOUT-METHOD-CD = 'B'
                   MOVE 5.00 TO XCLM-WEEKLY-YTD
               WHEN XCLM-PAYOUT-METHOD-CD = 'H'
                   MOVE 20.00 TO XCLM-WEEKLY-YTD
               WHEN OTHER
                   COMPUTE XCLM-WEEKLY-YTD ROUNDED = XCLT-CONTRIB-AMT *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-WEEKLY-YTD > 0 AND XCLM-WEEKLY-YTD < 2.00
               MOVE 2.00 TO XCLM-WEEKLY-YTD
           END-IF
           IF XCLM-WEEKLY-YTD > 25.00
               MOVE 25.00 TO XCLM-WEEKLY-YTD
           END-IF
           ADD XCLM-WEEKLY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BONUS AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3200-ACCRUE-BONUS.
           IF XCLM-BONUS-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-BONUS-AMT * XCLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-WEEKLY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GOAL BAND TIER FOR THE CONTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3210-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-GOAL-BAND-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCLM-CONTRIB-AMT NOT < WS-GOAL-BAND-LOW(WS-TX)
                   MOVE WS-GOAL-BAND-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB FEE ON THE GOAL YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3220-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO XCLM-CONTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE XCLM-CONTRIB-AMT ROUNDED = XCLM-GOAL-YTD *
                       XCLM-DISCOUNT / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 5.00 TO XCLM-CONTRIB-AMT
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 15.00 TO XCLM-CONTRIB-AMT
               WHEN OTHER
                   COMPUTE XCLM-CONTRIB-AMT ROUNDED = XCLM-GOAL-YTD *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-CONTRIB-AMT > 0 AND XCLM-CONTRIB-AMT < 1.00
               MOVE 1.00 TO XCLM-CONTRIB-AMT
           END-IF
           IF XCLM-CONTRIB-AMT > 75.00
               MOVE 75.00 TO XCLM-CONTRIB-AMT
           END-IF
           ADD XCLM-CONTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3230-CHECK-NEXT-DUE.
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
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUED FEE ON THE BONUS YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3240-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO XCLM-ACCRUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE XCLM-ACCRUED-AMT ROUNDED = XCLM-BONUS-YTD *
                       XCLM-RATE / 200
               WHEN XCLM-TYPE-CD = 'A'
                   MOVE 5.00 TO XCLM-ACCRUED-AMT
               WHEN XCLM-TYPE-CD = 'B'
                   MOVE 5.00 TO XCLM-ACCRUED-AMT
               WHEN OTHER
                   COMPUTE XCLM-ACCRUED-AMT ROUNDED = XCLM-BONUS-YTD *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-ACCRUED-AMT > 0 AND XCLM-ACCRUED-AMT < 5.00
               MOVE 5.00 TO XCLM-ACCRUED-AMT
           END-IF
           IF XCLM-ACCRUED-AMT > 75.00
               MOVE 75.00 TO XCLM-ACCRUED-AMT
           END-IF
           ADD XCLM-ACCRUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE BONUS FEE ON THE BONUS AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3250-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO XCLM-BONUS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-STOP-FLAG = 'Y'
                   COMPUTE XCLM-BONUS-AMT ROUNDED = XCLT-BONUS-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-PAYOUT-METHOD-CD = 'B'
                   MOVE 20.00 TO XCLM-BONUS-AMT
               WHEN XCLM-PAYOUT-METHOD-CD = 'H'
                   MOVE 10.00 TO XCLM-BONUS-AMT
               WHEN OTHER
                   COMPUTE XCLM-BONUS-AMT ROUNDED = XCLT-BONUS-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-BONUS-AMT > 0 AND XCLM-BONUS-AMT < 2.00
               MOVE 2.00 TO XCLM-BONUS-AMT
           END-IF
           IF XCLM-BONUS-AMT > 150.00
               MOVE 150.00 TO XCLM-BONUS-AMT
           END-IF
           ADD XCLM-BONUS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3260-AGE-LAST-ACT.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3270-CHECK-LAST-PAY.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ACCRUED YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3280-SPLIT-ACCRUED.
           COMPUTE WS-WORK-AMT = XCLM-ACCRUED-YTD / 12
           COMPUTE WS-WORK-AMT-2 = XCLM-ACCRUED-YTD - (WS-WORK-AMT * 12)
           COMPUTE XCLM-WEEKLY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3290-COUNT-NOTICE.
           IF XCLM-NOTICE-CNT < 9999999
               ADD 1 TO XCLM-NOTICE-CNT
           END-IF
           IF XCLM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BONUS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       3300-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-CONTRIB-AMT ROUNDED =
                   XCLM-BONUS-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLT-CONTRIB-AMT
           END-IF
           ADD XCLT-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3310-COUNT-PAYMENT.
           IF XCLM-PAYMENT-CNT < 9999999
               ADD 1 TO XCLM-PAYMENT-CNT
           END-IF
           IF XCLM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3320-COUNT-SEQ.
           IF XCLT-SEQ < 9999999
               ADD 1 TO XCLT-SEQ
           END-IF
           IF XCLT-SEQ NOT < 3
               MOVE 'Y' TO XCLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MISSED FEE ON THE ACCRUED AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3330-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCLM-MISSED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE XCLM-MISSED-AMT ROUNDED = XCLM-ACCRUED-AMT *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-CYCLE-CD = 'EM'
                   MOVE 7.00 TO XCLM-MISSED-AMT
               WHEN XCLM-CYCLE-CD = 'FE'
                   MOVE 5.00 TO XCLM-MISSED-AMT
               WHEN OTHER
                   COMPUTE XCLM-MISSED-AMT ROUNDED = XCLM-ACCRUED-AMT *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-MISSED-AMT > 0 AND XCLM-MISSED-AMT < 1.00
               MOVE 1.00 TO XCLM-MISSED-AMT
           END-IF
           IF XCLM-MISSED-AMT > 100.00
               MOVE 100.00 TO XCLM-MISSED-AMT
           END-IF
           ADD XCLM-MISSED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3350-AGE-EXPIRY.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT MISSED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3360-SPLIT-MISSED.
           COMPUTE WS-WORK-AMT = XCLM-MISSED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = XCLM-MISSED-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PENALTY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WEEKLY AMT.
      *----------------------------------------------------------------*
       3370-PRORATE-WEEKLY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-WEEKLY-AMT ROUNDED =
                   XCLM-PENALTY-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLT-WEEKLY-AMT
           END-IF
           ADD XCLT-WEEKLY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AL -> BT WHEN POST DATE IS PAST
      * BT -> CY WHEN POST DATE IS PAST
      * CY -> KM WHEN POST DATE IS PAST
      * KM -> MV WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3380-SET-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BT'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CY' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CY'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KM' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KM'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MV' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3390-CHECK-LAST-PAY.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO XCLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3400-DESCRIBE-TRAN.
           EVALUATE XCLT-TRAN-CD
               WHEN 'WD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PO'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYOUT YTD FEE ON THE PENALTY YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3410-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO XCLM-PAYOUT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-STOP-FLAG = 'Y'
                   COMPUTE XCLM-PAYOUT-YTD ROUNDED = XCLM-PENALTY-YTD *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-REGION-CD = 'FLM'
                   MOVE 15.00 TO XCLM-PAYOUT-YTD
               WHEN XCLM-REGION-CD = 'HLS'
                   MOVE 15.00 TO XCLM-PAYOUT-YTD
               WHEN OTHER
                   COMPUTE XCLM-PAYOUT-YTD ROUNDED = XCLM-PENALTY-YTD *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-PAYOUT-YTD > 0 AND XCLM-PAYOUT-YTD < 1.00
               MOVE 1.00 TO XCLM-PAYOUT-YTD
           END-IF
           IF XCLM-PAYOUT-YTD > 100.00
               MOVE 100.00 TO XCLM-PAYOUT-YTD
           END-IF
           ADD XCLM-PAYOUT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WEEKLY AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3420-FLAG-WEEKLY.
           IF XCLT-WEEKLY-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLT-WEEKLY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BONUS YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3430-ACCRUE-BONUS.
           IF XCLM-BONUS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-BONUS-YTD * XCLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-ACCRUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE CONTRIB YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3440-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = XCLM-CONTRIB-YTD *
                       XCLM-RATE / 200
               WHEN XCLM-PLAN-CD = 'GN'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN XCLM-PLAN-CD = 'JT'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = XCLM-CONTRIB-YTD *
                       XCLM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT BONUS YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3450-SPLIT-BONUS.
           COMPUTE WS-WORK-AMT = XCLM-BONUS-YTD / 2
           COMPUTE WS-WORK-AMT-2 = XCLM-BONUS-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3460-FLAG-PENALTY.
           IF XCLM-PENALTY-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLM-PENALTY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY FEE ON THE GOAL AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3470-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO XCLM-PENALTY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   COMPUTE XCLM-PENALTY-AMT ROUNDED = XCLT-GOAL-AMT *
                       XCLM-RATE / 200
               WHEN XCLM-PAYOUT-METHOD-CD = 'B'
                   MOVE 3.00 TO XCLM-PENALTY-AMT
               WHEN XCLM-PAYOUT-METHOD-CD = 'H'
                   MOVE 20.00 TO XCLM-PENALTY-AMT
               WHEN OTHER
                   COMPUTE XCLM-PENALTY-AMT ROUNDED = XCLT-GOAL-AMT *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-PENALTY-AMT > 0 AND XCLM-PENALTY-AMT < 10.00
               MOVE 10.00 TO XCLM-PENALTY-AMT
           END-IF
           IF XCLM-PENALTY-AMT > 35.00
               MOVE 35.00 TO XCLM-PENALTY-AMT
           END-IF
           ADD XCLM-PENALTY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3480-DESCRIBE-PLAN.
           EVALUATE XCLM-PLAN-CD
               WHEN 'GN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ACCRUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       3490-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-CONTRIB-AMT ROUNDED =
                   XCLM-ACCRUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLT-CONTRIB-AMT
           END-IF
           ADD XCLT-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WEEKLY AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3500-LIMIT-WEEKLY.
           IF XCLM-WEEKLY-AMT < 10.00
               MOVE 10.00 TO XCLM-WEEKLY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-WEEKLY-AMT > 500.00
               MOVE 500.00 TO XCLM-WEEKLY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE WEEKLY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3510-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF XCLT-WEEKLY-AMT NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E396 ADDR REQUIRED
      * E460 WEEKLY YTD NEGATIVE
      * E827 WEEKLY YTD OVER 25000
      * E678 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3520-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-ADDR = SPACES
                   MOVE 'E396' TO WS-REJECT-CODE
               WHEN XCLM-WEEKLY-YTD < 0
                   MOVE 'E460' TO WS-REJECT-CODE
               WHEN XCLM-WEEKLY-YTD > 25000.00
                   MOVE 'E827' TO WS-REJECT-CODE
               WHEN XCLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E678' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E650 NAME REQUIRED
      * E891 CONTRIB AMT NEGATIVE
      * E654 CONTRIB AMT OVER 1000000
      * E774 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3530-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-NAME = SPACES
                   MOVE 'E650' TO WS-REJECT-CODE
               WHEN XCLT-CONTRIB-AMT < 0
                   MOVE 'E891' TO WS-REJECT-CODE
               WHEN XCLT-CONTRIB-AMT > 1000000.00
                   MOVE 'E654' TO WS-REJECT-CODE
               WHEN XCLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E774' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3540-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCLM-CONTRIB-AMT / 4
           COMPUTE WS-WORK-AMT-2 = XCLM-CONTRIB-AMT - (WS-WORK-AMT * 4)
           COMPUTE XCLT-GOAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * WD -> DP WHEN POST DATE IS PAST
      * DP -> PO WHEN POST DATE IS PAST
      * PO -> BN WHEN POST DATE IS PAST
      * BN -> PN WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3550-SET-TRAN.
           EVALUATE XCLT-TRAN-CD
               WHEN 'WD'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PO' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PO'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BN' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BN'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PN' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MISSED AMT.
      *----------------------------------------------------------------*
       3560-PRORATE-MISSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-MISSED-AMT ROUNDED =
                   XCLM-CONTRIB-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-MISSED-AMT
           END-IF
           ADD XCLM-MISSED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN CD:
      * GN -> JT WHEN NEXT DUE DATE IS PAST
      * JT -> KC WHEN NEXT DUE DATE IS PAST
      * KC -> KW WHEN NEXT DUE DATE IS PAST
      * KW -> MX WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3570-SET-PLAN.
           EVALUATE XCLM-PLAN-CD
               WHEN 'GN'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JT'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KC' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KC'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MX' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE GOAL AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3580-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = XCLT-GOAL-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-PAYOUT-METHOD-CD = 'B'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN XCLM-PAYOUT-METHOD-CD = 'H'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = XCLT-GOAL-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3590-DESCRIBE-TYPE.
           EVALUATE XCLM-TYPE-CD
               WHEN 'A'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'D'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'E'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'W'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3600-AGE-LAST-STMT.
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
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3610-DESCRIBE-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GOAL YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3620-ACCRUE-GOAL.
           IF XCLM-GOAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-GOAL-YTD * XCLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-ACCRUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3630-AGE-EFF.
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
                   ADD XCLM-CONTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD XCLM-CONTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-CONTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-CONTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-CONTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WEEKLY AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3640-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-REGION-CD
                   ADD XCLM-WEEKLY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-WEEKLY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3650-CHECK-EXPIRY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO XCLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN CD:
      * GN -> JT WHEN POST DATE IS PAST
      * JT -> KC WHEN POST DATE IS PAST
      * KC -> KW WHEN POST DATE IS PAST
      * KW -> MX WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3660-SET-PLAN.
           EVALUATE XCLM-PLAN-CD
               WHEN 'GN'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JT'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KC' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KC'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MX' TO XCLM-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO XCLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E413 MEMO REQUIRED
      * E558 CONTRIB YTD NEGATIVE
      * E145 CONTRIB YTD OVER 1000000
      * E246 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3680-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-MEMO = SPACES
                   MOVE 'E413' TO WS-REJECT-CODE
               WHEN XCLM-CONTRIB-YTD < 0
                   MOVE 'E558' TO WS-REJECT-CODE
               WHEN XCLM-CONTRIB-YTD > 1000000.00
                   MOVE 'E145' TO WS-REJECT-CODE
               WHEN XCLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E246' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3690-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCLM-CONTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE XCLM-CONTRIB-YTD ROUNDED = WS-WORK-AMT-2 *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 12.00 TO XCLM-CONTRIB-YTD
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 10.00 TO XCLM-CONTRIB-YTD
               WHEN OTHER
                   COMPUTE XCLM-CONTRIB-YTD ROUNDED = WS-WORK-AMT-2 *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-CONTRIB-YTD > 0 AND XCLM-CONTRIB-YTD < 2.00
               MOVE 2.00 TO XCLM-CONTRIB-YTD
           END-IF
           IF XCLM-CONTRIB-YTD > 35.00
               MOVE 35.00 TO XCLM-CONTRIB-YTD
           END-IF
           ADD XCLM-CONTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE GOAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PENALTY YTD.
      *----------------------------------------------------------------*
       3700-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-PENALTY-YTD ROUNDED =
                   XCLT-GOAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-PENALTY-YTD
           END-IF
           ADD XCLM-PENALTY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PENALTY AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3710-ACCRUE-PENALTY.
           IF XCLM-PENALTY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-PENALTY-AMT * XCLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-WEEKLY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ACCRUED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3720-SPLIT-ACCRUED.
           COMPUTE WS-WORK-AMT = XCLM-ACCRUED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = XCLM-ACCRUED-AMT - (WS-WORK-AMT * 4)
           COMPUTE XCLM-PAYOUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PENALTY AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3730-SPLIT-PENALTY.
           COMPUTE WS-WORK-AMT = XCLM-PENALTY-AMT / 3
           COMPUTE WS-WORK-AMT-2 = XCLM-PENALTY-AMT - (WS-WORK-AMT * 3)
           COMPUTE XCLM-GOAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3740-COUNT-SEQ.
           IF XCLT-SEQ < 9999999
               ADD 1 TO XCLT-SEQ
           END-IF
           IF XCLT-SEQ NOT < 5
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * FLM -> HLS WHEN NEXT REVIEW DATE IS PAST
      * HLS -> KMJ WHEN NEXT REVIEW DATE IS PAST
      * KMJ -> PDA WHEN NEXT REVIEW DATE IS PAST
      * PDA -> RVG WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-REGION.
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
      * PRORATE WEEKLY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BONUS AMT.
      *----------------------------------------------------------------*
       3760-PRORATE-BONUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-BONUS-AMT ROUNDED =
                   XCLT-WEEKLY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-BONUS-AMT
           END-IF
           ADD XCLM-BONUS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED SCHED TIER FOR THE PENALTY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3770-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-SCHED-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCLM-PENALTY-AMT NOT < WS-ACCRUED-SCHED-LOW(WS-TX)
                   MOVE WS-ACCRUED-SCHED-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE BONUS AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3780-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = XCLT-BONUS-AMT *
                       XCLM-FEE-PCT / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = XCLT-BONUS-AMT *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3790-ACCRUE-CONTRIB.
           IF XCLM-CONTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-CONTRIB-YTD * XCLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-PENALTY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BONUS YTD BY PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3800-ACCUM-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-PLAN-CD
                   ADD XCLM-BONUS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-BONUS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED SCHED TIER FOR THE WEEKLY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3810-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-SCHED-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCLM-WEEKLY-AMT NOT < WS-ACCRUED-SCHED-LOW(WS-TX)
                   MOVE WS-ACCRUED-SCHED-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYOUT YTD.
      *----------------------------------------------------------------*
       3820-PRORATE-PAYOUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-PAYOUT-YTD ROUNDED =
                   XCLM-CONTRIB-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-PAYOUT-YTD
           END-IF
           ADD XCLM-PAYOUT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCRUED YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3830-FLAG-ACCRUED.
           IF XCLM-ACCRUED-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCLM-ACCRUED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3840-COUNT-NOTICE.
           IF XCLM-NOTICE-CNT < 9999999
               ADD 1 TO XCLM-NOTICE-CNT
           END-IF
           IF XCLM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MISSED AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3850-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-TYPE-CD
                   ADD XCLM-MISSED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-MISSED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BONUS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ACCRUED YTD.
      *----------------------------------------------------------------*
       3860-PRORATE-ACCRUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-ACCRUED-YTD ROUNDED =
                   XCLM-BONUS-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-ACCRUED-YTD
           END-IF
           ADD XCLM-ACCRUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3870-DESCRIBE-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3880-AGE-NOTICE.
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
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3890-AGE-OPEN.
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
                   ADD XCLM-PAYOUT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD XCLM-PAYOUT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-PAYOUT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-PAYOUT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-PAYOUT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3900-COUNT-RETRY.
           IF XCLM-RETRY-CNT < 9999999
               ADD 1 TO XCLM-RETRY-CNT
           END-IF
           IF XCLM-RETRY-CNT NOT < 5
               MOVE 'Y' TO XCLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3910-CHECK-LAST-PAY.
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
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3920-CHECK-EFF.
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
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3930-SPLIT-PENALTY.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE XCLM-PENALTY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PENALTY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       3940-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-CONTRIB-AMT ROUNDED =
                   XCLM-PENALTY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLT-CONTRIB-AMT
           END-IF
           ADD XCLT-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3950-DESCRIBE-TRAN.
           EVALUATE XCLT-TRAN-CD
               WHEN 'WD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PO'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE CONTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3960-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
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
      * SELECT THE GOAL BAND TIER FOR THE GOAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GOAL-BAND-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCLT-GOAL-AMT NOT < WS-GOAL-BAND-LOW(WS-TX)
                   MOVE WS-GOAL-BAND-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E290 MEMO REQUIRED
      * E081 PENALTY AMT NEGATIVE
      * E835 PENALTY AMT OVER 10000
      * E175 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3980-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-MEMO = SPACES
                   MOVE 'E290' TO WS-REJECT-CODE
               WHEN XCLM-PENALTY-AMT < 0
                   MOVE 'E081' TO WS-REJECT-CODE
               WHEN XCLM-PENALTY-AMT > 10000.00
                   MOVE 'E835' TO WS-REJECT-CODE
               WHEN XCLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E175' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUED YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3990-FLAG-ACCRUED.
           IF XCLM-ACCRUED-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLM-ACCRUED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BONUS AMT.
      *----------------------------------------------------------------*
       4000-PRORATE-BONUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-BONUS-AMT ROUNDED =
                   XCLM-CONTRIB-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-BONUS-AMT
           END-IF
           ADD XCLM-BONUS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED SCHED TIER FOR THE BONUS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4010-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-SCHED-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCLM-BONUS-YTD NOT < WS-ACCRUED-SCHED-LOW(WS-TX)
                   MOVE WS-ACCRUED-SCHED-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GOAL AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4020-ACCRUE-GOAL.
           IF XCLM-GOAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-GOAL-AMT * XCLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-CONTRIB-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BONUS AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4030-LIMIT-BONUS.
           IF XCLM-BONUS-AMT < 25.00
               MOVE 25.00 TO XCLM-BONUS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-BONUS-AMT > 1000.00
               MOVE 1000.00 TO XCLM-BONUS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GOAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PENALTY AMT.
      *----------------------------------------------------------------*
       4040-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-PENALTY-AMT ROUNDED =
                   XCLT-GOAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-PENALTY-AMT
           END-IF
           ADD XCLM-PENALTY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * FLM -> HLS WHEN EXPIRY DATE IS PAST
      * HLS -> KMJ WHEN EXPIRY DATE IS PAST
      * KMJ -> PDA WHEN EXPIRY DATE IS PAST
      * PDA -> RVG WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4050-SET-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HLS' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HLS'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KMJ' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KMJ'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PDA' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PDA'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RVG' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4060-DESCRIBE-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'CY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE BONUS YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-REASON.
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
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4080-AGE-POST.
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
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4090-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE XCLM-CONTRIB-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PENALTY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       4100-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-CONTRIB-AMT ROUNDED =
                   XCLM-PENALTY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLT-CONTRIB-AMT
           END-IF
           ADD XCLT-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4110-AGE-EXPIRY.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EM -> FE WHEN POST DATE IS PAST
      * FE -> PL WHEN POST DATE IS PAST
      * PL -> RU WHEN POST DATE IS PAST
      * RU -> SU WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4120-SET-CYCLE.
           EVALUATE XCLM-CYCLE-CD
               WHEN 'EM'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FE' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FE'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PL' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RU' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF XCLT-POST-DATE > 0 AND XCLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SU' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4130-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4140-COUNT-SEQ.
           IF XCLT-SEQ < 9999999
               ADD 1 TO XCLT-SEQ
           END-IF
           IF XCLT-SEQ NOT < 5
               MOVE 'Y' TO XCLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E470 NAME REQUIRED
      * E698 WEEKLY YTD NEGATIVE
      * E728 WEEKLY YTD OVER 25000
      * E294 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4150-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-NAME = SPACES
                   MOVE 'E470' TO WS-REJECT-CODE
               WHEN XCLM-WEEKLY-YTD < 0
                   MOVE 'E698' TO WS-REJECT-CODE
               WHEN XCLM-WEEKLY-YTD > 25000.00
                   MOVE 'E728' TO WS-REJECT-CODE
               WHEN XCLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E294' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WEEKLY YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4160-FLAG-WEEKLY.
           IF XCLM-WEEKLY-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLM-WEEKLY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE BONUS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4170-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF XCLT-BONUS-AMT NOT < WS-PENALTY-TIER-LOW(WS-TX)
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
       4180-PRORATE-GOAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-GOAL-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLT-GOAL-AMT
           END-IF
           ADD XCLT-GOAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE BONUS YTD FEE ON THE PENALTY AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4190-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO XCLM-BONUS-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-STOP-FLAG = 'Y'
                   COMPUTE XCLM-BONUS-YTD ROUNDED = XCLM-PENALTY-AMT *
                       XCLM-FEE-PCT / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 10.00 TO XCLM-BONUS-YTD
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 20.00 TO XCLM-BONUS-YTD
               WHEN OTHER
                   COMPUTE XCLM-BONUS-YTD ROUNDED = XCLM-PENALTY-AMT *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-BONUS-YTD > 0 AND XCLM-BONUS-YTD < 2.00
               MOVE 2.00 TO XCLM-BONUS-YTD
           END-IF
           IF XCLM-BONUS-YTD > 50.00
               MOVE 50.00 TO XCLM-BONUS-YTD
           END-IF
           ADD XCLM-BONUS-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GOAL AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4200-ACCRUE-GOAL.
           IF XCLT-GOAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLT-GOAL-AMT * XCLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-PENALTY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MISSED YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4210-FLAG-MISSED.
           IF XCLM-MISSED-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-MISSED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYOUT YTD FEE ON THE PENALTY AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4220-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO XCLM-PAYOUT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   COMPUTE XCLM-PAYOUT-YTD ROUNDED = XCLM-PENALTY-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-PAYOUT-METHOD-CD = 'B'
                   MOVE 10.00 TO XCLM-PAYOUT-YTD
               WHEN XCLM-PAYOUT-METHOD-CD = 'H'
                   MOVE 10.00 TO XCLM-PAYOUT-YTD
               WHEN OTHER
                   COMPUTE XCLM-PAYOUT-YTD ROUNDED = XCLM-PENALTY-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-PAYOUT-YTD > 0 AND XCLM-PAYOUT-YTD < 2.00
               MOVE 2.00 TO XCLM-PAYOUT-YTD
           END-IF
           IF XCLM-PAYOUT-YTD > 100.00
               MOVE 100.00 TO XCLM-PAYOUT-YTD
           END-IF
           ADD XCLM-PAYOUT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E407 CONTACT REQUIRED
      * E677 WEEKLY YTD NEGATIVE
      * E033 WEEKLY YTD OVER 5000
      * E734 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4230-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-CONTACT = SPACES
                   MOVE 'E407' TO WS-REJECT-CODE
               WHEN XCLM-WEEKLY-YTD < 0
                   MOVE 'E677' TO WS-REJECT-CODE
               WHEN XCLM-WEEKLY-YTD > 5000.00
                   MOVE 'E033' TO WS-REJECT-CODE
               WHEN XCLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E734' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE BONUS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4240-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF XCLM-BONUS-YTD NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E253 CONTACT REQUIRED
      * E412 PENALTY AMT NEGATIVE
      * E734 PENALTY AMT OVER 10000
      * E724 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4250-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-CONTACT = SPACES
                   MOVE 'E253' TO WS-REJECT-CODE
               WHEN XCLM-PENALTY-AMT < 0
                   MOVE 'E412' TO WS-REJECT-CODE
               WHEN XCLM-PENALTY-AMT > 10000.00
                   MOVE 'E734' TO WS-REJECT-CODE
               WHEN XCLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E724' TO WS-REJECT-CODE
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
       4260-DESCRIBE-TRAN.
           EVALUATE XCLT-TRAN-CD
               WHEN 'WD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PO'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE ACCRUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4270-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF XCLM-ACCRUED-YTD NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WEEKLY YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4280-SPLIT-WEEKLY.
           COMPUTE WS-WORK-AMT = XCLM-WEEKLY-YTD / 4
           COMPUTE WS-WORK-AMT-2 = XCLM-WEEKLY-YTD - (WS-WORK-AMT * 4)
           COMPUTE XCLM-PENALTY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4290-COUNT-RETRY.
           IF XCLM-RETRY-CNT < 9999999
               ADD 1 TO XCLM-RETRY-CNT
           END-IF
           IF XCLM-RETRY-CNT NOT < 12
               MOVE 'Y' TO XCLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E588 MEMO REQUIRED
      * E823 BONUS YTD NEGATIVE
      * E182 BONUS YTD OVER 5000
      * E658 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4300-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-MEMO = SPACES
                   MOVE 'E588' TO WS-REJECT-CODE
               WHEN XCLM-BONUS-YTD < 0
                   MOVE 'E823' TO WS-REJECT-CODE
               WHEN XCLM-BONUS-YTD > 5000.00
                   MOVE 'E182' TO WS-REJECT-CODE
               WHEN XCLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E658' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E045 REF NO REQUIRED
      * E730 CONTRIB AMT NEGATIVE
      * E912 CONTRIB AMT OVER 1000000
      * E032 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4310-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-REF-NO = SPACES
                   MOVE 'E045' TO WS-REJECT-CODE
               WHEN XCLT-CONTRIB-AMT < 0
                   MOVE 'E730' TO WS-REJECT-CODE
               WHEN XCLT-CONTRIB-AMT > 1000000.00
                   MOVE 'E912' TO WS-REJECT-CODE
               WHEN XCLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E032' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE GOAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4320-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF XCLM-GOAL-AMT NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4330-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB FEE ON THE PENALTY YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4340-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO XCLT-CONTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE XCLT-CONTRIB-AMT ROUNDED = XCLM-PENALTY-YTD *
                       XCLM-FEE-PCT / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 15.00 TO XCLT-CONTRIB-AMT
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 10.00 TO XCLT-CONTRIB-AMT
               WHEN OTHER
                   COMPUTE XCLT-CONTRIB-AMT ROUNDED = XCLM-PENALTY-YTD *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLT-CONTRIB-AMT > 0 AND XCLT-CONTRIB-AMT < 2.00
               MOVE 2.00 TO XCLT-CONTRIB-AMT
           END-IF
           IF XCLT-CONTRIB-AMT > 50.00
               MOVE 50.00 TO XCLT-CONTRIB-AMT
           END-IF
           ADD XCLT-CONTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4350-DESCRIBE-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUED YTD FEE ON THE BONUS AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4360-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO XCLM-ACCRUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-STOP-FLAG = 'Y'
                   COMPUTE XCLM-ACCRUED-YTD ROUNDED = XCLT-BONUS-AMT *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 12.00 TO XCLM-ACCRUED-YTD
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 20.00 TO XCLM-ACCRUED-YTD
               WHEN OTHER
                   COMPUTE XCLM-ACCRUED-YTD ROUNDED = XCLT-BONUS-AMT *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-ACCRUED-YTD > 0 AND XCLM-ACCRUED-YTD < 5.00
               MOVE 5.00 TO XCLM-ACCRUED-YTD
           END-IF
           IF XCLM-ACCRUED-YTD > 50.00
               MOVE 50.00 TO XCLM-ACCRUED-YTD
           END-IF
           ADD XCLM-ACCRUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4370-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO XCLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WEEKLY AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4380-ACCRUE-WEEKLY.
           IF XCLM-WEEKLY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-WEEKLY-AMT * XCLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-BONUS-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PENALTY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4390-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   XCLM-PENALTY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4400-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCLT-CONTRIB-AMT / 4
           COMPUTE WS-WORK-AMT-2 = XCLT-CONTRIB-AMT - (WS-WORK-AMT * 4)
           COMPUTE XCLM-ACCRUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4410-FLAG-PENALTY.
           IF XCLM-PENALTY-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-PENALTY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ACCRUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       4420-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-CONTRIB-AMT ROUNDED =
                   XCLM-ACCRUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-CONTRIB-AMT
           END-IF
           ADD XCLM-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4430-AGE-LAST-ACT.
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
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MISSED YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4440-ACCRUE-MISSED.
           IF XCLM-MISSED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-MISSED-YTD * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCLM-PAYOUT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4450-AGE-NEXT-DUE.
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
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4460-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4470-LIMIT-PENALTY.
           IF XCLM-PENALTY-YTD < 25.00
               MOVE 25.00 TO XCLM-PENALTY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-PENALTY-YTD > 2500.00
               MOVE 2500.00 TO XCLM-PENALTY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYOUT YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4480-ACCRUE-PAYOUT.
           IF XCLM-PAYOUT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-PAYOUT-YTD * XCLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-GOAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED SCHED TIER FOR THE BONUS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4490-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-SCHED-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCLT-BONUS-AMT NOT < WS-ACCRUED-SCHED-LOW(WS-TX)
                   MOVE WS-ACCRUED-SCHED-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BONUS AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4500-ACCRUE-BONUS.
           IF XCLM-BONUS-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-BONUS-AMT * XCLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-BONUS-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E605 NAME REQUIRED
      * E003 PAYOUT AMT NEGATIVE
      * E619 PAYOUT AMT OVER 10000
      * E143 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4510-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-NAME = SPACES
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-AMT < 0
                   MOVE 'E003' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-AMT > 10000.00
                   MOVE 'E619' TO WS-REJECT-CODE
               WHEN XCLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E143' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ACCRUED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4520-SPLIT-ACCRUED.
           COMPUTE WS-WORK-AMT = XCLM-ACCRUED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = XCLM-ACCRUED-AMT - (WS-WORK-AMT * 2)
           COMPUTE XCLT-BONUS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4530-COUNT-NOTICE.
           IF XCLM-NOTICE-CNT < 9999999
               ADD 1 TO XCLM-NOTICE-CNT
           END-IF
           IF XCLM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED SCHEDX TIER FOR THE CONTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4540-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-SCHEDX-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF XCLT-CONTRIB-AMT NOT < WS-ACCRUED-SCHEDX-LOW(WS-TX)
                   MOVE WS-ACCRUED-SCHEDX-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED SCHEDX TIER FOR THE WEEKLY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4550-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-SCHEDX-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF XCLT-WEEKLY-AMT NOT < WS-ACCRUED-SCHEDX-LOW(WS-TX)
                   MOVE WS-ACCRUED-SCHEDX-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

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
                   MOVE XCFEE-GOAL-AMT
                     TO XCLM-GOAL-AMT
                   MOVE XCFEE-EXEMPT-FLAG
                     TO XCLM-EXEMPT-FLAG
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
                   MOVE XCCAL-WEEKLY-YTD
                     TO XCLM-WEEKLY-YTD
                   MOVE XCCAL-MISSED-AMT
                     TO XCLM-MISSED-AMT
                   MOVE XCCAL-OVERRIDE-FLAG
                     TO XCLM-OVERRIDE-FLAG
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
           DISPLAY 'XCLRNEW READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'XCLRNEW' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
