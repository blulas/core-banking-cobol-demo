      *================================================================*
      * PROGRAM  : XCLNOTC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : CHRISTMAS CLUB SAVINGS - CUSTOMER NOTICES. READS THE*
      *            XCL TRANSACTIONS, UPDATES THE XCL MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. HOLIDAY CLUB SAVINGS   *
      *            PLANS WITH WEEKLY DEPOSITS AND NOVEMBER PAYOUT      *
      * INPUT    : XCLMST (KSDS), XCLTRN, BANKCTL                      *
      * OUTPUT   : XCLOUT, XCLNORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, XCLIM                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-12-05 BHS  ORIGINAL                                       *
      * 1993-08-07 MKL  WEEKLY NOTICE TEXT PER LEGAL PRB7648           *
      * 1995-10-20 SKN  WEEKLY FEE SCHEDULE UPDATE REQ6664             *
      * 1995-05-01 MKL  AUDIT FINDING - WEEKLY CONTROL TOTALS REQ7223  *
      * 1996-02-01 LMB  CONTRIB NOTICE TEXT PER LEGAL INC8903          *
      * 1997-07-08 LMB  AUDIT FINDING - GOAL CONTROL TOTALS PRB8814    *
      * 1997-11-23 JLM  WEEKLY CUTOFF MOVED REQ8695                    *
      * 1999-01-07 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-09-10 PQR  PENALTY EXCEPTIONS TO OPS QUEUE CHG3624        *
      * 2004-08-13 JLM  ACCRUED NOTICE TEXT PER LEGAL CHG6165          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. XCLNOTC.
       AUTHOR. B H SANDERS.

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
           SELECT REPORT-FILE ASSIGN TO "XCLNORPT"
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
      * WS-CONTRIB-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTRIB-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000434000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000805000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000174000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000755000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000815500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000544500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011600000000689000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011650000000040500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012650000000174000'.
       01  WS-CONTRIB-TIER-TBL REDEFINES WS-CONTRIB-TIER-VALUES.
           05  WS-CONTRIB-TIER-ROW OCCURS 9 TIMES.
               10  WS-CONTRIB-TIER-LOW     PIC 9(09)V99.
               10  WS-CONTRIB-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONTRIB-TIERX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTRIB-TIERX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000084000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000441000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000675500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001150000000243500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001650000000849000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001900000000659000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011900000000257000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012150000000605000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012650000000326000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013650000000072000'.
       01  WS-CONTRIB-TIERX-TBL REDEFINES WS-CONTRIB-TIERX-VALUES.
           05  WS-CONTRIB-TIERX-ROW OCCURS 10 TIMES.
               10  WS-CONTRIB-TIERX-LOW    PIC 9(09)V99.
               10  WS-CONTRIB-TIERX-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-WEEKLY-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-WEEKLY-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000100000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000194000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000719500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000679000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011550000000526000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021550000000597000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021650000000405000'.
       01  WS-WEEKLY-BAND-TBL REDEFINES WS-WEEKLY-BAND-VALUES.
           05  WS-WEEKLY-BAND-ROW OCCURS 7 TIMES.
               10  WS-WEEKLY-BAND-LOW      PIC 9(09)V99.
               10  WS-WEEKLY-BAND-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PENALTY-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PENALTY-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000231000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000737000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000321000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000709000'.
       01  WS-PENALTY-TIER-TBL REDEFINES WS-PENALTY-TIER-VALUES.
           05  WS-PENALTY-TIER-ROW OCCURS 4 TIMES.
               10  WS-PENALTY-TIER-LOW     PIC 9(09)V99.
               10  WS-PENALTY-TIER-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  XCLNOTC  CHRISTMAS CLUB SAVINGS'.
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
               PERFORM 3000-LIMIT-CONTRIB
               PERFORM 3010-SELECT-DISCOUNT
               PERFORM 3020-AGE-POST
               PERFORM 3030-FLAG-PAYOUT
               PERFORM 3040-PRORATE-WEEKLY
               PERFORM 3050-ACCRUE-GOAL
               PERFORM 3060-ACCUM-TRAN
               PERFORM 3070-AGE-NEXT-DUE
               PERFORM 3080-LIMIT-CONTRIB
               PERFORM 3090-LIMIT-MISSED
               PERFORM 3100-AGE-POST
               PERFORM 3110-FLAG-ITEM
               PERFORM 3120-SET-REASON
               PERFORM 3130-EDIT-CONTACT
               PERFORM 3140-FLAG-PAYOUT
               PERFORM 3150-ACCRUE-PAYOUT
               PERFORM 3160-COUNT-CYCLE
               PERFORM 3170-DESCRIBE-CYCLE
               PERFORM 3180-LIMIT-PENALTY
               PERFORM 3190-PRORATE-BONUS
               PERFORM 3200-DESCRIBE-CHANNEL
               PERFORM 3210-COUNT-CYCLE
               PERFORM 3220-PRORATE-WEEKLY
               PERFORM 3230-PRORATE-PENALTY
               PERFORM 3240-AGE-EFF
               PERFORM 3250-AGE-MATURITY
               PERFORM 3260-SET-PAYOUT-METHOD
               PERFORM 3270-SELECT-RATE
               PERFORM 3280-AGE-LAST-STMT
               PERFORM 3290-DESCRIBE-CHANNEL
               PERFORM 3300-ASSESS-SUSPEND
               PERFORM 3310-FLAG-PENALTY
               PERFORM 3320-CHECK-EFF
               PERFORM 3330-SPLIT-GOAL
               PERFORM 3340-SELECT-FEE-PCT
               PERFORM 3350-AGE-EXPIRY
               PERFORM 3360-ASSESS-SUSPEND
               PERFORM 3370-LIMIT-GOAL
               PERFORM 3380-ASSESS-STOP
               PERFORM 3390-ACCRUE-CONTRIB
               PERFORM 3400-SPLIT-WEEKLY
               PERFORM 3410-ASSESS-AUTO-RENEW
               PERFORM 3420-SELECT-RATE
               PERFORM 3430-COUNT-SEQ
               PERFORM 3440-SET-REASON
               PERFORM 3450-CHECK-EXPIRY
               PERFORM 3460-CHECK-EFF
               PERFORM 3470-SET-TRAN
               PERFORM 3480-EDIT-MEMO
               PERFORM 3490-CHECK-NEXT-REVIEW
               PERFORM 3500-AGE-LAST-PAY
               PERFORM 3510-DESCRIBE-REASON
               PERFORM 3520-SPLIT-CONTRIB
               PERFORM 3530-SPLIT-BONUS
               PERFORM 3540-SET-TRAN
               PERFORM 3550-ASSESS-PREFERRED
               PERFORM 3560-ACCUM-REGION
               PERFORM 3570-DESCRIBE-PLAN
               PERFORM 3580-ASSESS-OVERRIDE
               PERFORM 3590-EDIT-DESC
               PERFORM 3600-COUNT-SEQ
               PERFORM 3610-CHECK-LAST-STMT
               PERFORM 3620-COUNT-SEQ
               PERFORM 3630-DESCRIBE-TRAN
               PERFORM 3640-SELECT-RATE
               PERFORM 3650-FLAG-CONTRIB
               PERFORM 3660-FLAG-CONTRIB
               PERFORM 3670-ASSESS-OVERRIDE
               PERFORM 3680-LIMIT-BONUS
               PERFORM 3690-AGE-OPEN
               PERFORM 3700-ACCUM-PLAN
               PERFORM 3710-ACCRUE-WEEKLY
               PERFORM 3720-PRORATE-ACCRUED
               PERFORM 3730-PRORATE-BONUS
               PERFORM 3740-FLAG-WEEKLY
               PERFORM 3750-SET-CHANNEL
               PERFORM 3760-ACCRUE-BONUS
               PERFORM 3770-FLAG-BONUS
               PERFORM 3780-FLAG-GOAL
               PERFORM 3790-PRORATE-GOAL
               PERFORM 3800-SELECT-DISCOUNT
               PERFORM 3810-CHECK-EXPIRY
               PERFORM 3820-FLAG-PAYOUT
               PERFORM 3830-COUNT-PAYMENT
               PERFORM 3840-SELECT-RATE
               PERFORM 3850-SPLIT-MISSED
               PERFORM 3860-ASSESS-AUTO-RENEW
               PERFORM 3870-ASSESS-REVERSAL
               PERFORM 3880-DESCRIBE-TRAN
               PERFORM 3890-SPLIT-CONTRIB
               PERFORM 3900-CHECK-LAST-PAY
               PERFORM 3910-EDIT-REF-NO
               PERFORM 3920-CHECK-EXPIRY
               PERFORM 3930-EDIT-REF-NO
               PERFORM 3940-DESCRIBE-CYCLE
               PERFORM 3950-ASSESS-SUSPEND
               PERFORM 3960-SPLIT-PENALTY
               PERFORM 3970-ACCRUE-PAYOUT
               PERFORM 3980-DESCRIBE-TRAN
               PERFORM 3990-DESCRIBE-REGION
               PERFORM 4000-ACCRUE-WEEKLY
               PERFORM 4010-LIMIT-GOAL
               PERFORM 4020-LIMIT-BONUS
               PERFORM 4030-FLAG-MISSED
               PERFORM 4040-FLAG-WEEKLY
               PERFORM 4050-ACCUM-TRAN
               PERFORM 4060-FLAG-MISSED
               PERFORM 4070-FLAG-ACCRUED
               PERFORM 4080-SELECT-DISCOUNT
               PERFORM 4090-PRORATE-MISSED
               PERFORM 4100-COUNT-CYCLE
               PERFORM 4110-COUNT-SEQ
               PERFORM 4120-ACCUM-CHANNEL
               PERFORM 4130-FLAG-PENALTY
               PERFORM 4140-COUNT-NOTICE
               PERFORM 4150-ASSESS-SUSPEND
               PERFORM 4160-DESCRIBE-TRAN
               PERFORM 4170-PRORATE-ACCRUED
               PERFORM 4180-DESCRIBE-PAYOUT-METHOD
               PERFORM 4190-FLAG-GOAL
               PERFORM 4200-CHECK-LAST-ACT
               PERFORM 4210-ACCRUE-MISSED
               PERFORM 4220-FLAG-ITEM
               PERFORM 4230-SELECT-DISCOUNT
               PERFORM 4240-AGE-MATURITY
               PERFORM 4250-FLAG-ACCRUED
               PERFORM 4260-FLAG-BONUS
               PERFORM 4270-COUNT-CYCLE
               PERFORM 4280-ASSESS-OVERRIDE
               PERFORM 4290-LIMIT-WEEKLY
               PERFORM 4300-SET-REGION
               PERFORM 4310-ACCRUE-PAYOUT
               PERFORM 4320-CHECK-EFF
               PERFORM 4330-ASSESS-EXEMPT
               PERFORM 4340-CHECK-EFF
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
      * CONTRIB YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3000-LIMIT-CONTRIB.
           IF XCLM-CONTRIB-YTD < 100.00
               MOVE 100.00 TO XCLM-CONTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-CONTRIB-YTD > 50000.00
               MOVE 50000.00 TO XCLM-CONTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB TIERX TIER FOR THE PENALTY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3010-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-TIERX-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF XCLM-PENALTY-AMT NOT < WS-CONTRIB-TIERX-LOW(WS-TX)
                   MOVE WS-CONTRIB-TIERX-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3020-AGE-POST.
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
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYOUT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3030-FLAG-PAYOUT.
           IF XCLM-PAYOUT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-PAYOUT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WEEKLY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WEEKLY AMT.
      *----------------------------------------------------------------*
       3040-PRORATE-WEEKLY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-WEEKLY-AMT ROUNDED =
                   XCLM-WEEKLY-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLT-WEEKLY-AMT
           END-IF
           ADD XCLT-WEEKLY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GOAL YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3050-ACCRUE-GOAL.
           IF XCLM-GOAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-GOAL-YTD * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCLM-CONTRIB-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GOAL AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3060-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLT-TRAN-CD
                   ADD XCLT-GOAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLT-GOAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3070-AGE-NEXT-DUE.
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
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-GOAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONTRIB AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3080-LIMIT-CONTRIB.
           IF XCLM-CONTRIB-AMT < 0.00
               MOVE 0.00 TO XCLM-CONTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-CONTRIB-AMT > 10000.00
               MOVE 10000.00 TO XCLM-CONTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MISSED AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3090-LIMIT-MISSED.
           IF XCLM-MISSED-AMT < 10.00
               MOVE 10.00 TO XCLM-MISSED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-MISSED-AMT > 50000.00
               MOVE 50000.00 TO XCLM-MISSED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3100-AGE-POST.
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
                   ADD XCLM-WEEKLY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-WEEKLY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-WEEKLY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-WEEKLY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-WEEKLY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3110-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AL -> BT WHEN NOTICE DATE IS PAST
      * BT -> CY WHEN NOTICE DATE IS PAST
      * CY -> KM WHEN NOTICE DATE IS PAST
      * KM -> MV WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3120-SET-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BT'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CY' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CY'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KM' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KM'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MV' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E132 CONTACT REQUIRED
      * E876 ACCRUED AMT NEGATIVE
      * E010 ACCRUED AMT OVER 25000
      * E639 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3130-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-CONTACT = SPACES
                   MOVE 'E132' TO WS-REJECT-CODE
               WHEN XCLM-ACCRUED-AMT < 0
                   MOVE 'E876' TO WS-REJECT-CODE
               WHEN XCLM-ACCRUED-AMT > 25000.00
                   MOVE 'E010' TO WS-REJECT-CODE
               WHEN XCLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E639' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYOUT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3140-FLAG-PAYOUT.
           IF XCLM-PAYOUT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-PAYOUT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYOUT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3150-ACCRUE-PAYOUT.
           IF XCLM-PAYOUT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-PAYOUT-AMT * XCLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-BONUS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3160-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO XCLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3170-DESCRIBE-CYCLE.
           EVALUATE XCLM-CYCLE-CD
               WHEN 'EM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PENALTY YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3180-LIMIT-PENALTY.
           IF XCLM-PENALTY-YTD < 0.00
               MOVE 0.00 TO XCLM-PENALTY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-PENALTY-YTD > 10000.00
               MOVE 10000.00 TO XCLM-PENALTY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BONUS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BONUS AMT.
      *----------------------------------------------------------------*
       3190-PRORATE-BONUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-BONUS-AMT ROUNDED =
                   XCLT-BONUS-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-BONUS-AMT
           END-IF
           ADD XCLM-BONUS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3200-DESCRIBE-CHANNEL.
           EVALUATE XCLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BONUS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WEEKLY AMT.
      *----------------------------------------------------------------*
       3220-PRORATE-WEEKLY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-WEEKLY-AMT ROUNDED =
                   XCLM-BONUS-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-WEEKLY-AMT
           END-IF
           ADD XCLM-WEEKLY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE PENALTY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3230-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   XCLM-PENALTY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3240-AGE-EFF.
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
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3250-AGE-MATURITY.
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
               WHEN WS-DAYS NOT > 45
                   ADD XCLM-PAYOUT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-PAYOUT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-PAYOUT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-PAYOUT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYOUT METHOD CD:
      * B -> H WHEN EXPIRY DATE IS PAST
      * H -> U WHEN EXPIRY DATE IS PAST
      * U -> W WHEN EXPIRY DATE IS PAST
      * W -> B WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3260-SET-PAYOUT-METHOD.
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
      * SELECT THE CONTRIB TIERX TIER FOR THE PENALTY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-TIERX-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF XCLM-PENALTY-YTD NOT < WS-CONTRIB-TIERX-LOW(WS-TX)
                   MOVE WS-CONTRIB-TIERX-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3280-AGE-LAST-STMT.
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
                   ADD XCLM-ACCRUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLM-ACCRUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD XCLM-ACCRUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-ACCRUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-ACCRUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3290-DESCRIBE-CHANNEL.
           EVALUATE XCLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WEEKLY AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3300-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = XCLM-WEEKLY-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = XCLM-WEEKLY-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PENALTY AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3310-FLAG-PENALTY.
           IF XCLM-PENALTY-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLM-PENALTY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3320-CHECK-EFF.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GOAL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3330-SPLIT-GOAL.
           COMPUTE WS-WORK-AMT = XCLM-GOAL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = XCLM-GOAL-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE BONUS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3340-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCLM-BONUS-YTD NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
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
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-ACCRUED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUED FEE ON THE CONTRIB AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3360-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO XCLM-ACCRUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   COMPUTE XCLM-ACCRUED-AMT ROUNDED = XCLT-CONTRIB-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO XCLM-ACCRUED-AMT
               WHEN XCLT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO XCLM-ACCRUED-AMT
               WHEN OTHER
                   COMPUTE XCLM-ACCRUED-AMT ROUNDED = XCLT-CONTRIB-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-ACCRUED-AMT > 0 AND XCLM-ACCRUED-AMT < 1.00
               MOVE 1.00 TO XCLM-ACCRUED-AMT
           END-IF
           IF XCLM-ACCRUED-AMT > 75.00
               MOVE 75.00 TO XCLM-ACCRUED-AMT
           END-IF
           ADD XCLM-ACCRUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * GOAL AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3370-LIMIT-GOAL.
           IF XCLT-GOAL-AMT < 100.00
               MOVE 100.00 TO XCLT-GOAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLT-GOAL-AMT > 5000.00
               MOVE 5000.00 TO XCLT-GOAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MISSED YTD FEE ON THE WEEKLY AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3380-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCLM-MISSED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE XCLM-MISSED-YTD ROUNDED = XCLM-WEEKLY-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO XCLM-MISSED-YTD
               WHEN XCLT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO XCLM-MISSED-YTD
               WHEN OTHER
                   COMPUTE XCLM-MISSED-YTD ROUNDED = XCLM-WEEKLY-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-MISSED-YTD > 0 AND XCLM-MISSED-YTD < 10.00
               MOVE 10.00 TO XCLM-MISSED-YTD
           END-IF
           IF XCLM-MISSED-YTD > 75.00
               MOVE 75.00 TO XCLM-MISSED-YTD
           END-IF
           ADD XCLM-MISSED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3390-ACCRUE-CONTRIB.
           IF XCLT-CONTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLT-CONTRIB-AMT * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WEEKLY AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3400-SPLIT-WEEKLY.
           COMPUTE WS-WORK-AMT = XCLM-WEEKLY-AMT / 2
           COMPUTE WS-WORK-AMT-2 = XCLM-WEEKLY-AMT - (WS-WORK-AMT * 2)
           COMPUTE XCLM-WEEKLY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GOAL YTD FEE ON THE PENALTY AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3410-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO XCLM-GOAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   COMPUTE XCLM-GOAL-YTD ROUNDED = XCLM-PENALTY-AMT *
                       XCLM-RATE / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 12.00 TO XCLM-GOAL-YTD
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 15.00 TO XCLM-GOAL-YTD
               WHEN OTHER
                   COMPUTE XCLM-GOAL-YTD ROUNDED = XCLM-PENALTY-AMT *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-GOAL-YTD > 0 AND XCLM-GOAL-YTD < 10.00
               MOVE 10.00 TO XCLM-GOAL-YTD
           END-IF
           IF XCLM-GOAL-YTD > 100.00
               MOVE 100.00 TO XCLM-GOAL-YTD
           END-IF
           ADD XCLM-GOAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB TIERX TIER FOR THE BONUS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3420-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-TIERX-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF XCLM-BONUS-YTD NOT < WS-CONTRIB-TIERX-LOW(WS-TX)
                   MOVE WS-CONTRIB-TIERX-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3430-COUNT-SEQ.
           IF XCLT-SEQ < 9999999
               ADD 1 TO XCLT-SEQ
           END-IF
           IF XCLT-SEQ NOT < 6
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AL -> BT WHEN MATURITY DATE IS PAST
      * BT -> CY WHEN MATURITY DATE IS PAST
      * CY -> KM WHEN MATURITY DATE IS PAST
      * KM -> MV WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3440-SET-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   IF XCLM-MATURITY-DATE > 0 AND XCLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BT'
                   IF XCLM-MATURITY-DATE > 0 AND XCLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CY' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CY'
                   IF XCLM-MATURITY-DATE > 0 AND XCLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KM' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KM'
                   IF XCLM-MATURITY-DATE > 0 AND XCLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MV' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3450-CHECK-EXPIRY.
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
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3460-CHECK-EFF.
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
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * WD -> DP WHEN EFF DATE IS PAST
      * DP -> PO WHEN EFF DATE IS PAST
      * PO -> BN WHEN EFF DATE IS PAST
      * BN -> PN WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3470-SET-TRAN.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E925 MEMO REQUIRED
      * E405 PAYOUT AMT NEGATIVE
      * E578 PAYOUT AMT OVER 1000000
      * E952 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3480-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-MEMO = SPACES
                   MOVE 'E925' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-AMT < 0
                   MOVE 'E405' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-AMT > 1000000.00
                   MOVE 'E578' TO WS-REJECT-CODE
               WHEN XCLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E952' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3490-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3500-AGE-LAST-PAY.
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
                   ADD XCLT-CONTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD XCLT-CONTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCLT-CONTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLT-CONTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLT-CONTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3510-DESCRIBE-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'CY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3520-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCLM-CONTRIB-YTD / 12
           COMPUTE WS-WORK-AMT-2 = XCLM-CONTRIB-YTD - (WS-WORK-AMT * 12)
           COMPUTE XCLM-WEEKLY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BONUS AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3530-SPLIT-BONUS.
           COMPUTE WS-WORK-AMT = XCLT-BONUS-AMT / 12
           COMPUTE WS-WORK-AMT-2 = XCLT-BONUS-AMT - (WS-WORK-AMT * 12)
           COMPUTE XCLM-ACCRUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * WD -> DP WHEN POST DATE IS PAST
      * DP -> PO WHEN POST DATE IS PAST
      * PO -> BN WHEN POST DATE IS PAST
      * BN -> PN WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3540-SET-TRAN.
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
      * ASSESS THE GOAL FEE ON THE WEEKLY AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3550-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO XCLM-GOAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   COMPUTE XCLM-GOAL-AMT ROUNDED = XCLT-WEEKLY-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-CYCLE-CD = 'EM'
                   MOVE 7.00 TO XCLM-GOAL-AMT
               WHEN XCLM-CYCLE-CD = 'FE'
                   MOVE 20.00 TO XCLM-GOAL-AMT
               WHEN OTHER
                   COMPUTE XCLM-GOAL-AMT ROUNDED = XCLT-WEEKLY-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-GOAL-AMT > 0 AND XCLM-GOAL-AMT < 10.00
               MOVE 10.00 TO XCLM-GOAL-AMT
           END-IF
           IF XCLM-GOAL-AMT > 250.00
               MOVE 250.00 TO XCLM-GOAL-AMT
           END-IF
           ADD XCLM-GOAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WEEKLY AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3560-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-REGION-CD
                   ADD XCLT-WEEKLY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLT-WEEKLY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3570-DESCRIBE-PLAN.
           EVALUATE XCLM-PLAN-CD
               WHEN 'GN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUED FEE ON THE WEEKLY YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3580-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO XCLM-ACCRUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   COMPUTE XCLM-ACCRUED-AMT ROUNDED = XCLM-WEEKLY-YTD *
                       XCLM-FEE-PCT / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 3.00 TO XCLM-ACCRUED-AMT
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 20.00 TO XCLM-ACCRUED-AMT
               WHEN OTHER
                   COMPUTE XCLM-ACCRUED-AMT ROUNDED = XCLM-WEEKLY-YTD *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-ACCRUED-AMT > 0 AND XCLM-ACCRUED-AMT < 5.00
               MOVE 5.00 TO XCLM-ACCRUED-AMT
           END-IF
           IF XCLM-ACCRUED-AMT > 75.00
               MOVE 75.00 TO XCLM-ACCRUED-AMT
           END-IF
           ADD XCLM-ACCRUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E819 DESC REQUIRED
      * E167 WORK AMT 2 NEGATIVE
      * E757 WORK AMT 2 OVER 1000000
      * E361 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3590-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-DESC = SPACES
                   MOVE 'E819' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E167' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E757' TO WS-REJECT-CODE
               WHEN XCLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E361' TO WS-REJECT-CODE
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
       3600-COUNT-SEQ.
           IF XCLT-SEQ < 9999999
               ADD 1 TO XCLT-SEQ
           END-IF
           IF XCLT-SEQ NOT < 6
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3610-CHECK-LAST-STMT.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3620-COUNT-SEQ.
           IF XCLT-SEQ < 9999999
               ADD 1 TO XCLT-SEQ
           END-IF
           IF XCLT-SEQ NOT < 5
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-TRAN.
           EVALUATE XCLT-TRAN-CD
               WHEN 'WD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PO'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE WEEKLY BAND TIER FOR THE BONUS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3640-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-WEEKLY-BAND-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF XCLT-BONUS-AMT NOT < WS-WEEKLY-BAND-LOW(WS-TX)
                   MOVE WS-WEEKLY-BAND-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTRIB AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3650-FLAG-CONTRIB.
           IF XCLT-CONTRIB-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCLT-CONTRIB-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CONTRIB YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3660-FLAG-CONTRIB.
           IF XCLM-CONTRIB-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLM-CONTRIB-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUED FEE ON THE BONUS AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3670-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO XCLM-ACCRUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE XCLM-ACCRUED-AMT ROUNDED = XCLM-BONUS-AMT *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-CYCLE-CD = 'EM'
                   MOVE 3.00 TO XCLM-ACCRUED-AMT
               WHEN XCLM-CYCLE-CD = 'FE'
                   MOVE 20.00 TO XCLM-ACCRUED-AMT
               WHEN OTHER
                   COMPUTE XCLM-ACCRUED-AMT ROUNDED = XCLM-BONUS-AMT *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-ACCRUED-AMT > 0 AND XCLM-ACCRUED-AMT < 1.00
               MOVE 1.00 TO XCLM-ACCRUED-AMT
           END-IF
           IF XCLM-ACCRUED-AMT > 35.00
               MOVE 35.00 TO XCLM-ACCRUED-AMT
           END-IF
           ADD XCLM-ACCRUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * BONUS YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3680-LIMIT-BONUS.
           IF XCLM-BONUS-YTD < 100.00
               MOVE 100.00 TO XCLM-BONUS-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-BONUS-YTD > 500.00
               MOVE 500.00 TO XCLM-BONUS-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3690-AGE-OPEN.
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
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3700-ACCUM-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-PLAN-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WEEKLY AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3710-ACCRUE-WEEKLY.
           IF XCLT-WEEKLY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLT-WEEKLY-AMT * XCLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-BONUS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ACCRUED AMT.
      *----------------------------------------------------------------*
       3720-PRORATE-ACCRUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-ACCRUED-AMT ROUNDED =
                   XCLM-CONTRIB-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-ACCRUED-AMT
           END-IF
           ADD XCLM-ACCRUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BONUS AMT.
      *----------------------------------------------------------------*
       3730-PRORATE-BONUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-BONUS-AMT ROUNDED =
                   XCLM-CONTRIB-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLT-BONUS-AMT
           END-IF
           ADD XCLT-BONUS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WEEKLY AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3740-FLAG-WEEKLY.
           IF XCLT-WEEKLY-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLT-WEEKLY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT REVIEW DATE IS PAST
      * BAT -> MAL WHEN NEXT REVIEW DATE IS PAST
      * MAL -> TEL WHEN NEXT REVIEW DATE IS PAST
      * TEL -> ONL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-CHANNEL.
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
      * DAILY INTEREST ON BONUS YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3760-ACCRUE-BONUS.
           IF XCLM-BONUS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-BONUS-YTD * XCLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-PAYOUT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BONUS AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3770-FLAG-BONUS.
           IF XCLM-BONUS-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCLM-BONUS-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * GOAL AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3780-FLAG-GOAL.
           IF XCLM-GOAL-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCLM-GOAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ACCRUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO GOAL AMT.
      *----------------------------------------------------------------*
       3790-PRORATE-GOAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-GOAL-AMT ROUNDED =
                   XCLM-ACCRUED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLT-GOAL-AMT
           END-IF
           ADD XCLT-GOAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB TIERX TIER FOR THE GOAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3800-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-TIERX-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF XCLM-GOAL-YTD NOT < WS-CONTRIB-TIERX-LOW(WS-TX)
                   MOVE WS-CONTRIB-TIERX-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3810-CHECK-EXPIRY.
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
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYOUT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3820-FLAG-PAYOUT.
           IF XCLM-PAYOUT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLM-PAYOUT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3830-COUNT-PAYMENT.
           IF XCLM-PAYMENT-CNT < 9999999
               ADD 1 TO XCLM-PAYMENT-CNT
           END-IF
           IF XCLM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB TIER TIER FOR THE ACCRUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3840-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-TIER-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF XCLM-ACCRUED-YTD NOT < WS-CONTRIB-TIER-LOW(WS-TX)
                   MOVE WS-CONTRIB-TIER-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MISSED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3850-SPLIT-MISSED.
           COMPUTE WS-WORK-AMT = XCLM-MISSED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = XCLM-MISSED-AMT - (WS-WORK-AMT * 4)
           COMPUTE XCLM-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WEEKLY FEE ON THE CONTRIB AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3860-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO XCLT-WEEKLY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   COMPUTE XCLT-WEEKLY-AMT ROUNDED = XCLM-CONTRIB-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 3.00 TO XCLT-WEEKLY-AMT
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 20.00 TO XCLT-WEEKLY-AMT
               WHEN OTHER
                   COMPUTE XCLT-WEEKLY-AMT ROUNDED = XCLM-CONTRIB-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLT-WEEKLY-AMT > 0 AND XCLT-WEEKLY-AMT < 1.00
               MOVE 1.00 TO XCLT-WEEKLY-AMT
           END-IF
           IF XCLT-WEEKLY-AMT > 100.00
               MOVE 100.00 TO XCLT-WEEKLY-AMT
           END-IF
           ADD XCLT-WEEKLY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WEEKLY FEE ON THE PAYOUT YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3870-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO XCLM-WEEKLY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   COMPUTE XCLM-WEEKLY-AMT ROUNDED = XCLM-PAYOUT-YTD *
                       XCLM-RATE / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 20.00 TO XCLM-WEEKLY-AMT
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 15.00 TO XCLM-WEEKLY-AMT
               WHEN OTHER
                   COMPUTE XCLM-WEEKLY-AMT ROUNDED = XCLM-PAYOUT-YTD *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-WEEKLY-AMT > 0 AND XCLM-WEEKLY-AMT < 2.00
               MOVE 2.00 TO XCLM-WEEKLY-AMT
           END-IF
           IF XCLM-WEEKLY-AMT > 100.00
               MOVE 100.00 TO XCLM-WEEKLY-AMT
           END-IF
           ADD XCLM-WEEKLY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3880-DESCRIBE-TRAN.
           EVALUATE XCLT-TRAN-CD
               WHEN 'WD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PO'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3890-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = XCLM-CONTRIB-AMT / 4
           COMPUTE WS-WORK-AMT-2 = XCLM-CONTRIB-AMT - (WS-WORK-AMT * 4)
           COMPUTE XCLM-PAYOUT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3900-CHECK-LAST-PAY.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E998 REF NO REQUIRED
      * E548 BONUS AMT NEGATIVE
      * E713 BONUS AMT OVER 10000
      * E933 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3910-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-REF-NO = SPACES
                   MOVE 'E998' TO WS-REJECT-CODE
               WHEN XCLM-BONUS-AMT < 0
                   MOVE 'E548' TO WS-REJECT-CODE
               WHEN XCLM-BONUS-AMT > 10000.00
                   MOVE 'E713' TO WS-REJECT-CODE
               WHEN XCLM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E933' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3920-CHECK-EXPIRY.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E239 REF NO REQUIRED
      * E501 MISSED AMT NEGATIVE
      * E624 MISSED AMT OVER 50000
      * E530 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3930-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-REF-NO = SPACES
                   MOVE 'E239' TO WS-REJECT-CODE
               WHEN XCLM-MISSED-AMT < 0
                   MOVE 'E501' TO WS-REJECT-CODE
               WHEN XCLM-MISSED-AMT > 50000.00
                   MOVE 'E624' TO WS-REJECT-CODE
               WHEN XCLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E530' TO WS-REJECT-CODE
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
       3940-DESCRIBE-CYCLE.
           EVALUATE XCLM-CYCLE-CD
               WHEN 'EM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BONUS FEE ON THE GOAL AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3950-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO XCLM-BONUS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   COMPUTE XCLM-BONUS-AMT ROUNDED = XCLT-GOAL-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 5.00 TO XCLM-BONUS-AMT
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 5.00 TO XCLM-BONUS-AMT
               WHEN OTHER
                   COMPUTE XCLM-BONUS-AMT ROUNDED = XCLT-GOAL-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-BONUS-AMT > 0 AND XCLM-BONUS-AMT < 5.00
               MOVE 5.00 TO XCLM-BONUS-AMT
           END-IF
           IF XCLM-BONUS-AMT > 50.00
               MOVE 50.00 TO XCLM-BONUS-AMT
           END-IF
           ADD XCLM-BONUS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT PENALTY YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-PENALTY.
           COMPUTE WS-WORK-AMT = XCLM-PENALTY-YTD / 2
           COMPUTE WS-WORK-AMT-2 = XCLM-PENALTY-YTD - (WS-WORK-AMT * 2)
           COMPUTE XCLT-WEEKLY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYOUT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3970-ACCRUE-PAYOUT.
           IF XCLM-PAYOUT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-PAYOUT-AMT * XCLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO XCLT-WEEKLY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3980-DESCRIBE-TRAN.
           EVALUATE XCLT-TRAN-CD
               WHEN 'WD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PO'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HLS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KMJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PDA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RVG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WMB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WEEKLY YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4000-ACCRUE-WEEKLY.
           IF XCLM-WEEKLY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-WEEKLY-YTD * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCLM-ACCRUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * GOAL YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4010-LIMIT-GOAL.
           IF XCLM-GOAL-YTD < 100.00
               MOVE 100.00 TO XCLM-GOAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-GOAL-YTD > 1000.00
               MOVE 1000.00 TO XCLM-GOAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BONUS AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4020-LIMIT-BONUS.
           IF XCLT-BONUS-AMT < 10.00
               MOVE 10.00 TO XCLT-BONUS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLT-BONUS-AMT > 500.00
               MOVE 500.00 TO XCLT-BONUS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MISSED YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4030-FLAG-MISSED.
           IF XCLM-MISSED-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLM-MISSED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WEEKLY AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4040-FLAG-WEEKLY.
           IF XCLT-WEEKLY-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLT-WEEKLY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ACCRUED YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4050-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLT-TRAN-CD
                   ADD XCLM-ACCRUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-ACCRUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * MISSED AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4060-FLAG-MISSED.
           IF XCLM-MISSED-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLM-MISSED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUED AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4070-FLAG-ACCRUED.
           IF XCLM-ACCRUED-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-ACCRUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB TIERX TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4080-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-TIERX-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-CONTRIB-TIERX-LOW(WS-TX)
                   MOVE WS-CONTRIB-TIERX-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MISSED YTD.
      *----------------------------------------------------------------*
       4090-PRORATE-MISSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-MISSED-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-MISSED-YTD
           END-IF
           ADD XCLM-MISSED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4100-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4110-COUNT-SEQ.
           IF XCLT-SEQ < 9999999
               ADD 1 TO XCLT-SEQ
           END-IF
           IF XCLT-SEQ NOT < 10
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MISSED AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4120-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLT-CHANNEL-CD
                   ADD XCLM-MISSED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-MISSED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4130-FLAG-PENALTY.
           IF XCLM-PENALTY-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLM-PENALTY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4140-COUNT-NOTICE.
           IF XCLM-NOTICE-CNT < 9999999
               ADD 1 TO XCLM-NOTICE-CNT
           END-IF
           IF XCLM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO XCLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BONUS FEE ON THE BONUS YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4150-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO XCLT-BONUS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE XCLT-BONUS-AMT ROUNDED = XCLM-BONUS-YTD *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-PLAN-CD = 'GN'
                   MOVE 10.00 TO XCLT-BONUS-AMT
               WHEN XCLM-PLAN-CD = 'JT'
                   MOVE 3.00 TO XCLT-BONUS-AMT
               WHEN OTHER
                   COMPUTE XCLT-BONUS-AMT ROUNDED = XCLM-BONUS-YTD *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLT-BONUS-AMT > 0 AND XCLT-BONUS-AMT < 5.00
               MOVE 5.00 TO XCLT-BONUS-AMT
           END-IF
           IF XCLT-BONUS-AMT > 50.00
               MOVE 50.00 TO XCLT-BONUS-AMT
           END-IF
           ADD XCLT-BONUS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4160-DESCRIBE-TRAN.
           EVALUATE XCLT-TRAN-CD
               WHEN 'WD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PO'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ACCRUED AMT.
      *----------------------------------------------------------------*
       4170-PRORATE-ACCRUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-ACCRUED-AMT ROUNDED =
                   XCLM-CONTRIB-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-ACCRUED-AMT
           END-IF
           ADD XCLM-ACCRUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE PAYOUT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4180-DESCRIBE-PAYOUT-METHOD.
           EVALUATE XCLM-PAYOUT-METHOD-CD
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'H'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'U'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * GOAL AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4190-FLAG-GOAL.
           IF XCLM-GOAL-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-GOAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4200-CHECK-LAST-ACT.
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
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MISSED YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4210-ACCRUE-MISSED.
           IF XCLM-MISSED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-MISSED-YTD * XCLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO XCLM-MISSED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4220-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE CONTRIB YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4230-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF XCLM-CONTRIB-YTD NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4240-AGE-MATURITY.
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
               WHEN WS-DAYS NOT > 30
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCRUED YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4250-FLAG-ACCRUED.
           IF XCLM-ACCRUED-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCLM-ACCRUED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * BONUS AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4260-FLAG-BONUS.
           IF XCLM-BONUS-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-BONUS-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4270-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO XCLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GOAL FEE ON THE PAYOUT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4280-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO XCLT-GOAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   COMPUTE XCLT-GOAL-AMT ROUNDED = XCLM-PAYOUT-AMT *
                       XCLM-RATE / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 5.00 TO XCLT-GOAL-AMT
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 7.00 TO XCLT-GOAL-AMT
               WHEN OTHER
                   COMPUTE XCLT-GOAL-AMT ROUNDED = XCLM-PAYOUT-AMT *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLT-GOAL-AMT > 0 AND XCLT-GOAL-AMT < 10.00
               MOVE 10.00 TO XCLT-GOAL-AMT
           END-IF
           IF XCLT-GOAL-AMT > 150.00
               MOVE 150.00 TO XCLT-GOAL-AMT
           END-IF
           ADD XCLT-GOAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WEEKLY AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4290-LIMIT-WEEKLY.
           IF XCLM-WEEKLY-AMT < 100.00
               MOVE 100.00 TO XCLM-WEEKLY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-WEEKLY-AMT > 50000.00
               MOVE 50000.00 TO XCLM-WEEKLY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * FLM -> HLS WHEN OPEN DATE IS PAST
      * HLS -> KMJ WHEN OPEN DATE IS PAST
      * KMJ -> PDA WHEN OPEN DATE IS PAST
      * PDA -> RVG WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4300-SET-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HLS' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HLS'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KMJ' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KMJ'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PDA' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PDA'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RVG' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYOUT YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4310-ACCRUE-PAYOUT.
           IF XCLM-PAYOUT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-PAYOUT-YTD * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCLM-WEEKLY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4320-CHECK-EFF.
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
               MOVE 'Y' TO XCLM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MISSED YTD FEE ON THE PAYOUT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4330-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO XCLM-MISSED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-STOP-FLAG = 'Y'
                   COMPUTE XCLM-MISSED-YTD ROUNDED = XCLM-PAYOUT-AMT *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-REASON-CD = 'AL'
                   MOVE 7.00 TO XCLM-MISSED-YTD
               WHEN XCLM-REASON-CD = 'BT'
                   MOVE 12.00 TO XCLM-MISSED-YTD
               WHEN OTHER
                   COMPUTE XCLM-MISSED-YTD ROUNDED = XCLM-PAYOUT-AMT *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-MISSED-YTD > 0 AND XCLM-MISSED-YTD < 2.00
               MOVE 2.00 TO XCLM-MISSED-YTD
           END-IF
           IF XCLM-MISSED-YTD > 100.00
               MOVE 100.00 TO XCLM-MISSED-YTD
           END-IF
           ADD XCLM-MISSED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4340-CHECK-EFF.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

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
                   MOVE XCLIM-GOAL-AMT
                     TO XCLM-GOAL-AMT
                   MOVE XCLIM-PENALTY-YTD
                     TO XCLM-PENALTY-YTD
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
           DISPLAY 'XCLNOTC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'XCLNOTC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
