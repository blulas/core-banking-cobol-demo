      *================================================================*
      * PROGRAM  : BUDEDIT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : BUMP-UP CERTIFICATES - INPUT EDIT. READS THE BCD    *
      *            TRANSACTIONS, UPDATES THE BCD MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. BUMP-UP AND STEP-RATE           *
      *            CERTIFICATE RATE ELECTION AND ACCRUAL               *
      * INPUT    : BCDMST (KSDS), BCDTRN, BANKCTL                      *
      * OUTPUT   : BCDOUT, BUDEDRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, BUVAL, BUDTE            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-07-08 JLM  ORIGINAL                                       *
      * 1996-05-18 DPS  BUMP EXCEPTIONS TO OPS QUEUE INC9154           *
      * 1999-08-12 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-01-16 DPS  INTEREST CALCULATION CORRECTED CHG1698         *
      * 2001-12-18 BHS  AUDIT FINDING - PRINCIPAL CONTROL TOTALS CHG383*
      * 2006-01-19 LMB  AUDIT FINDING - PRINCIPAL CONTROL TOTALS CHG299*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUDEDIT.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "BCDMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS BCDM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "BCDTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "BCDOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "BUDEDRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY BCDMST.
       FD  TRAN-FILE.
       COPY BCDTRN.
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
      * WS-BUMP-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BUMP-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000797500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000779500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002000000000353000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002100000000284500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002200000000232500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003200000000636000'.
       01  WS-BUMP-BAND-TBL REDEFINES WS-BUMP-BAND-VALUES.
           05  WS-BUMP-BAND-ROW OCCURS 6 TIMES.
               10  WS-BUMP-BAND-LOW        PIC 9(09)V99.
               10  WS-BUMP-BAND-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INTEREST-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000378000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000448000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000714500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000393000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012700000000659000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013700000000141500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013800000000244000'.
       01  WS-INTEREST-SCHED-TBL REDEFINES WS-INTEREST-SCHED-VALUES.
           05  WS-INTEREST-SCHED-ROW OCCURS 7 TIMES.
               10  WS-INTEREST-SCHED-LOW   PIC 9(09)V99.
               10  WS-INTEREST-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PENALTY-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PENALTY-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000077000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000772500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000329000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010300000000435000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012800000000376500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012900000000192000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017900000000566500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017950000000007000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018200000000360000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019200000000878000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024200000000850000'.
       01  WS-PENALTY-SCHED-TBL REDEFINES WS-PENALTY-SCHED-VALUES.
           05  WS-PENALTY-SCHED-ROW OCCURS 11 TIMES.
               10  WS-PENALTY-SCHED-LOW    PIC 9(09)V99.
               10  WS-PENALTY-SCHED-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  BUDEDIT  BUMP-UP CERTIFICATES'.
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
       COPY BUVALP.
       COPY BUDTEP.

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
               MOVE 'BCDMST' TO ABND-FILE-NAME
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
           MOVE BCDT-KEY TO BCDM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SPLIT-INTEREST
               PERFORM 3010-DESCRIBE-TRAN
               PERFORM 3020-DESCRIBE-CHANNEL
               PERFORM 3030-LIMIT-INTEREST
               PERFORM 3040-ACCUM-CYCLE
               PERFORM 3050-ACCUM-STEP-PLAN
               PERFORM 3060-FLAG-BUMP
               PERFORM 3070-DESCRIBE-REGION
               PERFORM 3080-CHECK-EFFECTIVE
               PERFORM 3090-EDIT-DESC
               PERFORM 3100-PRORATE-PENALTY
               PERFORM 3110-SET-TRAN
               PERFORM 3120-DESCRIBE-SOURCE
               PERFORM 3130-ASSESS-EXEMPT
               PERFORM 3140-EDIT-DESC
               PERFORM 3150-SELECT-RATE
               PERFORM 3160-LIMIT-PENALTY
               PERFORM 3170-SPLIT-PENALTY
               PERFORM 3180-COUNT-SEQ
               PERFORM 3190-ASSESS-REVERSAL
               PERFORM 3200-LIMIT-ITEM
               PERFORM 3210-EDIT-REF-NO
               PERFORM 3220-AGE-EXPIRY
               PERFORM 3230-LIMIT-ADDON
               PERFORM 3240-AGE-LAST-MAINT
               PERFORM 3250-ACCRUE-INTEREST
               PERFORM 3260-AGE-EFFECTIVE
               PERFORM 3270-SET-REGION
               PERFORM 3280-PRORATE-PENALTY
               PERFORM 3290-LIMIT-PENALTY
               PERFORM 3300-SELECT-PENALTY-PCT
               PERFORM 3310-SELECT-PENALTY-PCT
               PERFORM 3320-COUNT-RETRY
               PERFORM 3330-CHECK-LAST-MAINT
               PERFORM 3340-PRORATE-INTEREST
               PERFORM 3350-SELECT-MARGIN
               PERFORM 3360-ACCUM-REGION
               PERFORM 3370-CHECK-EFFECTIVE
               PERFORM 3380-CHECK-EFFECTIVE
               PERFORM 3390-SET-CHANNEL
               PERFORM 3400-CHECK-LAST-STMT
               PERFORM 3410-ACCRUE-INTEREST
               PERFORM 3420-ASSESS-REVIEW
               PERFORM 3430-AGE-LAST-MAINT
               PERFORM 3440-EDIT-NAME
               PERFORM 3450-SELECT-RATE
               PERFORM 3460-ACCRUE-PENALTY
               PERFORM 3470-COUNT-CYCLE
               PERFORM 3480-CHECK-LAST-STMT
               PERFORM 3490-FLAG-ITEM
               PERFORM 3500-FLAG-ITEM
               PERFORM 3510-SELECT-RATE
               PERFORM 3520-LIMIT-ADDON
               PERFORM 3530-FLAG-ITEM
               PERFORM 3540-DESCRIBE-ELECTION
               PERFORM 3550-SET-CHANNEL
               PERFORM 3560-ACCUM-SOURCE
               PERFORM 3570-SET-TRAN
               PERFORM 3580-ACCUM-CHANNEL
               PERFORM 3590-LIMIT-PRINCIPAL
               PERFORM 3600-ACCUM-SOURCE
               PERFORM 3610-LIMIT-PRINCIPAL
               PERFORM 3620-ACCRUE-ADDON
               PERFORM 3630-ACCRUE-PRINCIPAL
               PERFORM 3640-LIMIT-INTEREST
               PERFORM 3650-SET-CHANNEL
               PERFORM 3660-ACCRUE-PENALTY
               PERFORM 3670-DESCRIBE-TRAN
               PERFORM 3680-EDIT-DESC
               PERFORM 3690-SPLIT-BUMP
               PERFORM 3700-SET-CYCLE
               PERFORM 3710-LIMIT-PENALTY
               PERFORM 3720-SET-CHANNEL
               PERFORM 3730-FLAG-ADDON
               PERFORM 3740-SELECT-MARGIN
               PERFORM 3750-ACCRUE-PRINCIPAL
               PERFORM 3760-SET-CHANNEL
               PERFORM 3770-ASSESS-REVIEW
               PERFORM 3780-SPLIT-ADDON
               PERFORM 3790-SET-CHANNEL
               PERFORM 3800-FLAG-BUMP
               PERFORM 3810-ACCUM-STEP-PLAN
               PERFORM 3820-ACCUM-STEP-PLAN
               PERFORM 3830-ASSESS-SUSPEND
               PERFORM 3840-AGE-LAST-PAY
               PERFORM 3850-DESCRIBE-REGION
               PERFORM 3860-SET-REGION
               PERFORM 3870-LIMIT-ADDON
               PERFORM 3880-SET-STEP-PLAN
               PERFORM 3890-ASSESS-STOP
               PERFORM 3900-ASSESS-HOLD
               PERFORM 3910-SET-ELECTION
               PERFORM 3920-FLAG-PENALTY
               PERFORM 3930-EDIT-REF-NO
               PERFORM 3940-SPLIT-INTEREST
               PERFORM 3950-DESCRIBE-CHANNEL
               PERFORM 3960-PRORATE-ADDON
               PERFORM 3970-SET-CHANNEL
               PERFORM 3980-ACCUM-CYCLE
               PERFORM 3990-PRORATE-BUMP
               PERFORM 4000-AGE-LAST-MAINT
               PERFORM 4010-ACCUM-REGION
               PERFORM 4020-SELECT-PENALTY-PCT
               PERFORM 4030-ACCUM-REGION
               PERFORM 4040-COUNT-RETRY
               PERFORM 4050-FLAG-BUMP
               PERFORM 4060-FLAG-PRINCIPAL
               PERFORM 4070-CHECK-NEXT-REVIEW
               PERFORM 4080-FLAG-PRINCIPAL
               PERFORM 4090-ACCUM-STEP-PLAN
               PERFORM 4100-SELECT-PENALTY-PCT
               PERFORM 4110-SPLIT-INTEREST
               PERFORM 4120-COUNT-CYCLE
               PERFORM 4130-ASSESS-SUSPEND
               PERFORM 4140-SET-ELECTION
               PERFORM 4150-DESCRIBE-CHANNEL
               PERFORM 4160-EDIT-CUST-ID
               PERFORM 4170-ACCUM-REGION
               PERFORM 4180-EDIT-CUST-ID
               PERFORM 4190-PRORATE-BUMP
               PERFORM 4200-PRORATE-ADDON
               PERFORM 4210-PRORATE-PENALTY
               PERFORM 4220-AGE-EFF
               PERFORM 4230-LIMIT-INTEREST
               PERFORM 4240-EDIT-DESC
               PERFORM 4250-PRORATE-ADDON
               PERFORM 4260-PRORATE-INTEREST
               PERFORM 4270-SET-SOURCE
               PERFORM 4280-SELECT-RATE
               PERFORM 4290-DESCRIBE-CHANNEL
               PERFORM 4300-EDIT-MEMO
               PERFORM 4310-ACCUM-ELECTION
               PERFORM 4320-EDIT-OPERATOR
               PERFORM 4330-SET-REGION
               PERFORM 4340-AGE-CLOSE
               PERFORM 4350-ACCUM-CHANNEL
               PERFORM 4360-PRORATE-ADDON
               PERFORM 4370-COUNT-CYCLE
               PERFORM 4380-PRORATE-BUMP
               PERFORM 4390-SELECT-MARGIN
               PERFORM 4400-ACCRUE-RATE
               PERFORM 4410-SET-STEP-PLAN
               PERFORM 4420-EDIT-CONTACT
               PERFORM 4430-AGE-LAST-PAY
               PERFORM 4440-LIMIT-ADDON
               PERFORM 4450-SELECT-RATE
               PERFORM 4460-PRORATE-PRINCIPAL
               PERFORM 4470-CHECK-LAST-STMT
               PERFORM 4480-COUNT-CYCLE
               PERFORM 4490-CHECK-EXPIRY
               PERFORM 4500-FLAG-ADDON
               PERFORM 4510-AGE-EFF
               PERFORM 4520-LIMIT-ADDON
               PERFORM 4530-DESCRIBE-SOURCE
               PERFORM 4540-AGE-LAST-STMT
               PERFORM 4550-LIMIT-PENALTY
               PERFORM 4560-DESCRIBE-SOURCE
               PERFORM 4570-SET-CHANNEL
               PERFORM 4580-DESCRIBE-REGION
               PERFORM 4590-PRORATE-ADDON
               PERFORM 4600-ACCRUE-ADDON
               PERFORM 4610-COUNT-RETRY
               PERFORM 7010-CALL-BUVAL
               PERFORM 7000-CALL-BUDTE
               REWRITE BCD-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'BCDMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM BCD-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3000-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BCDT-INTEREST-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BCDT-INTEREST-AMT - (WS-WORK-AMT * 3)
           COMPUTE BCDT-BUMP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3010-DESCRIBE-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'AD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3020-DESCRIBE-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3030-LIMIT-INTEREST.
           IF BCDM-INTEREST-YTD < 25.00
               MOVE 25.00 TO BCDM-INTEREST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-INTEREST-YTD > 2500.00
               MOVE 2500.00 TO BCDM-INTEREST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3040-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-CYCLE-CD
                   ADD BCDT-ADDON-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-ADDON-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON YTD BY STEP PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3050-ACCUM-STEP-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-STEP-PLAN-CD
                   ADD BCDM-ADDON-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-STEP-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-ADDON-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * BUMP AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3060-FLAG-BUMP.
           IF BCDT-BUMP-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDT-BUMP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3070-DESCRIBE-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LFX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MLA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ULA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3080-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO BCDM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E525 DESC REQUIRED
      * E333 ADDON AMT NEGATIVE
      * E603 ADDON AMT OVER 10000
      * E667 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3090-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-DESC = SPACES
                   MOVE 'E525' TO WS-REJECT-CODE
               WHEN BCDT-ADDON-AMT < 0
                   MOVE 'E333' TO WS-REJECT-CODE
               WHEN BCDT-ADDON-AMT > 10000.00
                   MOVE 'E603' TO WS-REJECT-CODE
               WHEN BCDT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E667' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PENALTY AMT.
      *----------------------------------------------------------------*
       3100-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDT-PENALTY-AMT ROUNDED =
                   BCDM-INTEREST-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDT-PENALTY-AMT
           END-IF
           ADD BCDT-PENALTY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BU -> AD WHEN NEXT DUE DATE IS PAST
      * AD -> WD WHEN NEXT DUE DATE IS PAST
      * WD -> MT WHEN NEXT DUE DATE IS PAST
      * MT -> BU WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WD'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MT' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MT'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BU' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3120-DESCRIBE-SOURCE.
           EVALUATE BCDM-SOURCE-CD
               WHEN 'AG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE PENALTY YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3130-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BCDM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-STOP-FLAG = 'Y'
                   COMPUTE BCDM-INTEREST-YTD ROUNDED = BCDM-PENALTY-YTD
                       * BCDM-RATE / 200
               WHEN BCDM-CYCLE-CD = 'A'
                   MOVE 15.00 TO BCDM-INTEREST-YTD
               WHEN BCDM-CYCLE-CD = 'S'
                   MOVE 3.00 TO BCDM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE BCDM-INTEREST-YTD ROUNDED = BCDM-PENALTY-YTD
                       * BCDM-RATE / 100
           END-EVALUATE
           IF BCDM-INTEREST-YTD > 0 AND BCDM-INTEREST-YTD < 10.00
               MOVE 10.00 TO BCDM-INTEREST-YTD
           END-IF
           IF BCDM-INTEREST-YTD > 150.00
               MOVE 150.00 TO BCDM-INTEREST-YTD
           END-IF
           ADD BCDM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E225 DESC REQUIRED
      * E428 WORK AMT NEGATIVE
      * E989 WORK AMT OVER 10000
      * E767 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3140-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-DESC = SPACES
                   MOVE 'E225' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E428' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E989' TO WS-REJECT-CODE
               WHEN BCDM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E767' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE PENALTY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BCDT-PENALTY-AMT NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3160-LIMIT-PENALTY.
           IF BCDT-PENALTY-AMT < 50.00
               MOVE 50.00 TO BCDT-PENALTY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDT-PENALTY-AMT > 50000.00
               MOVE 50000.00 TO BCDT-PENALTY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PENALTY AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-PENALTY.
           COMPUTE WS-WORK-AMT = BCDM-PENALTY-AMT / 12
           COMPUTE WS-WORK-AMT-2 = BCDM-PENALTY-AMT - (WS-WORK-AMT * 12)
           COMPUTE BCDM-ADDON-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3180-COUNT-SEQ.
           IF BCDT-SEQ < 9999999
               ADD 1 TO BCDT-SEQ
           END-IF
           IF BCDT-SEQ NOT < 5
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE INTEREST AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3190-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-REVIEW-FLAG = 'Y'
                   COMPUTE BCDM-PRINCIPAL-YTD ROUNDED =
                       BCDT-INTEREST-AMT * BCDM-MARGIN / 200
               WHEN BCDT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO BCDM-PRINCIPAL-YTD
               WHEN BCDT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO BCDM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE BCDM-PRINCIPAL-YTD ROUNDED =
                       BCDT-INTEREST-AMT * BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDM-PRINCIPAL-YTD > 0 AND BCDM-PRINCIPAL-YTD < 2.00
               MOVE 2.00 TO BCDM-PRINCIPAL-YTD
           END-IF
           IF BCDM-PRINCIPAL-YTD > 35.00
               MOVE 35.00 TO BCDM-PRINCIPAL-YTD
           END-IF
           ADD BCDM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3200-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E954 REF NO REQUIRED
      * E990 INTEREST YTD NEGATIVE
      * E152 INTEREST YTD OVER 99999
      * E338 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3210-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-REF-NO = SPACES
                   MOVE 'E954' TO WS-REJECT-CODE
               WHEN BCDM-INTEREST-YTD < 0
                   MOVE 'E990' TO WS-REJECT-CODE
               WHEN BCDM-INTEREST-YTD > 99999.00
                   MOVE 'E152' TO WS-REJECT-CODE
               WHEN BCDM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E338' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3220-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ADDON AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3230-LIMIT-ADDON.
           IF BCDT-ADDON-AMT < 50.00
               MOVE 50.00 TO BCDT-ADDON-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDT-ADDON-AMT > 500.00
               MOVE 500.00 TO BCDT-ADDON-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3240-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3250-ACCRUE-INTEREST.
           IF BCDM-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-INTEREST-AMT * BCDM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BCDM-BUMP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3260-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EFFECTIVE-DATE > 0 AND BCDM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDT-PENALTY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BCDT-PENALTY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BCDT-PENALTY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BCDT-PENALTY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDT-PENALTY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BGJ -> LFX WHEN POST DATE IS PAST
      * LFX -> MLA WHEN POST DATE IS PAST
      * MLA -> ULA WHEN POST DATE IS PAST
      * ULA -> BGJ WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3270-SET-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LFX' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LFX'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MLA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MLA'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ULA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULA'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BGJ' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PENALTY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PENALTY AMT.
      *----------------------------------------------------------------*
       3280-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDT-PENALTY-AMT ROUNDED =
                   BCDM-PENALTY-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDT-PENALTY-AMT
           END-IF
           ADD BCDT-PENALTY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PENALTY YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3290-LIMIT-PENALTY.
           IF BCDM-PENALTY-YTD < 50.00
               MOVE 50.00 TO BCDM-PENALTY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-PENALTY-YTD > 1000.00
               MOVE 1000.00 TO BCDM-PENALTY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUMP BAND TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3300-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-BAND-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF BCDM-INTEREST-AMT NOT < WS-BUMP-BAND-LOW(WS-TX)
                   MOVE WS-BUMP-BAND-RATE(WS-TX) TO BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUMP BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3310-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-BAND-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-BUMP-BAND-LOW(WS-TX)
                   MOVE WS-BUMP-BAND-RATE(WS-TX) TO BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3320-COUNT-RETRY.
           IF BCDM-RETRY-CNT < 9999999
               ADD 1 TO BCDM-RETRY-CNT
           END-IF
           IF BCDM-RETRY-CNT NOT < 10
               MOVE 'Y' TO BCDM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3330-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUMP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       3340-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-INTEREST-YTD ROUNDED =
                   BCDM-BUMP-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDM-INTEREST-YTD
           END-IF
           ADD BCDM-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE PENALTY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3350-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO BCDM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BCDM-PENALTY-YTD NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO BCDM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3360-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-REGION-CD
                   ADD BCDT-ADDON-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-ADDON-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3370-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO BCDM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3380-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST MAINT DATE IS PAST
      * BAT -> MAL WHEN LAST MAINT DATE IS PAST
      * MAL -> TEL WHEN LAST MAINT DATE IS PAST
      * TEL -> ONL WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3390-SET-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3400-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3410-ACCRUE-INTEREST.
           IF BCDM-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-INTEREST-AMT * BCDM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BCDM-ADDON-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY FEE ON THE ADDON AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3420-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN BCDM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO BCDT-PENALTY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-OVERRIDE-FLAG = 'Y'
                   COMPUTE BCDT-PENALTY-AMT ROUNDED = BCDT-ADDON-AMT *
                       BCDM-MARGIN / 200
               WHEN BCDT-TRAN-CD = 'BU'
                   MOVE 7.00 TO BCDT-PENALTY-AMT
               WHEN BCDT-TRAN-CD = 'AD'
                   MOVE 3.00 TO BCDT-PENALTY-AMT
               WHEN OTHER
                   COMPUTE BCDT-PENALTY-AMT ROUNDED = BCDT-ADDON-AMT *
                       BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDT-PENALTY-AMT > 0 AND BCDT-PENALTY-AMT < 1.00
               MOVE 1.00 TO BCDT-PENALTY-AMT
           END-IF
           IF BCDT-PENALTY-AMT > 100.00
               MOVE 100.00 TO BCDT-PENALTY-AMT
           END-IF
           ADD BCDT-PENALTY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3430-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-ADDON-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-ADDON-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BCDM-ADDON-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BCDM-ADDON-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-ADDON-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E598 NAME REQUIRED
      * E090 PENALTY YTD NEGATIVE
      * E018 PENALTY YTD OVER 25000
      * E919 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3440-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-NAME = SPACES
                   MOVE 'E598' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-YTD < 0
                   MOVE 'E090' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-YTD > 25000.00
                   MOVE 'E018' TO WS-REJECT-CODE
               WHEN BCDM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E919' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE ADDON AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3450-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BCDT-ADDON-AMT NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PENALTY YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3460-ACCRUE-PENALTY.
           IF BCDM-PENALTY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-PENALTY-YTD * BCDM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BCDM-ADDON-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3470-COUNT-CYCLE.
           IF BCDM-CYCLE-CNT < 9999999
               ADD 1 TO BCDM-CYCLE-CNT
           END-IF
           IF BCDM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO BCDM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3480-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BCDM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3490-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3500-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY SCHED TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3510-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-SCHED-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF BCDM-INTEREST-YTD NOT < WS-PENALTY-SCHED-LOW(WS-TX)
                   MOVE WS-PENALTY-SCHED-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ADDON YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3520-LIMIT-ADDON.
           IF BCDM-ADDON-YTD < 100.00
               MOVE 100.00 TO BCDM-ADDON-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-ADDON-YTD > 50000.00
               MOVE 50000.00 TO BCDM-ADDON-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3530-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ELECTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3540-DESCRIBE-ELECTION.
           EVALUATE BCDM-ELECTION-CD
               WHEN 'L'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'R'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN MATURITY DATE IS PAST
      * BAT -> MAL WHEN MATURITY DATE IS PAST
      * MAL -> TEL WHEN MATURITY DATE IS PAST
      * TEL -> ONL WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3550-SET-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3560-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-SOURCE-CD
                   ADD BCDM-ADDON-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-ADDON-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BU -> AD WHEN NEXT DUE DATE IS PAST
      * AD -> WD WHEN NEXT DUE DATE IS PAST
      * WD -> MT WHEN NEXT DUE DATE IS PAST
      * MT -> BU WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3570-SET-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WD'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MT' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MT'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BU' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE BUMP YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDT-CHANNEL-CD
                   ADD BCDM-BUMP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-BUMP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3590-LIMIT-PRINCIPAL.
           IF BCDM-PRINCIPAL-AMT < 100.00
               MOVE 100.00 TO BCDM-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-PRINCIPAL-AMT > 10000.00
               MOVE 10000.00 TO BCDM-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3600-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-SOURCE-CD
                   ADD BCDM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3610-LIMIT-PRINCIPAL.
           IF BCDM-PRINCIPAL-AMT < 0.00
               MOVE 0.00 TO BCDM-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-PRINCIPAL-AMT > 10000.00
               MOVE 10000.00 TO BCDM-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADDON AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3620-ACCRUE-ADDON.
           IF BCDM-ADDON-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-ADDON-AMT * BCDM-RATE / 100 / 365
               ADD WS-WORK-AMT TO BCDT-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3630-ACCRUE-PRINCIPAL.
           IF BCDM-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-PRINCIPAL-AMT * BCDM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BCDM-PENALTY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3640-LIMIT-INTEREST.
           IF BCDT-INTEREST-AMT < 10.00
               MOVE 10.00 TO BCDT-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDT-INTEREST-AMT > 500.00
               MOVE 500.00 TO BCDT-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * EK -> JE WHEN EFF DATE IS PAST
      * JE -> PN WHEN EFF DATE IS PAST
      * PN -> WV WHEN EFF DATE IS PAST
      * WV -> EK WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3650-SET-CHANNEL.
           EVALUATE BCDM-CHANNEL-CD
               WHEN 'EK'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JE' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PN' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PN'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WV' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EK' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PENALTY AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-PENALTY.
           IF BCDM-PENALTY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-PENALTY-AMT * BCDM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BCDT-BUMP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3670-DESCRIBE-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'AD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E347 DESC REQUIRED
      * E839 ADDON AMT NEGATIVE
      * E595 ADDON AMT OVER 50000
      * E026 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3680-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-DESC = SPACES
                   MOVE 'E347' TO WS-REJECT-CODE
               WHEN BCDT-ADDON-AMT < 0
                   MOVE 'E839' TO WS-REJECT-CODE
               WHEN BCDT-ADDON-AMT > 50000.00
                   MOVE 'E595' TO WS-REJECT-CODE
               WHEN BCDM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E026' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUMP YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3690-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = BCDM-BUMP-YTD / 4
           COMPUTE WS-WORK-AMT-2 = BCDM-BUMP-YTD - (WS-WORK-AMT * 4)
           COMPUTE BCDT-BUMP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> S WHEN NEXT DUE DATE IS PAST
      * S -> T WHEN NEXT DUE DATE IS PAST
      * T -> U WHEN NEXT DUE DATE IS PAST
      * U -> X WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3700-SET-CYCLE.
           EVALUATE BCDM-CYCLE-CD
               WHEN 'A'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PENALTY YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3710-LIMIT-PENALTY.
           IF BCDM-PENALTY-YTD < 100.00
               MOVE 100.00 TO BCDM-PENALTY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-PENALTY-YTD > 500.00
               MOVE 500.00 TO BCDM-PENALTY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST MAINT DATE IS PAST
      * BAT -> MAL WHEN LAST MAINT DATE IS PAST
      * MAL -> TEL WHEN LAST MAINT DATE IS PAST
      * TEL -> ONL WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ADDON YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3730-FLAG-ADDON.
           IF BCDM-ADDON-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BCDM-ADDON-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY SCHED TIER FOR THE ADDON YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3740-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-SCHED-RATE(1) TO BCDM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF BCDM-ADDON-YTD NOT < WS-PENALTY-SCHED-LOW(WS-TX)
                   MOVE WS-PENALTY-SCHED-RATE(WS-TX) TO BCDM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3750-ACCRUE-PRINCIPAL.
           IF BCDM-PRINCIPAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-PRINCIPAL-YTD * BCDM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY YTD FEE ON THE INTEREST AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3770-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN BCDM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PENALTY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-STOP-FLAG = 'Y'
                   COMPUTE BCDM-PENALTY-YTD ROUNDED = BCDT-INTEREST-AMT
                       * BCDM-PENALTY-PCT / 200
               WHEN BCDM-ELECTION-CD = 'L'
                   MOVE 15.00 TO BCDM-PENALTY-YTD
               WHEN BCDM-ELECTION-CD = 'M'
                   MOVE 3.00 TO BCDM-PENALTY-YTD
               WHEN OTHER
                   COMPUTE BCDM-PENALTY-YTD ROUNDED = BCDT-INTEREST-AMT
                       * BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF BCDM-PENALTY-YTD > 0 AND BCDM-PENALTY-YTD < 2.00
               MOVE 2.00 TO BCDM-PENALTY-YTD
           END-IF
           IF BCDM-PENALTY-YTD > 150.00
               MOVE 150.00 TO BCDM-PENALTY-YTD
           END-IF
           ADD BCDM-PENALTY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT ADDON AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3780-SPLIT-ADDON.
           COMPUTE WS-WORK-AMT = BCDT-ADDON-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BCDT-ADDON-AMT - (WS-WORK-AMT * 4)
           COMPUTE BCDT-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EXPIRY DATE IS PAST
      * BAT -> MAL WHEN EXPIRY DATE IS PAST
      * MAL -> TEL WHEN EXPIRY DATE IS PAST
      * TEL -> ONL WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BUMP YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3800-FLAG-BUMP.
           IF BCDM-BUMP-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDM-BUMP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY STEP PLAN CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3810-ACCUM-STEP-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-STEP-PLAN-CD
                   ADD BCDM-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-STEP-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY STEP PLAN CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3820-ACCUM-STEP-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-STEP-PLAN-CD
                   ADD BCDM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-STEP-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY FEE ON THE BUMP AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3830-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PENALTY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   COMPUTE BCDM-PENALTY-AMT ROUNDED = BCDT-BUMP-AMT *
                       BCDM-RATE / 200
               WHEN BCDM-CHANNEL-CD = 'EK'
                   MOVE 20.00 TO BCDM-PENALTY-AMT
               WHEN BCDM-CHANNEL-CD = 'JE'
                   MOVE 7.00 TO BCDM-PENALTY-AMT
               WHEN OTHER
                   COMPUTE BCDM-PENALTY-AMT ROUNDED = BCDT-BUMP-AMT *
                       BCDM-RATE / 100
           END-EVALUATE
           IF BCDM-PENALTY-AMT > 0 AND BCDM-PENALTY-AMT < 2.00
               MOVE 2.00 TO BCDM-PENALTY-AMT
           END-IF
           IF BCDM-PENALTY-AMT > 35.00
               MOVE 35.00 TO BCDM-PENALTY-AMT
           END-IF
           ADD BCDM-PENALTY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3840-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDT-ADDON-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD BCDT-ADDON-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BCDT-ADDON-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDT-ADDON-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDT-ADDON-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3850-DESCRIBE-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'LFX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MLA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ULA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BGJ -> LFX WHEN EFF DATE IS PAST
      * LFX -> MLA WHEN EFF DATE IS PAST
      * MLA -> ULA WHEN EFF DATE IS PAST
      * ULA -> BGJ WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3860-SET-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LFX' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LFX'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MLA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MLA'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ULA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULA'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BGJ' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ADDON AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3870-LIMIT-ADDON.
           IF BCDM-ADDON-AMT < 50.00
               MOVE 50.00 TO BCDM-ADDON-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-ADDON-AMT > 500.00
               MOVE 500.00 TO BCDM-ADDON-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STEP PLAN CD:
      * AYX -> DGU WHEN NEXT DUE DATE IS PAST
      * DGU -> GWJ WHEN NEXT DUE DATE IS PAST
      * GWJ -> RJL WHEN NEXT DUE DATE IS PAST
      * RJL -> UPM WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3880-SET-STEP-PLAN.
           EVALUATE BCDM-STEP-PLAN-CD
               WHEN 'AYX'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DGU'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GWJ' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GWJ'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJL' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJL'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UPM' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE ADDON AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3890-ASSESS-STOP.
           EVALUATE TRUE
               WHEN BCDM-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BCDT-ADDON-AMT *
                       BCDM-MARGIN / 200
               WHEN BCDM-CHANNEL-CD = 'EK'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN BCDM-CHANNEL-CD = 'JE'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BCDT-ADDON-AMT *
                       BCDM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON FEE ON THE PENALTY AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3900-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BCDM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BCDT-ADDON-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-REVIEW-FLAG = 'Y'
                   COMPUTE BCDT-ADDON-AMT ROUNDED = BCDT-PENALTY-AMT *
                       BCDM-RATE / 200
               WHEN BCDM-CYCLE-CD = 'A'
                   MOVE 20.00 TO BCDT-ADDON-AMT
               WHEN BCDM-CYCLE-CD = 'S'
                   MOVE 5.00 TO BCDT-ADDON-AMT
               WHEN OTHER
                   COMPUTE BCDT-ADDON-AMT ROUNDED = BCDT-PENALTY-AMT *
                       BCDM-RATE / 100
           END-EVALUATE
           IF BCDT-ADDON-AMT > 0 AND BCDT-ADDON-AMT < 5.00
               MOVE 5.00 TO BCDT-ADDON-AMT
           END-IF
           IF BCDT-ADDON-AMT > 50.00
               MOVE 50.00 TO BCDT-ADDON-AMT
           END-IF
           ADD BCDT-ADDON-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN NEXT REVIEW DATE IS PAST
      * M -> N WHEN NEXT REVIEW DATE IS PAST
      * N -> R WHEN NEXT REVIEW DATE IS PAST
      * R -> V WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3910-SET-ELECTION.
           EVALUATE BCDM-ELECTION-CD
               WHEN 'L'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'M' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'N' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'R' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'V' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PENALTY AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3920-FLAG-PENALTY.
           IF BCDT-PENALTY-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDT-PENALTY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E142 REF NO REQUIRED
      * E879 WORK AMT NEGATIVE
      * E658 WORK AMT OVER 250000
      * E903 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3930-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-REF-NO = SPACES
                   MOVE 'E142' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E879' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E658' TO WS-REJECT-CODE
               WHEN BCDM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E903' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BCDM-INTEREST-AMT / 12
           COMPUTE WS-WORK-AMT-2 = BCDM-INTEREST-AMT - (WS-WORK-AMT *
               12)
           COMPUTE BCDM-BUMP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3950-DESCRIBE-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ADDON AMT.
      *----------------------------------------------------------------*
       3960-PRORATE-ADDON.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDT-ADDON-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDT-ADDON-AMT
           END-IF
           ADD BCDT-ADDON-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN CLOSE DATE IS PAST
      * BAT -> MAL WHEN CLOSE DATE IS PAST
      * MAL -> TEL WHEN CLOSE DATE IS PAST
      * TEL -> ONL WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3970-SET-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3980-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-CYCLE-CD
                   ADD BCDT-PENALTY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-PENALTY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ADDON AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BUMP AMT.
      *----------------------------------------------------------------*
       3990-PRORATE-BUMP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-BUMP-AMT ROUNDED =
                   BCDT-ADDON-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-BUMP-AMT
           END-IF
           ADD BCDM-BUMP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4000-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4010-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-REGION-CD
                   ADD BCDM-PENALTY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-PENALTY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUMP BAND TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4020-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-BAND-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF BCDT-INTEREST-AMT NOT < WS-BUMP-BAND-LOW(WS-TX)
                   MOVE WS-BUMP-BAND-RATE(WS-TX) TO BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUMP AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4030-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-REGION-CD
                   ADD BCDT-BUMP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-BUMP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4040-COUNT-RETRY.
           IF BCDM-RETRY-CNT < 9999999
               ADD 1 TO BCDM-RETRY-CNT
           END-IF
           IF BCDM-RETRY-CNT NOT < 3
               MOVE 'Y' TO BCDM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BUMP AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4050-FLAG-BUMP.
           IF BCDT-BUMP-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDT-BUMP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4060-FLAG-PRINCIPAL.
           IF BCDM-PRINCIPAL-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDM-PRINCIPAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4070-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF BCDM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4080-FLAG-PRINCIPAL.
           IF BCDM-PRINCIPAL-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BCDM-PRINCIPAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON YTD BY STEP PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-STEP-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-STEP-PLAN-CD
                   ADD BCDM-ADDON-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-STEP-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-ADDON-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY SCHED TIER FOR THE ADDON AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4100-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-SCHED-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF BCDT-ADDON-AMT NOT < WS-PENALTY-SCHED-LOW(WS-TX)
                   MOVE WS-PENALTY-SCHED-RATE(WS-TX) TO BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4110-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BCDM-INTEREST-YTD / 3
           COMPUTE WS-WORK-AMT-2 = BCDM-INTEREST-YTD - (WS-WORK-AMT * 3)
           COMPUTE BCDM-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4120-COUNT-CYCLE.
           IF BCDM-CYCLE-CNT < 9999999
               ADD 1 TO BCDM-CYCLE-CNT
           END-IF
           IF BCDM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO BCDM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BUMP FEE ON THE ADDON YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4130-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BCDM-BUMP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-HOLD-FLAG = 'Y'
                   COMPUTE BCDM-BUMP-AMT ROUNDED = BCDM-ADDON-YTD *
                       BCDM-MARGIN / 200
               WHEN BCDT-TRAN-CD = 'BU'
                   MOVE 15.00 TO BCDM-BUMP-AMT
               WHEN BCDT-TRAN-CD = 'AD'
                   MOVE 10.00 TO BCDM-BUMP-AMT
               WHEN OTHER
                   COMPUTE BCDM-BUMP-AMT ROUNDED = BCDM-ADDON-YTD *
                       BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDM-BUMP-AMT > 0 AND BCDM-BUMP-AMT < 2.00
               MOVE 2.00 TO BCDM-BUMP-AMT
           END-IF
           IF BCDM-BUMP-AMT > 50.00
               MOVE 50.00 TO BCDM-BUMP-AMT
           END-IF
           ADD BCDM-BUMP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN POST DATE IS PAST
      * M -> N WHEN POST DATE IS PAST
      * N -> R WHEN POST DATE IS PAST
      * R -> V WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4140-SET-ELECTION.
           EVALUATE BCDM-ELECTION-CD
               WHEN 'L'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4150-DESCRIBE-CHANNEL.
           EVALUATE BCDM-CHANNEL-CD
               WHEN 'EK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E806 CUST ID REQUIRED
      * E444 INTEREST YTD NEGATIVE
      * E571 INTEREST YTD OVER 250000
      * E013 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4160-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-CUST-ID = SPACES
                   MOVE 'E806' TO WS-REJECT-CODE
               WHEN BCDM-INTEREST-YTD < 0
                   MOVE 'E444' TO WS-REJECT-CODE
               WHEN BCDM-INTEREST-YTD > 250000.00
                   MOVE 'E571' TO WS-REJECT-CODE
               WHEN BCDM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E013' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4170-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-REGION-CD
                   ADD BCDM-PENALTY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-PENALTY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E453 CUST ID REQUIRED
      * E756 ADDON YTD NEGATIVE
      * E485 ADDON YTD OVER 10000
      * E377 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4180-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-CUST-ID = SPACES
                   MOVE 'E453' TO WS-REJECT-CODE
               WHEN BCDM-ADDON-YTD < 0
                   MOVE 'E756' TO WS-REJECT-CODE
               WHEN BCDM-ADDON-YTD > 10000.00
                   MOVE 'E485' TO WS-REJECT-CODE
               WHEN BCDM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E377' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUMP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BUMP AMT.
      *----------------------------------------------------------------*
       4190-PRORATE-BUMP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDT-BUMP-AMT ROUNDED =
                   BCDM-BUMP-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDT-BUMP-AMT
           END-IF
           ADD BCDT-BUMP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE ADDON AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4200-PRORATE-ADDON.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-ADDON-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PENALTY AMT.
      *----------------------------------------------------------------*
       4210-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-PENALTY-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDM-PENALTY-AMT
           END-IF
           ADD BCDM-PENALTY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4220-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDT-PENALTY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD BCDT-PENALTY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BCDT-PENALTY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDT-PENALTY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDT-PENALTY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4230-LIMIT-INTEREST.
           IF BCDT-INTEREST-AMT < 50.00
               MOVE 50.00 TO BCDT-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDT-INTEREST-AMT > 5000.00
               MOVE 5000.00 TO BCDT-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E501 DESC REQUIRED
      * E384 PENALTY AMT NEGATIVE
      * E782 PENALTY AMT OVER 25000
      * E257 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4240-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-DESC = SPACES
                   MOVE 'E501' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-AMT < 0
                   MOVE 'E384' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-AMT > 25000.00
                   MOVE 'E782' TO WS-REJECT-CODE
               WHEN BCDM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E257' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ADDON AMT.
      *----------------------------------------------------------------*
       4250-PRORATE-ADDON.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDT-ADDON-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDT-ADDON-AMT
           END-IF
           ADD BCDT-ADDON-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE PENALTY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       4260-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-INTEREST-YTD ROUNDED =
                   BCDM-PENALTY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDM-INTEREST-YTD
           END-IF
           ADD BCDM-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN POST DATE IS PAST
      * BF -> HF WHEN POST DATE IS PAST
      * HF -> HR WHEN POST DATE IS PAST
      * HR -> UF WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4270-SET-SOURCE.
           EVALUATE BCDM-SOURCE-CD
               WHEN 'AG'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE BUMP BAND TIER FOR THE BUMP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4280-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-BAND-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF BCDM-BUMP-AMT NOT < WS-BUMP-BAND-LOW(WS-TX)
                   MOVE WS-BUMP-BAND-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4290-DESCRIBE-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E043 MEMO REQUIRED
      * E649 ADDON AMT NEGATIVE
      * E517 ADDON AMT OVER 1000000
      * E656 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4300-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-MEMO = SPACES
                   MOVE 'E043' TO WS-REJECT-CODE
               WHEN BCDM-ADDON-AMT < 0
                   MOVE 'E649' TO WS-REJECT-CODE
               WHEN BCDM-ADDON-AMT > 1000000.00
                   MOVE 'E517' TO WS-REJECT-CODE
               WHEN BCDM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E656' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY AMT BY ELECTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4310-ACCUM-ELECTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-ELECTION-CD
                   ADD BCDT-PENALTY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-ELECTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-PENALTY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E210 OPERATOR REQUIRED
      * E165 ADDON AMT NEGATIVE
      * E983 ADDON AMT OVER 50000
      * E626 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4320-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-OPERATOR = SPACES
                   MOVE 'E210' TO WS-REJECT-CODE
               WHEN BCDT-ADDON-AMT < 0
                   MOVE 'E165' TO WS-REJECT-CODE
               WHEN BCDT-ADDON-AMT > 50000.00
                   MOVE 'E983' TO WS-REJECT-CODE
               WHEN BCDM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E626' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BGJ -> LFX WHEN POST DATE IS PAST
      * LFX -> MLA WHEN POST DATE IS PAST
      * MLA -> ULA WHEN POST DATE IS PAST
      * ULA -> BGJ WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4330-SET-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LFX' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LFX'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MLA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MLA'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ULA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULA'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BGJ' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4340-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4350-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDT-CHANNEL-CD
                   ADD BCDT-PENALTY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-PENALTY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ADDON YTD.
      *----------------------------------------------------------------*
       4360-PRORATE-ADDON.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-ADDON-YTD ROUNDED =
                   BCDM-PRINCIPAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-ADDON-YTD
           END-IF
           ADD BCDM-ADDON-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4370-COUNT-CYCLE.
           IF BCDM-CYCLE-CNT < 9999999
               ADD 1 TO BCDM-CYCLE-CNT
           END-IF
           IF BCDM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BUMP AMT.
      *----------------------------------------------------------------*
       4380-PRORATE-BUMP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-BUMP-AMT ROUNDED =
                   BCDT-INTEREST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDM-BUMP-AMT
           END-IF
           ADD BCDM-BUMP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY SCHED TIER FOR THE BUMP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4390-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-SCHED-RATE(1) TO BCDM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF BCDM-BUMP-YTD NOT < WS-PENALTY-SCHED-LOW(WS-TX)
                   MOVE WS-PENALTY-SCHED-RATE(WS-TX) TO BCDM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4400-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * BCDM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BCDT-ADDON-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STEP PLAN CD:
      * AYX -> DGU WHEN EFFECTIVE DATE IS PAST
      * DGU -> GWJ WHEN EFFECTIVE DATE IS PAST
      * GWJ -> RJL WHEN EFFECTIVE DATE IS PAST
      * RJL -> UPM WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4410-SET-STEP-PLAN.
           EVALUATE BCDM-STEP-PLAN-CD
               WHEN 'AYX'
                   IF BCDM-EFFECTIVE-DATE > 0 AND BCDM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DGU'
                   IF BCDM-EFFECTIVE-DATE > 0 AND BCDM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GWJ' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GWJ'
                   IF BCDM-EFFECTIVE-DATE > 0 AND BCDM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJL' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJL'
                   IF BCDM-EFFECTIVE-DATE > 0 AND BCDM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UPM' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E898 CONTACT REQUIRED
      * E825 BUMP AMT NEGATIVE
      * E433 BUMP AMT OVER 5000
      * E341 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4420-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-CONTACT = SPACES
                   MOVE 'E898' TO WS-REJECT-CODE
               WHEN BCDT-BUMP-AMT < 0
                   MOVE 'E825' TO WS-REJECT-CODE
               WHEN BCDT-BUMP-AMT > 5000.00
                   MOVE 'E433' TO WS-REJECT-CODE
               WHEN BCDM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E341' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4430-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ADDON YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4440-LIMIT-ADDON.
           IF BCDM-ADDON-YTD < 25.00
               MOVE 25.00 TO BCDM-ADDON-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-ADDON-YTD > 5000.00
               MOVE 5000.00 TO BCDM-ADDON-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY SCHED TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4450-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-SCHED-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF BCDT-INTEREST-AMT NOT < WS-PENALTY-SCHED-LOW(WS-TX)
                   MOVE WS-PENALTY-SCHED-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUMP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       4460-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-PRINCIPAL-AMT ROUNDED =
                   BCDM-BUMP-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDM-PRINCIPAL-AMT
           END-IF
           ADD BCDM-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4470-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4480-COUNT-CYCLE.
           IF BCDM-CYCLE-CNT < 9999999
               ADD 1 TO BCDM-CYCLE-CNT
           END-IF
           IF BCDM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4490-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ADDON AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4500-FLAG-ADDON.
           IF BCDM-ADDON-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDM-ADDON-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4510-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ADDON YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4520-LIMIT-ADDON.
           IF BCDM-ADDON-YTD < 50.00
               MOVE 50.00 TO BCDM-ADDON-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-ADDON-YTD > 10000.00
               MOVE 10000.00 TO BCDM-ADDON-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4530-DESCRIBE-SOURCE.
           EVALUATE BCDM-SOURCE-CD
               WHEN 'AG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4540-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PENALTY YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4550-LIMIT-PENALTY.
           IF BCDM-PENALTY-YTD < 50.00
               MOVE 50.00 TO BCDM-PENALTY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-PENALTY-YTD > 500.00
               MOVE 500.00 TO BCDM-PENALTY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4560-DESCRIBE-SOURCE.
           EVALUATE BCDM-SOURCE-CD
               WHEN 'AG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * EK -> JE WHEN LAST PAY DATE IS PAST
      * JE -> PN WHEN LAST PAY DATE IS PAST
      * PN -> WV WHEN LAST PAY DATE IS PAST
      * WV -> EK WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4570-SET-CHANNEL.
           EVALUATE BCDM-CHANNEL-CD
               WHEN 'EK'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JE' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PN' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PN'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WV' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EK' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4580-DESCRIBE-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LFX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MLA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ULA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ADDON YTD.
      *----------------------------------------------------------------*
       4590-PRORATE-ADDON.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-ADDON-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-ADDON-YTD
           END-IF
           ADD BCDM-ADDON-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADDON AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4600-ACCRUE-ADDON.
           IF BCDT-ADDON-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDT-ADDON-AMT * BCDM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BCDT-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4610-COUNT-RETRY.
           IF BCDM-RETRY-CNT < 9999999
               ADD 1 TO BCDM-RETRY-CNT
           END-IF
           IF BCDM-RETRY-CNT NOT < 6
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE BCD FIELDS TO BUVAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-BUVAL.
           INITIALIZE BUVAL-PARMS
           MOVE BCDM-PENALTY-AMT
             TO BUVAL-PENALTY-AMT
           MOVE BCDM-INTEREST-YTD
             TO BUVAL-INTEREST-YTD
           MOVE BCDM-BUMP-AMT
             TO BUVAL-BUMP-AMT
           MOVE BCDM-PRINCIPAL-YTD
             TO BUVAL-PRINCIPAL-YTD
           MOVE BCDM-ADDON-YTD
             TO BUVAL-ADDON-YTD
           MOVE BCDM-PRINCIPAL-AMT
             TO BUVAL-PRINCIPAL-AMT
           MOVE BCDM-LAST-STMT-DATE
             TO BUVAL-LAST-STMT-DATE
           MOVE BCDM-LAST-MAINT-DATE
             TO BUVAL-LAST-MAINT-DATE
           MOVE BCDM-EFFECTIVE-DATE
             TO BUVAL-EFFECTIVE-DATE
           MOVE BCDM-LAST-PAY-DATE
             TO BUVAL-LAST-PAY-DATE
           MOVE BCDM-STEP-PLAN-CD
             TO BUVAL-STEP-PLAN-CD
           MOVE BCDM-CHANNEL-CD
             TO BUVAL-CHANNEL-CD
           MOVE BCDM-SOURCE-CD
             TO BUVAL-SOURCE-CD
           MOVE BCDM-REGION-CD
             TO BUVAL-REGION-CD
           MOVE BCDM-EXEMPT-FLAG
             TO BUVAL-EXEMPT-FLAG
           MOVE BCDM-STOP-FLAG
             TO BUVAL-STOP-FLAG
           MOVE BCDM-OVERRIDE-FLAG
             TO BUVAL-OVERRIDE-FLAG
           MOVE BCDM-CYCLE-CNT
             TO BUVAL-CYCLE-CNT
           MOVE BCDM-ITEM-CNT
             TO BUVAL-ITEM-CNT
           MOVE BCDM-RATE
             TO BUVAL-RATE
           MOVE BCDM-MARGIN
             TO BUVAL-MARGIN
           MOVE BCDM-DESC
             TO BUVAL-DESC
           MOVE BCDM-NAME
             TO BUVAL-NAME
           CALL 'BUVAL' USING BUVAL-PARMS
           EVALUATE BUVAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE BUVAL-PENALTY-AMT
                     TO BCDM-PENALTY-AMT
                   MOVE BUVAL-INTEREST-YTD
                     TO BCDM-INTEREST-YTD
                   MOVE BUVAL-PRINCIPAL-YTD
                     TO BCDM-PRINCIPAL-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-BUVAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE BCD FIELDS TO BUDTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-BUDTE.
           INITIALIZE BUDTE-PARMS
           MOVE BCDM-BUMP-YTD
             TO BUDTE-BUMP-YTD
           MOVE BCDM-BUMP-AMT
             TO BUDTE-BUMP-AMT
           MOVE BCDM-PRINCIPAL-YTD
             TO BUDTE-PRINCIPAL-YTD
           MOVE BCDM-ADDON-AMT
             TO BUDTE-ADDON-AMT
           MOVE BCDM-PENALTY-YTD
             TO BUDTE-PENALTY-YTD
           MOVE BCDM-INTEREST-YTD
             TO BUDTE-INTEREST-YTD
           MOVE BCDM-LAST-MAINT-DATE
             TO BUDTE-LAST-MAINT-DATE
           MOVE BCDM-LAST-PAY-DATE
             TO BUDTE-LAST-PAY-DATE
           MOVE BCDM-EXPIRY-DATE
             TO BUDTE-EXPIRY-DATE
           MOVE BCDM-LAST-STMT-DATE
             TO BUDTE-LAST-STMT-DATE
           MOVE BCDM-ELECTION-CD
             TO BUDTE-ELECTION-CD
           MOVE BCDM-SOURCE-CD
             TO BUDTE-SOURCE-CD
           MOVE BCDM-REGION-CD
             TO BUDTE-REGION-CD
           MOVE BCDM-CYCLE-CD
             TO BUDTE-CYCLE-CD
           MOVE BCDM-REVIEW-FLAG
             TO BUDTE-REVIEW-FLAG
           MOVE BCDM-OVERRIDE-FLAG
             TO BUDTE-OVERRIDE-FLAG
           MOVE BCDM-SUSPEND-FLAG
             TO BUDTE-SUSPEND-FLAG
           MOVE BCDM-CYCLE-CNT
             TO BUDTE-CYCLE-CNT
           MOVE BCDM-EXCEPT-CNT
             TO BUDTE-EXCEPT-CNT
           MOVE BCDM-MARGIN
             TO BUDTE-MARGIN
           MOVE BCDM-PENALTY-PCT
             TO BUDTE-PENALTY-PCT
           MOVE BCDM-NAME
             TO BUDTE-NAME
           MOVE BCDM-DESC
             TO BUDTE-DESC
           CALL 'BUDTE' USING BUDTE-PARMS
           EVALUATE BUDTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE BUDTE-BUMP-YTD
                     TO BCDM-BUMP-YTD
                   MOVE BUDTE-BUMP-AMT
                     TO BCDM-BUMP-AMT
                   MOVE BUDTE-INTEREST-YTD
                     TO BCDM-INTEREST-YTD
                   MOVE BUDTE-REVIEW-FLAG
                     TO BCDM-REVIEW-FLAG
                   MOVE BUDTE-SUSPEND-FLAG
                     TO BCDM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-BUDTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE BCDT-KEY TO RD-KEY
           MOVE BCDT-TRAN-CD TO RD-CODE
           MOVE BCDT-INTEREST-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE BCDT-KEY TO RD-KEY
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
           DISPLAY 'BUDEDIT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'BUDEDIT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
