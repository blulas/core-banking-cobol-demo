      *================================================================*
      * PROGRAM  : BUDPOST                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : BUMP-UP CERTIFICATES - DAILY POSTING. READS THE BCD *
      *            TRANSACTIONS, UPDATES THE BCD MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. BUMP-UP AND STEP-RATE           *
      *            CERTIFICATE RATE ELECTION AND ACCRUAL               *
      * INPUT    : BCDMST (KSDS), BCDTRN, BANKCTL                      *
      * OUTPUT   : BCDOUT, BUDPORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, BUDTE, BUTIER, BUVAL    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-08-25 LMB  ORIGINAL                                       *
      * 1994-07-03 TWH  BUMP TABLE EXPANDED CHG5705                    *
      * 1997-04-21 LMB  PENALTY LIMIT REVISED INC7332                  *
      * 1997-03-23 EWS  AUDIT FINDING - ADDON CONTROL TOTALS PRB0223   *
      * 1997-11-20 CRP  PENALTY CALCULATION CORRECTED CHG3561          *
      * 1999-03-18 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-11-12 RJT  ADDON STATUS CODES ADDED INC3094               *
      * 2004-12-15 MKL  BUMP ROUNDING - HALF UP PRB6846                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUDPOST.
       AUTHOR. D P SHAW.

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
           SELECT REPORT-FILE ASSIGN TO "BUDPORPT"
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
      * WS-PENALTY-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PENALTY-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000357500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000207000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000538000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000137500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020150000000247000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020200000000564000'.
       01  WS-PENALTY-TIER-TBL REDEFINES WS-PENALTY-TIER-VALUES.
           05  WS-PENALTY-TIER-ROW OCCURS 6 TIMES.
               10  WS-PENALTY-TIER-LOW     PIC 9(09)V99.
               10  WS-PENALTY-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ADDON-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ADDON-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000006000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000226500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000035000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011050000000556000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011550000000714000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012050000000624500'.
       01  WS-ADDON-TIER-TBL REDEFINES WS-ADDON-TIER-VALUES.
           05  WS-ADDON-TIER-ROW OCCURS 6 TIMES.
               10  WS-ADDON-TIER-LOW       PIC 9(09)V99.
               10  WS-ADDON-TIER-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PRINCIPA-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINCIPA-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000022500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000514500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000028500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002250000000842500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002350000000907500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004850000000869500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004950000000088500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005950000000328000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006200000000139000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008700000000564500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009700000000715500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010700000000799000'.
       01  WS-PRINCIPA-SCHED-TBL REDEFINES WS-PRINCIPA-SCHED-VALUES.
           05  WS-PRINCIPA-SCHED-ROW OCCURS 12 TIMES.
               10  WS-PRINCIPA-SCHED-LOW   PIC 9(09)V99.
               10  WS-PRINCIPA-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-BUMP-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BUMP-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000769500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000257500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000107500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003750000000655000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003800000000239000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008800000000325000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008850000000361500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008900000000307500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009150000000208000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009650000000387000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012150000000578500'.
       01  WS-BUMP-SCHED-TBL REDEFINES WS-BUMP-SCHED-VALUES.
           05  WS-BUMP-SCHED-ROW OCCURS 11 TIMES.
               10  WS-BUMP-SCHED-LOW       PIC 9(09)V99.
               10  WS-BUMP-SCHED-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  BUDPOST  BUMP-UP CERTIFICATES'.
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
       COPY BUDTEP.
       COPY BUTIERP.
       COPY BUVALP.

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
               PERFORM 3000-PRORATE-PENALTY
               PERFORM 3010-PRORATE-INTEREST
               PERFORM 3020-CHECK-EFF
               PERFORM 3030-ACCRUE-MARGIN
               PERFORM 3040-PRORATE-INTEREST
               PERFORM 3050-ACCUM-SOURCE
               PERFORM 3060-ASSESS-OVERRIDE
               PERFORM 3070-PRORATE-INTEREST
               PERFORM 3080-SELECT-PENALTY-PCT
               PERFORM 3090-DESCRIBE-CHANNEL
               PERFORM 3100-CHECK-EXPIRY
               PERFORM 3110-PRORATE-PRINCIPAL
               PERFORM 3120-SPLIT-ADDON
               PERFORM 3130-AGE-LAST-PAY
               PERFORM 3140-ACCRUE-ADDON
               PERFORM 3150-ASSESS-REVIEW
               PERFORM 3160-ACCRUE-ADDON
               PERFORM 3170-FLAG-PENALTY
               PERFORM 3180-EDIT-OPERATOR
               PERFORM 3190-SET-TRAN
               PERFORM 3200-COUNT-EXCEPT
               PERFORM 3210-CHECK-EXPIRY
               PERFORM 3220-SELECT-RATE
               PERFORM 3230-SELECT-MARGIN
               PERFORM 3240-LIMIT-PRINCIPAL
               PERFORM 3250-CHECK-NEXT-REVIEW
               PERFORM 3260-FLAG-INTEREST
               PERFORM 3270-CHECK-LAST-PAY
               PERFORM 3280-SPLIT-INTEREST
               PERFORM 3290-SET-CHANNEL
               PERFORM 3300-COUNT-SEQ
               PERFORM 3310-SPLIT-PRINCIPAL
               PERFORM 3320-SELECT-RATE
               PERFORM 3330-CHECK-EFFECTIVE
               PERFORM 3340-CHECK-LAST-STMT
               PERFORM 3350-SELECT-PENALTY-PCT
               PERFORM 3360-FLAG-BUMP
               PERFORM 3370-ACCRUE-RATE
               PERFORM 3380-SELECT-PENALTY-PCT
               PERFORM 3390-EDIT-MEMO
               PERFORM 3400-DESCRIBE-REGION
               PERFORM 3410-LIMIT-INTEREST
               PERFORM 3420-SET-CHANNEL
               PERFORM 3430-FLAG-INTEREST
               PERFORM 3440-FLAG-INTEREST
               PERFORM 3450-SET-STEP-PLAN
               PERFORM 3460-ACCRUE-ADDON
               PERFORM 3470-SET-SOURCE
               PERFORM 3480-LIMIT-ITEM
               PERFORM 3490-FLAG-BUMP
               PERFORM 3500-PRORATE-BUMP
               PERFORM 3510-ACCUM-CYCLE
               PERFORM 3520-ACCRUE-BUMP
               PERFORM 3530-EDIT-DESC
               PERFORM 3540-SET-CYCLE
               PERFORM 3550-ASSESS-SUSPEND
               PERFORM 3560-ASSESS-HOLD
               PERFORM 3570-EDIT-ADDR
               PERFORM 3580-ACCUM-TRAN
               PERFORM 3590-AGE-CLOSE
               PERFORM 3600-SPLIT-PRINCIPAL
               PERFORM 3610-PRORATE-ADDON
               PERFORM 3620-COUNT-EXCEPT
               PERFORM 3630-ASSESS-REVIEW
               PERFORM 3640-SPLIT-PENALTY
               PERFORM 3650-SELECT-RATE
               PERFORM 3660-PRORATE-ADDON
               PERFORM 3670-ASSESS-HOLD
               PERFORM 3680-COUNT-EXCEPT
               PERFORM 3690-LIMIT-ADDON
               PERFORM 3700-SELECT-MARGIN
               PERFORM 3710-FLAG-INTEREST
               PERFORM 3720-DESCRIBE-CYCLE
               PERFORM 3730-COUNT-CYCLE
               PERFORM 3740-SPLIT-BUMP
               PERFORM 3750-SET-ELECTION
               PERFORM 3760-SET-STEP-PLAN
               PERFORM 3770-SET-SOURCE
               PERFORM 3780-ASSESS-REVIEW
               PERFORM 3790-ASSESS-SUSPEND
               PERFORM 3800-SELECT-RATE
               PERFORM 3810-DESCRIBE-CHANNEL
               PERFORM 3820-ACCRUE-PENALTY
               PERFORM 3830-SELECT-MARGIN
               PERFORM 3840-EDIT-ADDR
               PERFORM 3850-SET-TRAN
               PERFORM 3860-ASSESS-STOP
               PERFORM 3870-ASSESS-SUSPEND
               PERFORM 3880-AGE-NEXT-DUE
               PERFORM 3890-ACCUM-CHANNEL
               PERFORM 3900-SELECT-MARGIN
               PERFORM 3910-FLAG-INTEREST
               PERFORM 3920-SET-STEP-PLAN
               PERFORM 3930-PRORATE-BUMP
               PERFORM 3940-ACCUM-ELECTION
               PERFORM 3950-CHECK-MATURITY
               PERFORM 3960-FLAG-BUMP
               PERFORM 3970-ACCRUE-PENALTY
               PERFORM 3980-ACCRUE-BUMP
               PERFORM 3990-AGE-EFF
               PERFORM 4000-SELECT-RATE
               PERFORM 4010-FLAG-ADDON
               PERFORM 4020-DESCRIBE-ELECTION
               PERFORM 4030-COUNT-ITEM
               PERFORM 4040-CHECK-NEXT-REVIEW
               PERFORM 4050-COUNT-CYCLE
               PERFORM 4060-COUNT-RETRY
               PERFORM 4070-SPLIT-INTEREST
               PERFORM 4080-FLAG-PENALTY
               PERFORM 4090-ASSESS-REVERSAL
               PERFORM 4100-ACCUM-REGION
               PERFORM 4110-SET-ELECTION
               PERFORM 4120-FLAG-ADDON
               PERFORM 4130-CHECK-LAST-MAINT
               PERFORM 4140-AGE-MATURITY
               PERFORM 4150-LIMIT-ADDON
               PERFORM 4160-EDIT-DESC
               PERFORM 4170-ASSESS-OVERRIDE
               PERFORM 4180-EDIT-MEMO
               PERFORM 4190-LIMIT-BUMP
               PERFORM 4200-CHECK-LAST-MAINT
               PERFORM 4210-EDIT-DESC
               PERFORM 4220-ACCRUE-INTEREST
               PERFORM 4230-LIMIT-ITEM
               PERFORM 4240-ASSESS-OVERRIDE
               PERFORM 4250-AGE-EXPIRY
               PERFORM 4260-AGE-MATURITY
               PERFORM 4270-CHECK-MATURITY
               PERFORM 4280-ASSESS-SUSPEND
               PERFORM 4290-ASSESS-REVIEW
               PERFORM 4300-SET-REGION
               PERFORM 4310-SPLIT-ADDON
               PERFORM 4320-DESCRIBE-SOURCE
               PERFORM 4330-ACCUM-CYCLE
               PERFORM 4340-LIMIT-PRINCIPAL
               PERFORM 7000-CALL-BUDTE
               PERFORM 7040-CALL-BUTIER
               PERFORM 7010-CALL-BUVAL
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
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PENALTY AMT.
      *----------------------------------------------------------------*
       3000-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-PENALTY-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDM-PENALTY-AMT
           END-IF
           ADD BCDM-PENALTY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE INTEREST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3010-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-INTEREST-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3020-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF BCDT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3030-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * BCDM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BCDM-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUMP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       3040-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDT-INTEREST-AMT ROUNDED =
                   BCDM-BUMP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDT-INTEREST-AMT
           END-IF
           ADD BCDT-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3050-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-SOURCE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY YTD FEE ON THE BUMP AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3060-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BCDM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PENALTY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   COMPUTE BCDM-PENALTY-YTD ROUNDED = BCDM-BUMP-AMT *
                       BCDM-PENALTY-PCT / 200
               WHEN BCDM-ELECTION-CD = 'L'
                   MOVE 15.00 TO BCDM-PENALTY-YTD
               WHEN BCDM-ELECTION-CD = 'M'
                   MOVE 10.00 TO BCDM-PENALTY-YTD
               WHEN OTHER
                   COMPUTE BCDM-PENALTY-YTD ROUNDED = BCDM-BUMP-AMT *
                       BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF BCDM-PENALTY-YTD > 0 AND BCDM-PENALTY-YTD < 5.00
               MOVE 5.00 TO BCDM-PENALTY-YTD
           END-IF
           IF BCDM-PENALTY-YTD > 35.00
               MOVE 35.00 TO BCDM-PENALTY-YTD
           END-IF
           ADD BCDM-PENALTY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE BUMP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       3070-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-INTEREST-YTD ROUNDED =
                   BCDM-BUMP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-INTEREST-YTD
           END-IF
           ADD BCDM-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA SCHED TIER FOR THE PENALTY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3080-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-SCHED-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BCDT-PENALTY-AMT NOT < WS-PRINCIPA-SCHED-LOW(WS-TX)
                   MOVE WS-PRINCIPA-SCHED-RATE(WS-TX) TO
                       BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3090-DESCRIBE-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-EXPIRY.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO BCDM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PENALTY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       3110-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-PRINCIPAL-AMT ROUNDED =
                   BCDM-PENALTY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-PRINCIPAL-AMT
           END-IF
           ADD BCDM-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT ADDON YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3120-SPLIT-ADDON.
           COMPUTE WS-WORK-AMT = BCDM-ADDON-YTD / 12
           COMPUTE WS-WORK-AMT-2 = BCDM-ADDON-YTD - (WS-WORK-AMT * 12)
           COMPUTE BCDM-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3130-AGE-LAST-PAY.
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
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADDON YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3140-ACCRUE-ADDON.
           IF BCDM-ADDON-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-ADDON-YTD * BCDM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BCDT-PENALTY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON FEE ON THE WORK AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3150-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN BCDM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO BCDT-ADDON-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-OVERRIDE-FLAG = 'Y'
                   COMPUTE BCDT-ADDON-AMT ROUNDED = WS-WORK-AMT *
                       BCDM-RATE / 200
               WHEN BCDT-TRAN-CD = 'BU'
                   MOVE 20.00 TO BCDT-ADDON-AMT
               WHEN BCDT-TRAN-CD = 'AD'
                   MOVE 7.00 TO BCDT-ADDON-AMT
               WHEN OTHER
                   COMPUTE BCDT-ADDON-AMT ROUNDED = WS-WORK-AMT *
                       BCDM-RATE / 100
           END-EVALUATE
           IF BCDT-ADDON-AMT > 0 AND BCDT-ADDON-AMT < 1.00
               MOVE 1.00 TO BCDT-ADDON-AMT
           END-IF
           IF BCDT-ADDON-AMT > 150.00
               MOVE 150.00 TO BCDT-ADDON-AMT
           END-IF
           ADD BCDT-ADDON-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADDON AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-ADDON.
           IF BCDM-ADDON-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-ADDON-AMT * BCDM-RATE / 100 / 365
               ADD WS-WORK-AMT TO BCDT-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3170-FLAG-PENALTY.
           IF BCDT-PENALTY-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDT-PENALTY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E123 OPERATOR REQUIRED
      * E288 INTEREST AMT NEGATIVE
      * E098 INTEREST AMT OVER 50000
      * E691 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3180-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-OPERATOR = SPACES
                   MOVE 'E123' TO WS-REJECT-CODE
               WHEN BCDT-INTEREST-AMT < 0
                   MOVE 'E288' TO WS-REJECT-CODE
               WHEN BCDT-INTEREST-AMT > 50000.00
                   MOVE 'E098' TO WS-REJECT-CODE
               WHEN BCDM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E691' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BU -> AD WHEN MATURITY DATE IS PAST
      * AD -> WD WHEN MATURITY DATE IS PAST
      * WD -> MT WHEN MATURITY DATE IS PAST
      * MT -> BU WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3190-SET-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WD'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MT' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MT'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BU' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3200-COUNT-EXCEPT.
           IF BCDM-EXCEPT-CNT < 9999999
               ADD 1 TO BCDM-EXCEPT-CNT
           END-IF
           IF BCDM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO BCDM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3210-CHECK-EXPIRY.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA SCHED TIER FOR THE BUMP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3220-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-SCHED-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BCDM-BUMP-YTD NOT < WS-PRINCIPA-SCHED-LOW(WS-TX)
                   MOVE WS-PRINCIPA-SCHED-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUMP SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3230-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-SCHED-RATE(1) TO BCDM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-BUMP-SCHED-LOW(WS-TX)
                   MOVE WS-BUMP-SCHED-RATE(WS-TX) TO BCDM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3240-LIMIT-PRINCIPAL.
           IF BCDM-PRINCIPAL-AMT < 0.00
               MOVE 0.00 TO BCDM-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-PRINCIPAL-AMT > 1000.00
               MOVE 1000.00 TO BCDM-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3250-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO BCDM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3260-FLAG-INTEREST.
           IF BCDM-INTEREST-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BCDM-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3270-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-PAY-DATE TO DTU-DATE-1
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
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3280-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE BCDM-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3290-SET-CHANNEL.
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
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-SEQ.
           IF BCDT-SEQ < 9999999
               ADD 1 TO BCDT-SEQ
           END-IF
           IF BCDT-SEQ NOT < 5
               MOVE 'Y' TO BCDM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3310-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE BCDM-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA SCHED TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3320-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-SCHED-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BCDM-INTEREST-YTD NOT < WS-PRINCIPA-SCHED-LOW(WS-TX)
                   MOVE WS-PRINCIPA-SCHED-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3330-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-LAST-STMT.
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
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA SCHED TIER FOR THE ADDON YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3350-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-SCHED-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BCDM-ADDON-YTD NOT < WS-PRINCIPA-SCHED-LOW(WS-TX)
                   MOVE WS-PRINCIPA-SCHED-RATE(WS-TX) TO
                       BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BUMP AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3360-FLAG-BUMP.
           IF BCDT-BUMP-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDT-BUMP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3370-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * BCDM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BCDM-PENALTY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUMP SCHED TIER FOR THE PENALTY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3380-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-SCHED-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF BCDM-PENALTY-AMT NOT < WS-BUMP-SCHED-LOW(WS-TX)
                   MOVE WS-BUMP-SCHED-RATE(WS-TX) TO BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E988 MEMO REQUIRED
      * E537 PRINCIPAL AMT NEGATIVE
      * E241 PRINCIPAL AMT OVER 5000
      * E975 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3390-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-MEMO = SPACES
                   MOVE 'E988' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-AMT < 0
                   MOVE 'E537' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-AMT > 5000.00
                   MOVE 'E241' TO WS-REJECT-CODE
               WHEN BCDM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E975' TO WS-REJECT-CODE
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
       3400-DESCRIBE-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LFX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MLA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ULA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3410-LIMIT-INTEREST.
           IF BCDM-INTEREST-YTD < 10.00
               MOVE 10.00 TO BCDM-INTEREST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-INTEREST-YTD > 1000.00
               MOVE 1000.00 TO BCDM-INTEREST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * EK -> JE WHEN POST DATE IS PAST
      * JE -> PN WHEN POST DATE IS PAST
      * PN -> WV WHEN POST DATE IS PAST
      * WV -> EK WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3420-SET-CHANNEL.
           EVALUATE BCDM-CHANNEL-CD
               WHEN 'EK'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JE' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PN' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PN'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WV' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EK' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3430-FLAG-INTEREST.
           IF BCDM-INTEREST-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BCDM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3440-FLAG-INTEREST.
           IF BCDM-INTEREST-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STEP PLAN CD:
      * AYX -> DGU WHEN LAST STMT DATE IS PAST
      * DGU -> GWJ WHEN LAST STMT DATE IS PAST
      * GWJ -> RJL WHEN LAST STMT DATE IS PAST
      * RJL -> UPM WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3450-SET-STEP-PLAN.
           EVALUATE BCDM-STEP-PLAN-CD
               WHEN 'AYX'
                   IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DGU'
                   IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GWJ' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GWJ'
                   IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJL' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJL'
                   IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UPM' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADDON AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3460-ACCRUE-ADDON.
           IF BCDM-ADDON-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-ADDON-AMT * BCDM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BCDM-ADDON-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN CLOSE DATE IS PAST
      * BF -> HF WHEN CLOSE DATE IS PAST
      * HF -> HR WHEN CLOSE DATE IS PAST
      * HR -> UF WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3470-SET-SOURCE.
           EVALUATE BCDM-SOURCE-CD
               WHEN 'AG'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3480-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BUMP YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3490-FLAG-BUMP.
           IF BCDM-BUMP-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDM-BUMP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ADDON AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BUMP YTD.
      *----------------------------------------------------------------*
       3500-PRORATE-BUMP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-BUMP-YTD ROUNDED =
                   BCDM-ADDON-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDM-BUMP-YTD
           END-IF
           ADD BCDM-BUMP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3510-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-CYCLE-CD
                   ADD BCDM-PENALTY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-PENALTY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUMP YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3520-ACCRUE-BUMP.
           IF BCDM-BUMP-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-BUMP-YTD * BCDM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BCDM-ADDON-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E681 DESC REQUIRED
      * E046 PENALTY AMT NEGATIVE
      * E484 PENALTY AMT OVER 99999
      * E090 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3530-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-DESC = SPACES
                   MOVE 'E681' TO WS-REJECT-CODE
               WHEN BCDT-PENALTY-AMT < 0
                   MOVE 'E046' TO WS-REJECT-CODE
               WHEN BCDT-PENALTY-AMT > 99999.00
                   MOVE 'E484' TO WS-REJECT-CODE
               WHEN BCDT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E090' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> S WHEN NEXT REVIEW DATE IS PAST
      * S -> T WHEN NEXT REVIEW DATE IS PAST
      * T -> U WHEN NEXT REVIEW DATE IS PAST
      * U -> X WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3540-SET-CYCLE.
           EVALUATE BCDM-CYCLE-CD
               WHEN 'A'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'S' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'T' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'U' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'X' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON YTD FEE ON THE INTEREST YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3550-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BCDM-ADDON-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-STOP-FLAG = 'Y'
                   COMPUTE BCDM-ADDON-YTD ROUNDED = BCDM-INTEREST-YTD *
                       BCDM-PENALTY-PCT / 200
               WHEN BCDM-ELECTION-CD = 'L'
                   MOVE 3.00 TO BCDM-ADDON-YTD
               WHEN BCDM-ELECTION-CD = 'M'
                   MOVE 7.00 TO BCDM-ADDON-YTD
               WHEN OTHER
                   COMPUTE BCDM-ADDON-YTD ROUNDED = BCDM-INTEREST-YTD *
                       BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF BCDM-ADDON-YTD > 0 AND BCDM-ADDON-YTD < 10.00
               MOVE 10.00 TO BCDM-ADDON-YTD
           END-IF
           IF BCDM-ADDON-YTD > 35.00
               MOVE 35.00 TO BCDM-ADDON-YTD
           END-IF
           ADD BCDM-ADDON-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON FEE ON THE PRINCIPAL YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3560-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BCDM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BCDM-ADDON-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   COMPUTE BCDM-ADDON-AMT ROUNDED = BCDM-PRINCIPAL-YTD *
                       BCDM-RATE / 200
               WHEN BCDM-STEP-PLAN-CD = 'AYX'
                   MOVE 7.00 TO BCDM-ADDON-AMT
               WHEN BCDM-STEP-PLAN-CD = 'DGU'
                   MOVE 10.00 TO BCDM-ADDON-AMT
               WHEN OTHER
                   COMPUTE BCDM-ADDON-AMT ROUNDED = BCDM-PRINCIPAL-YTD *
                       BCDM-RATE / 100
           END-EVALUATE
           IF BCDM-ADDON-AMT > 0 AND BCDM-ADDON-AMT < 2.00
               MOVE 2.00 TO BCDM-ADDON-AMT
           END-IF
           IF BCDM-ADDON-AMT > 25.00
               MOVE 25.00 TO BCDM-ADDON-AMT
           END-IF
           ADD BCDM-ADDON-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E721 ADDR REQUIRED
      * E333 BUMP YTD NEGATIVE
      * E479 BUMP YTD OVER 10000
      * E713 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3570-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-ADDR = SPACES
                   MOVE 'E721' TO WS-REJECT-CODE
               WHEN BCDM-BUMP-YTD < 0
                   MOVE 'E333' TO WS-REJECT-CODE
               WHEN BCDM-BUMP-YTD > 10000.00
                   MOVE 'E479' TO WS-REJECT-CODE
               WHEN BCDM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E713' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDT-TRAN-CD
                   ADD BCDT-ADDON-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-ADDON-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-CLOSE.
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
                   ADD BCDM-PENALTY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-PENALTY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BCDM-PENALTY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-PENALTY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-PENALTY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3600-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = BCDM-PRINCIPAL-YTD / 4
           COMPUTE WS-WORK-AMT-2 = BCDM-PRINCIPAL-YTD - (WS-WORK-AMT *
               4)
           COMPUTE BCDM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUMP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ADDON YTD.
      *----------------------------------------------------------------*
       3610-PRORATE-ADDON.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-ADDON-YTD ROUNDED =
                   BCDM-BUMP-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDM-ADDON-YTD
           END-IF
           ADD BCDM-ADDON-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3620-COUNT-EXCEPT.
           IF BCDM-EXCEPT-CNT < 9999999
               ADD 1 TO BCDM-EXCEPT-CNT
           END-IF
           IF BCDM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO BCDM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3630-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN BCDM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO BCDT-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-HOLD-FLAG = 'Y'
                   COMPUTE BCDT-INTEREST-AMT ROUNDED = WS-WORK-AMT-2 *
                       BCDM-RATE / 200
               WHEN BCDT-TRAN-CD = 'BU'
                   MOVE 7.00 TO BCDT-INTEREST-AMT
               WHEN BCDT-TRAN-CD = 'AD'
                   MOVE 10.00 TO BCDT-INTEREST-AMT
               WHEN OTHER
                   COMPUTE BCDT-INTEREST-AMT ROUNDED = WS-WORK-AMT-2 *
                       BCDM-RATE / 100
           END-EVALUATE
           IF BCDT-INTEREST-AMT > 0 AND BCDT-INTEREST-AMT < 10.00
               MOVE 10.00 TO BCDT-INTEREST-AMT
           END-IF
           IF BCDT-INTEREST-AMT > 100.00
               MOVE 100.00 TO BCDT-INTEREST-AMT
           END-IF
           ADD BCDT-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT PENALTY YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3640-SPLIT-PENALTY.
           COMPUTE WS-WORK-AMT = BCDM-PENALTY-YTD / 3
           COMPUTE WS-WORK-AMT-2 = BCDM-PENALTY-YTD - (WS-WORK-AMT * 3)
           COMPUTE BCDM-ADDON-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3650-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-SCHED-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-PRINCIPA-SCHED-LOW(WS-TX)
                   MOVE WS-PRINCIPA-SCHED-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ADDON AMT.
      *----------------------------------------------------------------*
       3660-PRORATE-ADDON.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-ADDON-AMT ROUNDED =
                   BCDT-INTEREST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-ADDON-AMT
           END-IF
           ADD BCDM-ADDON-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON FEE ON THE PENALTY AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3670-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BCDM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BCDM-ADDON-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   COMPUTE BCDM-ADDON-AMT ROUNDED = BCDT-PENALTY-AMT *
                       BCDM-PENALTY-PCT / 200
               WHEN BCDM-STEP-PLAN-CD = 'AYX'
                   MOVE 3.00 TO BCDM-ADDON-AMT
               WHEN BCDM-STEP-PLAN-CD = 'DGU'
                   MOVE 3.00 TO BCDM-ADDON-AMT
               WHEN OTHER
                   COMPUTE BCDM-ADDON-AMT ROUNDED = BCDT-PENALTY-AMT *
                       BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF BCDM-ADDON-AMT > 0 AND BCDM-ADDON-AMT < 5.00
               MOVE 5.00 TO BCDM-ADDON-AMT
           END-IF
           IF BCDM-ADDON-AMT > 35.00
               MOVE 35.00 TO BCDM-ADDON-AMT
           END-IF
           ADD BCDM-ADDON-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3680-COUNT-EXCEPT.
           IF BCDM-EXCEPT-CNT < 9999999
               ADD 1 TO BCDM-EXCEPT-CNT
           END-IF
           IF BCDM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO BCDM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ADDON AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3690-LIMIT-ADDON.
           IF BCDM-ADDON-AMT < 10.00
               MOVE 10.00 TO BCDM-ADDON-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-ADDON-AMT > 1000.00
               MOVE 1000.00 TO BCDM-ADDON-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA SCHED TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3700-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-SCHED-RATE(1) TO BCDM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BCDM-PRINCIPAL-AMT NOT < WS-PRINCIPA-SCHED-LOW(WS-TX)
                   MOVE WS-PRINCIPA-SCHED-RATE(WS-TX) TO BCDM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3710-FLAG-INTEREST.
           IF BCDM-INTEREST-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BCDM-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3720-DESCRIBE-CYCLE.
           EVALUATE BCDM-CYCLE-CD
               WHEN 'A'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'S'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3730-COUNT-CYCLE.
           IF BCDM-CYCLE-CNT < 9999999
               ADD 1 TO BCDM-CYCLE-CNT
           END-IF
           IF BCDM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO BCDM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUMP YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3740-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = BCDM-BUMP-YTD / 3
           COMPUTE WS-WORK-AMT-2 = BCDM-BUMP-YTD - (WS-WORK-AMT * 3)
           COMPUTE BCDM-ADDON-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN EFF DATE IS PAST
      * M -> N WHEN EFF DATE IS PAST
      * N -> R WHEN EFF DATE IS PAST
      * R -> V WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-ELECTION.
           EVALUATE BCDM-ELECTION-CD
               WHEN 'L'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STEP PLAN CD:
      * AYX -> DGU WHEN NEXT DUE DATE IS PAST
      * DGU -> GWJ WHEN NEXT DUE DATE IS PAST
      * GWJ -> RJL WHEN NEXT DUE DATE IS PAST
      * RJL -> UPM WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-STEP-PLAN.
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
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN NEXT REVIEW DATE IS PAST
      * BF -> HF WHEN NEXT REVIEW DATE IS PAST
      * HF -> HR WHEN NEXT REVIEW DATE IS PAST
      * HR -> UF WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3770-SET-SOURCE.
           EVALUATE BCDM-SOURCE-CD
               WHEN 'AG'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HR' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY FEE ON THE BUMP AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3780-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN BCDM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PENALTY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-OVERRIDE-FLAG = 'Y'
                   COMPUTE BCDM-PENALTY-AMT ROUNDED = BCDM-BUMP-AMT *
                       BCDM-RATE / 200
               WHEN BCDM-ELECTION-CD = 'L'
                   MOVE 7.00 TO BCDM-PENALTY-AMT
               WHEN BCDM-ELECTION-CD = 'M'
                   MOVE 12.00 TO BCDM-PENALTY-AMT
               WHEN OTHER
                   COMPUTE BCDM-PENALTY-AMT ROUNDED = BCDM-BUMP-AMT *
                       BCDM-RATE / 100
           END-EVALUATE
           IF BCDM-PENALTY-AMT > 0 AND BCDM-PENALTY-AMT < 1.00
               MOVE 1.00 TO BCDM-PENALTY-AMT
           END-IF
           IF BCDM-PENALTY-AMT > 100.00
               MOVE 100.00 TO BCDM-PENALTY-AMT
           END-IF
           ADD BCDM-PENALTY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON FEE ON THE ADDON AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3790-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BCDM-ADDON-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-OVERRIDE-FLAG = 'Y'
                   COMPUTE BCDM-ADDON-AMT ROUNDED = BCDT-ADDON-AMT *
                       BCDM-RATE / 200
               WHEN BCDT-TRAN-CD = 'BU'
                   MOVE 7.00 TO BCDM-ADDON-AMT
               WHEN BCDT-TRAN-CD = 'AD'
                   MOVE 15.00 TO BCDM-ADDON-AMT
               WHEN OTHER
                   COMPUTE BCDM-ADDON-AMT ROUNDED = BCDT-ADDON-AMT *
                       BCDM-RATE / 100
           END-EVALUATE
           IF BCDM-ADDON-AMT > 0 AND BCDM-ADDON-AMT < 1.00
               MOVE 1.00 TO BCDM-ADDON-AMT
           END-IF
           IF BCDM-ADDON-AMT > 150.00
               MOVE 150.00 TO BCDM-ADDON-AMT
           END-IF
           ADD BCDM-ADDON-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE PENALTY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3800-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF BCDM-PENALTY-YTD NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO BCDM-RATE
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
       3810-DESCRIBE-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PENALTY AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3820-ACCRUE-PENALTY.
           IF BCDT-PENALTY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDT-PENALTY-AMT * BCDM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BCDM-PENALTY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA SCHED TIER FOR THE ADDON AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3830-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-SCHED-RATE(1) TO BCDM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BCDM-ADDON-AMT NOT < WS-PRINCIPA-SCHED-LOW(WS-TX)
                   MOVE WS-PRINCIPA-SCHED-RATE(WS-TX) TO BCDM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E204 ADDR REQUIRED
      * E249 ADDON AMT NEGATIVE
      * E396 ADDON AMT OVER 5000
      * E091 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3840-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-ADDR = SPACES
                   MOVE 'E204' TO WS-REJECT-CODE
               WHEN BCDM-ADDON-AMT < 0
                   MOVE 'E249' TO WS-REJECT-CODE
               WHEN BCDM-ADDON-AMT > 5000.00
                   MOVE 'E396' TO WS-REJECT-CODE
               WHEN BCDM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E091' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BU -> AD WHEN LAST MAINT DATE IS PAST
      * AD -> WD WHEN LAST MAINT DATE IS PAST
      * WD -> MT WHEN LAST MAINT DATE IS PAST
      * MT -> BU WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3850-SET-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WD'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MT' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MT'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BU' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BUMP FEE ON THE INTEREST YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3860-ASSESS-STOP.
           EVALUATE TRUE
               WHEN BCDM-STOP-FLAG = 'Y'
                   MOVE ZERO TO BCDM-BUMP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-OVERRIDE-FLAG = 'Y'
                   COMPUTE BCDM-BUMP-AMT ROUNDED = BCDM-INTEREST-YTD *
                       BCDM-PENALTY-PCT / 200
               WHEN BCDT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO BCDM-BUMP-AMT
               WHEN BCDT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO BCDM-BUMP-AMT
               WHEN OTHER
                   COMPUTE BCDM-BUMP-AMT ROUNDED = BCDM-INTEREST-YTD *
                       BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF BCDM-BUMP-AMT > 0 AND BCDM-BUMP-AMT < 10.00
               MOVE 10.00 TO BCDM-BUMP-AMT
           END-IF
           IF BCDM-BUMP-AMT > 35.00
               MOVE 35.00 TO BCDM-BUMP-AMT
           END-IF
           ADD BCDM-BUMP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE BUMP FEE ON THE PENALTY YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3870-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BCDM-BUMP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   COMPUTE BCDM-BUMP-AMT ROUNDED = BCDM-PENALTY-YTD *
                       BCDM-MARGIN / 200
               WHEN BCDM-ELECTION-CD = 'L'
                   MOVE 3.00 TO BCDM-BUMP-AMT
               WHEN BCDM-ELECTION-CD = 'M'
                   MOVE 5.00 TO BCDM-BUMP-AMT
               WHEN OTHER
                   COMPUTE BCDM-BUMP-AMT ROUNDED = BCDM-PENALTY-YTD *
                       BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDM-BUMP-AMT > 0 AND BCDM-BUMP-AMT < 5.00
               MOVE 5.00 TO BCDM-BUMP-AMT
           END-IF
           IF BCDM-BUMP-AMT > 25.00
               MOVE 25.00 TO BCDM-BUMP-AMT
           END-IF
           ADD BCDM-BUMP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3880-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDT-CHANNEL-CD
                   ADD BCDM-PENALTY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-PENALTY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PENALTY TIER TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3900-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PENALTY-TIER-RATE(1) TO BCDM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF BCDM-PRINCIPAL-AMT NOT < WS-PENALTY-TIER-LOW(WS-TX)
                   MOVE WS-PENALTY-TIER-RATE(WS-TX) TO BCDM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3910-FLAG-INTEREST.
           IF BCDM-INTEREST-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDM-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STEP PLAN CD:
      * AYX -> DGU WHEN LAST PAY DATE IS PAST
      * DGU -> GWJ WHEN LAST PAY DATE IS PAST
      * GWJ -> RJL WHEN LAST PAY DATE IS PAST
      * RJL -> UPM WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3920-SET-STEP-PLAN.
           EVALUATE BCDM-STEP-PLAN-CD
               WHEN 'AYX'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DGU'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GWJ' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GWJ'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJL' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJL'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UPM' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ADDON AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BUMP YTD.
      *----------------------------------------------------------------*
       3930-PRORATE-BUMP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-BUMP-YTD ROUNDED =
                   BCDT-ADDON-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-BUMP-YTD
           END-IF
           ADD BCDM-BUMP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY ELECTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3940-ACCUM-ELECTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-ELECTION-CD
                   ADD BCDM-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-ELECTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-MATURITY-DATE TO DTU-DATE-1
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
      * BUMP YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3960-FLAG-BUMP.
           IF BCDM-BUMP-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDM-BUMP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PENALTY AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3970-ACCRUE-PENALTY.
           IF BCDM-PENALTY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-PENALTY-AMT * BCDM-RATE / 100 / 365
               ADD WS-WORK-AMT TO BCDT-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUMP AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3980-ACCRUE-BUMP.
           IF BCDM-BUMP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-BUMP-AMT * BCDM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BCDT-BUMP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3990-AGE-EFF.
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
               WHEN WS-DAYS NOT > 45
                   ADD BCDT-PENALTY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BCDT-PENALTY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BCDT-PENALTY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDT-PENALTY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ADDON TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4000-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ADDON-TIER-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-ADDON-TIER-LOW(WS-TX)
                   MOVE WS-ADDON-TIER-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ADDON AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4010-FLAG-ADDON.
           IF BCDM-ADDON-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDM-ADDON-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ELECTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4020-DESCRIBE-ELECTION.
           EVALUATE BCDM-ELECTION-CD
               WHEN 'L'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'N'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'V'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4030-COUNT-ITEM.
           IF BCDM-ITEM-CNT < 9999999
               ADD 1 TO BCDM-ITEM-CNT
           END-IF
           IF BCDM-ITEM-CNT NOT < 5
               MOVE 'Y' TO BCDM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4040-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO BCDM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4050-COUNT-CYCLE.
           IF BCDM-CYCLE-CNT < 9999999
               ADD 1 TO BCDM-CYCLE-CNT
           END-IF
           IF BCDM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO BCDM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4060-COUNT-RETRY.
           IF BCDM-RETRY-CNT < 9999999
               ADD 1 TO BCDM-RETRY-CNT
           END-IF
           IF BCDM-RETRY-CNT NOT < 6
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4070-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BCDT-INTEREST-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BCDT-INTEREST-AMT - (WS-WORK-AMT * 4)
           COMPUTE BCDM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4080-FLAG-PENALTY.
           IF BCDM-PENALTY-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDM-PENALTY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY FEE ON THE ADDON YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4090-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PENALTY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   COMPUTE BCDM-PENALTY-AMT ROUNDED = BCDM-ADDON-YTD *
                       BCDM-MARGIN / 200
               WHEN BCDM-REGION-CD = 'BGJ'
                   MOVE 3.00 TO BCDM-PENALTY-AMT
               WHEN BCDM-REGION-CD = 'LFX'
                   MOVE 5.00 TO BCDM-PENALTY-AMT
               WHEN OTHER
                   COMPUTE BCDM-PENALTY-AMT ROUNDED = BCDM-ADDON-YTD *
                       BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDM-PENALTY-AMT > 0 AND BCDM-PENALTY-AMT < 2.00
               MOVE 2.00 TO BCDM-PENALTY-AMT
           END-IF
           IF BCDM-PENALTY-AMT > 150.00
               MOVE 150.00 TO BCDM-PENALTY-AMT
           END-IF
           ADD BCDM-PENALTY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE BUMP YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4100-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-REGION-CD
                   ADD BCDM-BUMP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-BUMP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN EXPIRY DATE IS PAST
      * M -> N WHEN EXPIRY DATE IS PAST
      * N -> R WHEN EXPIRY DATE IS PAST
      * R -> V WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4110-SET-ELECTION.
           EVALUATE BCDM-ELECTION-CD
               WHEN 'L'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ADDON AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4120-FLAG-ADDON.
           IF BCDM-ADDON-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDM-ADDON-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4130-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4140-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ADDON YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4150-LIMIT-ADDON.
           IF BCDM-ADDON-YTD < 50.00
               MOVE 50.00 TO BCDM-ADDON-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-ADDON-YTD > 50000.00
               MOVE 50000.00 TO BCDM-ADDON-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E144 DESC REQUIRED
      * E616 PRINCIPAL YTD NEGATIVE
      * E102 PRINCIPAL YTD OVER 50000
      * E355 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4160-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-DESC = SPACES
                   MOVE 'E144' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-YTD < 0
                   MOVE 'E616' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-YTD > 50000.00
                   MOVE 'E102' TO WS-REJECT-CODE
               WHEN BCDM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E355' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE BUMP AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4170-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BCDM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-REVIEW-FLAG = 'Y'
                   COMPUTE BCDM-PRINCIPAL-AMT ROUNDED = BCDT-BUMP-AMT *
                       BCDM-MARGIN / 200
               WHEN BCDT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO BCDM-PRINCIPAL-AMT
               WHEN BCDT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO BCDM-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE BCDM-PRINCIPAL-AMT ROUNDED = BCDT-BUMP-AMT *
                       BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDM-PRINCIPAL-AMT > 0 AND BCDM-PRINCIPAL-AMT < 2.00
               MOVE 2.00 TO BCDM-PRINCIPAL-AMT
           END-IF
           IF BCDM-PRINCIPAL-AMT > 150.00
               MOVE 150.00 TO BCDM-PRINCIPAL-AMT
           END-IF
           ADD BCDM-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E447 MEMO REQUIRED
      * E603 INTEREST YTD NEGATIVE
      * E174 INTEREST YTD OVER 10000
      * E260 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4180-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-MEMO = SPACES
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN BCDM-INTEREST-YTD < 0
                   MOVE 'E603' TO WS-REJECT-CODE
               WHEN BCDM-INTEREST-YTD > 10000.00
                   MOVE 'E174' TO WS-REJECT-CODE
               WHEN BCDT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E260' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BUMP AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4190-LIMIT-BUMP.
           IF BCDT-BUMP-AMT < 25.00
               MOVE 25.00 TO BCDT-BUMP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDT-BUMP-AMT > 50000.00
               MOVE 50000.00 TO BCDT-BUMP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4200-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO BCDM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E226 DESC REQUIRED
      * E011 INTEREST AMT NEGATIVE
      * E643 INTEREST AMT OVER 1000000
      * E668 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4210-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-DESC = SPACES
                   MOVE 'E226' TO WS-REJECT-CODE
               WHEN BCDT-INTEREST-AMT < 0
                   MOVE 'E011' TO WS-REJECT-CODE
               WHEN BCDT-INTEREST-AMT > 1000000.00
                   MOVE 'E643' TO WS-REJECT-CODE
               WHEN BCDM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E668' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4220-ACCRUE-INTEREST.
           IF BCDM-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-INTEREST-AMT * BCDM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BCDM-ADDON-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4230-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY FEE ON THE ADDON AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4240-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BCDM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PENALTY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   COMPUTE BCDM-PENALTY-AMT ROUNDED = BCDM-ADDON-AMT *
                       BCDM-MARGIN / 200
               WHEN BCDM-ELECTION-CD = 'L'
                   MOVE 7.00 TO BCDM-PENALTY-AMT
               WHEN BCDM-ELECTION-CD = 'M'
                   MOVE 5.00 TO BCDM-PENALTY-AMT
               WHEN OTHER
                   COMPUTE BCDM-PENALTY-AMT ROUNDED = BCDM-ADDON-AMT *
                       BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDM-PENALTY-AMT > 0 AND BCDM-PENALTY-AMT < 10.00
               MOVE 10.00 TO BCDM-PENALTY-AMT
           END-IF
           IF BCDM-PENALTY-AMT > 100.00
               MOVE 100.00 TO BCDM-PENALTY-AMT
           END-IF
           ADD BCDM-PENALTY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4250-AGE-EXPIRY.
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
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4260-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4270-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE WORK AMT 2.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4280-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BCDT-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-STOP-FLAG = 'Y'
                   COMPUTE BCDT-INTEREST-AMT ROUNDED = WS-WORK-AMT-2 *
                       BCDM-RATE / 200
               WHEN BCDM-ELECTION-CD = 'L'
                   MOVE 3.00 TO BCDT-INTEREST-AMT
               WHEN BCDM-ELECTION-CD = 'M'
                   MOVE 15.00 TO BCDT-INTEREST-AMT
               WHEN OTHER
                   COMPUTE BCDT-INTEREST-AMT ROUNDED = WS-WORK-AMT-2 *
                       BCDM-RATE / 100
           END-EVALUATE
           IF BCDT-INTEREST-AMT > 0 AND BCDT-INTEREST-AMT < 2.00
               MOVE 2.00 TO BCDT-INTEREST-AMT
           END-IF
           IF BCDT-INTEREST-AMT > 50.00
               MOVE 50.00 TO BCDT-INTEREST-AMT
           END-IF
           ADD BCDT-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE INTEREST AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4290-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN BCDM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO BCDM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-OVERRIDE-FLAG = 'Y'
                   COMPUTE BCDM-INTEREST-YTD ROUNDED = BCDT-INTEREST-AMT
                       * BCDM-MARGIN / 200
               WHEN BCDM-CHANNEL-CD = 'EK'
                   MOVE 5.00 TO BCDM-INTEREST-YTD
               WHEN BCDM-CHANNEL-CD = 'JE'
                   MOVE 10.00 TO BCDM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE BCDM-INTEREST-YTD ROUNDED = BCDT-INTEREST-AMT
                       * BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDM-INTEREST-YTD > 0 AND BCDM-INTEREST-YTD < 10.00
               MOVE 10.00 TO BCDM-INTEREST-YTD
           END-IF
           IF BCDM-INTEREST-YTD > 150.00
               MOVE 150.00 TO BCDM-INTEREST-YTD
           END-IF
           ADD BCDM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BGJ -> LFX WHEN EFFECTIVE DATE IS PAST
      * LFX -> MLA WHEN EFFECTIVE DATE IS PAST
      * MLA -> ULA WHEN EFFECTIVE DATE IS PAST
      * ULA -> BGJ WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4300-SET-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   IF BCDM-EFFECTIVE-DATE > 0 AND BCDM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LFX' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LFX'
                   IF BCDM-EFFECTIVE-DATE > 0 AND BCDM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MLA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MLA'
                   IF BCDM-EFFECTIVE-DATE > 0 AND BCDM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ULA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULA'
                   IF BCDM-EFFECTIVE-DATE > 0 AND BCDM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BGJ' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ADDON AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4310-SPLIT-ADDON.
           COMPUTE WS-WORK-AMT = BCDT-ADDON-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BCDT-ADDON-AMT - (WS-WORK-AMT * 3)
           COMPUTE BCDM-ADDON-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4320-DESCRIBE-SOURCE.
           EVALUATE BCDM-SOURCE-CD
               WHEN 'AG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE BUMP YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4330-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-CYCLE-CD
                   ADD BCDM-BUMP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-BUMP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4340-LIMIT-PRINCIPAL.
           IF BCDM-PRINCIPAL-YTD < 0.00
               MOVE 0.00 TO BCDM-PRINCIPAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-PRINCIPAL-YTD > 1000.00
               MOVE 1000.00 TO BCDM-PRINCIPAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

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
                   MOVE BUDTE-BUMP-AMT
                     TO BCDM-BUMP-AMT
                   MOVE BUDTE-PRINCIPAL-YTD
                     TO BCDM-PRINCIPAL-YTD
                   MOVE BUDTE-REVIEW-FLAG
                     TO BCDM-REVIEW-FLAG
                   MOVE BUDTE-OVERRIDE-FLAG
                     TO BCDM-OVERRIDE-FLAG
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

      *----------------------------------------------------------------*
      * PASS THE BCD FIELDS TO BUTIER AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-BUTIER.
           INITIALIZE BUTIER-PARMS
           MOVE BCDM-PENALTY-YTD
             TO BUTIER-PENALTY-YTD
           MOVE BCDM-ADDON-AMT
             TO BUTIER-ADDON-AMT
           MOVE BCDM-BUMP-AMT
             TO BUTIER-BUMP-AMT
           MOVE BCDM-PRINCIPAL-AMT
             TO BUTIER-PRINCIPAL-AMT
           MOVE BCDM-INTEREST-AMT
             TO BUTIER-INTEREST-AMT
           MOVE BCDM-PRINCIPAL-YTD
             TO BUTIER-PRINCIPAL-YTD
           MOVE BCDM-NEXT-DUE-DATE
             TO BUTIER-NEXT-DUE-DATE
           MOVE BCDM-CLOSE-DATE
             TO BUTIER-CLOSE-DATE
           MOVE BCDM-EFFECTIVE-DATE
             TO BUTIER-EFFECTIVE-DATE
           MOVE BCDM-MATURITY-DATE
             TO BUTIER-MATURITY-DATE
           MOVE BCDM-REGION-CD
             TO BUTIER-REGION-CD
           MOVE BCDM-CYCLE-CD
             TO BUTIER-CYCLE-CD
           MOVE BCDM-CHANNEL-CD
             TO BUTIER-CHANNEL-CD
           MOVE BCDM-SOURCE-CD
             TO BUTIER-SOURCE-CD
           MOVE BCDM-REVIEW-FLAG
             TO BUTIER-REVIEW-FLAG
           MOVE BCDM-EXEMPT-FLAG
             TO BUTIER-EXEMPT-FLAG
           MOVE BCDM-OVERRIDE-FLAG
             TO BUTIER-OVERRIDE-FLAG
           MOVE BCDM-EXCEPT-CNT
             TO BUTIER-EXCEPT-CNT
           MOVE BCDM-RETRY-CNT
             TO BUTIER-RETRY-CNT
           MOVE BCDM-MARGIN
             TO BUTIER-MARGIN
           MOVE BCDM-RATE
             TO BUTIER-RATE
           MOVE BCDM-CUST-ID
             TO BUTIER-CUST-ID
           MOVE BCDM-REF-NO
             TO BUTIER-REF-NO
           CALL 'BUTIER' USING BUTIER-PARMS
           EVALUATE BUTIER-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE BUTIER-PENALTY-YTD
                     TO BCDM-PENALTY-YTD
                   MOVE BUTIER-BUMP-AMT
                     TO BCDM-BUMP-AMT
                   MOVE BUTIER-PRINCIPAL-AMT
                     TO BCDM-PRINCIPAL-AMT
                   MOVE BUTIER-INTEREST-AMT
                     TO BCDM-INTEREST-AMT
                   MOVE BUTIER-PRINCIPAL-YTD
                     TO BCDM-PRINCIPAL-YTD
                   MOVE BUTIER-OVERRIDE-FLAG
                     TO BCDM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RIER' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-BUTIER' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

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
                   MOVE BUVAL-BUMP-AMT
                     TO BCDM-BUMP-AMT
                   MOVE BUVAL-OVERRIDE-FLAG
                     TO BCDM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-BUVAL' TO ABND-PARAGRAPH
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
           DISPLAY 'BUDPOST READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'BUDPOST' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
