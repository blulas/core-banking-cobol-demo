      *================================================================*
      * PROGRAM  : IRANOTC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : IRA CUSTODY - CUSTOMER NOTICES. READS THE IRA       *
      *            TRANSACTIONS, UPDATES THE IRA MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. INDIVIDUAL RETIREMENT ACCOUNT   *
      *            CONTRIBUTIONS, DISTRIBUTIONS AND 5498               *
      * INPUT    : IRAMST (KSDS), IRATRN, BANKCTL                      *
      * OUTPUT   : IRAOUT, IRANORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, IRELG, IRPRC            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-10-17 DPS  ORIGINAL                                       *
      * 1998-05-06 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-01-15 BHS  FMV LIMIT REVISED REQ8646                      *
      * 2003-01-07 BHS  AUDIT FINDING - DISTRIB CONTROL TOTALS PRB9827 *
      * 2004-05-15 RJT  WITHHOLD CUTOFF MOVED CHG3743                  *
      * 2006-12-25 GAF  CONTRIB RERUN PROTECTION INC8592               *
      * 2006-10-17 CRP  EXCESS TABLE EXPANDED INC8192                  *
      * 2007-03-14 MKL  WITHHOLD RERUN PROTECTION CHG7255              *
      * 2008-01-07 SKN  WITHHOLD LIMIT REVISED CHG5089                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IRANOTC.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "IRAMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS IRAM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "IRATRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "IRAOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "IRANORPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY IRAMST.
       FD  TRAN-FILE.
       COPY IRATRN.
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
                                           VALUE 22.000000.
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
      * WS-ROLLOVER-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ROLLOVER-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000462000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000320500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000814000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000400000000873000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000900000000816500'.
       01  WS-ROLLOVER-GRID-TBL REDEFINES WS-ROLLOVER-GRID-VALUES.
           05  WS-ROLLOVER-GRID-ROW OCCURS 5 TIMES.
               10  WS-ROLLOVER-GRID-LOW    PIC 9(09)V99.
               10  WS-ROLLOVER-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-EXCESS-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-EXCESS-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000013000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000510500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000459500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000180000'.
       01  WS-EXCESS-TIER-TBL REDEFINES WS-EXCESS-TIER-VALUES.
           05  WS-EXCESS-TIER-ROW OCCURS 4 TIMES.
               10  WS-EXCESS-TIER-LOW      PIC 9(09)V99.
               10  WS-EXCESS-TIER-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DISTRIB-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISTRIB-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000579000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000551000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000021000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000622000'.
       01  WS-DISTRIB-SCHED-TBL REDEFINES WS-DISTRIB-SCHED-VALUES.
           05  WS-DISTRIB-SCHED-ROW OCCURS 4 TIMES.
               10  WS-DISTRIB-SCHED-LOW    PIC 9(09)V99.
               10  WS-DISTRIB-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONTRIB-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTRIB-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000322500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000763000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000473000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011250000000549500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012250000000846500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012350000000346500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014850000000905500'.
       01  WS-CONTRIB-TIER-TBL REDEFINES WS-CONTRIB-TIER-VALUES.
           05  WS-CONTRIB-TIER-ROW OCCURS 7 TIMES.
               10  WS-CONTRIB-TIER-LOW     PIC 9(09)V99.
               10  WS-CONTRIB-TIER-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  IRANOTC  IRA CUSTODY'.
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
       COPY IRELGP.
       COPY IRPRCP.

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
               MOVE 'IRAMST' TO ABND-FILE-NAME
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
           MOVE IRAT-KEY TO IRAM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-EDIT-REF-NO
               PERFORM 3010-ASSESS-HOLD
               PERFORM 3020-EDIT-ADDR
               PERFORM 3030-DESCRIBE-REASON
               PERFORM 3040-DESCRIBE-CHANNEL
               PERFORM 3050-LIMIT-DISTRIB
               PERFORM 3060-LIMIT-CONTRIB
               PERFORM 3070-CHECK-OPEN
               PERFORM 3080-COUNT-RETRY
               PERFORM 3090-SET-TRAN
               PERFORM 3100-PRORATE-DISTRIB
               PERFORM 3110-ACCRUE-FEE-PCT
               PERFORM 3120-CHECK-EXPIRY
               PERFORM 3130-ACCUM-TRAN
               PERFORM 3140-DESCRIBE-CHANNEL
               PERFORM 3150-ACCRUE-RMD
               PERFORM 3160-SET-REASON
               PERFORM 3170-ASSESS-HOLD
               PERFORM 3180-COUNT-RETRY
               PERFORM 3190-AGE-LAST-ACT
               PERFORM 3200-EDIT-REF-NO
               PERFORM 3210-FLAG-EXCESS
               PERFORM 3220-EDIT-CONTACT
               PERFORM 3230-SELECT-MARGIN
               PERFORM 3240-ACCRUE-DISTRIB
               PERFORM 3250-ASSESS-OVERRIDE
               PERFORM 3260-LIMIT-FMV
               PERFORM 3270-DESCRIBE-DIST-CODE
               PERFORM 3280-ACCUM-REASON
               PERFORM 3290-FLAG-ITEM
               PERFORM 3300-COUNT-SEQ
               PERFORM 3310-SPLIT-WITHHOLD
               PERFORM 3320-AGE-MATURITY
               PERFORM 3330-PRORATE-WITHHOLD
               PERFORM 3340-AGE-EFF
               PERFORM 3350-ASSESS-REVERSAL
               PERFORM 3360-EDIT-DESC
               PERFORM 3370-AGE-LAST-ACT
               PERFORM 3380-EDIT-OPERATOR
               PERFORM 3390-COUNT-SEQ
               PERFORM 3400-COUNT-PAYMENT
               PERFORM 3410-ACCRUE-DISTRIB
               PERFORM 3420-DESCRIBE-REASON
               PERFORM 3430-SPLIT-DISTRIB
               PERFORM 3440-ACCRUE-ROLLOVER
               PERFORM 3450-ACCRUE-ROLLOVER
               PERFORM 3460-ACCUM-TRAN
               PERFORM 3470-ACCUM-CHANNEL
               PERFORM 3480-CHECK-POST
               PERFORM 3490-PRORATE-DISTRIB
               PERFORM 3500-FLAG-FMV
               PERFORM 3510-LIMIT-FMV
               PERFORM 3520-ACCRUE-DISTRIB
               PERFORM 3530-SPLIT-RMD
               PERFORM 3540-LIMIT-DISTRIB
               PERFORM 3550-PRORATE-RMD
               PERFORM 3560-COUNT-PAYMENT
               PERFORM 3570-COUNT-CYCLE
               PERFORM 3580-ACCRUE-DISTRIB
               PERFORM 3590-ASSESS-HOLD
               PERFORM 3600-DESCRIBE-IRA-TYPE
               PERFORM 3610-PRORATE-CONTRIB
               PERFORM 3620-SELECT-FEE-PCT
               PERFORM 3630-SET-TRAN
               PERFORM 3640-ASSESS-HOLD
               PERFORM 3650-ACCUM-CHANNEL
               PERFORM 3660-EDIT-MEMO
               PERFORM 3670-DESCRIBE-DIST-CODE
               PERFORM 3680-SELECT-FEE-PCT
               PERFORM 3690-SELECT-MARGIN
               PERFORM 3700-DESCRIBE-TRAN
               PERFORM 3710-ASSESS-REVERSAL
               PERFORM 3720-FLAG-DISTRIB
               PERFORM 3730-ACCRUE-RMD
               PERFORM 3740-COUNT-RETRY
               PERFORM 3750-LIMIT-WITHHOLD
               PERFORM 3760-PRORATE-DISTRIB
               PERFORM 3770-LIMIT-DISTRIB
               PERFORM 3780-PRORATE-FMV
               PERFORM 3790-SELECT-RATE
               PERFORM 3800-SELECT-MARGIN
               PERFORM 3810-LIMIT-EXCESS
               PERFORM 3820-SET-CHANNEL
               PERFORM 3830-PRORATE-RMD
               PERFORM 3840-SET-DIST-CODE
               PERFORM 3850-DESCRIBE-IRA-TYPE
               PERFORM 3860-SELECT-FEE-PCT
               PERFORM 3870-EDIT-REF-NO
               PERFORM 3880-CHECK-LAST-ACT
               PERFORM 3890-EDIT-OPERATOR
               PERFORM 3900-LIMIT-RMD
               PERFORM 3910-ACCRUE-EXCESS
               PERFORM 3920-CHECK-LAST-STMT
               PERFORM 3930-CHECK-EFFECTIVE
               PERFORM 3940-SPLIT-EXCESS
               PERFORM 3950-ASSESS-AUTO-RENEW
               PERFORM 3960-SET-TRAN
               PERFORM 3970-ACCRUE-DISTRIB
               PERFORM 3980-EDIT-DESC
               PERFORM 3990-PRORATE-RMD
               PERFORM 4000-SPLIT-ROLLOVER
               PERFORM 4010-DESCRIBE-IRA-TYPE
               PERFORM 4020-LIMIT-DISTRIB
               PERFORM 4030-SPLIT-ROLLOVER
               PERFORM 4040-COUNT-CYCLE
               PERFORM 4050-COUNT-ITEM
               PERFORM 4060-SPLIT-ROLLOVER
               PERFORM 4070-SET-DIST-CODE
               PERFORM 4080-SET-REGION
               PERFORM 4090-ACCUM-DIST-CODE
               PERFORM 4100-SPLIT-RMD
               PERFORM 4110-ACCRUE-WITHHOLD
               PERFORM 4120-SELECT-MARGIN
               PERFORM 4130-ASSESS-SUSPEND
               PERFORM 4140-ACCUM-REGION
               PERFORM 4150-EDIT-REJECT
               PERFORM 4160-ASSESS-AUTO-RENEW
               PERFORM 4170-SET-TRAN
               PERFORM 4180-PRORATE-RMD
               PERFORM 4190-SET-DIST-CODE
               PERFORM 4200-ASSESS-OVERRIDE
               PERFORM 4210-SELECT-MARGIN
               PERFORM 4220-SELECT-RATE
               PERFORM 4230-ASSESS-OVERRIDE
               PERFORM 4240-FLAG-CONTRIB
               PERFORM 4250-CHECK-EFFECTIVE
               PERFORM 4260-COUNT-ITEM
               PERFORM 4270-ASSESS-STOP
               PERFORM 4280-ACCRUE-ROLLOVER
               PERFORM 4290-EDIT-ADDR
               PERFORM 4300-AGE-EFFECTIVE
               PERFORM 4310-ACCUM-REASON
               PERFORM 4320-AGE-EXPIRY
               PERFORM 4330-SPLIT-EXCESS
               PERFORM 4340-ACCUM-CHANNEL
               PERFORM 4350-SPLIT-CONTRIB
               PERFORM 4360-SET-CHANNEL
               PERFORM 4370-ACCUM-REASON
               PERFORM 4380-FLAG-EXCESS
               PERFORM 4390-ACCRUE-WITHHOLD
               PERFORM 4400-DESCRIBE-CHANNEL
               PERFORM 4410-AGE-NOTICE
               PERFORM 4420-FLAG-WITHHOLD
               PERFORM 4430-SPLIT-DISTRIB
               PERFORM 4440-LIMIT-RMD
               PERFORM 4450-SPLIT-RMD
               PERFORM 4460-LIMIT-EXCESS
               PERFORM 4470-FLAG-FMV
               PERFORM 4480-SET-TYPE
               PERFORM 4490-PRORATE-FMV
               PERFORM 4500-SELECT-MARGIN
               PERFORM 4510-ACCUM-IRA-TYPE
               PERFORM 4520-AGE-NEXT-REVIEW
               PERFORM 4530-ASSESS-STOP
               PERFORM 4540-PRORATE-WITHHOLD
               PERFORM 4550-SELECT-FEE-PCT
               PERFORM 4560-FLAG-WITHHOLD
               PERFORM 4570-SELECT-RATE
               PERFORM 4580-AGE-EXPIRY
               PERFORM 4590-ASSESS-HOLD
               PERFORM 4600-EDIT-CONTACT
               PERFORM 4610-COUNT-RETRY
               PERFORM 7020-CALL-IRELG
               PERFORM 7000-CALL-IRPRC
               REWRITE IRA-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'IRAMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM IRA-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E470 REF NO REQUIRED
      * E446 CONTRIB AMT NEGATIVE
      * E410 CONTRIB AMT OVER 250000
      * E971 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3000-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-REF-NO = SPACES
                   MOVE 'E470' TO WS-REJECT-CODE
               WHEN IRAM-CONTRIB-AMT < 0
                   MOVE 'E446' TO WS-REJECT-CODE
               WHEN IRAM-CONTRIB-AMT > 250000.00
                   MOVE 'E410' TO WS-REJECT-CODE
               WHEN IRAT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E971' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB YTD FEE ON THE ROLLOVER AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3010-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN IRAM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO IRAM-CONTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE IRAM-CONTRIB-YTD ROUNDED = IRAM-ROLLOVER-AMT
                       * IRAM-RATE / 200
               WHEN IRAM-DIST-CODE-CD = 'C'
                   MOVE 3.00 TO IRAM-CONTRIB-YTD
               WHEN IRAM-DIST-CODE-CD = 'J'
                   MOVE 12.00 TO IRAM-CONTRIB-YTD
               WHEN OTHER
                   COMPUTE IRAM-CONTRIB-YTD ROUNDED = IRAM-ROLLOVER-AMT
                       * IRAM-RATE / 100
           END-EVALUATE
           IF IRAM-CONTRIB-YTD > 0 AND IRAM-CONTRIB-YTD < 10.00
               MOVE 10.00 TO IRAM-CONTRIB-YTD
           END-IF
           IF IRAM-CONTRIB-YTD > 250.00
               MOVE 250.00 TO IRAM-CONTRIB-YTD
           END-IF
           ADD IRAM-CONTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E786 ADDR REQUIRED
      * E368 FMV YTD NEGATIVE
      * E376 FMV YTD OVER 1000000
      * E283 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-ADDR = SPACES
                   MOVE 'E786' TO WS-REJECT-CODE
               WHEN IRAM-FMV-YTD < 0
                   MOVE 'E368' TO WS-REJECT-CODE
               WHEN IRAM-FMV-YTD > 1000000.00
                   MOVE 'E376' TO WS-REJECT-CODE
               WHEN IRAM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E283' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3030-DESCRIBE-REASON.
           EVALUATE IRAM-REASON-CD
               WHEN 'CMR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LJS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NRT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UGA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WBS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZYU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3040-DESCRIBE-CHANNEL.
           EVALUATE IRAT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DISTRIB YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3050-LIMIT-DISTRIB.
           IF IRAM-DISTRIB-YTD < 50.00
               MOVE 50.00 TO IRAM-DISTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-DISTRIB-YTD > 5000.00
               MOVE 5000.00 TO IRAM-DISTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTRIB YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3060-LIMIT-CONTRIB.
           IF IRAM-CONTRIB-YTD < 100.00
               MOVE 100.00 TO IRAM-CONTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-CONTRIB-YTD > 500.00
               MOVE 500.00 TO IRAM-CONTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3070-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF IRAM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO IRAM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3080-COUNT-RETRY.
           IF IRAM-RETRY-CNT < 9999999
               ADD 1 TO IRAM-RETRY-CNT
           END-IF
           IF IRAM-RETRY-CNT NOT < 12
               MOVE 'Y' TO IRAM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CN -> DS WHEN MATURITY DATE IS PAST
      * DS -> RO WHEN MATURITY DATE IS PAST
      * RO -> RC WHEN MATURITY DATE IS PAST
      * RC -> WH WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3090-SET-TRAN.
           EVALUATE IRAT-TRAN-CD
               WHEN 'CN'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DS' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DS'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RO' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RO'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WH' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ROLLOVER YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISTRIB YTD.
      *----------------------------------------------------------------*
       3100-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-DISTRIB-YTD ROUNDED =
                   IRAM-ROLLOVER-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAM-DISTRIB-YTD
           END-IF
           ADD IRAM-DISTRIB-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3110-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * IRAM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO IRAM-FMV-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3120-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF IRAM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO IRAM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3130-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAT-TRAN-CD
                   ADD IRAM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3140-DESCRIBE-CHANNEL.
           EVALUATE IRAM-CHANNEL-CD
               WHEN 'AS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'AY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RMD YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3150-ACCRUE-RMD.
           IF IRAM-RMD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-RMD-YTD * IRAM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO IRAM-FMV-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * CMR -> LJS WHEN LAST ACT DATE IS PAST
      * LJS -> NRT WHEN LAST ACT DATE IS PAST
      * NRT -> UGA WHEN LAST ACT DATE IS PAST
      * UGA -> WBS WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3160-SET-REASON.
           EVALUATE IRAM-REASON-CD
               WHEN 'CMR'
                   IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LJS' TO IRAM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LJS'
                   IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NRT' TO IRAM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NRT'
                   IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UGA' TO IRAM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UGA'
                   IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WBS' TO IRAM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE RMD FEE ON THE EXCESS YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3170-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN IRAM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO IRAM-RMD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAT-REVERSAL-FLAG = 'Y'
                   COMPUTE IRAM-RMD-AMT ROUNDED = IRAM-EXCESS-YTD *
                       IRAM-FEE-PCT / 200
               WHEN IRAM-CHANNEL-CD = 'AS'
                   MOVE 12.00 TO IRAM-RMD-AMT
               WHEN IRAM-CHANNEL-CD = 'AY'
                   MOVE 7.00 TO IRAM-RMD-AMT
               WHEN OTHER
                   COMPUTE IRAM-RMD-AMT ROUNDED = IRAM-EXCESS-YTD *
                       IRAM-FEE-PCT / 100
           END-EVALUATE
           IF IRAM-RMD-AMT > 0 AND IRAM-RMD-AMT < 2.00
               MOVE 2.00 TO IRAM-RMD-AMT
           END-IF
           IF IRAM-RMD-AMT > 35.00
               MOVE 35.00 TO IRAM-RMD-AMT
           END-IF
           ADD IRAM-RMD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3180-COUNT-RETRY.
           IF IRAM-RETRY-CNT < 9999999
               ADD 1 TO IRAM-RETRY-CNT
           END-IF
           IF IRAM-RETRY-CNT NOT < 5
               MOVE 'Y' TO IRAM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3190-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-LAST-ACT-DATE TO DTU-DATE-1
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E045 REF NO REQUIRED
      * E463 RMD YTD NEGATIVE
      * E284 RMD YTD OVER 1000000
      * E997 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3200-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-REF-NO = SPACES
                   MOVE 'E045' TO WS-REJECT-CODE
               WHEN IRAM-RMD-YTD < 0
                   MOVE 'E463' TO WS-REJECT-CODE
               WHEN IRAM-RMD-YTD > 1000000.00
                   MOVE 'E284' TO WS-REJECT-CODE
               WHEN IRAM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E997' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCESS YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3210-FLAG-EXCESS.
           IF IRAM-EXCESS-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE IRAM-EXCESS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E968 CONTACT REQUIRED
      * E820 EXCESS AMT NEGATIVE
      * E126 EXCESS AMT OVER 99999
      * E392 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3220-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-CONTACT = SPACES
                   MOVE 'E968' TO WS-REJECT-CODE
               WHEN IRAM-EXCESS-AMT < 0
                   MOVE 'E820' TO WS-REJECT-CODE
               WHEN IRAM-EXCESS-AMT > 99999.00
                   MOVE 'E126' TO WS-REJECT-CODE
               WHEN IRAT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E392' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB SCHED TIER FOR THE RMD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3230-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-SCHED-RATE(1) TO IRAM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF IRAM-RMD-YTD NOT < WS-DISTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-DISTRIB-SCHED-RATE(WS-TX) TO IRAM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3240-ACCRUE-DISTRIB.
           IF IRAT-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAT-DISTRIB-AMT * IRAM-RATE / 100 / 360
               ADD WS-WORK-AMT TO IRAM-WITHHOLD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WITHHOLD YTD FEE ON THE DISTRIB AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3250-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN IRAM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO IRAM-WITHHOLD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-REVIEW-FLAG = 'Y'
                   COMPUTE IRAM-WITHHOLD-YTD ROUNDED = IRAT-DISTRIB-AMT
                       * IRAM-FEE-PCT / 200
               WHEN IRAM-TYPE-CD = 'EA'
                   MOVE 10.00 TO IRAM-WITHHOLD-YTD
               WHEN IRAM-TYPE-CD = 'HR'
                   MOVE 3.00 TO IRAM-WITHHOLD-YTD
               WHEN OTHER
                   COMPUTE IRAM-WITHHOLD-YTD ROUNDED = IRAT-DISTRIB-AMT
                       * IRAM-FEE-PCT / 100
           END-EVALUATE
           IF IRAM-WITHHOLD-YTD > 0 AND IRAM-WITHHOLD-YTD < 10.00
               MOVE 10.00 TO IRAM-WITHHOLD-YTD
           END-IF
           IF IRAM-WITHHOLD-YTD > 75.00
               MOVE 75.00 TO IRAM-WITHHOLD-YTD
           END-IF
           ADD IRAM-WITHHOLD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FMV YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3260-LIMIT-FMV.
           IF IRAM-FMV-YTD < 0.00
               MOVE 0.00 TO IRAM-FMV-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-FMV-YTD > 50000.00
               MOVE 50000.00 TO IRAM-FMV-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DIST CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3270-DESCRIBE-DIST-CODE.
           EVALUATE IRAM-DIST-CODE-CD
               WHEN 'C'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'J'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'U'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ROLLOVER YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3280-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-REASON-CD
                   ADD IRAM-ROLLOVER-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-ROLLOVER-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3290-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-SEQ.
           IF IRAT-SEQ < 9999999
               ADD 1 TO IRAT-SEQ
           END-IF
           IF IRAT-SEQ NOT < 6
               MOVE 'Y' TO IRAM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WITHHOLD AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3310-SPLIT-WITHHOLD.
           COMPUTE WS-WORK-AMT = IRAM-WITHHOLD-AMT / 2
           COMPUTE WS-WORK-AMT-2 = IRAM-WITHHOLD-AMT - (WS-WORK-AMT * 2)
           COMPUTE IRAM-FMV-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3320-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-RMD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD IRAM-RMD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD IRAM-RMD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRAM-RMD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-RMD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RMD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WITHHOLD YTD.
      *----------------------------------------------------------------*
       3330-PRORATE-WITHHOLD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-WITHHOLD-YTD ROUNDED =
                   IRAT-RMD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAM-WITHHOLD-YTD
           END-IF
           ADD IRAM-WITHHOLD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3340-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-WITHHOLD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD IRAM-WITHHOLD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD IRAM-WITHHOLD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRAM-WITHHOLD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-WITHHOLD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WITHHOLD YTD FEE ON THE EXCESS AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3350-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN IRAT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO IRAM-WITHHOLD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-HOLD-FLAG = 'Y'
                   COMPUTE IRAM-WITHHOLD-YTD ROUNDED = IRAM-EXCESS-AMT *
                       IRAM-FEE-PCT / 200
               WHEN IRAM-REASON-CD = 'CMR'
                   MOVE 15.00 TO IRAM-WITHHOLD-YTD
               WHEN IRAM-REASON-CD = 'LJS'
                   MOVE 15.00 TO IRAM-WITHHOLD-YTD
               WHEN OTHER
                   COMPUTE IRAM-WITHHOLD-YTD ROUNDED = IRAM-EXCESS-AMT *
                       IRAM-FEE-PCT / 100
           END-EVALUATE
           IF IRAM-WITHHOLD-YTD > 0 AND IRAM-WITHHOLD-YTD < 10.00
               MOVE 10.00 TO IRAM-WITHHOLD-YTD
           END-IF
           IF IRAM-WITHHOLD-YTD > 150.00
               MOVE 150.00 TO IRAM-WITHHOLD-YTD
           END-IF
           ADD IRAM-WITHHOLD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E034 DESC REQUIRED
      * E298 DISTRIB AMT NEGATIVE
      * E432 DISTRIB AMT OVER 25000
      * E728 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-DESC = SPACES
                   MOVE 'E034' TO WS-REJECT-CODE
               WHEN IRAT-DISTRIB-AMT < 0
                   MOVE 'E298' TO WS-REJECT-CODE
               WHEN IRAT-DISTRIB-AMT > 25000.00
                   MOVE 'E432' TO WS-REJECT-CODE
               WHEN IRAM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E728' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-WITHHOLD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD IRAM-WITHHOLD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD IRAM-WITHHOLD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRAM-WITHHOLD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-WITHHOLD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E421 OPERATOR REQUIRED
      * E545 WORK AMT NEGATIVE
      * E510 WORK AMT OVER 5000
      * E131 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3380-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAT-OPERATOR = SPACES
                   MOVE 'E421' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E545' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E510' TO WS-REJECT-CODE
               WHEN IRAM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E131' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3390-COUNT-SEQ.
           IF IRAT-SEQ < 9999999
               ADD 1 TO IRAT-SEQ
           END-IF
           IF IRAT-SEQ NOT < 3
               MOVE 'Y' TO IRAM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3400-COUNT-PAYMENT.
           IF IRAM-PAYMENT-CNT < 9999999
               ADD 1 TO IRAM-PAYMENT-CNT
           END-IF
           IF IRAM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO IRAT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3410-ACCRUE-DISTRIB.
           IF IRAT-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAT-DISTRIB-AMT * IRAM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3420-DESCRIBE-REASON.
           EVALUATE IRAM-REASON-CD
               WHEN 'CMR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LJS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NRT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UGA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WBS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZYU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3430-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = IRAT-DISTRIB-AMT / 3
           COMPUTE WS-WORK-AMT-2 = IRAT-DISTRIB-AMT - (WS-WORK-AMT * 3)
           COMPUTE IRAM-EXCESS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROLLOVER AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3440-ACCRUE-ROLLOVER.
           IF IRAT-ROLLOVER-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAT-ROLLOVER-AMT * IRAM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO IRAM-WITHHOLD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROLLOVER YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3450-ACCRUE-ROLLOVER.
           IF IRAM-ROLLOVER-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-ROLLOVER-YTD * IRAM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO IRAM-EXCESS-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RMD AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3460-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAT-TRAN-CD
                   ADD IRAM-RMD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-RMD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCESS YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3470-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-CHANNEL-CD
                   ADD IRAM-EXCESS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-EXCESS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3480-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF IRAT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO IRAM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WITHHOLD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISTRIB YTD.
      *----------------------------------------------------------------*
       3490-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-DISTRIB-YTD ROUNDED =
                   IRAM-WITHHOLD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAM-DISTRIB-YTD
           END-IF
           ADD IRAM-DISTRIB-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FMV YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3500-FLAG-FMV.
           IF IRAM-FMV-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE IRAM-FMV-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FMV AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3510-LIMIT-FMV.
           IF IRAM-FMV-AMT < 50.00
               MOVE 50.00 TO IRAM-FMV-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-FMV-AMT > 500.00
               MOVE 500.00 TO IRAM-FMV-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3520-ACCRUE-DISTRIB.
           IF IRAT-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAT-DISTRIB-AMT * IRAM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO IRAT-EXCESS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RMD AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3530-SPLIT-RMD.
           COMPUTE WS-WORK-AMT = IRAT-RMD-AMT / 12
           COMPUTE WS-WORK-AMT-2 = IRAT-RMD-AMT - (WS-WORK-AMT * 12)
           COMPUTE IRAM-FMV-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3540-LIMIT-DISTRIB.
           IF IRAM-DISTRIB-YTD < 100.00
               MOVE 100.00 TO IRAM-DISTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-DISTRIB-YTD > 1000.00
               MOVE 1000.00 TO IRAM-DISTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RMD YTD.
      *----------------------------------------------------------------*
       3550-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-RMD-YTD ROUNDED =
                   IRAT-DISTRIB-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-RMD-YTD
           END-IF
           ADD IRAM-RMD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3560-COUNT-PAYMENT.
           IF IRAM-PAYMENT-CNT < 9999999
               ADD 1 TO IRAM-PAYMENT-CNT
           END-IF
           IF IRAM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO IRAT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3570-COUNT-CYCLE.
           IF IRAM-CYCLE-CNT < 9999999
               ADD 1 TO IRAM-CYCLE-CNT
           END-IF
           IF IRAM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO IRAM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3580-ACCRUE-DISTRIB.
           IF IRAT-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAT-DISTRIB-AMT * IRAM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO IRAM-ROLLOVER-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB FEE ON THE WORK AMT 2.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3590-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN IRAM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO IRAT-DISTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAT-REVERSAL-FLAG = 'Y'
                   COMPUTE IRAT-DISTRIB-AMT ROUNDED = WS-WORK-AMT-2 *
                       IRAM-MARGIN / 200
               WHEN IRAM-DIST-CODE-CD = 'C'
                   MOVE 5.00 TO IRAT-DISTRIB-AMT
               WHEN IRAM-DIST-CODE-CD = 'J'
                   MOVE 7.00 TO IRAT-DISTRIB-AMT
               WHEN OTHER
                   COMPUTE IRAT-DISTRIB-AMT ROUNDED = WS-WORK-AMT-2 *
                       IRAM-MARGIN / 100
           END-EVALUATE
           IF IRAT-DISTRIB-AMT > 0 AND IRAT-DISTRIB-AMT < 10.00
               MOVE 10.00 TO IRAT-DISTRIB-AMT
           END-IF
           IF IRAT-DISTRIB-AMT > 100.00
               MOVE 100.00 TO IRAT-DISTRIB-AMT
           END-IF
           ADD IRAT-DISTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE IRA TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3600-DESCRIBE-IRA-TYPE.
           EVALUATE IRAM-IRA-TYPE-CD
               WHEN 'F'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RMD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       3610-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-CONTRIB-AMT ROUNDED =
                   IRAM-RMD-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-CONTRIB-AMT
           END-IF
           ADD IRAM-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB TIER TIER FOR THE RMD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3620-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-TIER-RATE(1) TO IRAM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF IRAM-RMD-AMT NOT < WS-CONTRIB-TIER-LOW(WS-TX)
                   MOVE WS-CONTRIB-TIER-RATE(WS-TX) TO IRAM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CN -> DS WHEN EFFECTIVE DATE IS PAST
      * DS -> RO WHEN EFFECTIVE DATE IS PAST
      * RO -> RC WHEN EFFECTIVE DATE IS PAST
      * RC -> WH WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-TRAN.
           EVALUATE IRAT-TRAN-CD
               WHEN 'CN'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DS' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DS'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RO' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RO'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WH' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB FEE ON THE RMD YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3640-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN IRAM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO IRAM-CONTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE IRAM-CONTRIB-AMT ROUNDED = IRAM-RMD-YTD *
                       IRAM-RATE / 200
               WHEN IRAT-TRAN-CD = 'CN'
                   MOVE 20.00 TO IRAM-CONTRIB-AMT
               WHEN IRAT-TRAN-CD = 'DS'
                   MOVE 3.00 TO IRAM-CONTRIB-AMT
               WHEN OTHER
                   COMPUTE IRAM-CONTRIB-AMT ROUNDED = IRAM-RMD-YTD *
                       IRAM-RATE / 100
           END-EVALUATE
           IF IRAM-CONTRIB-AMT > 0 AND IRAM-CONTRIB-AMT < 2.00
               MOVE 2.00 TO IRAM-CONTRIB-AMT
           END-IF
           IF IRAM-CONTRIB-AMT > 75.00
               MOVE 75.00 TO IRAM-CONTRIB-AMT
           END-IF
           ADD IRAM-CONTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE FMV AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3650-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAT-CHANNEL-CD
                   ADD IRAM-FMV-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-FMV-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E176 MEMO REQUIRED
      * E809 RMD YTD NEGATIVE
      * E151 RMD YTD OVER 250000
      * E801 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3660-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAT-MEMO = SPACES
                   MOVE 'E176' TO WS-REJECT-CODE
               WHEN IRAM-RMD-YTD < 0
                   MOVE 'E809' TO WS-REJECT-CODE
               WHEN IRAM-RMD-YTD > 250000.00
                   MOVE 'E151' TO WS-REJECT-CODE
               WHEN IRAM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E801' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DIST CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3670-DESCRIBE-DIST-CODE.
           EVALUATE IRAM-DIST-CODE-CD
               WHEN 'C'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'J'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'U'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE EXCESS TIER TIER FOR THE RMD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3680-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EXCESS-TIER-RATE(1) TO IRAM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF IRAM-RMD-YTD NOT < WS-EXCESS-TIER-LOW(WS-TX)
                   MOVE WS-EXCESS-TIER-RATE(WS-TX) TO IRAM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ROLLOVER GRID TIER FOR THE DISTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3690-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ROLLOVER-GRID-RATE(1) TO IRAM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF IRAM-DISTRIB-AMT NOT < WS-ROLLOVER-GRID-LOW(WS-TX)
                   MOVE WS-ROLLOVER-GRID-RATE(WS-TX) TO IRAM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3700-DESCRIBE-TRAN.
           EVALUATE IRAT-TRAN-CD
               WHEN 'CN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'DS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RO'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ROLLOVER FEE ON THE CONTRIB YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3710-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN IRAT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO IRAT-ROLLOVER-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-OVERRIDE-FLAG = 'Y'
                   COMPUTE IRAT-ROLLOVER-AMT ROUNDED = IRAM-CONTRIB-YTD
                       * IRAM-FEE-PCT / 200
               WHEN IRAT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO IRAT-ROLLOVER-AMT
               WHEN IRAT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO IRAT-ROLLOVER-AMT
               WHEN OTHER
                   COMPUTE IRAT-ROLLOVER-AMT ROUNDED = IRAM-CONTRIB-YTD
                       * IRAM-FEE-PCT / 100
           END-EVALUATE
           IF IRAT-ROLLOVER-AMT > 0 AND IRAT-ROLLOVER-AMT < 1.00
               MOVE 1.00 TO IRAT-ROLLOVER-AMT
           END-IF
           IF IRAT-ROLLOVER-AMT > 25.00
               MOVE 25.00 TO IRAT-ROLLOVER-AMT
           END-IF
           ADD IRAT-ROLLOVER-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DISTRIB YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3720-FLAG-DISTRIB.
           IF IRAM-DISTRIB-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRAM-DISTRIB-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RMD AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3730-ACCRUE-RMD.
           IF IRAT-RMD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAT-RMD-AMT * IRAM-RATE / 100 / 360
               ADD WS-WORK-AMT TO IRAM-DISTRIB-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3740-COUNT-RETRY.
           IF IRAM-RETRY-CNT < 9999999
               ADD 1 TO IRAM-RETRY-CNT
           END-IF
           IF IRAM-RETRY-CNT NOT < 3
               MOVE 'Y' TO IRAM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3750-LIMIT-WITHHOLD.
           IF IRAM-WITHHOLD-AMT < 10.00
               MOVE 10.00 TO IRAM-WITHHOLD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-WITHHOLD-AMT > 50000.00
               MOVE 50000.00 TO IRAM-WITHHOLD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISTRIB AMT.
      *----------------------------------------------------------------*
       3760-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-DISTRIB-AMT ROUNDED =
                   IRAM-CONTRIB-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAM-DISTRIB-AMT
           END-IF
           ADD IRAM-DISTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DISTRIB AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3770-LIMIT-DISTRIB.
           IF IRAM-DISTRIB-AMT < 100.00
               MOVE 100.00 TO IRAM-DISTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-DISTRIB-AMT > 5000.00
               MOVE 5000.00 TO IRAM-DISTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCESS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FMV YTD.
      *----------------------------------------------------------------*
       3780-PRORATE-FMV.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-FMV-YTD ROUNDED =
                   IRAM-EXCESS-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-FMV-YTD
           END-IF
           ADD IRAM-FMV-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB SCHED TIER FOR THE CONTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3790-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-SCHED-RATE(1) TO IRAM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF IRAM-CONTRIB-AMT NOT < WS-DISTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-DISTRIB-SCHED-RATE(WS-TX) TO IRAM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB SCHED TIER FOR THE ROLLOVER YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3800-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-SCHED-RATE(1) TO IRAM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF IRAM-ROLLOVER-YTD NOT < WS-DISTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-DISTRIB-SCHED-RATE(WS-TX) TO IRAM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCESS AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3810-LIMIT-EXCESS.
           IF IRAT-EXCESS-AMT < 100.00
               MOVE 100.00 TO IRAT-EXCESS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAT-EXCESS-AMT > 2500.00
               MOVE 2500.00 TO IRAT-EXCESS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3820-SET-CHANNEL.
           EVALUATE IRAT-CHANNEL-CD
               WHEN 'BRN'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO IRAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO IRAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO IRAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO IRAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RMD YTD.
      *----------------------------------------------------------------*
       3830-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-RMD-YTD ROUNDED =
                   IRAM-CONTRIB-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAM-RMD-YTD
           END-IF
           ADD IRAM-RMD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DIST CODE CD:
      * C -> J WHEN LAST STMT DATE IS PAST
      * J -> U WHEN LAST STMT DATE IS PAST
      * U -> X WHEN LAST STMT DATE IS PAST
      * X -> C WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3840-SET-DIST-CODE.
           EVALUATE IRAM-DIST-CODE-CD
               WHEN 'C'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE IRA TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3850-DESCRIBE-IRA-TYPE.
           EVALUATE IRAM-IRA-TYPE-CD
               WHEN 'F'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'V'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ROLLOVER GRID TIER FOR THE RMD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3860-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ROLLOVER-GRID-RATE(1) TO IRAM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF IRAT-RMD-AMT NOT < WS-ROLLOVER-GRID-LOW(WS-TX)
                   MOVE WS-ROLLOVER-GRID-RATE(WS-TX) TO IRAM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E518 REF NO REQUIRED
      * E262 ROLLOVER YTD NEGATIVE
      * E313 ROLLOVER YTD OVER 1000000
      * E924 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3870-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-REF-NO = SPACES
                   MOVE 'E518' TO WS-REJECT-CODE
               WHEN IRAM-ROLLOVER-YTD < 0
                   MOVE 'E262' TO WS-REJECT-CODE
               WHEN IRAM-ROLLOVER-YTD > 1000000.00
                   MOVE 'E313' TO WS-REJECT-CODE
               WHEN IRAM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E924' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3880-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF IRAM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO IRAT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E171 OPERATOR REQUIRED
      * E196 WORK AMT NEGATIVE
      * E328 WORK AMT OVER 99999
      * E646 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3890-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAT-OPERATOR = SPACES
                   MOVE 'E171' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E196' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E328' TO WS-REJECT-CODE
               WHEN IRAM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E646' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RMD AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3900-LIMIT-RMD.
           IF IRAT-RMD-AMT < 50.00
               MOVE 50.00 TO IRAT-RMD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAT-RMD-AMT > 500.00
               MOVE 500.00 TO IRAT-RMD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCESS YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3910-ACCRUE-EXCESS.
           IF IRAM-EXCESS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-EXCESS-YTD * IRAM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO IRAM-ROLLOVER-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3920-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF IRAM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO IRAM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3930-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO IRAT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCESS AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-EXCESS.
           COMPUTE WS-WORK-AMT = IRAT-EXCESS-AMT / 3
           COMPUTE WS-WORK-AMT-2 = IRAT-EXCESS-AMT - (WS-WORK-AMT * 3)
           COMPUTE IRAM-CONTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FMV YTD FEE ON THE ROLLOVER YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3950-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN IRAM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO IRAM-FMV-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-REVIEW-FLAG = 'Y'
                   COMPUTE IRAM-FMV-YTD ROUNDED = IRAM-ROLLOVER-YTD *
                       IRAM-RATE / 200
               WHEN IRAT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO IRAM-FMV-YTD
               WHEN IRAT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO IRAM-FMV-YTD
               WHEN OTHER
                   COMPUTE IRAM-FMV-YTD ROUNDED = IRAM-ROLLOVER-YTD *
                       IRAM-RATE / 100
           END-EVALUATE
           IF IRAM-FMV-YTD > 0 AND IRAM-FMV-YTD < 5.00
               MOVE 5.00 TO IRAM-FMV-YTD
           END-IF
           IF IRAM-FMV-YTD > 50.00
               MOVE 50.00 TO IRAM-FMV-YTD
           END-IF
           ADD IRAM-FMV-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CN -> DS WHEN MATURITY DATE IS PAST
      * DS -> RO WHEN MATURITY DATE IS PAST
      * RO -> RC WHEN MATURITY DATE IS PAST
      * RC -> WH WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3960-SET-TRAN.
           EVALUATE IRAT-TRAN-CD
               WHEN 'CN'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DS' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DS'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RO' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RO'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WH' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3970-ACCRUE-DISTRIB.
           IF IRAM-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-DISTRIB-AMT * IRAM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO IRAM-WITHHOLD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E253 DESC REQUIRED
      * E326 EXCESS YTD NEGATIVE
      * E442 EXCESS YTD OVER 250000
      * E562 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3980-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-DESC = SPACES
                   MOVE 'E253' TO WS-REJECT-CODE
               WHEN IRAM-EXCESS-YTD < 0
                   MOVE 'E326' TO WS-REJECT-CODE
               WHEN IRAM-EXCESS-YTD > 250000.00
                   MOVE 'E442' TO WS-REJECT-CODE
               WHEN IRAM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E562' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ROLLOVER AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RMD YTD.
      *----------------------------------------------------------------*
       3990-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-RMD-YTD ROUNDED =
                   IRAM-ROLLOVER-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-RMD-YTD
           END-IF
           ADD IRAM-RMD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT ROLLOVER AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4000-SPLIT-ROLLOVER.
           COMPUTE WS-WORK-AMT = IRAM-ROLLOVER-AMT / 3
           COMPUTE WS-WORK-AMT-2 = IRAM-ROLLOVER-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE IRA TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4010-DESCRIBE-IRA-TYPE.
           EVALUATE IRAM-IRA-TYPE-CD
               WHEN 'F'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'V'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DISTRIB AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4020-LIMIT-DISTRIB.
           IF IRAT-DISTRIB-AMT < 0.00
               MOVE 0.00 TO IRAT-DISTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAT-DISTRIB-AMT > 50000.00
               MOVE 50000.00 TO IRAT-DISTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ROLLOVER AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4030-SPLIT-ROLLOVER.
           COMPUTE WS-WORK-AMT = IRAT-ROLLOVER-AMT / 3
           COMPUTE WS-WORK-AMT-2 = IRAT-ROLLOVER-AMT - (WS-WORK-AMT * 3)
           COMPUTE IRAT-RMD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4040-COUNT-CYCLE.
           IF IRAM-CYCLE-CNT < 9999999
               ADD 1 TO IRAM-CYCLE-CNT
           END-IF
           IF IRAM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO IRAT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4050-COUNT-ITEM.
           IF IRAM-ITEM-CNT < 9999999
               ADD 1 TO IRAM-ITEM-CNT
           END-IF
           IF IRAM-ITEM-CNT NOT < 10
               MOVE 'Y' TO IRAM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ROLLOVER AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4060-SPLIT-ROLLOVER.
           COMPUTE WS-WORK-AMT = IRAM-ROLLOVER-AMT / 4
           COMPUTE WS-WORK-AMT-2 = IRAM-ROLLOVER-AMT - (WS-WORK-AMT * 4)
           COMPUTE IRAM-FMV-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DIST CODE CD:
      * C -> J WHEN POST DATE IS PAST
      * J -> U WHEN POST DATE IS PAST
      * U -> X WHEN POST DATE IS PAST
      * X -> C WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4070-SET-DIST-CODE.
           EVALUATE IRAM-DIST-CODE-CD
               WHEN 'C'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * CE -> FL WHEN NEXT DUE DATE IS PAST
      * FL -> GY WHEN NEXT DUE DATE IS PAST
      * GY -> HU WHEN NEXT DUE DATE IS PAST
      * HU -> KA WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4080-SET-REGION.
           EVALUATE IRAM-REGION-CD
               WHEN 'CE'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GY' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GY'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HU' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HU'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KA' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE RMD AMT BY DIST CODE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-DIST-CODE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-DIST-CODE-CD
                   ADD IRAT-RMD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-DIST-CODE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAT-RMD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RMD AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4100-SPLIT-RMD.
           COMPUTE WS-WORK-AMT = IRAM-RMD-AMT / 3
           COMPUTE WS-WORK-AMT-2 = IRAM-RMD-AMT - (WS-WORK-AMT * 3)
           COMPUTE IRAM-WITHHOLD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WITHHOLD YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4110-ACCRUE-WITHHOLD.
           IF IRAM-WITHHOLD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-WITHHOLD-YTD * IRAM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO IRAT-EXCESS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB TIER TIER FOR THE WITHHOLD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4120-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-TIER-RATE(1) TO IRAM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF IRAM-WITHHOLD-AMT NOT < WS-CONTRIB-TIER-LOW(WS-TX)
                   MOVE WS-CONTRIB-TIER-RATE(WS-TX) TO IRAM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WITHHOLD YTD FEE ON THE ROLLOVER AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4130-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN IRAM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO IRAM-WITHHOLD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE IRAM-WITHHOLD-YTD ROUNDED = IRAT-ROLLOVER-AMT
                       * IRAM-FEE-PCT / 200
               WHEN IRAM-TYPE-CD = 'EA'
                   MOVE 7.00 TO IRAM-WITHHOLD-YTD
               WHEN IRAM-TYPE-CD = 'HR'
                   MOVE 3.00 TO IRAM-WITHHOLD-YTD
               WHEN OTHER
                   COMPUTE IRAM-WITHHOLD-YTD ROUNDED = IRAT-ROLLOVER-AMT
                       * IRAM-FEE-PCT / 100
           END-EVALUATE
           IF IRAM-WITHHOLD-YTD > 0 AND IRAM-WITHHOLD-YTD < 2.00
               MOVE 2.00 TO IRAM-WITHHOLD-YTD
           END-IF
           IF IRAM-WITHHOLD-YTD > 100.00
               MOVE 100.00 TO IRAM-WITHHOLD-YTD
           END-IF
           ADD IRAM-WITHHOLD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCESS YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4140-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-REGION-CD
                   ADD IRAM-EXCESS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-EXCESS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E073 REJECT CD REQUIRED
      * E190 RMD AMT NEGATIVE
      * E510 RMD AMT OVER 50000
      * E752 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4150-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAT-REJECT-CD = SPACES
                   MOVE 'E073' TO WS-REJECT-CODE
               WHEN IRAT-RMD-AMT < 0
                   MOVE 'E190' TO WS-REJECT-CODE
               WHEN IRAT-RMD-AMT > 50000.00
                   MOVE 'E510' TO WS-REJECT-CODE
               WHEN IRAM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E752' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EXCESS YTD FEE ON THE EXCESS AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4160-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN IRAM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO IRAM-EXCESS-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-REVIEW-FLAG = 'Y'
                   COMPUTE IRAM-EXCESS-YTD ROUNDED = IRAM-EXCESS-AMT *
                       IRAM-MARGIN / 200
               WHEN IRAT-TRAN-CD = 'CN'
                   MOVE 20.00 TO IRAM-EXCESS-YTD
               WHEN IRAT-TRAN-CD = 'DS'
                   MOVE 15.00 TO IRAM-EXCESS-YTD
               WHEN OTHER
                   COMPUTE IRAM-EXCESS-YTD ROUNDED = IRAM-EXCESS-AMT *
                       IRAM-MARGIN / 100
           END-EVALUATE
           IF IRAM-EXCESS-YTD > 0 AND IRAM-EXCESS-YTD < 2.00
               MOVE 2.00 TO IRAM-EXCESS-YTD
           END-IF
           IF IRAM-EXCESS-YTD > 75.00
               MOVE 75.00 TO IRAM-EXCESS-YTD
           END-IF
           ADD IRAM-EXCESS-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CN -> DS WHEN MATURITY DATE IS PAST
      * DS -> RO WHEN MATURITY DATE IS PAST
      * RO -> RC WHEN MATURITY DATE IS PAST
      * RC -> WH WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4170-SET-TRAN.
           EVALUATE IRAT-TRAN-CD
               WHEN 'CN'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DS' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DS'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RO' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RO'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WH' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RMD YTD.
      *----------------------------------------------------------------*
       4180-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-RMD-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAM-RMD-YTD
           END-IF
           ADD IRAM-RMD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DIST CODE CD:
      * C -> J WHEN POST DATE IS PAST
      * J -> U WHEN POST DATE IS PAST
      * U -> X WHEN POST DATE IS PAST
      * X -> C WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4190-SET-DIST-CODE.
           EVALUATE IRAM-DIST-CODE-CD
               WHEN 'C'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ROLLOVER FEE ON THE WITHHOLD YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4200-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN IRAM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO IRAT-ROLLOVER-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAT-REVERSAL-FLAG = 'Y'
                   COMPUTE IRAT-ROLLOVER-AMT ROUNDED = IRAM-WITHHOLD-YTD
                       * IRAM-RATE / 200
               WHEN IRAT-TRAN-CD = 'CN'
                   MOVE 15.00 TO IRAT-ROLLOVER-AMT
               WHEN IRAT-TRAN-CD = 'DS'
                   MOVE 7.00 TO IRAT-ROLLOVER-AMT
               WHEN OTHER
                   COMPUTE IRAT-ROLLOVER-AMT ROUNDED = IRAM-WITHHOLD-YTD
                       * IRAM-RATE / 100
           END-EVALUATE
           IF IRAT-ROLLOVER-AMT > 0 AND IRAT-ROLLOVER-AMT < 5.00
               MOVE 5.00 TO IRAT-ROLLOVER-AMT
           END-IF
           IF IRAT-ROLLOVER-AMT > 250.00
               MOVE 250.00 TO IRAT-ROLLOVER-AMT
           END-IF
           ADD IRAT-ROLLOVER-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB TIER TIER FOR THE RMD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4210-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-TIER-RATE(1) TO IRAM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF IRAM-RMD-YTD NOT < WS-CONTRIB-TIER-LOW(WS-TX)
                   MOVE WS-CONTRIB-TIER-RATE(WS-TX) TO IRAM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCESS TIER TIER FOR THE RMD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4220-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-EXCESS-TIER-RATE(1) TO IRAM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF IRAM-RMD-AMT NOT < WS-EXCESS-TIER-LOW(WS-TX)
                   MOVE WS-EXCESS-TIER-RATE(WS-TX) TO IRAM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ROLLOVER FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4230-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN IRAM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO IRAT-ROLLOVER-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-REVIEW-FLAG = 'Y'
                   COMPUTE IRAT-ROLLOVER-AMT ROUNDED = WS-WORK-AMT-2 *
                       IRAM-MARGIN / 200
               WHEN IRAM-IRA-TYPE-CD = 'F'
                   MOVE 7.00 TO IRAT-ROLLOVER-AMT
               WHEN IRAM-IRA-TYPE-CD = 'V'
                   MOVE 12.00 TO IRAT-ROLLOVER-AMT
               WHEN OTHER
                   COMPUTE IRAT-ROLLOVER-AMT ROUNDED = WS-WORK-AMT-2 *
                       IRAM-MARGIN / 100
           END-EVALUATE
           IF IRAT-ROLLOVER-AMT > 0 AND IRAT-ROLLOVER-AMT < 1.00
               MOVE 1.00 TO IRAT-ROLLOVER-AMT
           END-IF
           IF IRAT-ROLLOVER-AMT > 25.00
               MOVE 25.00 TO IRAT-ROLLOVER-AMT
           END-IF
           ADD IRAT-ROLLOVER-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CONTRIB AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4240-FLAG-CONTRIB.
           IF IRAM-CONTRIB-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRAM-CONTRIB-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4250-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO IRAM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4260-COUNT-ITEM.
           IF IRAM-ITEM-CNT < 9999999
               ADD 1 TO IRAM-ITEM-CNT
           END-IF
           IF IRAM-ITEM-CNT NOT < 3
               MOVE 'Y' TO IRAT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB YTD FEE ON THE ROLLOVER AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4270-ASSESS-STOP.
           EVALUATE TRUE
               WHEN IRAM-STOP-FLAG = 'Y'
                   MOVE ZERO TO IRAM-CONTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAT-REVERSAL-FLAG = 'Y'
                   COMPUTE IRAM-CONTRIB-YTD ROUNDED = IRAT-ROLLOVER-AMT
                       * IRAM-MARGIN / 200
               WHEN IRAM-IRA-TYPE-CD = 'F'
                   MOVE 20.00 TO IRAM-CONTRIB-YTD
               WHEN IRAM-IRA-TYPE-CD = 'V'
                   MOVE 7.00 TO IRAM-CONTRIB-YTD
               WHEN OTHER
                   COMPUTE IRAM-CONTRIB-YTD ROUNDED = IRAT-ROLLOVER-AMT
                       * IRAM-MARGIN / 100
           END-EVALUATE
           IF IRAM-CONTRIB-YTD > 0 AND IRAM-CONTRIB-YTD < 1.00
               MOVE 1.00 TO IRAM-CONTRIB-YTD
           END-IF
           IF IRAM-CONTRIB-YTD > 100.00
               MOVE 100.00 TO IRAM-CONTRIB-YTD
           END-IF
           ADD IRAM-CONTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROLLOVER YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4280-ACCRUE-ROLLOVER.
           IF IRAM-ROLLOVER-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-ROLLOVER-YTD * IRAM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO IRAT-ROLLOVER-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E455 ADDR REQUIRED
      * E482 CONTRIB AMT NEGATIVE
      * E054 CONTRIB AMT OVER 1000000
      * E092 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4290-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-ADDR = SPACES
                   MOVE 'E455' TO WS-REJECT-CODE
               WHEN IRAM-CONTRIB-AMT < 0
                   MOVE 'E482' TO WS-REJECT-CODE
               WHEN IRAM-CONTRIB-AMT > 1000000.00
                   MOVE 'E054' TO WS-REJECT-CODE
               WHEN IRAM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E092' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4300-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-WITHHOLD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD IRAM-WITHHOLD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD IRAM-WITHHOLD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD IRAM-WITHHOLD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-WITHHOLD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4310-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-REASON-CD
                   ADD IRAT-DISTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAT-DISTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4320-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-DISTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD IRAM-DISTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD IRAM-DISTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRAM-DISTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-DISTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT EXCESS AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4330-SPLIT-EXCESS.
           COMPUTE WS-WORK-AMT = IRAM-EXCESS-AMT / 3
           COMPUTE WS-WORK-AMT-2 = IRAM-EXCESS-AMT - (WS-WORK-AMT * 3)
           COMPUTE IRAT-ROLLOVER-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4340-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAT-CHANNEL-CD
                   ADD IRAM-DISTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-DISTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4350-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE IRAM-CONTRIB-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AS -> AY WHEN NOTICE DATE IS PAST
      * AY -> JR WHEN NOTICE DATE IS PAST
      * JR -> TA WHEN NOTICE DATE IS PAST
      * TA -> UM WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4360-SET-CHANNEL.
           EVALUATE IRAM-CHANNEL-CD
               WHEN 'AS'
                   IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AY' TO IRAM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AY'
                   IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JR' TO IRAM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JR'
                   IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TA' TO IRAM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UM' TO IRAM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE RMD YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4370-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-REASON-CD
                   ADD IRAM-RMD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-RMD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCESS AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4380-FLAG-EXCESS.
           IF IRAT-EXCESS-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE IRAT-EXCESS-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WITHHOLD AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4390-ACCRUE-WITHHOLD.
           IF IRAM-WITHHOLD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-WITHHOLD-AMT * IRAM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4400-DESCRIBE-CHANNEL.
           EVALUATE IRAM-CHANNEL-CD
               WHEN 'AS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'AY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4410-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-WITHHOLD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD IRAM-WITHHOLD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD IRAM-WITHHOLD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRAM-WITHHOLD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-WITHHOLD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WITHHOLD YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4420-FLAG-WITHHOLD.
           IF IRAM-WITHHOLD-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE IRAM-WITHHOLD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4430-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = IRAM-DISTRIB-YTD / 4
           COMPUTE WS-WORK-AMT-2 = IRAM-DISTRIB-YTD - (WS-WORK-AMT * 4)
           COMPUTE IRAT-ROLLOVER-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RMD YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4440-LIMIT-RMD.
           IF IRAM-RMD-YTD < 25.00
               MOVE 25.00 TO IRAM-RMD-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-RMD-YTD > 1000.00
               MOVE 1000.00 TO IRAM-RMD-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4450-SPLIT-RMD.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE IRAM-RMD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCESS AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4460-LIMIT-EXCESS.
           IF IRAM-EXCESS-AMT < 25.00
               MOVE 25.00 TO IRAM-EXCESS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-EXCESS-AMT > 2500.00
               MOVE 2500.00 TO IRAM-EXCESS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FMV YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4470-FLAG-FMV.
           IF IRAM-FMV-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE IRAM-FMV-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * EA -> HR WHEN NOTICE DATE IS PAST
      * HR -> XZ WHEN NOTICE DATE IS PAST
      * XZ -> EA WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4480-SET-TYPE.
           EVALUATE IRAM-TYPE-CD
               WHEN 'EA'
                   IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XZ' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XZ'
                   IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EA' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ROLLOVER AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FMV AMT.
      *----------------------------------------------------------------*
       4490-PRORATE-FMV.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-FMV-AMT ROUNDED =
                   IRAT-ROLLOVER-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAM-FMV-AMT
           END-IF
           ADD IRAM-FMV-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB SCHED TIER FOR THE DISTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4500-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-SCHED-RATE(1) TO IRAM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF IRAT-DISTRIB-AMT NOT < WS-DISTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-DISTRIB-SCHED-RATE(WS-TX) TO IRAM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FMV AMT BY IRA TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4510-ACCUM-IRA-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-IRA-TYPE-CD
                   ADD IRAM-FMV-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-IRA-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-FMV-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4520-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NEXT-REVIEW-DATE > 0 AND IRAM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB FEE ON THE CONTRIB YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4530-ASSESS-STOP.
           EVALUATE TRUE
               WHEN IRAM-STOP-FLAG = 'Y'
                   MOVE ZERO TO IRAM-DISTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-SUSPEND-FLAG = 'Y'
                   COMPUTE IRAM-DISTRIB-AMT ROUNDED = IRAM-CONTRIB-YTD *
                       IRAM-FEE-PCT / 200
               WHEN IRAM-REASON-CD = 'CMR'
                   MOVE 20.00 TO IRAM-DISTRIB-AMT
               WHEN IRAM-REASON-CD = 'LJS'
                   MOVE 3.00 TO IRAM-DISTRIB-AMT
               WHEN OTHER
                   COMPUTE IRAM-DISTRIB-AMT ROUNDED = IRAM-CONTRIB-YTD *
                       IRAM-FEE-PCT / 100
           END-EVALUATE
           IF IRAM-DISTRIB-AMT > 0 AND IRAM-DISTRIB-AMT < 1.00
               MOVE 1.00 TO IRAM-DISTRIB-AMT
           END-IF
           IF IRAM-DISTRIB-AMT > 75.00
               MOVE 75.00 TO IRAM-DISTRIB-AMT
           END-IF
           ADD IRAM-DISTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE ROLLOVER AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WITHHOLD YTD.
      *----------------------------------------------------------------*
       4540-PRORATE-WITHHOLD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-WITHHOLD-YTD ROUNDED =
                   IRAT-ROLLOVER-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAM-WITHHOLD-YTD
           END-IF
           ADD IRAM-WITHHOLD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE ROLLOVER GRID TIER FOR THE DISTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4550-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ROLLOVER-GRID-RATE(1) TO IRAM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF IRAT-DISTRIB-AMT NOT < WS-ROLLOVER-GRID-LOW(WS-TX)
                   MOVE WS-ROLLOVER-GRID-RATE(WS-TX) TO IRAM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4560-FLAG-WITHHOLD.
           IF IRAM-WITHHOLD-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE IRAM-WITHHOLD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB TIER TIER FOR THE DISTRIB YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4570-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-TIER-RATE(1) TO IRAM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF IRAM-DISTRIB-YTD NOT < WS-CONTRIB-TIER-LOW(WS-TX)
                   MOVE WS-CONTRIB-TIER-RATE(WS-TX) TO IRAM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4580-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-EXCESS-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD IRAM-EXCESS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD IRAM-EXCESS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRAM-EXCESS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-EXCESS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WITHHOLD YTD FEE ON THE CONTRIB YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4590-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN IRAM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO IRAM-WITHHOLD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-STOP-FLAG = 'Y'
                   COMPUTE IRAM-WITHHOLD-YTD ROUNDED = IRAM-CONTRIB-YTD
                       * IRAM-RATE / 200
               WHEN IRAM-IRA-TYPE-CD = 'F'
                   MOVE 3.00 TO IRAM-WITHHOLD-YTD
               WHEN IRAM-IRA-TYPE-CD = 'V'
                   MOVE 7.00 TO IRAM-WITHHOLD-YTD
               WHEN OTHER
                   COMPUTE IRAM-WITHHOLD-YTD ROUNDED = IRAM-CONTRIB-YTD
                       * IRAM-RATE / 100
           END-EVALUATE
           IF IRAM-WITHHOLD-YTD > 0 AND IRAM-WITHHOLD-YTD < 10.00
               MOVE 10.00 TO IRAM-WITHHOLD-YTD
           END-IF
           IF IRAM-WITHHOLD-YTD > 35.00
               MOVE 35.00 TO IRAM-WITHHOLD-YTD
           END-IF
           ADD IRAM-WITHHOLD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E256 CONTACT REQUIRED
      * E068 ROLLOVER AMT NEGATIVE
      * E894 ROLLOVER AMT OVER 5000
      * E956 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4600-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-CONTACT = SPACES
                   MOVE 'E256' TO WS-REJECT-CODE
               WHEN IRAM-ROLLOVER-AMT < 0
                   MOVE 'E068' TO WS-REJECT-CODE
               WHEN IRAM-ROLLOVER-AMT > 5000.00
                   MOVE 'E894' TO WS-REJECT-CODE
               WHEN IRAM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E956' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4610-COUNT-RETRY.
           IF IRAM-RETRY-CNT < 9999999
               ADD 1 TO IRAM-RETRY-CNT
           END-IF
           IF IRAM-RETRY-CNT NOT < 6
               MOVE 'Y' TO IRAM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE IRA FIELDS TO IRELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-IRELG.
           INITIALIZE IRELG-PARMS
           MOVE IRAM-FMV-AMT
             TO IRELG-FMV-AMT
           MOVE IRAM-EXCESS-AMT
             TO IRELG-EXCESS-AMT
           MOVE IRAM-ROLLOVER-YTD
             TO IRELG-ROLLOVER-YTD
           MOVE IRAM-RMD-AMT
             TO IRELG-RMD-AMT
           MOVE IRAM-ROLLOVER-AMT
             TO IRELG-ROLLOVER-AMT
           MOVE IRAM-FMV-YTD
             TO IRELG-FMV-YTD
           MOVE IRAM-LAST-ACT-DATE
             TO IRELG-LAST-ACT-DATE
           MOVE IRAM-LAST-STMT-DATE
             TO IRELG-LAST-STMT-DATE
           MOVE IRAM-EXPIRY-DATE
             TO IRELG-EXPIRY-DATE
           MOVE IRAM-NEXT-REVIEW-DATE
             TO IRELG-NEXT-REVIEW-DATE
           MOVE IRAM-CHANNEL-CD
             TO IRELG-CHANNEL-CD
           MOVE IRAM-REGION-CD
             TO IRELG-REGION-CD
           MOVE IRAM-REASON-CD
             TO IRELG-REASON-CD
           MOVE IRAM-TYPE-CD
             TO IRELG-TYPE-CD
           MOVE IRAM-STOP-FLAG
             TO IRELG-STOP-FLAG
           MOVE IRAM-REVIEW-FLAG
             TO IRELG-REVIEW-FLAG
           MOVE IRAM-AUTO-RENEW-FLAG
             TO IRELG-AUTO-RENEW-FLAG
           MOVE IRAM-CYCLE-CNT
             TO IRELG-CYCLE-CNT
           MOVE IRAM-RETRY-CNT
             TO IRELG-RETRY-CNT
           MOVE IRAM-MARGIN
             TO IRELG-MARGIN
           MOVE IRAM-FEE-PCT
             TO IRELG-FEE-PCT
           MOVE IRAM-ADDR
             TO IRELG-ADDR
           MOVE IRAM-NAME
             TO IRELG-NAME
           CALL 'IRELG' USING IRELG-PARMS
           EVALUATE IRELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE IRELG-FMV-AMT
                     TO IRAM-FMV-AMT
                   MOVE IRELG-ROLLOVER-AMT
                     TO IRAM-ROLLOVER-AMT
                   MOVE IRELG-FMV-YTD
                     TO IRAM-FMV-YTD
                   MOVE IRELG-REVIEW-FLAG
                     TO IRAM-REVIEW-FLAG
                   MOVE IRELG-AUTO-RENEW-FLAG
                     TO IRAM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-IRELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE IRA FIELDS TO IRPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-IRPRC.
           INITIALIZE IRPRC-PARMS
           MOVE IRAM-RMD-YTD
             TO IRPRC-RMD-YTD
           MOVE IRAM-CONTRIB-YTD
             TO IRPRC-CONTRIB-YTD
           MOVE IRAM-EXCESS-YTD
             TO IRPRC-EXCESS-YTD
           MOVE IRAM-DISTRIB-YTD
             TO IRPRC-DISTRIB-YTD
           MOVE IRAM-FMV-AMT
             TO IRPRC-FMV-AMT
           MOVE IRAM-ROLLOVER-AMT
             TO IRPRC-ROLLOVER-AMT
           MOVE IRAM-NEXT-REVIEW-DATE
             TO IRPRC-NEXT-REVIEW-DATE
           MOVE IRAM-LAST-STMT-DATE
             TO IRPRC-LAST-STMT-DATE
           MOVE IRAM-LAST-ACT-DATE
             TO IRPRC-LAST-ACT-DATE
           MOVE IRAM-EXPIRY-DATE
             TO IRPRC-EXPIRY-DATE
           MOVE IRAM-IRA-TYPE-CD
             TO IRPRC-IRA-TYPE-CD
           MOVE IRAM-TYPE-CD
             TO IRPRC-TYPE-CD
           MOVE IRAM-CHANNEL-CD
             TO IRPRC-CHANNEL-CD
           MOVE IRAM-REASON-CD
             TO IRPRC-REASON-CD
           MOVE IRAM-OVERRIDE-FLAG
             TO IRPRC-OVERRIDE-FLAG
           MOVE IRAM-SUSPEND-FLAG
             TO IRPRC-SUSPEND-FLAG
           MOVE IRAM-AUTO-RENEW-FLAG
             TO IRPRC-AUTO-RENEW-FLAG
           MOVE IRAM-PAYMENT-CNT
             TO IRPRC-PAYMENT-CNT
           MOVE IRAM-RETRY-CNT
             TO IRPRC-RETRY-CNT
           MOVE IRAM-MARGIN
             TO IRPRC-MARGIN
           MOVE IRAM-FEE-PCT
             TO IRPRC-FEE-PCT
           MOVE IRAM-NAME
             TO IRPRC-NAME
           MOVE IRAM-ADDR
             TO IRPRC-ADDR
           CALL 'IRPRC' USING IRPRC-PARMS
           EVALUATE IRPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE IRPRC-EXCESS-YTD
                     TO IRAM-EXCESS-YTD
                   MOVE IRPRC-DISTRIB-YTD
                     TO IRAM-DISTRIB-YTD
                   MOVE IRPRC-FMV-AMT
                     TO IRAM-FMV-AMT
                   MOVE IRPRC-ROLLOVER-AMT
                     TO IRAM-ROLLOVER-AMT
                   MOVE IRPRC-AUTO-RENEW-FLAG
                     TO IRAM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-IRPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE IRAT-KEY TO RD-KEY
           MOVE IRAT-TRAN-CD TO RD-CODE
           MOVE IRAT-DISTRIB-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE IRAT-KEY TO RD-KEY
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
           DISPLAY 'IRANOTC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'IRANOTC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
