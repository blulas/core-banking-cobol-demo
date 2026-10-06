      *================================================================*
      * PROGRAM  : ARPANAL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : ACCOUNT RECONCILEMENT - ANALYSIS. READS THE ARP     *
      *            TRANSACTIONS, UPDATES THE ARP MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. FULL AND PARTIAL ACCOUNT        *
      *            RECONCILEMENT SERVICE FOR CORPORATES                *
      * INPUT    : ARPMST (KSDS), ARPTRN, BANKCTL                      *
      * OUTPUT   : ARPOUT, ARPANRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ARPRC, ARRISK           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-03-27 PQR  ORIGINAL                                       *
      * 1996-04-19 SKN  OUTSTAND STATUS CODES ADDED CHG3925            *
      * 1999-01-23 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2005-11-21 PQR  VOID RERUN PROTECTION REQ3871                  *
      * 2005-06-18 GAF  VOID CUTOFF MOVED INC9559                      *
      * 2005-04-16 EWS  FEE CALCULATION CORRECTED CHG8547              *
      * 2008-11-03 TWH  FEE CALCULATION CORRECTED CHG0657              *
      * 2009-11-23 GAF  VOID CALCULATION CORRECTED INC4332             *
      * 2009-09-17 PQR  PERFORMANCE - OUTSTAND READ AHEAD PRB4351      *
      * 2011-12-13 DPS  STOP FEE SCHEDULE UPDATE INC1244               *
      * 2011-06-14 PQR  NEW ISSUED REPORT COLUMNS INC6919              *
      * 2014-03-07 MKL  ISSUED CALCULATION CORRECTED CHG5097           *
      * 2014-10-01 CRP  FEE CUTOFF MOVED INC9333                       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARPANAL.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "ARPMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ARPM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "ARPTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "ARPOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "ARPANRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY ARPMST.
       FD  TRAN-FILE.
       COPY ARPTRN.
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
                                           VALUE 18.000000.
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
      * WS-OUTSTAND-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-OUTSTAND-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000711000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000656000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000469000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002650000000880000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007650000000886000'.
       01  WS-OUTSTAND-TIER-TBL REDEFINES WS-OUTSTAND-TIER-VALUES.
           05  WS-OUTSTAND-TIER-ROW OCCURS 5 TIMES.
               10  WS-OUTSTAND-TIER-LOW    PIC 9(09)V99.
               10  WS-OUTSTAND-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VOID-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOID-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000315000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000891500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000226000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003600000000170000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013600000000145000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014600000000483500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014650000000389000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019650000000690000'.
       01  WS-VOID-BAND-TBL REDEFINES WS-VOID-BAND-VALUES.
           05  WS-VOID-BAND-ROW OCCURS 8 TIMES.
               10  WS-VOID-BAND-LOW        PIC 9(09)V99.
               10  WS-VOID-BAND-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VOID-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOID-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000713500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000050500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000841000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015250000000536500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000474000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025500000000835500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025600000000875500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026600000000356000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026650000000561000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026750000000782500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027750000000861000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032750000000809000'.
       01  WS-VOID-SCHED-TBL REDEFINES WS-VOID-SCHED-VALUES.
           05  WS-VOID-SCHED-ROW OCCURS 12 TIMES.
               10  WS-VOID-SCHED-LOW       PIC 9(09)V99.
               10  WS-VOID-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-OUTSTAND-TIERX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-OUTSTAND-TIERX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000033500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000010000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000620000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008000000000189500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009000000000042500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000171500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011550000000752000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011800000000604500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014300000000772000'.
       01  WS-OUTSTAND-TIERX-TBL REDEFINES WS-OUTSTAND-TIERX-VALUES.
           05  WS-OUTSTAND-TIERX-ROW OCCURS 9 TIMES.
               10  WS-OUTSTAND-TIERX-LOW   PIC 9(09)V99.
               10  WS-OUTSTAND-TIERX-RATE  PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ARPANAL  ACCOUNT RECONCILEMENT'.
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
       COPY ARPRCP.
       COPY ARRISKP.

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
               MOVE 'ARPMST' TO ABND-FILE-NAME
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
           MOVE ARPT-KEY TO ARPM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-COUNT-SEQ
               PERFORM 3010-ACCRUE-PAID
               PERFORM 3020-PRORATE-OUTSTAND
               PERFORM 3030-DESCRIBE-BRANCH
               PERFORM 3040-LIMIT-PAID
               PERFORM 3050-COUNT-CYCLE
               PERFORM 3060-DESCRIBE-CYCLE
               PERFORM 3070-CHECK-NEXT-REVIEW
               PERFORM 3080-SPLIT-VOID
               PERFORM 3090-COUNT-EXCEPT
               PERFORM 3100-SELECT-PENALTY-PCT
               PERFORM 3110-FLAG-PAID
               PERFORM 3120-CHECK-EFF
               PERFORM 3130-AGE-OPEN
               PERFORM 3140-AGE-CLOSE
               PERFORM 3150-CHECK-NEXT-DUE
               PERFORM 3160-SELECT-DISCOUNT
               PERFORM 3170-COUNT-PAYMENT
               PERFORM 3180-SPLIT-VOID
               PERFORM 3190-LIMIT-ISSUED
               PERFORM 3200-ASSESS-PREFERRED
               PERFORM 3210-EDIT-ADDR
               PERFORM 3220-COUNT-EXCEPT
               PERFORM 3230-EDIT-DESC
               PERFORM 3240-SELECT-PENALTY-PCT
               PERFORM 3250-PRORATE-PAID
               PERFORM 3260-ASSESS-EXEMPT
               PERFORM 3270-LIMIT-PAID
               PERFORM 3280-EDIT-NAME
               PERFORM 3290-FLAG-OUTSTAND
               PERFORM 3300-CHECK-LAST-STMT
               PERFORM 3310-PRORATE-PAID
               PERFORM 3320-SPLIT-OUTSTAND
               PERFORM 3330-LIMIT-PAID
               PERFORM 3340-COUNT-EXCEPT
               PERFORM 3350-SPLIT-FEE
               PERFORM 3360-LIMIT-VOID
               PERFORM 3370-SELECT-DISCOUNT
               PERFORM 3380-FLAG-ITEM
               PERFORM 3390-LIMIT-OUTSTAND
               PERFORM 3400-SET-CHANNEL
               PERFORM 3410-DESCRIBE-CYCLE
               PERFORM 3420-COUNT-ITEM
               PERFORM 3430-CHECK-LAST-ACT
               PERFORM 3440-ASSESS-REVERSAL
               PERFORM 3450-SET-CHANNEL
               PERFORM 3460-COUNT-SEQ
               PERFORM 3470-SELECT-DISCOUNT
               PERFORM 3480-EDIT-ADDR
               PERFORM 3490-ACCUM-BRANCH
               PERFORM 3500-COUNT-SEQ
               PERFORM 3510-ACCRUE-FEE
               PERFORM 3520-AGE-LAST-ACT
               PERFORM 3530-ACCUM-TRAN
               PERFORM 3540-CHECK-LAST-MAINT
               PERFORM 3550-SPLIT-FEE
               PERFORM 3560-ASSESS-NOTICE-SENT
               PERFORM 3570-COUNT-SEQ
               PERFORM 3580-SELECT-DISCOUNT
               PERFORM 3590-PRORATE-FEE
               PERFORM 3600-CHECK-LAST-PAY
               PERFORM 3610-CHECK-LAST-STMT
               PERFORM 3620-CHECK-EFFECTIVE
               PERFORM 3630-ACCRUE-STOP
               PERFORM 3640-DESCRIBE-RECON-TYPE
               PERFORM 3650-ASSESS-HOLD
               PERFORM 3660-AGE-OPEN
               PERFORM 3670-ACCRUE-VOID
               PERFORM 3680-LIMIT-ISSUED
               PERFORM 3690-CHECK-NEXT-REVIEW
               PERFORM 3700-COUNT-SEQ
               PERFORM 3710-ACCUM-SOURCE
               PERFORM 3720-SPLIT-ISSUED
               PERFORM 3730-EDIT-ADDR
               PERFORM 3740-CHECK-LAST-STMT
               PERFORM 3750-AGE-POST
               PERFORM 3760-SET-CHANNEL
               PERFORM 3770-SET-CHANNEL
               PERFORM 3780-LIMIT-PAID
               PERFORM 3790-AGE-LAST-ACT
               PERFORM 3800-DESCRIBE-BRANCH
               PERFORM 3810-ASSESS-OVERRIDE
               PERFORM 3820-SELECT-PENALTY-PCT
               PERFORM 3830-ASSESS-REVERSAL
               PERFORM 3840-EDIT-REF-NO
               PERFORM 3850-SET-SOURCE
               PERFORM 3860-COUNT-PAYMENT
               PERFORM 3870-ACCUM-TRAN
               PERFORM 3880-ASSESS-OVERRIDE
               PERFORM 3890-SET-TRAN
               PERFORM 3900-PRORATE-VOID
               PERFORM 3910-DESCRIBE-CYCLE
               PERFORM 3920-FLAG-OUTSTAND
               PERFORM 3930-DESCRIBE-RECON-TYPE
               PERFORM 3940-COUNT-PAYMENT
               PERFORM 3950-SELECT-PENALTY-PCT
               PERFORM 3960-PRORATE-OUTSTAND
               PERFORM 3970-SPLIT-FEE
               PERFORM 3980-EDIT-CUST-ID
               PERFORM 3990-PRORATE-OUTSTAND
               PERFORM 4000-SET-CYCLE
               PERFORM 4010-SELECT-PENALTY-PCT
               PERFORM 4020-DESCRIBE-SOURCE
               PERFORM 4030-ACCRUE-VOID
               PERFORM 4040-FLAG-VOID
               PERFORM 4050-DESCRIBE-CHANNEL
               PERFORM 4060-LIMIT-STOP
               PERFORM 4070-SPLIT-VOID
               PERFORM 4080-ACCUM-SOURCE
               PERFORM 4090-ACCUM-RECON-TYPE
               PERFORM 4100-AGE-CLOSE
               PERFORM 4110-ACCRUE-OUTSTAND
               PERFORM 4120-EDIT-CUST-ID
               PERFORM 4130-SELECT-FEE-PCT
               PERFORM 4140-AGE-NEXT-DUE
               PERFORM 4150-FLAG-FEE
               PERFORM 4160-SET-REGION
               PERFORM 4170-COUNT-PAYMENT
               PERFORM 4180-ACCUM-CHANNEL
               PERFORM 4190-SELECT-DISCOUNT
               PERFORM 4200-PRORATE-STOP
               PERFORM 4210-SET-CHANNEL
               PERFORM 4220-SELECT-PENALTY-PCT
               PERFORM 4230-EDIT-REJECT
               PERFORM 4240-SPLIT-STOP
               PERFORM 4250-SELECT-DISCOUNT
               PERFORM 4260-EDIT-ADDR
               PERFORM 4270-LIMIT-ISSUED
               PERFORM 4280-SPLIT-FEE
               PERFORM 4290-ACCUM-REGION
               PERFORM 4300-COUNT-ITEM
               PERFORM 4310-ASSESS-OVERRIDE
               PERFORM 4320-LIMIT-FEE
               PERFORM 4330-ACCUM-CHANNEL
               PERFORM 4340-SPLIT-FEE
               PERFORM 4350-SPLIT-VOID
               PERFORM 4360-ACCRUE-STOP
               PERFORM 4370-FLAG-ITEM
               PERFORM 4380-FLAG-PAID
               PERFORM 4390-FLAG-ITEM
               PERFORM 4400-SET-REGION
               PERFORM 4410-LIMIT-ISSUED
               PERFORM 4420-CHECK-LAST-MAINT
               PERFORM 4430-LIMIT-FEE
               PERFORM 4440-AGE-LAST-ACT
               PERFORM 4450-AGE-LAST-ACT
               PERFORM 4460-SPLIT-VOID
               PERFORM 4470-COUNT-CYCLE
               PERFORM 7000-CALL-ARPRC
               PERFORM 7030-CALL-ARRISK
               REWRITE ARP-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'ARPMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM ARP-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3000-COUNT-SEQ.
           IF ARPT-SEQ < 9999999
               ADD 1 TO ARPT-SEQ
           END-IF
           IF ARPT-SEQ NOT < 3
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-PAID.
           IF ARPM-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-PAID-AMT * ARPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OUTSTAND AMT.
      *----------------------------------------------------------------*
       3020-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-OUTSTAND-AMT ROUNDED =
                   ARPT-OUTSTAND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-OUTSTAND-AMT
           END-IF
           ADD ARPM-OUTSTAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3030-DESCRIBE-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAID YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3040-LIMIT-PAID.
           IF ARPM-PAID-YTD < 25.00
               MOVE 25.00 TO ARPM-PAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-PAID-YTD > 2500.00
               MOVE 2500.00 TO ARPM-PAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3050-COUNT-CYCLE.
           IF ARPM-CYCLE-CNT < 9999999
               ADD 1 TO ARPM-CYCLE-CNT
           END-IF
           IF ARPM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO ARPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3060-DESCRIBE-CYCLE.
           EVALUATE ARPM-CYCLE-CD
               WHEN 'CP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3070-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3080-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = ARPT-VOID-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ARPT-VOID-AMT - (WS-WORK-AMT * 12)
           COMPUTE ARPM-PAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3090-COUNT-EXCEPT.
           IF ARPM-EXCEPT-CNT < 9999999
               ADD 1 TO ARPM-EXCEPT-CNT
           END-IF
           IF ARPM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND TIERX TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3100-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-TIERX-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-OUTSTAND-TIERX-LOW(WS-TX)
                   MOVE WS-OUTSTAND-TIERX-RATE(WS-TX) TO
                       ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3110-FLAG-PAID.
           IF ARPM-PAID-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARPM-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3120-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF ARPT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ARPM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3130-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3140-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-VOID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ARPM-VOID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ARPM-VOID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ARPM-VOID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-VOID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND TIER TIER FOR THE STOP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3160-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-TIER-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ARPM-STOP-AMT NOT < WS-OUTSTAND-TIER-LOW(WS-TX)
                   MOVE WS-OUTSTAND-TIER-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3170-COUNT-PAYMENT.
           IF ARPM-PAYMENT-CNT < 9999999
               ADD 1 TO ARPM-PAYMENT-CNT
           END-IF
           IF ARPM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3180-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = ARPT-VOID-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ARPT-VOID-AMT - (WS-WORK-AMT * 12)
           COMPUTE ARPM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3190-LIMIT-ISSUED.
           IF ARPT-ISSUED-AMT < 25.00
               MOVE 25.00 TO ARPT-ISSUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPT-ISSUED-AMT > 10000.00
               MOVE 10000.00 TO ARPT-ISSUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STOP YTD FEE ON THE FEE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3200-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ARPM-STOP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-STOP-FLAG = 'Y'
                   COMPUTE ARPM-STOP-YTD ROUNDED = ARPM-FEE-AMT *
                       ARPM-FEE-PCT / 200
               WHEN ARPT-TRAN-CD = 'IS'
                   MOVE 7.00 TO ARPM-STOP-YTD
               WHEN ARPT-TRAN-CD = 'PD'
                   MOVE 3.00 TO ARPM-STOP-YTD
               WHEN OTHER
                   COMPUTE ARPM-STOP-YTD ROUNDED = ARPM-FEE-AMT *
                       ARPM-FEE-PCT / 100
           END-EVALUATE
           IF ARPM-STOP-YTD > 0 AND ARPM-STOP-YTD < 5.00
               MOVE 5.00 TO ARPM-STOP-YTD
           END-IF
           IF ARPM-STOP-YTD > 25.00
               MOVE 25.00 TO ARPM-STOP-YTD
           END-IF
           ADD ARPM-STOP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E627 ADDR REQUIRED
      * E783 OUTSTAND AMT NEGATIVE
      * E405 OUTSTAND AMT OVER 50000
      * E409 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3210-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-ADDR = SPACES
                   MOVE 'E627' TO WS-REJECT-CODE
               WHEN ARPT-OUTSTAND-AMT < 0
                   MOVE 'E783' TO WS-REJECT-CODE
               WHEN ARPT-OUTSTAND-AMT > 50000.00
                   MOVE 'E405' TO WS-REJECT-CODE
               WHEN ARPM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E409' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3220-COUNT-EXCEPT.
           IF ARPM-EXCEPT-CNT < 9999999
               ADD 1 TO ARPM-EXCEPT-CNT
           END-IF
           IF ARPM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E146 DESC REQUIRED
      * E267 STOP AMT NEGATIVE
      * E143 STOP AMT OVER 99999
      * E559 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3230-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-DESC = SPACES
                   MOVE 'E146' TO WS-REJECT-CODE
               WHEN ARPM-STOP-AMT < 0
                   MOVE 'E267' TO WS-REJECT-CODE
               WHEN ARPM-STOP-AMT > 99999.00
                   MOVE 'E143' TO WS-REJECT-CODE
               WHEN ARPM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E559' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID BAND TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3240-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-BAND-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ARPT-ISSUED-AMT NOT < WS-VOID-BAND-LOW(WS-TX)
                   MOVE WS-VOID-BAND-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       3250-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-PAID-AMT ROUNDED =
                   ARPT-VOID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-PAID-AMT
           END-IF
           ADD ARPM-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE VOID AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3260-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ARPM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ARPM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-HOLD-FLAG = 'Y'
                   COMPUTE ARPM-FEE-AMT ROUNDED = ARPT-VOID-AMT *
                       ARPM-DISCOUNT / 200
               WHEN ARPM-RECON-TYPE-CD = 'EEH'
                   MOVE 3.00 TO ARPM-FEE-AMT
               WHEN ARPM-RECON-TYPE-CD = 'KBS'
                   MOVE 5.00 TO ARPM-FEE-AMT
               WHEN OTHER
                   COMPUTE ARPM-FEE-AMT ROUNDED = ARPT-VOID-AMT *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPM-FEE-AMT > 0 AND ARPM-FEE-AMT < 2.00
               MOVE 2.00 TO ARPM-FEE-AMT
           END-IF
           IF ARPM-FEE-AMT > 25.00
               MOVE 25.00 TO ARPM-FEE-AMT
           END-IF
           ADD ARPM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3270-LIMIT-PAID.
           IF ARPM-PAID-AMT < 10.00
               MOVE 10.00 TO ARPM-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-PAID-AMT > 2500.00
               MOVE 2500.00 TO ARPM-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E520 NAME REQUIRED
      * E927 FEE YTD NEGATIVE
      * E377 FEE YTD OVER 250000
      * E646 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3280-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-NAME = SPACES
                   MOVE 'E520' TO WS-REJECT-CODE
               WHEN ARPM-FEE-YTD < 0
                   MOVE 'E927' TO WS-REJECT-CODE
               WHEN ARPM-FEE-YTD > 250000.00
                   MOVE 'E377' TO WS-REJECT-CODE
               WHEN ARPM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E646' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3290-FLAG-OUTSTAND.
           IF ARPM-OUTSTAND-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ARPM-OUTSTAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3300-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID YTD.
      *----------------------------------------------------------------*
       3310-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-PAID-YTD ROUNDED =
                   ARPM-ISSUED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-PAID-YTD
           END-IF
           ADD ARPM-PAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3320-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = ARPT-OUTSTAND-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ARPT-OUTSTAND-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3330-LIMIT-PAID.
           IF ARPM-PAID-YTD < 10.00
               MOVE 10.00 TO ARPM-PAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-PAID-YTD > 1000.00
               MOVE 1000.00 TO ARPM-PAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3340-COUNT-EXCEPT.
           IF ARPM-EXCEPT-CNT < 9999999
               ADD 1 TO ARPM-EXCEPT-CNT
           END-IF
           IF ARPM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3350-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = ARPT-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ARPT-FEE-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3360-LIMIT-VOID.
           IF ARPT-VOID-AMT < 50.00
               MOVE 50.00 TO ARPT-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPT-VOID-AMT > 10000.00
               MOVE 10000.00 TO ARPT-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND TIER TIER FOR THE OUTSTAND YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3370-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-TIER-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ARPM-OUTSTAND-YTD NOT < WS-OUTSTAND-TIER-LOW(WS-TX)
                   MOVE WS-OUTSTAND-TIER-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3380-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3390-LIMIT-OUTSTAND.
           IF ARPT-OUTSTAND-AMT < 50.00
               MOVE 50.00 TO ARPT-OUTSTAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPT-OUTSTAND-AMT > 5000.00
               MOVE 5000.00 TO ARPT-OUTSTAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST STMT DATE IS PAST
      * BAT -> MAL WHEN LAST STMT DATE IS PAST
      * MAL -> TEL WHEN LAST STMT DATE IS PAST
      * TEL -> ONL WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3400-SET-CHANNEL.
           EVALUATE ARPT-CHANNEL-CD
               WHEN 'BRN'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3410-DESCRIBE-CYCLE.
           EVALUATE ARPM-CYCLE-CD
               WHEN 'CP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3420-COUNT-ITEM.
           IF ARPM-ITEM-CNT < 9999999
               ADD 1 TO ARPM-ITEM-CNT
           END-IF
           IF ARPM-ITEM-CNT NOT < 5
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3430-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ARPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STOP FEE ON THE OUTSTAND AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3440-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ARPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ARPM-STOP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-OVERRIDE-FLAG = 'Y'
                   COMPUTE ARPM-STOP-AMT ROUNDED = ARPT-OUTSTAND-AMT *
                       ARPM-FEE-PCT / 200
               WHEN ARPM-CYCLE-CD = 'CP'
                   MOVE 15.00 TO ARPM-STOP-AMT
               WHEN ARPM-CYCLE-CD = 'MR'
                   MOVE 12.00 TO ARPM-STOP-AMT
               WHEN OTHER
                   COMPUTE ARPM-STOP-AMT ROUNDED = ARPT-OUTSTAND-AMT *
                       ARPM-FEE-PCT / 100
           END-EVALUATE
           IF ARPM-STOP-AMT > 0 AND ARPM-STOP-AMT < 2.00
               MOVE 2.00 TO ARPM-STOP-AMT
           END-IF
           IF ARPM-STOP-AMT > 35.00
               MOVE 35.00 TO ARPM-STOP-AMT
           END-IF
           ADD ARPM-STOP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST MAINT DATE IS PAST
      * BAT -> MAL WHEN LAST MAINT DATE IS PAST
      * MAL -> TEL WHEN LAST MAINT DATE IS PAST
      * TEL -> ONL WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3450-SET-CHANNEL.
           EVALUATE ARPT-CHANNEL-CD
               WHEN 'BRN'
                   IF ARPM-LAST-MAINT-DATE > 0 AND ARPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ARPM-LAST-MAINT-DATE > 0 AND ARPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ARPM-LAST-MAINT-DATE > 0 AND ARPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ARPM-LAST-MAINT-DATE > 0 AND ARPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3460-COUNT-SEQ.
           IF ARPT-SEQ < 9999999
               ADD 1 TO ARPT-SEQ
           END-IF
           IF ARPT-SEQ NOT < 10
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID SCHED TIER FOR THE VOID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3470-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-SCHED-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ARPM-VOID-YTD NOT < WS-VOID-SCHED-LOW(WS-TX)
                   MOVE WS-VOID-SCHED-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E885 ADDR REQUIRED
      * E267 OUTSTAND AMT NEGATIVE
      * E639 OUTSTAND AMT OVER 25000
      * E372 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3480-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-ADDR = SPACES
                   MOVE 'E885' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-AMT < 0
                   MOVE 'E267' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-AMT > 25000.00
                   MOVE 'E639' TO WS-REJECT-CODE
               WHEN ARPM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E372' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3490-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-BRANCH-CD
                   ADD ARPT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3500-COUNT-SEQ.
           IF ARPT-SEQ < 9999999
               ADD 1 TO ARPT-SEQ
           END-IF
           IF ARPT-SEQ NOT < 12
               MOVE 'Y' TO ARPM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3510-ACCRUE-FEE.
           IF ARPM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-FEE-AMT * ARPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARPT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3520-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE OUTSTAND AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3530-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPT-TRAN-CD
                   ADD ARPM-OUTSTAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-OUTSTAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3540-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ARPM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3550-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE ARPT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE STOP YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3560-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ARPT-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPT-REVERSAL-FLAG = 'Y'
                   COMPUTE ARPT-ISSUED-AMT ROUNDED = ARPM-STOP-YTD *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO ARPT-ISSUED-AMT
               WHEN ARPT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO ARPT-ISSUED-AMT
               WHEN OTHER
                   COMPUTE ARPT-ISSUED-AMT ROUNDED = ARPM-STOP-YTD *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPT-ISSUED-AMT > 0 AND ARPT-ISSUED-AMT < 2.00
               MOVE 2.00 TO ARPT-ISSUED-AMT
           END-IF
           IF ARPT-ISSUED-AMT > 50.00
               MOVE 50.00 TO ARPT-ISSUED-AMT
           END-IF
           ADD ARPT-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3570-COUNT-SEQ.
           IF ARPT-SEQ < 9999999
               ADD 1 TO ARPT-SEQ
           END-IF
           IF ARPT-SEQ NOT < 10
               MOVE 'Y' TO ARPM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND TIERX TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3580-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-TIERX-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-OUTSTAND-TIERX-LOW(WS-TX)
                   MOVE WS-OUTSTAND-TIERX-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3590-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-FEE-AMT ROUNDED =
                   ARPT-ISSUED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-FEE-AMT
           END-IF
           ADD ARPM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3600-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3610-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO ARPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3620-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STOP AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3630-ACCRUE-STOP.
           IF ARPM-STOP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-STOP-AMT * ARPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-ISSUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE RECON TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3640-DESCRIBE-RECON-TYPE.
           EVALUATE ARPM-RECON-TYPE-CD
               WHEN 'EEH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KBS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PAC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VKT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YMX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZVT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND FEE ON THE VOID YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3650-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ARPM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ARPT-OUTSTAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-EXEMPT-FLAG = 'Y'
                   COMPUTE ARPT-OUTSTAND-AMT ROUNDED = ARPM-VOID-YTD *
                       ARPM-FEE-PCT / 200
               WHEN ARPM-RECON-TYPE-CD = 'EEH'
                   MOVE 15.00 TO ARPT-OUTSTAND-AMT
               WHEN ARPM-RECON-TYPE-CD = 'KBS'
                   MOVE 3.00 TO ARPT-OUTSTAND-AMT
               WHEN OTHER
                   COMPUTE ARPT-OUTSTAND-AMT ROUNDED = ARPM-VOID-YTD *
                       ARPM-FEE-PCT / 100
           END-EVALUATE
           IF ARPT-OUTSTAND-AMT > 0 AND ARPT-OUTSTAND-AMT < 10.00
               MOVE 10.00 TO ARPT-OUTSTAND-AMT
           END-IF
           IF ARPT-OUTSTAND-AMT > 25.00
               MOVE 25.00 TO ARPT-OUTSTAND-AMT
           END-IF
           ADD ARPT-OUTSTAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3660-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3670-ACCRUE-VOID.
           IF ARPM-VOID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-VOID-YTD * ARPM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3680-LIMIT-ISSUED.
           IF ARPM-ISSUED-YTD < 0.00
               MOVE 0.00 TO ARPM-ISSUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-ISSUED-YTD > 2500.00
               MOVE 2500.00 TO ARPM-ISSUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3690-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3700-COUNT-SEQ.
           IF ARPT-SEQ < 9999999
               ADD 1 TO ARPT-SEQ
           END-IF
           IF ARPT-SEQ NOT < 12
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3710-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-SOURCE-CD
                   ADD ARPT-VOID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPT-VOID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3720-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = ARPM-ISSUED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ARPM-ISSUED-AMT - (WS-WORK-AMT * 12)
           COMPUTE ARPM-STOP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E229 ADDR REQUIRED
      * E064 OUTSTAND AMT NEGATIVE
      * E701 OUTSTAND AMT OVER 50000
      * E657 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3730-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-ADDR = SPACES
                   MOVE 'E229' TO WS-REJECT-CODE
               WHEN ARPT-OUTSTAND-AMT < 0
                   MOVE 'E064' TO WS-REJECT-CODE
               WHEN ARPT-OUTSTAND-AMT > 50000.00
                   MOVE 'E701' TO WS-REJECT-CODE
               WHEN ARPM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E657' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3740-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ARPM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3750-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF ARPT-POST-DATE > 0 AND ARPT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-PAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ARPM-PAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ARPM-PAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARPM-PAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-PAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN CLOSE DATE IS PAST
      * BAT -> MAL WHEN CLOSE DATE IS PAST
      * MAL -> TEL WHEN CLOSE DATE IS PAST
      * TEL -> ONL WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-CHANNEL.
           EVALUATE ARPT-CHANNEL-CD
               WHEN 'BRN'
                   IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * ME -> SW WHEN NEXT REVIEW DATE IS PAST
      * SW -> UA WHEN NEXT REVIEW DATE IS PAST
      * UA -> WB WHEN NEXT REVIEW DATE IS PAST
      * WB -> ME WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3770-SET-CHANNEL.
           EVALUATE ARPM-CHANNEL-CD
               WHEN 'ME'
                   IF ARPM-NEXT-REVIEW-DATE > 0 AND
                       ARPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARPM-NEXT-REVIEW-DATE > 0 AND
                       ARPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UA' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF ARPM-NEXT-REVIEW-DATE > 0 AND
                       ARPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'WB' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF ARPM-NEXT-REVIEW-DATE > 0 AND
                       ARPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ME' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3780-LIMIT-PAID.
           IF ARPM-PAID-AMT < 100.00
               MOVE 100.00 TO ARPM-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-PAID-AMT > 5000.00
               MOVE 5000.00 TO ARPM-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3790-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ARPM-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ARPM-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARPM-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3800-DESCRIBE-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE PAID AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3810-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ARPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ARPM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-STOP-FLAG = 'Y'
                   COMPUTE ARPM-FEE-AMT ROUNDED = ARPM-PAID-AMT *
                       ARPM-DISCOUNT / 200
               WHEN ARPT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO ARPM-FEE-AMT
               WHEN ARPT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO ARPM-FEE-AMT
               WHEN OTHER
                   COMPUTE ARPM-FEE-AMT ROUNDED = ARPM-PAID-AMT *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPM-FEE-AMT > 0 AND ARPM-FEE-AMT < 1.00
               MOVE 1.00 TO ARPM-FEE-AMT
           END-IF
           IF ARPM-FEE-AMT > 100.00
               MOVE 100.00 TO ARPM-FEE-AMT
           END-IF
           ADD ARPM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE VOID SCHED TIER FOR THE OUTSTAND YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3820-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-SCHED-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ARPM-OUTSTAND-YTD NOT < WS-VOID-SCHED-LOW(WS-TX)
                   MOVE WS-VOID-SCHED-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND YTD FEE ON THE WORK AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3830-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ARPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ARPM-OUTSTAND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   COMPUTE ARPM-OUTSTAND-YTD ROUNDED = WS-WORK-AMT *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPM-RECON-TYPE-CD = 'EEH'
                   MOVE 10.00 TO ARPM-OUTSTAND-YTD
               WHEN ARPM-RECON-TYPE-CD = 'KBS'
                   MOVE 20.00 TO ARPM-OUTSTAND-YTD
               WHEN OTHER
                   COMPUTE ARPM-OUTSTAND-YTD ROUNDED = WS-WORK-AMT *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPM-OUTSTAND-YTD > 0 AND ARPM-OUTSTAND-YTD < 1.00
               MOVE 1.00 TO ARPM-OUTSTAND-YTD
           END-IF
           IF ARPM-OUTSTAND-YTD > 150.00
               MOVE 150.00 TO ARPM-OUTSTAND-YTD
           END-IF
           ADD ARPM-OUTSTAND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E160 REF NO REQUIRED
      * E880 FEE AMT NEGATIVE
      * E470 FEE AMT OVER 99999
      * E057 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3840-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-REF-NO = SPACES
                   MOVE 'E160' TO WS-REJECT-CODE
               WHEN ARPT-FEE-AMT < 0
                   MOVE 'E880' TO WS-REJECT-CODE
               WHEN ARPT-FEE-AMT > 99999.00
                   MOVE 'E470' TO WS-REJECT-CODE
               WHEN ARPM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E057' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * GW -> MB WHEN CLOSE DATE IS PAST
      * MB -> MH WHEN CLOSE DATE IS PAST
      * MH -> UG WHEN CLOSE DATE IS PAST
      * UG -> UR WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3850-SET-SOURCE.
           EVALUATE ARPM-SOURCE-CD
               WHEN 'GW'
                   IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MB' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MB'
                   IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MH' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MH'
                   IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3860-COUNT-PAYMENT.
           IF ARPM-PAYMENT-CNT < 9999999
               ADD 1 TO ARPM-PAYMENT-CNT
           END-IF
           IF ARPM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO ARPM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3870-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPT-TRAN-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE VOID AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3880-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ARPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ARPT-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPT-REVERSAL-FLAG = 'Y'
                   COMPUTE ARPT-VOID-AMT ROUNDED = ARPM-VOID-AMT *
                       ARPM-DISCOUNT / 200
               WHEN ARPM-REGION-CD = 'AD'
                   MOVE 15.00 TO ARPT-VOID-AMT
               WHEN ARPM-REGION-CD = 'BU'
                   MOVE 3.00 TO ARPT-VOID-AMT
               WHEN OTHER
                   COMPUTE ARPT-VOID-AMT ROUNDED = ARPM-VOID-AMT *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPT-VOID-AMT > 0 AND ARPT-VOID-AMT < 1.00
               MOVE 1.00 TO ARPT-VOID-AMT
           END-IF
           IF ARPT-VOID-AMT > 25.00
               MOVE 25.00 TO ARPT-VOID-AMT
           END-IF
           ADD ARPT-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN POST DATE IS PAST
      * PD -> SP WHEN POST DATE IS PAST
      * SP -> VD WHEN POST DATE IS PAST
      * VD -> FE WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3890-SET-TRAN.
           EVALUATE ARPT-TRAN-CD
               WHEN 'IS'
                   IF ARPT-POST-DATE > 0 AND ARPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF ARPT-POST-DATE > 0 AND ARPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF ARPT-POST-DATE > 0 AND ARPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF ARPT-POST-DATE > 0 AND ARPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FE' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       3900-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-VOID-AMT ROUNDED =
                   ARPT-ISSUED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-VOID-AMT
           END-IF
           ADD ARPM-VOID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3910-DESCRIBE-CYCLE.
           EVALUATE ARPM-CYCLE-CD
               WHEN 'CP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'SP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OUTSTAND AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3920-FLAG-OUTSTAND.
           IF ARPT-OUTSTAND-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARPT-OUTSTAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE RECON TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3930-DESCRIBE-RECON-TYPE.
           EVALUATE ARPM-RECON-TYPE-CD
               WHEN 'EEH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KBS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PAC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VKT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YMX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZVT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3940-COUNT-PAYMENT.
           IF ARPM-PAYMENT-CNT < 9999999
               ADD 1 TO ARPM-PAYMENT-CNT
           END-IF
           IF ARPM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO ARPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID SCHED TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3950-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-SCHED-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ARPT-VOID-AMT NOT < WS-VOID-SCHED-LOW(WS-TX)
                   MOVE WS-VOID-SCHED-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OUTSTAND AMT.
      *----------------------------------------------------------------*
       3960-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPT-OUTSTAND-AMT ROUNDED =
                   ARPT-VOID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPT-OUTSTAND-AMT
           END-IF
           ADD ARPT-OUTSTAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3970-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = ARPM-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ARPM-FEE-YTD - (WS-WORK-AMT * 2)
           COMPUTE ARPM-ISSUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E760 CUST ID REQUIRED
      * E425 OUTSTAND AMT NEGATIVE
      * E379 OUTSTAND AMT OVER 25000
      * E607 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3980-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-CUST-ID = SPACES
                   MOVE 'E760' TO WS-REJECT-CODE
               WHEN ARPT-OUTSTAND-AMT < 0
                   MOVE 'E425' TO WS-REJECT-CODE
               WHEN ARPT-OUTSTAND-AMT > 25000.00
                   MOVE 'E379' TO WS-REJECT-CODE
               WHEN ARPM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E607' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OUTSTAND AMT.
      *----------------------------------------------------------------*
       3990-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-OUTSTAND-AMT ROUNDED =
                   ARPM-VOID-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-OUTSTAND-AMT
           END-IF
           ADD ARPM-OUTSTAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CP -> MR WHEN CLOSE DATE IS PAST
      * MR -> PY WHEN CLOSE DATE IS PAST
      * PY -> SP WHEN CLOSE DATE IS PAST
      * SP -> TP WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4000-SET-CYCLE.
           EVALUATE ARPM-CYCLE-CD
               WHEN 'CP'
                   IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TP' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND TIER TIER FOR THE OUTSTAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4010-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-TIER-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ARPM-OUTSTAND-AMT NOT < WS-OUTSTAND-TIER-LOW(WS-TX)
                   MOVE WS-OUTSTAND-TIER-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4020-DESCRIBE-SOURCE.
           EVALUATE ARPM-SOURCE-CD
               WHEN 'GW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4030-ACCRUE-VOID.
           IF ARPM-VOID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-VOID-YTD * ARPM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-STOP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4040-FLAG-VOID.
           IF ARPT-VOID-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARPT-VOID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4050-DESCRIBE-CHANNEL.
           EVALUATE ARPM-CHANNEL-CD
               WHEN 'ME'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STOP AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4060-LIMIT-STOP.
           IF ARPM-STOP-AMT < 10.00
               MOVE 10.00 TO ARPM-STOP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-STOP-AMT > 2500.00
               MOVE 2500.00 TO ARPM-STOP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4070-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = ARPT-VOID-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ARPT-VOID-AMT - (WS-WORK-AMT * 2)
           COMPUTE ARPM-ISSUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4080-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-SOURCE-CD
                   ADD ARPT-VOID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPT-VOID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID AMT BY RECON TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-RECON-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-RECON-TYPE-CD
                   ADD ARPT-VOID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-RECON-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPT-VOID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4100-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ARPM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ARPM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ARPM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4110-ACCRUE-OUTSTAND.
           IF ARPM-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-OUTSTAND-AMT * ARPM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E533 CUST ID REQUIRED
      * E200 VOID AMT NEGATIVE
      * E404 VOID AMT OVER 5000
      * E337 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4120-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-CUST-ID = SPACES
                   MOVE 'E533' TO WS-REJECT-CODE
               WHEN ARPM-VOID-AMT < 0
                   MOVE 'E200' TO WS-REJECT-CODE
               WHEN ARPM-VOID-AMT > 5000.00
                   MOVE 'E404' TO WS-REJECT-CODE
               WHEN ARPM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E337' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4130-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-SCHED-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-VOID-SCHED-LOW(WS-TX)
                   MOVE WS-VOID-SCHED-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4140-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-VOID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ARPM-VOID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ARPM-VOID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARPM-VOID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-VOID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4150-FLAG-FEE.
           IF ARPM-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ARPM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AD -> BU WHEN EFF DATE IS PAST
      * BU -> GM WHEN EFF DATE IS PAST
      * GM -> SW WHEN EFF DATE IS PAST
      * SW -> WD WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4160-SET-REGION.
           EVALUATE ARPM-REGION-CD
               WHEN 'AD'
                   IF ARPT-EFF-DATE > 0 AND ARPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BU' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BU'
                   IF ARPT-EFF-DATE > 0 AND ARPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARPT-EFF-DATE > 0 AND ARPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARPT-EFF-DATE > 0 AND ARPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4170-COUNT-PAYMENT.
           IF ARPM-PAYMENT-CNT < 9999999
               ADD 1 TO ARPM-PAYMENT-CNT
           END-IF
           IF ARPM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO ARPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STOP YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4180-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-CHANNEL-CD
                   ADD ARPM-STOP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-STOP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID BAND TIER FOR THE STOP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4190-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-BAND-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ARPM-STOP-AMT NOT < WS-VOID-BAND-LOW(WS-TX)
                   MOVE WS-VOID-BAND-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STOP AMT.
      *----------------------------------------------------------------*
       4200-PRORATE-STOP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-STOP-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-STOP-AMT
           END-IF
           ADD ARPM-STOP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN CLOSE DATE IS PAST
      * BAT -> MAL WHEN CLOSE DATE IS PAST
      * MAL -> TEL WHEN CLOSE DATE IS PAST
      * TEL -> ONL WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4210-SET-CHANNEL.
           EVALUATE ARPT-CHANNEL-CD
               WHEN 'BRN'
                   IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE VOID BAND TIER FOR THE OUTSTAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4220-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-BAND-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ARPM-OUTSTAND-AMT NOT < WS-VOID-BAND-LOW(WS-TX)
                   MOVE WS-VOID-BAND-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E600 REJECT CD REQUIRED
      * E850 PAID AMT NEGATIVE
      * E644 PAID AMT OVER 25000
      * E242 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4230-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-REJECT-CD = SPACES
                   MOVE 'E600' TO WS-REJECT-CODE
               WHEN ARPM-PAID-AMT < 0
                   MOVE 'E850' TO WS-REJECT-CODE
               WHEN ARPM-PAID-AMT > 25000.00
                   MOVE 'E644' TO WS-REJECT-CODE
               WHEN ARPM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E242' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STOP AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4240-SPLIT-STOP.
           COMPUTE WS-WORK-AMT = ARPM-STOP-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ARPM-STOP-AMT - (WS-WORK-AMT * 12)
           COMPUTE ARPM-PAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND TIER TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4250-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-TIER-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ARPM-ISSUED-AMT NOT < WS-OUTSTAND-TIER-LOW(WS-TX)
                   MOVE WS-OUTSTAND-TIER-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E409 ADDR REQUIRED
      * E492 FEE AMT NEGATIVE
      * E439 FEE AMT OVER 10000
      * E215 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4260-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-ADDR = SPACES
                   MOVE 'E409' TO WS-REJECT-CODE
               WHEN ARPM-FEE-AMT < 0
                   MOVE 'E492' TO WS-REJECT-CODE
               WHEN ARPM-FEE-AMT > 10000.00
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN ARPM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E215' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4270-LIMIT-ISSUED.
           IF ARPM-ISSUED-YTD < 25.00
               MOVE 25.00 TO ARPM-ISSUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-ISSUED-YTD > 50000.00
               MOVE 50000.00 TO ARPM-ISSUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4280-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = ARPM-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ARPM-FEE-AMT - (WS-WORK-AMT * 12)
           COMPUTE ARPT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4290-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-REGION-CD
                   ADD ARPT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4300-COUNT-ITEM.
           IF ARPM-ITEM-CNT < 9999999
               ADD 1 TO ARPM-ITEM-CNT
           END-IF
           IF ARPM-ITEM-CNT NOT < 10
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE OUTSTAND AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4310-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ARPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ARPT-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   COMPUTE ARPT-ISSUED-AMT ROUNDED = ARPM-OUTSTAND-AMT *
                       ARPM-DISCOUNT / 200
               WHEN ARPT-TRAN-CD = 'IS'
                   MOVE 7.00 TO ARPT-ISSUED-AMT
               WHEN ARPT-TRAN-CD = 'PD'
                   MOVE 12.00 TO ARPT-ISSUED-AMT
               WHEN OTHER
                   COMPUTE ARPT-ISSUED-AMT ROUNDED = ARPM-OUTSTAND-AMT *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPT-ISSUED-AMT > 0 AND ARPT-ISSUED-AMT < 1.00
               MOVE 1.00 TO ARPT-ISSUED-AMT
           END-IF
           IF ARPT-ISSUED-AMT > 50.00
               MOVE 50.00 TO ARPT-ISSUED-AMT
           END-IF
           ADD ARPT-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4320-LIMIT-FEE.
           IF ARPT-FEE-AMT < 50.00
               MOVE 50.00 TO ARPT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPT-FEE-AMT > 5000.00
               MOVE 5000.00 TO ARPT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4330-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4340-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = ARPT-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ARPT-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE ARPM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4350-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = ARPM-VOID-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ARPM-VOID-YTD - (WS-WORK-AMT * 4)
           COMPUTE ARPM-STOP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STOP AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4360-ACCRUE-STOP.
           IF ARPM-STOP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-STOP-AMT * ARPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-PAID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4370-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4380-FLAG-PAID.
           IF ARPM-PAID-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARPM-PAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4390-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AD -> BU WHEN OPEN DATE IS PAST
      * BU -> GM WHEN OPEN DATE IS PAST
      * GM -> SW WHEN OPEN DATE IS PAST
      * SW -> WD WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4400-SET-REGION.
           EVALUATE ARPM-REGION-CD
               WHEN 'AD'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BU' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BU'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ISSUED AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4410-LIMIT-ISSUED.
           IF ARPM-ISSUED-AMT < 10.00
               MOVE 10.00 TO ARPM-ISSUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-ISSUED-AMT > 10000.00
               MOVE 10000.00 TO ARPM-ISSUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4420-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4430-LIMIT-FEE.
           IF ARPM-FEE-AMT < 50.00
               MOVE 50.00 TO ARPM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-FEE-AMT > 1000.00
               MOVE 1000.00 TO ARPM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4440-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4450-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPT-VOID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ARPT-VOID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ARPT-VOID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARPT-VOID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPT-VOID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4460-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = ARPT-VOID-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ARPT-VOID-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4470-COUNT-CYCLE.
           IF ARPM-CYCLE-CNT < 9999999
               ADD 1 TO ARPM-CYCLE-CNT
           END-IF
           IF ARPM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO ARPM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE ARP FIELDS TO ARPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-ARPRC.
           INITIALIZE ARPRC-PARMS
           MOVE ARPM-OUTSTAND-YTD
             TO ARPRC-OUTSTAND-YTD
           MOVE ARPM-ISSUED-AMT
             TO ARPRC-ISSUED-AMT
           MOVE ARPM-ISSUED-YTD
             TO ARPRC-ISSUED-YTD
           MOVE ARPM-FEE-YTD
             TO ARPRC-FEE-YTD
           MOVE ARPM-OUTSTAND-AMT
             TO ARPRC-OUTSTAND-AMT
           MOVE ARPM-STOP-YTD
             TO ARPRC-STOP-YTD
           MOVE ARPM-OPEN-DATE
             TO ARPRC-OPEN-DATE
           MOVE ARPM-NEXT-DUE-DATE
             TO ARPRC-NEXT-DUE-DATE
           MOVE ARPM-NEXT-REVIEW-DATE
             TO ARPRC-NEXT-REVIEW-DATE
           MOVE ARPM-LAST-ACT-DATE
             TO ARPRC-LAST-ACT-DATE
           MOVE ARPM-RECON-TYPE-CD
             TO ARPRC-RECON-TYPE-CD
           MOVE ARPM-BRANCH-CD
             TO ARPRC-BRANCH-CD
           MOVE ARPM-CHANNEL-CD
             TO ARPRC-CHANNEL-CD
           MOVE ARPM-SOURCE-CD
             TO ARPRC-SOURCE-CD
           MOVE ARPM-HOLD-FLAG
             TO ARPRC-HOLD-FLAG
           MOVE ARPM-STOP-FLAG
             TO ARPRC-STOP-FLAG
           MOVE ARPM-PREFERRED-FLAG
             TO ARPRC-PREFERRED-FLAG
           MOVE ARPM-PAYMENT-CNT
             TO ARPRC-PAYMENT-CNT
           MOVE ARPM-EXCEPT-CNT
             TO ARPRC-EXCEPT-CNT
           MOVE ARPM-DISCOUNT
             TO ARPRC-DISCOUNT
           MOVE ARPM-PENALTY-PCT
             TO ARPRC-PENALTY-PCT
           MOVE ARPM-NAME
             TO ARPRC-NAME
           MOVE ARPM-ADDR
             TO ARPRC-ADDR
           CALL 'ARPRC' USING ARPRC-PARMS
           EVALUATE ARPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ARPRC-ISSUED-AMT
                     TO ARPM-ISSUED-AMT
                   MOVE ARPRC-ISSUED-YTD
                     TO ARPM-ISSUED-YTD
                   MOVE ARPRC-FEE-YTD
                     TO ARPM-FEE-YTD
                   MOVE ARPRC-HOLD-FLAG
                     TO ARPM-HOLD-FLAG
                   MOVE ARPRC-PREFERRED-FLAG
                     TO ARPM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-ARPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE ARP FIELDS TO ARRISK AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-ARRISK.
           INITIALIZE ARRISK-PARMS
           MOVE ARPM-ISSUED-YTD
             TO ARRISK-ISSUED-YTD
           MOVE ARPM-PAID-YTD
             TO ARRISK-PAID-YTD
           MOVE ARPM-VOID-YTD
             TO ARRISK-VOID-YTD
           MOVE ARPM-STOP-YTD
             TO ARRISK-STOP-YTD
           MOVE ARPM-ISSUED-AMT
             TO ARRISK-ISSUED-AMT
           MOVE ARPM-OUTSTAND-YTD
             TO ARRISK-OUTSTAND-YTD
           MOVE ARPM-LAST-STMT-DATE
             TO ARRISK-LAST-STMT-DATE
           MOVE ARPM-NEXT-REVIEW-DATE
             TO ARRISK-NEXT-REVIEW-DATE
           MOVE ARPM-LAST-PAY-DATE
             TO ARRISK-LAST-PAY-DATE
           MOVE ARPM-NEXT-DUE-DATE
             TO ARRISK-NEXT-DUE-DATE
           MOVE ARPM-CYCLE-CD
             TO ARRISK-CYCLE-CD
           MOVE ARPM-CHANNEL-CD
             TO ARRISK-CHANNEL-CD
           MOVE ARPM-REGION-CD
             TO ARRISK-REGION-CD
           MOVE ARPM-BRANCH-CD
             TO ARRISK-BRANCH-CD
           MOVE ARPM-NOTICE-SENT-FLAG
             TO ARRISK-NOTICE-SENT-FLAG
           MOVE ARPM-STOP-FLAG
             TO ARRISK-STOP-FLAG
           MOVE ARPM-HOLD-FLAG
             TO ARRISK-HOLD-FLAG
           MOVE ARPM-ITEM-CNT
             TO ARRISK-ITEM-CNT
           MOVE ARPM-CYCLE-CNT
             TO ARRISK-CYCLE-CNT
           MOVE ARPM-PENALTY-PCT
             TO ARRISK-PENALTY-PCT
           MOVE ARPM-DISCOUNT
             TO ARRISK-DISCOUNT
           MOVE ARPM-ADDR
             TO ARRISK-ADDR
           MOVE ARPM-DESC
             TO ARRISK-DESC
           CALL 'ARRISK' USING ARRISK-PARMS
           EVALUATE ARRISK-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ARRISK-PAID-YTD
                     TO ARPM-PAID-YTD
                   MOVE ARRISK-VOID-YTD
                     TO ARPM-VOID-YTD
                   MOVE ARRISK-ISSUED-AMT
                     TO ARPM-ISSUED-AMT
                   MOVE ARRISK-OUTSTAND-YTD
                     TO ARPM-OUTSTAND-YTD
                   MOVE ARRISK-NOTICE-SENT-FLAG
                     TO ARPM-NOTICE-SENT-FLAG
                   MOVE ARRISK-STOP-FLAG
                     TO ARPM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-ARRISK' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE ARPT-KEY TO RD-KEY
           MOVE ARPT-TRAN-CD TO RD-CODE
           MOVE ARPT-OUTSTAND-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE ARPT-KEY TO RD-KEY
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
           DISPLAY 'ARPANAL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ARPANAL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
