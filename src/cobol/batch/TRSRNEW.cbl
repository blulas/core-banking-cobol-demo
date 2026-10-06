      *================================================================*
      * PROGRAM  : TRSRNEW                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PERSONAL TRUST - RENEWAL. READS THE TRS             *
      *            TRANSACTIONS, UPDATES THE TRS MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. PERSONAL TRUST ACCOUNTING,      *
      *            INCOME DISTRIBUTION AND TRUSTEE FEES                *
      * INPUT    : TRSMST (KSDS), TRSTRN, BANKCTL                      *
      * OUTPUT   : TRSOUT, TRSRNRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TRVAL, TRLIM            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-05-09 JLM  ORIGINAL                                       *
      * 1989-11-07 BHS  TAX STATUS CODES ADDED CHG2502                 *
      * 1993-04-03 BHS  AUDIT FINDING - MARKET CONTROL TOTALS PRB0812  *
      * 2003-05-17 SKN  NEW MARKET REPORT COLUMNS REQ2489              *
      * 2004-03-21 BHS  TRUSTEE-FEE LIMIT REVISED PRB3092              *
      * 2005-06-02 EWS  PRINCIPAL CALCULATION CORRECTED PRB0562        *
      * 2007-04-02 LMB  MARKET LIMIT REVISED PRB8929                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRSRNEW.
       AUTHOR. C R PATEL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "TRSMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TRSM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "TRSTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "TRSOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "TRSRNRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY TRSMST.
       FD  TRAN-FILE.
       COPY TRSTRN.
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
      * WS-INCOME-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INCOME-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000558000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000104000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000174000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001300000000290000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006300000000775000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008800000000022000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009800000000013000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014800000000766500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024800000000821000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024900000000432500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00034900000000708000'.
       01  WS-INCOME-BAND-TBL REDEFINES WS-INCOME-BAND-VALUES.
           05  WS-INCOME-BAND-ROW OCCURS 11 TIMES.
               10  WS-INCOME-BAND-LOW      PIC 9(09)V99.
               10  WS-INCOME-BAND-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DISTRIB-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISTRIB-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000215000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000184000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000231500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000102000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002700000000238000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012700000000275500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012750000000548000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012800000000165500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012850000000551000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013850000000183000'.
       01  WS-DISTRIB-TIER-TBL REDEFINES WS-DISTRIB-TIER-VALUES.
           05  WS-DISTRIB-TIER-ROW OCCURS 10 TIMES.
               10  WS-DISTRIB-TIER-LOW     PIC 9(09)V99.
               10  WS-DISTRIB-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-TRUSTEE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TRUSTEE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000395500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000361500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000776500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000154000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013550000000607000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018550000000745000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019050000000239000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021550000000669000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021800000000507000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031800000000353500'.
       01  WS-TRUSTEE-BAND-TBL REDEFINES WS-TRUSTEE-BAND-VALUES.
           05  WS-TRUSTEE-BAND-ROW OCCURS 10 TIMES.
               10  WS-TRUSTEE-BAND-LOW     PIC 9(09)V99.
               10  WS-TRUSTEE-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-TRUSTEE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TRUSTEE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000340000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000516000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000283000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000440500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012100000000604000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012200000000478500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012450000000347000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000821000'.
       01  WS-TRUSTEE-SCHED-TBL REDEFINES WS-TRUSTEE-SCHED-VALUES.
           05  WS-TRUSTEE-SCHED-ROW OCCURS 8 TIMES.
               10  WS-TRUSTEE-SCHED-LOW    PIC 9(09)V99.
               10  WS-TRUSTEE-SCHED-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TRSRNEW  PERSONAL TRUST'.
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
       COPY TRVALP.
       COPY TRLIMP.

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
               MOVE 'TRSMST' TO ABND-FILE-NAME
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
           MOVE TRST-KEY TO TRSM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-PRORATE-PRINCIPAL
               PERFORM 3010-DESCRIBE-TRAN
               PERFORM 3020-PRORATE-TAX
               PERFORM 3030-ACCRUE-PRINCIPAL
               PERFORM 3040-COUNT-CYCLE
               PERFORM 3050-DESCRIBE-REASON
               PERFORM 3060-LIMIT-TAX
               PERFORM 3070-AGE-CLOSE
               PERFORM 3080-AGE-OPEN
               PERFORM 3090-ASSESS-PREFERRED
               PERFORM 3100-ACCUM-REASON
               PERFORM 3110-ACCRUE-MARKET
               PERFORM 3120-DESCRIBE-TRAN
               PERFORM 3130-SPLIT-INCOME
               PERFORM 3140-PRORATE-MARKET
               PERFORM 3150-LIMIT-TRUSTEE-FEE
               PERFORM 3160-ACCRUE-DISTRIB
               PERFORM 3170-ACCRUE-TRUSTEE-FEE
               PERFORM 3180-PRORATE-PRINCIPAL
               PERFORM 3190-FLAG-INCOME
               PERFORM 3200-SELECT-DISCOUNT
               PERFORM 3210-PRORATE-MARKET
               PERFORM 3220-ACCUM-STATUS
               PERFORM 3230-PRORATE-PRINCIPAL
               PERFORM 3240-ACCRUE-TRUSTEE-FEE
               PERFORM 3250-FLAG-PRINCIPAL
               PERFORM 3260-AGE-LAST-PAY
               PERFORM 3270-FLAG-TAX
               PERFORM 3280-LIMIT-PRINCIPAL
               PERFORM 3290-FLAG-INCOME
               PERFORM 3300-AGE-LAST-STMT
               PERFORM 3310-ASSESS-STOP
               PERFORM 3320-SET-CYCLE
               PERFORM 3330-ACCRUE-PRINCIPAL
               PERFORM 3340-DESCRIBE-CHANNEL
               PERFORM 3350-PRORATE-MARKET
               PERFORM 3360-PRORATE-TAX
               PERFORM 3370-COUNT-NOTICE
               PERFORM 3380-EDIT-CONTACT
               PERFORM 3390-FLAG-DISTRIB
               PERFORM 3400-SPLIT-TAX
               PERFORM 3410-ACCUM-STATUS
               PERFORM 3420-LIMIT-PRINCIPAL
               PERFORM 3430-LIMIT-INCOME
               PERFORM 3440-LIMIT-PRINCIPAL
               PERFORM 3450-PRORATE-INCOME
               PERFORM 3460-ACCUM-CHANNEL
               PERFORM 3470-ACCUM-CHANNEL
               PERFORM 3480-SELECT-MARGIN
               PERFORM 3490-EDIT-CONTACT
               PERFORM 3500-PRORATE-PRINCIPAL
               PERFORM 3510-AGE-EFF
               PERFORM 3520-DESCRIBE-REGION
               PERFORM 3530-EDIT-NAME
               PERFORM 3540-PRORATE-TRUSTEE-FEE
               PERFORM 3550-PRORATE-PRINCIPAL
               PERFORM 3560-SELECT-MARGIN
               PERFORM 3570-ACCRUE-TAX
               PERFORM 3580-COUNT-PAYMENT
               PERFORM 3590-SELECT-DISCOUNT
               PERFORM 3600-AGE-MATURITY
               PERFORM 3610-ASSESS-PREFERRED
               PERFORM 3620-PRORATE-PRINCIPAL
               PERFORM 3630-LIMIT-DISTRIB
               PERFORM 3640-EDIT-MEMO
               PERFORM 3650-ASSESS-PREFERRED
               PERFORM 3660-ACCRUE-INCOME
               PERFORM 3670-AGE-EFF
               PERFORM 3680-AGE-EXPIRY
               PERFORM 3690-AGE-EXPIRY
               PERFORM 3700-ACCUM-TRAN
               PERFORM 3710-CHECK-EFFECTIVE
               PERFORM 3720-SET-CHANNEL
               PERFORM 3730-SET-DIST-FREQ
               PERFORM 3740-LIMIT-INCOME
               PERFORM 3750-SPLIT-DISTRIB
               PERFORM 3760-FLAG-PRINCIPAL
               PERFORM 3770-ASSESS-EXEMPT
               PERFORM 3780-PRORATE-TRUSTEE-FEE
               PERFORM 3790-LIMIT-TAX
               PERFORM 3800-ASSESS-REVERSAL
               PERFORM 3810-SET-REASON
               PERFORM 3820-ACCUM-STATUS
               PERFORM 3830-AGE-EXPIRY
               PERFORM 3840-ACCRUE-INCOME
               PERFORM 3850-PRORATE-MARKET
               PERFORM 3860-DESCRIBE-CYCLE
               PERFORM 3870-AGE-LAST-STMT
               PERFORM 3880-ASSESS-NOTICE-SENT
               PERFORM 3890-ASSESS-STOP
               PERFORM 3900-LIMIT-MARKET
               PERFORM 3910-SELECT-MARGIN
               PERFORM 3920-ACCUM-DIST-FREQ
               PERFORM 3930-FLAG-TRUSTEE-FEE
               PERFORM 3940-ASSESS-PREFERRED
               PERFORM 3950-LIMIT-PRINCIPAL
               PERFORM 3960-ASSESS-STOP
               PERFORM 3970-EDIT-MEMO
               PERFORM 3980-ACCUM-STATUS
               PERFORM 3990-AGE-LAST-STMT
               PERFORM 4000-ASSESS-PREFERRED
               PERFORM 4010-PRORATE-PRINCIPAL
               PERFORM 4020-EDIT-DESC
               PERFORM 4030-ACCRUE-DISTRIB
               PERFORM 4040-EDIT-DESC
               PERFORM 4050-EDIT-OPERATOR
               PERFORM 4060-SELECT-MARGIN
               PERFORM 4070-AGE-LAST-MAINT
               PERFORM 4080-PRORATE-MARKET
               PERFORM 4090-SET-STATUS
               PERFORM 4100-SPLIT-INCOME
               PERFORM 4110-EDIT-REF-NO
               PERFORM 4120-SELECT-MARGIN
               PERFORM 4130-DESCRIBE-REASON
               PERFORM 4140-SPLIT-TRUSTEE-FEE
               PERFORM 4150-PRORATE-TRUSTEE-FEE
               PERFORM 4160-FLAG-DISTRIB
               PERFORM 4170-COUNT-SEQ
               PERFORM 4180-SET-CHANNEL
               PERFORM 4190-SELECT-MARGIN
               PERFORM 4200-SELECT-RATE
               PERFORM 4210-ACCUM-REASON
               PERFORM 4220-ACCRUE-DISTRIB
               PERFORM 4230-ASSESS-PREFERRED
               PERFORM 4240-FLAG-INCOME
               PERFORM 4250-AGE-CLOSE
               PERFORM 4260-AGE-NOTICE
               PERFORM 4270-AGE-EXPIRY
               PERFORM 4280-ACCUM-CHANNEL
               PERFORM 4290-ACCUM-REGION
               PERFORM 4300-ACCUM-STATUS
               PERFORM 4310-LIMIT-DISTRIB
               PERFORM 4320-LIMIT-TAX
               PERFORM 4330-ACCRUE-INCOME
               PERFORM 4340-ACCRUE-RATE
               PERFORM 4350-ASSESS-REVIEW
               PERFORM 4360-EDIT-DESC
               PERFORM 4370-PRORATE-INCOME
               PERFORM 4380-PRORATE-TRUSTEE-FEE
               PERFORM 4390-SELECT-RATE
               PERFORM 4400-PRORATE-INCOME
               PERFORM 4410-ACCUM-CHANNEL
               PERFORM 4420-EDIT-REJECT
               PERFORM 4430-ACCUM-REGION
               PERFORM 4440-ACCRUE-TRUSTEE-FEE
               PERFORM 4450-FLAG-DISTRIB
               PERFORM 4460-ACCUM-CHANNEL
               PERFORM 4470-FLAG-TAX
               PERFORM 4480-LIMIT-MARKET
               PERFORM 4490-COUNT-CYCLE
               PERFORM 4500-AGE-POST
               PERFORM 4510-ASSESS-NOTICE-SENT
               PERFORM 4520-ASSESS-REVIEW
               PERFORM 4530-PRORATE-INCOME
               PERFORM 4540-COUNT-SEQ
               PERFORM 4550-COUNT-CYCLE
               PERFORM 4560-ASSESS-SUSPEND
               PERFORM 4570-ACCUM-TRAN
               PERFORM 4580-FLAG-PRINCIPAL
               PERFORM 4590-SPLIT-PRINCIPAL
               PERFORM 4600-CHECK-EFF
               PERFORM 4610-AGE-CLOSE
               PERFORM 4620-ACCUM-CYCLE
               PERFORM 4630-SPLIT-PRINCIPAL
               PERFORM 4640-PRORATE-TRUSTEE-FEE
               PERFORM 4650-DESCRIBE-DIST-FREQ
               PERFORM 4660-ACCRUE-MARKET
               PERFORM 4670-FLAG-DISTRIB
               PERFORM 4680-EDIT-REF-NO
               PERFORM 4690-SET-REASON
               PERFORM 4700-LIMIT-PRINCIPAL
               PERFORM 4710-SELECT-RATE
               PERFORM 4720-LIMIT-DISTRIB
               PERFORM 4730-FLAG-MARKET
               PERFORM 4740-SPLIT-INCOME
               PERFORM 4750-ACCUM-CYCLE
               PERFORM 4760-SELECT-DISCOUNT
               PERFORM 4770-LIMIT-TAX
               PERFORM 4780-LIMIT-TAX
               PERFORM 4790-COUNT-RETRY
               PERFORM 4800-COUNT-PAYMENT
               PERFORM 4810-COUNT-RETRY
               PERFORM 4820-ASSESS-REVERSAL
               PERFORM 4830-ACCRUE-INCOME
               PERFORM 4840-ACCUM-CYCLE
               PERFORM 7020-CALL-TRVAL
               PERFORM 7000-CALL-TRLIM
               REWRITE TRS-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'TRSMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM TRS-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * PRORATE INCOME AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       3000-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRST-PRINCIPAL-AMT ROUNDED =
                   TRSM-INCOME-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRST-PRINCIPAL-AMT
           END-IF
           ADD TRST-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3010-DESCRIBE-TRAN.
           EVALUATE TRST-TRAN-CD
               WHEN 'IN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3020-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TRSM-TAX-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3030-ACCRUE-PRINCIPAL.
           IF TRSM-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-PRINCIPAL-AMT * TRSM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRST-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-CYCLE.
           IF TRSM-CYCLE-CNT < 9999999
               ADD 1 TO TRSM-CYCLE-CNT
           END-IF
           IF TRSM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3050-DESCRIBE-REASON.
           EVALUATE TRSM-REASON-CD
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TAX YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3060-LIMIT-TAX.
           IF TRSM-TAX-YTD < 10.00
               MOVE 10.00 TO TRSM-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-TAX-YTD > 2500.00
               MOVE 2500.00 TO TRSM-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3070-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3080-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INCOME FEE ON THE TAX YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3090-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TRST-INCOME-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRST-INCOME-AMT ROUNDED = TRSM-TAX-YTD *
                       TRSM-RATE / 200
               WHEN TRST-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO TRST-INCOME-AMT
               WHEN TRST-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO TRST-INCOME-AMT
               WHEN OTHER
                   COMPUTE TRST-INCOME-AMT ROUNDED = TRSM-TAX-YTD *
                       TRSM-RATE / 100
           END-EVALUATE
           IF TRST-INCOME-AMT > 0 AND TRST-INCOME-AMT < 5.00
               MOVE 5.00 TO TRST-INCOME-AMT
           END-IF
           IF TRST-INCOME-AMT > 150.00
               MOVE 150.00 TO TRST-INCOME-AMT
           END-IF
           ADD TRST-INCOME-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-REASON-CD
                   ADD TRST-DISTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRST-DISTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARKET YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3110-ACCRUE-MARKET.
           IF TRSM-MARKET-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-MARKET-YTD * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRSM-PRINCIPAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3120-DESCRIBE-TRAN.
           EVALUATE TRST-TRAN-CD
               WHEN 'IN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'DS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT INCOME YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3130-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = TRSM-INCOME-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TRSM-INCOME-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MARKET AMT.
      *----------------------------------------------------------------*
       3140-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-MARKET-AMT ROUNDED =
                   TRST-PRINCIPAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-MARKET-AMT
           END-IF
           ADD TRSM-MARKET-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRUSTEE FEE YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3150-LIMIT-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-YTD < 10.00
               MOVE 10.00 TO TRSM-TRUSTEE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-TRUSTEE-FEE-YTD > 10000.00
               MOVE 10000.00 TO TRSM-TRUSTEE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-DISTRIB.
           IF TRSM-DISTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-DISTRIB-YTD * TRSM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TRST-INCOME-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TRUSTEE FEE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3170-ACCRUE-TRUSTEE-FEE.
           IF TRST-TRUSTEE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRST-TRUSTEE-FEE-AMT * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRST-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       3180-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-PRINCIPAL-AMT ROUNDED =
                   TRSM-DISTRIB-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-PRINCIPAL-AMT
           END-IF
           ADD TRSM-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * INCOME AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3190-FLAG-INCOME.
           IF TRSM-INCOME-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRSM-INCOME-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INCOME BAND TIER FOR THE MARKET AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3200-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-BAND-RATE(1) TO TRSM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TRSM-MARKET-AMT NOT < WS-INCOME-BAND-LOW(WS-TX)
                   MOVE WS-INCOME-BAND-RATE(WS-TX) TO TRSM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INCOME YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MARKET AMT.
      *----------------------------------------------------------------*
       3210-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-MARKET-AMT ROUNDED =
                   TRSM-INCOME-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-MARKET-AMT
           END-IF
           ADD TRSM-MARKET-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3220-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-STATUS-CD
                   ADD TRSM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3230-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TRSM-PRINCIPAL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TRUSTEE FEE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3240-ACCRUE-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TRUSTEE-FEE-YTD * TRSM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3250-FLAG-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRST-PRINCIPAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3260-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TAX YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3270-FLAG-TAX.
           IF TRSM-TAX-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRSM-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3280-LIMIT-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT < 50.00
               MOVE 50.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-PRINCIPAL-AMT > 2500.00
               MOVE 2500.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INCOME YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3290-FLAG-INCOME.
           IF TRSM-INCOME-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-INCOME-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3300-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-TAX-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRSM-TAX-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-TAX-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-TAX-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-TAX-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE MARKET AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3310-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRST-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   COMPUTE TRST-PRINCIPAL-AMT ROUNDED = TRSM-MARKET-AMT
                       * TRSM-RATE / 200
               WHEN TRSM-REGION-CD = 'A'
                   MOVE 10.00 TO TRST-PRINCIPAL-AMT
               WHEN TRSM-REGION-CD = 'D'
                   MOVE 5.00 TO TRST-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE TRST-PRINCIPAL-AMT ROUNDED = TRSM-MARKET-AMT
                       * TRSM-RATE / 100
           END-EVALUATE
           IF TRST-PRINCIPAL-AMT > 0 AND TRST-PRINCIPAL-AMT < 10.00
               MOVE 10.00 TO TRST-PRINCIPAL-AMT
           END-IF
           IF TRST-PRINCIPAL-AMT > 100.00
               MOVE 100.00 TO TRST-PRINCIPAL-AMT
           END-IF
           ADD TRST-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EN -> FY WHEN EFFECTIVE DATE IS PAST
      * FY -> HP WHEN EFFECTIVE DATE IS PAST
      * HP -> JB WHEN EFFECTIVE DATE IS PAST
      * JB -> KZ WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3320-SET-CYCLE.
           EVALUATE TRSM-CYCLE-CD
               WHEN 'EN'
                   IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FY' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FY'
                   IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HP' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HP'
                   IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JB' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JB'
                   IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KZ' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3330-ACCRUE-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRST-PRINCIPAL-AMT * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3340-DESCRIBE-CHANNEL.
           EVALUATE TRST-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE TRUSTEE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MARKET YTD.
      *----------------------------------------------------------------*
       3350-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-MARKET-YTD ROUNDED =
                   TRSM-TRUSTEE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-MARKET-YTD
           END-IF
           ADD TRSM-MARKET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE TRUSTEE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TAX YTD.
      *----------------------------------------------------------------*
       3360-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TAX-YTD ROUNDED =
                   TRSM-TRUSTEE-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-TAX-YTD
           END-IF
           ADD TRSM-TAX-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3370-COUNT-NOTICE.
           IF TRSM-NOTICE-CNT < 9999999
               ADD 1 TO TRSM-NOTICE-CNT
           END-IF
           IF TRSM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E912 CONTACT REQUIRED
      * E112 INCOME YTD NEGATIVE
      * E325 INCOME YTD OVER 250000
      * E182 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3380-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-CONTACT = SPACES
                   MOVE 'E912' TO WS-REJECT-CODE
               WHEN TRSM-INCOME-YTD < 0
                   MOVE 'E112' TO WS-REJECT-CODE
               WHEN TRSM-INCOME-YTD > 250000.00
                   MOVE 'E325' TO WS-REJECT-CODE
               WHEN TRST-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E182' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3390-FLAG-DISTRIB.
           IF TRSM-DISTRIB-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-DISTRIB-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAX AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3400-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = TRSM-TAX-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TRSM-TAX-AMT - (WS-WORK-AMT * 12)
           COMPUTE TRSM-MARKET-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TRUSTEE FEE AMT BY STATUS CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3410-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-STATUS-CD
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-TRUSTEE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3420-LIMIT-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT < 10.00
               MOVE 10.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-PRINCIPAL-AMT > 50000.00
               MOVE 50000.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INCOME AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3430-LIMIT-INCOME.
           IF TRST-INCOME-AMT < 100.00
               MOVE 100.00 TO TRST-INCOME-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-INCOME-AMT > 2500.00
               MOVE 2500.00 TO TRST-INCOME-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3440-LIMIT-PRINCIPAL.
           IF TRSM-PRINCIPAL-AMT < 25.00
               MOVE 25.00 TO TRSM-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-PRINCIPAL-AMT > 5000.00
               MOVE 5000.00 TO TRSM-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TRUSTEE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INCOME AMT.
      *----------------------------------------------------------------*
       3450-PRORATE-INCOME.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-INCOME-AMT ROUNDED =
                   TRST-TRUSTEE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-INCOME-AMT
           END-IF
           ADD TRSM-INCOME-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE INCOME YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3460-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-CHANNEL-CD
                   ADD TRSM-INCOME-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-INCOME-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3470-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-CHANNEL-CD
                   ADD TRSM-DISTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-DISTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE BAND TIER FOR THE INCOME YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3480-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-BAND-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TRSM-INCOME-YTD NOT < WS-TRUSTEE-BAND-LOW(WS-TX)
                   MOVE WS-TRUSTEE-BAND-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E096 CONTACT REQUIRED
      * E854 MARKET AMT NEGATIVE
      * E515 MARKET AMT OVER 99999
      * E454 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3490-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-CONTACT = SPACES
                   MOVE 'E096' TO WS-REJECT-CODE
               WHEN TRSM-MARKET-AMT < 0
                   MOVE 'E854' TO WS-REJECT-CODE
               WHEN TRSM-MARKET-AMT > 99999.00
                   MOVE 'E515' TO WS-REJECT-CODE
               WHEN TRSM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E454' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       3500-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRST-PRINCIPAL-AMT ROUNDED =
                   TRSM-PRINCIPAL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRST-PRINCIPAL-AMT
           END-IF
           ADD TRST-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3510-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF TRST-EFF-DATE > 0 AND TRST-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRST-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3520-DESCRIBE-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'D'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'N'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E692 NAME REQUIRED
      * E948 TRUSTEE FEE YTD NEGATIVE
      * E858 TRUSTEE FEE YTD OVER 10000
      * E455 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3530-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-NAME = SPACES
                   MOVE 'E692' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-YTD < 0
                   MOVE 'E948' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-YTD > 10000.00
                   MOVE 'E858' TO WS-REJECT-CODE
               WHEN TRSM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E455' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TRUSTEE FEE AMT.
      *----------------------------------------------------------------*
       3540-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TRUSTEE-FEE-AMT ROUNDED =
                   TRSM-TAX-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-TRUSTEE-FEE-AMT
           END-IF
           ADD TRSM-TRUSTEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       3550-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRST-PRINCIPAL-AMT ROUNDED =
                   TRSM-PRINCIPAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRST-PRINCIPAL-AMT
           END-IF
           ADD TRST-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE SCHED TIER FOR THE TRUSTEE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3560-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-SCHED-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRST-TRUSTEE-FEE-AMT NOT < WS-TRUSTEE-SCHED-LOW(WS-TX)
                   MOVE WS-TRUSTEE-SCHED-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3570-ACCRUE-TAX.
           IF TRSM-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TAX-AMT * TRSM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRST-TRUSTEE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3580-COUNT-PAYMENT.
           IF TRSM-PAYMENT-CNT < 9999999
               ADD 1 TO TRSM-PAYMENT-CNT
           END-IF
           IF TRSM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO TRSM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INCOME BAND TIER FOR THE INCOME AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3590-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-BAND-RATE(1) TO TRSM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TRSM-INCOME-AMT NOT < WS-INCOME-BAND-LOW(WS-TX)
                   MOVE WS-INCOME-BAND-RATE(WS-TX) TO TRSM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3600-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-TAX-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-TAX-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRSM-TAX-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-TAX-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-TAX-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE MARKET FEE ON THE PRINCIPAL YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3610-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TRSM-MARKET-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   COMPUTE TRSM-MARKET-AMT ROUNDED = TRSM-PRINCIPAL-YTD
                       * TRSM-MARGIN / 200
               WHEN TRSM-REGION-CD = 'A'
                   MOVE 15.00 TO TRSM-MARKET-AMT
               WHEN TRSM-REGION-CD = 'D'
                   MOVE 10.00 TO TRSM-MARKET-AMT
               WHEN OTHER
                   COMPUTE TRSM-MARKET-AMT ROUNDED = TRSM-PRINCIPAL-YTD
                       * TRSM-MARGIN / 100
           END-EVALUATE
           IF TRSM-MARKET-AMT > 0 AND TRSM-MARKET-AMT < 10.00
               MOVE 10.00 TO TRSM-MARKET-AMT
           END-IF
           IF TRSM-MARKET-AMT > 150.00
               MOVE 150.00 TO TRSM-MARKET-AMT
           END-IF
           ADD TRSM-MARKET-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE TRUSTEE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINCIPAL YTD.
      *----------------------------------------------------------------*
       3620-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-PRINCIPAL-YTD ROUNDED =
                   TRSM-TRUSTEE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-PRINCIPAL-YTD
           END-IF
           ADD TRSM-PRINCIPAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DISTRIB AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3630-LIMIT-DISTRIB.
           IF TRSM-DISTRIB-AMT < 10.00
               MOVE 10.00 TO TRSM-DISTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-DISTRIB-AMT > 5000.00
               MOVE 5000.00 TO TRSM-DISTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E647 MEMO REQUIRED
      * E316 TAX YTD NEGATIVE
      * E050 TAX YTD OVER 50000
      * E260 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-MEMO = SPACES
                   MOVE 'E647' TO WS-REJECT-CODE
               WHEN TRSM-TAX-YTD < 0
                   MOVE 'E316' TO WS-REJECT-CODE
               WHEN TRSM-TAX-YTD > 50000.00
                   MOVE 'E050' TO WS-REJECT-CODE
               WHEN TRSM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E260' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE TAX AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3650-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TRST-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TRST-PRINCIPAL-AMT ROUNDED = TRSM-TAX-AMT *
                       TRSM-MARGIN / 200
               WHEN TRSM-REASON-CD = 'H'
                   MOVE 3.00 TO TRST-PRINCIPAL-AMT
               WHEN TRSM-REASON-CD = 'M'
                   MOVE 20.00 TO TRST-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE TRST-PRINCIPAL-AMT ROUNDED = TRSM-TAX-AMT *
                       TRSM-MARGIN / 100
           END-EVALUATE
           IF TRST-PRINCIPAL-AMT > 0 AND TRST-PRINCIPAL-AMT < 2.00
               MOVE 2.00 TO TRST-PRINCIPAL-AMT
           END-IF
           IF TRST-PRINCIPAL-AMT > 50.00
               MOVE 50.00 TO TRST-PRINCIPAL-AMT
           END-IF
           ADD TRST-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INCOME AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-INCOME.
           IF TRST-INCOME-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRST-INCOME-AMT * TRSM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRSM-MARKET-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3670-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF TRST-EFF-DATE > 0 AND TRST-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRST-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3680-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3690-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE MARKET AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3700-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-TRAN-CD
                   ADD TRSM-MARKET-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-MARKET-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3710-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TRSM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-CHANNEL.
           EVALUATE TRST-CHANNEL-CD
               WHEN 'BRN'
                   IF TRST-EFF-DATE > 0 AND TRST-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TRST-EFF-DATE > 0 AND TRST-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TRST-EFF-DATE > 0 AND TRST-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TRST-EFF-DATE > 0 AND TRST-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DIST FREQ CD:
      * E -> K WHEN EXPIRY DATE IS PAST
      * K -> P WHEN EXPIRY DATE IS PAST
      * P -> W WHEN EXPIRY DATE IS PAST
      * W -> Y WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3730-SET-DIST-FREQ.
           EVALUATE TRSM-DIST-FREQ-CD
               WHEN 'E'
                   IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INCOME AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3740-LIMIT-INCOME.
           IF TRSM-INCOME-AMT < 25.00
               MOVE 25.00 TO TRSM-INCOME-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-INCOME-AMT > 500.00
               MOVE 500.00 TO TRSM-INCOME-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3750-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = TRSM-DISTRIB-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TRSM-DISTRIB-YTD - (WS-WORK-AMT * 12)
           COMPUTE TRST-TRUSTEE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3760-FLAG-PRINCIPAL.
           IF TRSM-PRINCIPAL-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-PRINCIPAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE TAX YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3770-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TRSM-PRINCIPAL-YTD ROUNDED = TRSM-TAX-YTD *
                       TRSM-RATE / 200
               WHEN TRSM-DIST-FREQ-CD = 'E'
                   MOVE 10.00 TO TRSM-PRINCIPAL-YTD
               WHEN TRSM-DIST-FREQ-CD = 'K'
                   MOVE 15.00 TO TRSM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE TRSM-PRINCIPAL-YTD ROUNDED = TRSM-TAX-YTD *
                       TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-PRINCIPAL-YTD > 0 AND TRSM-PRINCIPAL-YTD < 1.00
               MOVE 1.00 TO TRSM-PRINCIPAL-YTD
           END-IF
           IF TRSM-PRINCIPAL-YTD > 75.00
               MOVE 75.00 TO TRSM-PRINCIPAL-YTD
           END-IF
           ADD TRSM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TRUSTEE FEE AMT.
      *----------------------------------------------------------------*
       3780-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TRUSTEE-FEE-AMT ROUNDED =
                   TRST-PRINCIPAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-TRUSTEE-FEE-AMT
           END-IF
           ADD TRSM-TRUSTEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TAX AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3790-LIMIT-TAX.
           IF TRSM-TAX-AMT < 50.00
               MOVE 50.00 TO TRSM-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-TAX-AMT > 500.00
               MOVE 500.00 TO TRSM-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MARKET YTD FEE ON THE DISTRIB AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3800-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TRSM-MARKET-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-STOP-FLAG = 'Y'
                   COMPUTE TRSM-MARKET-YTD ROUNDED = TRSM-DISTRIB-AMT *
                       TRSM-RATE / 200
               WHEN TRSM-REGION-CD = 'A'
                   MOVE 5.00 TO TRSM-MARKET-YTD
               WHEN TRSM-REGION-CD = 'D'
                   MOVE 20.00 TO TRSM-MARKET-YTD
               WHEN OTHER
                   COMPUTE TRSM-MARKET-YTD ROUNDED = TRSM-DISTRIB-AMT *
                       TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-MARKET-YTD > 0 AND TRSM-MARKET-YTD < 2.00
               MOVE 2.00 TO TRSM-MARKET-YTD
           END-IF
           IF TRSM-MARKET-YTD > 50.00
               MOVE 50.00 TO TRSM-MARKET-YTD
           END-IF
           ADD TRSM-MARKET-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * H -> M WHEN MATURITY DATE IS PAST
      * M -> T WHEN MATURITY DATE IS PAST
      * T -> H WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3810-SET-REASON.
           EVALUATE TRSM-REASON-CD
               WHEN 'H'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE TRUSTEE FEE AMT BY STATUS CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3820-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-STATUS-CD
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRST-TRUSTEE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3830-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INCOME AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3840-ACCRUE-INCOME.
           IF TRST-INCOME-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRST-INCOME-AMT * TRSM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRSM-MARKET-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MARKET YTD.
      *----------------------------------------------------------------*
       3850-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-MARKET-YTD ROUNDED =
                   TRSM-TAX-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-MARKET-YTD
           END-IF
           ADD TRSM-MARKET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3860-DESCRIBE-CYCLE.
           EVALUATE TRSM-CYCLE-CD
               WHEN 'EN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3870-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB YTD FEE ON THE MARKET YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3880-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-DISTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   COMPUTE TRSM-DISTRIB-YTD ROUNDED = TRSM-MARKET-YTD *
                       TRSM-RATE / 200
               WHEN TRST-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO TRSM-DISTRIB-YTD
               WHEN TRST-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO TRSM-DISTRIB-YTD
               WHEN OTHER
                   COMPUTE TRSM-DISTRIB-YTD ROUNDED = TRSM-MARKET-YTD *
                       TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-DISTRIB-YTD > 0 AND TRSM-DISTRIB-YTD < 1.00
               MOVE 1.00 TO TRSM-DISTRIB-YTD
           END-IF
           IF TRSM-DISTRIB-YTD > 100.00
               MOVE 100.00 TO TRSM-DISTRIB-YTD
           END-IF
           ADD TRSM-DISTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE TRUSTEE FEE YTD FEE ON THE DISTRIB YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3890-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TRUSTEE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED =
                       TRSM-DISTRIB-YTD * TRSM-RATE / 200
               WHEN TRSM-REASON-CD = 'H'
                   MOVE 20.00 TO TRSM-TRUSTEE-FEE-YTD
               WHEN TRSM-REASON-CD = 'M'
                   MOVE 3.00 TO TRSM-TRUSTEE-FEE-YTD
               WHEN OTHER
                   COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED =
                       TRSM-DISTRIB-YTD * TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-TRUSTEE-FEE-YTD > 0 AND TRSM-TRUSTEE-FEE-YTD < 5.00
               MOVE 5.00 TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           IF TRSM-TRUSTEE-FEE-YTD > 35.00
               MOVE 35.00 TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           ADD TRSM-TRUSTEE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * MARKET AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3900-LIMIT-MARKET.
           IF TRSM-MARKET-AMT < 50.00
               MOVE 50.00 TO TRSM-MARKET-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-MARKET-AMT > 10000.00
               MOVE 10000.00 TO TRSM-MARKET-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INCOME BAND TIER FOR THE MARKET YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3910-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-BAND-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TRSM-MARKET-YTD NOT < WS-INCOME-BAND-LOW(WS-TX)
                   MOVE WS-INCOME-BAND-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY DIST FREQ CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3920-ACCUM-DIST-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-DIST-FREQ-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-DIST-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRUSTEE FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3930-FLAG-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRSM-TRUSTEE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRUSTEE FEE FEE ON THE DISTRIB AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3940-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TRST-TRUSTEE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   COMPUTE TRST-TRUSTEE-FEE-AMT ROUNDED =
                       TRST-DISTRIB-AMT * TRSM-RATE / 200
               WHEN TRSM-REGION-CD = 'A'
                   MOVE 20.00 TO TRST-TRUSTEE-FEE-AMT
               WHEN TRSM-REGION-CD = 'D'
                   MOVE 3.00 TO TRST-TRUSTEE-FEE-AMT
               WHEN OTHER
                   COMPUTE TRST-TRUSTEE-FEE-AMT ROUNDED =
                       TRST-DISTRIB-AMT * TRSM-RATE / 100
           END-EVALUATE
           IF TRST-TRUSTEE-FEE-AMT > 0 AND TRST-TRUSTEE-FEE-AMT < 2.00
               MOVE 2.00 TO TRST-TRUSTEE-FEE-AMT
           END-IF
           IF TRST-TRUSTEE-FEE-AMT > 100.00
               MOVE 100.00 TO TRST-TRUSTEE-FEE-AMT
           END-IF
           ADD TRST-TRUSTEE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3950-LIMIT-PRINCIPAL.
           IF TRSM-PRINCIPAL-YTD < 0.00
               MOVE 0.00 TO TRSM-PRINCIPAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-PRINCIPAL-YTD > 1000.00
               MOVE 1000.00 TO TRSM-PRINCIPAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB FEE ON THE MARKET AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3960-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRST-DISTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   COMPUTE TRST-DISTRIB-AMT ROUNDED = TRSM-MARKET-AMT *
                       TRSM-RATE / 200
               WHEN TRSM-TRUST-TYPE-CD = 'FLJ'
                   MOVE 10.00 TO TRST-DISTRIB-AMT
               WHEN TRSM-TRUST-TYPE-CD = 'MNG'
                   MOVE 15.00 TO TRST-DISTRIB-AMT
               WHEN OTHER
                   COMPUTE TRST-DISTRIB-AMT ROUNDED = TRSM-MARKET-AMT *
                       TRSM-RATE / 100
           END-EVALUATE
           IF TRST-DISTRIB-AMT > 0 AND TRST-DISTRIB-AMT < 2.00
               MOVE 2.00 TO TRST-DISTRIB-AMT
           END-IF
           IF TRST-DISTRIB-AMT > 25.00
               MOVE 25.00 TO TRST-DISTRIB-AMT
           END-IF
           ADD TRST-DISTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E907 MEMO REQUIRED
      * E238 WORK AMT 2 NEGATIVE
      * E612 WORK AMT 2 OVER 1000000
      * E685 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3970-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-MEMO = SPACES
                   MOVE 'E907' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E238' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E612' TO WS-REJECT-CODE
               WHEN TRSM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E685' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3980-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-STATUS-CD
                   ADD TRSM-DISTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-DISTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3990-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE TAX FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4000-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TRSM-TAX-AMT ROUNDED = WS-WORK-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRSM-CYCLE-CD = 'EN'
                   MOVE 10.00 TO TRSM-TAX-AMT
               WHEN TRSM-CYCLE-CD = 'FY'
                   MOVE 15.00 TO TRSM-TAX-AMT
               WHEN OTHER
                   COMPUTE TRSM-TAX-AMT ROUNDED = WS-WORK-AMT *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-TAX-AMT > 0 AND TRSM-TAX-AMT < 10.00
               MOVE 10.00 TO TRSM-TAX-AMT
           END-IF
           IF TRSM-TAX-AMT > 250.00
               MOVE 250.00 TO TRSM-TAX-AMT
           END-IF
           ADD TRSM-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       4010-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRST-PRINCIPAL-AMT ROUNDED =
                   TRSM-DISTRIB-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRST-PRINCIPAL-AMT
           END-IF
           ADD TRST-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E597 DESC REQUIRED
      * E623 INCOME AMT NEGATIVE
      * E391 INCOME AMT OVER 50000
      * E760 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4020-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-DESC = SPACES
                   MOVE 'E597' TO WS-REJECT-CODE
               WHEN TRSM-INCOME-AMT < 0
                   MOVE 'E623' TO WS-REJECT-CODE
               WHEN TRSM-INCOME-AMT > 50000.00
                   MOVE 'E391' TO WS-REJECT-CODE
               WHEN TRST-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E760' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4030-ACCRUE-DISTRIB.
           IF TRSM-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-DISTRIB-AMT * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRSM-MARKET-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E949 DESC REQUIRED
      * E058 DISTRIB AMT NEGATIVE
      * E717 DISTRIB AMT OVER 5000
      * E205 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4040-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-DESC = SPACES
                   MOVE 'E949' TO WS-REJECT-CODE
               WHEN TRSM-DISTRIB-AMT < 0
                   MOVE 'E058' TO WS-REJECT-CODE
               WHEN TRSM-DISTRIB-AMT > 5000.00
                   MOVE 'E717' TO WS-REJECT-CODE
               WHEN TRSM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E205' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E921 OPERATOR REQUIRED
      * E379 PRINCIPAL AMT NEGATIVE
      * E881 PRINCIPAL AMT OVER 5000
      * E644 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4050-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-OPERATOR = SPACES
                   MOVE 'E921' TO WS-REJECT-CODE
               WHEN TRSM-PRINCIPAL-AMT < 0
                   MOVE 'E379' TO WS-REJECT-CODE
               WHEN TRSM-PRINCIPAL-AMT > 5000.00
                   MOVE 'E881' TO WS-REJECT-CODE
               WHEN TRST-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E644' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE BAND TIER FOR THE DISTRIB YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4060-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-BAND-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TRSM-DISTRIB-YTD NOT < WS-TRUSTEE-BAND-LOW(WS-TX)
                   MOVE WS-TRUSTEE-BAND-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4070-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE INCOME AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MARKET AMT.
      *----------------------------------------------------------------*
       4080-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-MARKET-AMT ROUNDED =
                   TRSM-INCOME-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-MARKET-AMT
           END-IF
           ADD TRSM-MARKET-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EH -> GG WHEN OPEN DATE IS PAST
      * GG -> JG WHEN OPEN DATE IS PAST
      * JG -> EH WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4090-SET-STATUS.
           EVALUATE TRSM-STATUS-CD
               WHEN 'EH'
                   IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JG' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JG'
                   IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EH' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT INCOME AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4100-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = TRSM-INCOME-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TRSM-INCOME-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E267 REF NO REQUIRED
      * E295 TRUSTEE FEE YTD NEGATIVE
      * E714 TRUSTEE FEE YTD OVER 50000
      * E350 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4110-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-REF-NO = SPACES
                   MOVE 'E267' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-YTD < 0
                   MOVE 'E295' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-YTD > 50000.00
                   MOVE 'E714' TO WS-REJECT-CODE
               WHEN TRSM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E350' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4120-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-SCHED-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT-2 NOT < WS-TRUSTEE-SCHED-LOW(WS-TX)
                   MOVE WS-TRUSTEE-SCHED-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4130-DESCRIBE-REASON.
           EVALUATE TRSM-REASON-CD
               WHEN 'H'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT TRUSTEE FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4140-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = TRSM-TRUSTEE-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TRSM-TRUSTEE-FEE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE TRSM-TRUSTEE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TRUSTEE FEE AMT.
      *----------------------------------------------------------------*
       4150-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRST-TRUSTEE-FEE-AMT ROUNDED =
                   TRSM-PRINCIPAL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRST-TRUSTEE-FEE-AMT
           END-IF
           ADD TRST-TRUSTEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DISTRIB AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4160-FLAG-DISTRIB.
           IF TRST-DISTRIB-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRST-DISTRIB-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4170-COUNT-SEQ.
           IF TRST-SEQ < 9999999
               ADD 1 TO TRST-SEQ
           END-IF
           IF TRST-SEQ NOT < 6
               MOVE 'Y' TO TRSM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EXPIRY DATE IS PAST
      * BAT -> MAL WHEN EXPIRY DATE IS PAST
      * MAL -> TEL WHEN EXPIRY DATE IS PAST
      * TEL -> ONL WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4180-SET-CHANNEL.
           EVALUATE TRST-CHANNEL-CD
               WHEN 'BRN'
                   IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4190-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-BAND-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-TRUSTEE-BAND-LOW(WS-TX)
                   MOVE WS-TRUSTEE-BAND-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB TIER TIER FOR THE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4200-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-TIER-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TRSM-TAX-AMT NOT < WS-DISTRIB-TIER-LOW(WS-TX)
                   MOVE WS-DISTRIB-TIER-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TRUSTEE FEE YTD BY REASON CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4210-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-REASON-CD
                   ADD TRSM-TRUSTEE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-TRUSTEE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4220-ACCRUE-DISTRIB.
           IF TRSM-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-DISTRIB-AMT * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRSM-TRUSTEE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE TRUSTEE FEE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4230-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TRSM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRSM-PRINCIPAL-YTD ROUNDED =
                       TRST-TRUSTEE-FEE-AMT * TRSM-MARGIN / 200
               WHEN TRST-TRAN-CD = 'IN'
                   MOVE 15.00 TO TRSM-PRINCIPAL-YTD
               WHEN TRST-TRAN-CD = 'DS'
                   MOVE 5.00 TO TRSM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE TRSM-PRINCIPAL-YTD ROUNDED =
                       TRST-TRUSTEE-FEE-AMT * TRSM-MARGIN / 100
           END-EVALUATE
           IF TRSM-PRINCIPAL-YTD > 0 AND TRSM-PRINCIPAL-YTD < 1.00
               MOVE 1.00 TO TRSM-PRINCIPAL-YTD
           END-IF
           IF TRSM-PRINCIPAL-YTD > 100.00
               MOVE 100.00 TO TRSM-PRINCIPAL-YTD
           END-IF
           ADD TRSM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * INCOME AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4240-FLAG-INCOME.
           IF TRSM-INCOME-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-INCOME-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4250-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4260-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-TAX-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-TAX-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRSM-TAX-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-TAX-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-TAX-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4270-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4280-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-CHANNEL-CD
                   ADD TRSM-TAX-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-TAX-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INCOME YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4290-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-REGION-CD
                   ADD TRSM-INCOME-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-INCOME-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INCOME YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4300-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-STATUS-CD
                   ADD TRSM-INCOME-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-INCOME-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4310-LIMIT-DISTRIB.
           IF TRST-DISTRIB-AMT < 25.00
               MOVE 25.00 TO TRST-DISTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-DISTRIB-AMT > 10000.00
               MOVE 10000.00 TO TRST-DISTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAX AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4320-LIMIT-TAX.
           IF TRSM-TAX-AMT < 50.00
               MOVE 50.00 TO TRSM-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-TAX-AMT > 5000.00
               MOVE 5000.00 TO TRSM-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INCOME YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4330-ACCRUE-INCOME.
           IF TRSM-INCOME-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-INCOME-YTD * TRSM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TRST-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4340-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TRSM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRSM-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE DISTRIB AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4350-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TRSM-DISTRIB-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRSM-STATUS-CD = 'EH'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN TRSM-STATUS-CD = 'GG'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TRSM-DISTRIB-AMT *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E163 DESC REQUIRED
      * E054 TAX YTD NEGATIVE
      * E335 TAX YTD OVER 1000000
      * E453 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4360-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-DESC = SPACES
                   MOVE 'E163' TO WS-REJECT-CODE
               WHEN TRSM-TAX-YTD < 0
                   MOVE 'E054' TO WS-REJECT-CODE
               WHEN TRSM-TAX-YTD > 1000000.00
                   MOVE 'E335' TO WS-REJECT-CODE
               WHEN TRSM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E453' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INCOME AMT.
      *----------------------------------------------------------------*
       4370-PRORATE-INCOME.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-INCOME-AMT ROUNDED =
                   TRSM-DISTRIB-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-INCOME-AMT
           END-IF
           ADD TRSM-INCOME-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE INCOME YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TRUSTEE FEE AMT.
      *----------------------------------------------------------------*
       4380-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRST-TRUSTEE-FEE-AMT ROUNDED =
                   TRSM-INCOME-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRST-TRUSTEE-FEE-AMT
           END-IF
           ADD TRST-TRUSTEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE INCOME BAND TIER FOR THE DISTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4390-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-BAND-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TRST-DISTRIB-AMT NOT < WS-INCOME-BAND-LOW(WS-TX)
                   MOVE WS-INCOME-BAND-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TRUSTEE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INCOME YTD.
      *----------------------------------------------------------------*
       4400-PRORATE-INCOME.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-INCOME-YTD ROUNDED =
                   TRSM-TRUSTEE-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-INCOME-YTD
           END-IF
           ADD TRSM-INCOME-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE TRUSTEE FEE YTD BY CHANNEL CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4410-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-CHANNEL-CD
                   ADD TRSM-TRUSTEE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-TRUSTEE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E211 REJECT CD REQUIRED
      * E322 DISTRIB AMT NEGATIVE
      * E386 DISTRIB AMT OVER 50000
      * E350 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4420-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-REJECT-CD = SPACES
                   MOVE 'E211' TO WS-REJECT-CODE
               WHEN TRST-DISTRIB-AMT < 0
                   MOVE 'E322' TO WS-REJECT-CODE
               WHEN TRST-DISTRIB-AMT > 50000.00
                   MOVE 'E386' TO WS-REJECT-CODE
               WHEN TRSM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E350' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4430-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-REGION-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TRUSTEE FEE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4440-ACCRUE-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TRUSTEE-FEE-AMT * TRSM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRST-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4450-FLAG-DISTRIB.
           IF TRST-DISTRIB-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRST-DISTRIB-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MARKET AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4460-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-CHANNEL-CD
                   ADD TRSM-MARKET-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-MARKET-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TAX YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4470-FLAG-TAX.
           IF TRSM-TAX-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRSM-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * MARKET YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4480-LIMIT-MARKET.
           IF TRSM-MARKET-YTD < 100.00
               MOVE 100.00 TO TRSM-MARKET-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-MARKET-YTD > 500.00
               MOVE 500.00 TO TRSM-MARKET-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4490-COUNT-CYCLE.
           IF TRSM-CYCLE-CNT < 9999999
               ADD 1 TO TRSM-CYCLE-CNT
           END-IF
           IF TRSM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO TRSM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4500-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF TRST-POST-DATE > 0 AND TRST-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRST-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4510-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRST-DISTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   COMPUTE TRST-DISTRIB-AMT ROUNDED = TRSM-PRINCIPAL-AMT
                       * TRSM-MARGIN / 200
               WHEN TRSM-REASON-CD = 'H'
                   MOVE 10.00 TO TRST-DISTRIB-AMT
               WHEN TRSM-REASON-CD = 'M'
                   MOVE 15.00 TO TRST-DISTRIB-AMT
               WHEN OTHER
                   COMPUTE TRST-DISTRIB-AMT ROUNDED = TRSM-PRINCIPAL-AMT
                       * TRSM-MARGIN / 100
           END-EVALUATE
           IF TRST-DISTRIB-AMT > 0 AND TRST-DISTRIB-AMT < 10.00
               MOVE 10.00 TO TRST-DISTRIB-AMT
           END-IF
           IF TRST-DISTRIB-AMT > 50.00
               MOVE 50.00 TO TRST-DISTRIB-AMT
           END-IF
           ADD TRST-DISTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE INCOME FEE ON THE DISTRIB AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4520-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TRSM-INCOME-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   COMPUTE TRSM-INCOME-AMT ROUNDED = TRSM-DISTRIB-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRST-TRAN-CD = 'IN'
                   MOVE 7.00 TO TRSM-INCOME-AMT
               WHEN TRST-TRAN-CD = 'DS'
                   MOVE 5.00 TO TRSM-INCOME-AMT
               WHEN OTHER
                   COMPUTE TRSM-INCOME-AMT ROUNDED = TRSM-DISTRIB-AMT *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-INCOME-AMT > 0 AND TRSM-INCOME-AMT < 10.00
               MOVE 10.00 TO TRSM-INCOME-AMT
           END-IF
           IF TRSM-INCOME-AMT > 50.00
               MOVE 50.00 TO TRSM-INCOME-AMT
           END-IF
           ADD TRSM-INCOME-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INCOME AMT.
      *----------------------------------------------------------------*
       4530-PRORATE-INCOME.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-INCOME-AMT ROUNDED =
                   TRSM-TAX-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-INCOME-AMT
           END-IF
           ADD TRSM-INCOME-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4540-COUNT-SEQ.
           IF TRST-SEQ < 9999999
               ADD 1 TO TRST-SEQ
           END-IF
           IF TRST-SEQ NOT < 5
               MOVE 'Y' TO TRST-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4550-COUNT-CYCLE.
           IF TRSM-CYCLE-CNT < 9999999
               ADD 1 TO TRSM-CYCLE-CNT
           END-IF
           IF TRSM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO TRSM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE DISTRIB YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4560-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TRSM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   COMPUTE TRSM-PRINCIPAL-YTD ROUNDED = TRSM-DISTRIB-YTD
                       * TRSM-RATE / 200
               WHEN TRSM-CYCLE-CD = 'EN'
                   MOVE 3.00 TO TRSM-PRINCIPAL-YTD
               WHEN TRSM-CYCLE-CD = 'FY'
                   MOVE 7.00 TO TRSM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE TRSM-PRINCIPAL-YTD ROUNDED = TRSM-DISTRIB-YTD
                       * TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-PRINCIPAL-YTD > 0 AND TRSM-PRINCIPAL-YTD < 1.00
               MOVE 1.00 TO TRSM-PRINCIPAL-YTD
           END-IF
           IF TRSM-PRINCIPAL-YTD > 100.00
               MOVE 100.00 TO TRSM-PRINCIPAL-YTD
           END-IF
           ADD TRSM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE TRUSTEE FEE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4570-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-TRAN-CD
                   ADD TRSM-TRUSTEE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-TRUSTEE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4580-FLAG-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRST-PRINCIPAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4590-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE TRSM-PRINCIPAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4600-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF TRST-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRST-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TRSM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       4610-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4620-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-CYCLE-CD
                   ADD TRSM-DISTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-DISTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4630-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE TRSM-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TRUSTEE FEE AMT.
      *----------------------------------------------------------------*
       4640-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TRUSTEE-FEE-AMT ROUNDED =
                   TRSM-PRINCIPAL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-TRUSTEE-FEE-AMT
           END-IF
           ADD TRSM-TRUSTEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE DIST FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4650-DESCRIBE-DIST-FREQ.
           EVALUATE TRSM-DIST-FREQ-CD
               WHEN 'E'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'W'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARKET AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4660-ACCRUE-MARKET.
           IF TRSM-MARKET-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-MARKET-AMT * TRSM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRSM-DISTRIB-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4670-FLAG-DISTRIB.
           IF TRSM-DISTRIB-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRSM-DISTRIB-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E395 REF NO REQUIRED
      * E313 TRUSTEE FEE AMT NEGATIVE
      * E515 TRUSTEE FEE AMT OVER 25000
      * E839 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4680-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-REF-NO = SPACES
                   MOVE 'E395' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-AMT < 0
                   MOVE 'E313' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-AMT > 25000.00
                   MOVE 'E515' TO WS-REJECT-CODE
               WHEN TRST-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E839' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * H -> M WHEN LAST MAINT DATE IS PAST
      * M -> T WHEN LAST MAINT DATE IS PAST
      * T -> H WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4690-SET-REASON.
           EVALUATE TRSM-REASON-CD
               WHEN 'H'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'M' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'H' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4700-LIMIT-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT < 100.00
               MOVE 100.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-PRINCIPAL-AMT > 2500.00
               MOVE 2500.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE SCHED TIER FOR THE INCOME AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4710-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-SCHED-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRST-INCOME-AMT NOT < WS-TRUSTEE-SCHED-LOW(WS-TX)
                   MOVE WS-TRUSTEE-SCHED-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4720-LIMIT-DISTRIB.
           IF TRSM-DISTRIB-YTD < 100.00
               MOVE 100.00 TO TRSM-DISTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-DISTRIB-YTD > 1000.00
               MOVE 1000.00 TO TRSM-DISTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARKET AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4730-FLAG-MARKET.
           IF TRSM-MARKET-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRSM-MARKET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INCOME AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4740-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = TRST-INCOME-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TRST-INCOME-AMT - (WS-WORK-AMT * 12)
           COMPUTE TRST-TRUSTEE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INCOME YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4750-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-CYCLE-CD
                   ADD TRSM-INCOME-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-INCOME-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4760-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-BAND-RATE(1) TO TRSM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-TRUSTEE-BAND-LOW(WS-TX)
                   MOVE WS-TRUSTEE-BAND-RATE(WS-TX) TO TRSM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAX YTD IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4770-LIMIT-TAX.
           IF TRSM-TAX-YTD < 10.00
               MOVE 10.00 TO TRSM-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-TAX-YTD > 500.00
               MOVE 500.00 TO TRSM-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAX YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4780-LIMIT-TAX.
           IF TRSM-TAX-YTD < 50.00
               MOVE 50.00 TO TRSM-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-TAX-YTD > 10000.00
               MOVE 10000.00 TO TRSM-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4790-COUNT-RETRY.
           IF TRSM-RETRY-CNT < 9999999
               ADD 1 TO TRSM-RETRY-CNT
           END-IF
           IF TRSM-RETRY-CNT NOT < 6
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4800-COUNT-PAYMENT.
           IF TRSM-PAYMENT-CNT < 9999999
               ADD 1 TO TRSM-PAYMENT-CNT
           END-IF
           IF TRSM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO TRSM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4810-COUNT-RETRY.
           IF TRSM-RETRY-CNT < 9999999
               ADD 1 TO TRSM-RETRY-CNT
           END-IF
           IF TRSM-RETRY-CNT NOT < 5
               MOVE 'Y' TO TRST-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB YTD FEE ON THE TRUSTEE FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4820-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TRSM-DISTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   COMPUTE TRSM-DISTRIB-YTD ROUNDED =
                       TRSM-TRUSTEE-FEE-AMT * TRSM-DISCOUNT / 200
               WHEN TRSM-TRUST-TYPE-CD = 'FLJ'
                   MOVE 5.00 TO TRSM-DISTRIB-YTD
               WHEN TRSM-TRUST-TYPE-CD = 'MNG'
                   MOVE 7.00 TO TRSM-DISTRIB-YTD
               WHEN OTHER
                   COMPUTE TRSM-DISTRIB-YTD ROUNDED =
                       TRSM-TRUSTEE-FEE-AMT * TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-DISTRIB-YTD > 0 AND TRSM-DISTRIB-YTD < 10.00
               MOVE 10.00 TO TRSM-DISTRIB-YTD
           END-IF
           IF TRSM-DISTRIB-YTD > 75.00
               MOVE 75.00 TO TRSM-DISTRIB-YTD
           END-IF
           ADD TRSM-DISTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INCOME AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4830-ACCRUE-INCOME.
           IF TRST-INCOME-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRST-INCOME-AMT * TRSM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TRSM-DISTRIB-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MARKET AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4840-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-CYCLE-CD
                   ADD TRSM-MARKET-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-MARKET-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE TRS FIELDS TO TRVAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-TRVAL.
           INITIALIZE TRVAL-PARMS
           MOVE TRSM-MARKET-AMT
             TO TRVAL-MARKET-AMT
           MOVE TRSM-TAX-AMT
             TO TRVAL-TAX-AMT
           MOVE TRSM-PRINCIPAL-YTD
             TO TRVAL-PRINCIPAL-YTD
           MOVE TRSM-DISTRIB-YTD
             TO TRVAL-DISTRIB-YTD
           MOVE TRSM-MARKET-YTD
             TO TRVAL-MARKET-YTD
           MOVE TRSM-PRINCIPAL-AMT
             TO TRVAL-PRINCIPAL-AMT
           MOVE TRSM-NOTICE-DATE
             TO TRVAL-NOTICE-DATE
           MOVE TRSM-LAST-MAINT-DATE
             TO TRVAL-LAST-MAINT-DATE
           MOVE TRSM-MATURITY-DATE
             TO TRVAL-MATURITY-DATE
           MOVE TRSM-CLOSE-DATE
             TO TRVAL-CLOSE-DATE
           MOVE TRSM-REASON-CD
             TO TRVAL-REASON-CD
           MOVE TRSM-REGION-CD
             TO TRVAL-REGION-CD
           MOVE TRSM-STATUS-CD
             TO TRVAL-STATUS-CD
           MOVE TRSM-TRUST-TYPE-CD
             TO TRVAL-TRUST-TYPE-CD
           MOVE TRSM-NOTICE-SENT-FLAG
             TO TRVAL-NOTICE-SENT-FLAG
           MOVE TRSM-REVIEW-FLAG
             TO TRVAL-REVIEW-FLAG
           MOVE TRSM-EXEMPT-FLAG
             TO TRVAL-EXEMPT-FLAG
           MOVE TRSM-CYCLE-CNT
             TO TRVAL-CYCLE-CNT
           MOVE TRSM-NOTICE-CNT
             TO TRVAL-NOTICE-CNT
           MOVE TRSM-RATE
             TO TRVAL-RATE
           MOVE TRSM-MARGIN
             TO TRVAL-MARGIN
           MOVE TRSM-ADDR
             TO TRVAL-ADDR
           MOVE TRSM-CONTACT
             TO TRVAL-CONTACT
           CALL 'TRVAL' USING TRVAL-PARMS
           EVALUATE TRVAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TRVAL-MARKET-AMT
                     TO TRSM-MARKET-AMT
                   MOVE TRVAL-TAX-AMT
                     TO TRSM-TAX-AMT
                   MOVE TRVAL-PRINCIPAL-YTD
                     TO TRSM-PRINCIPAL-YTD
                   MOVE TRVAL-DISTRIB-YTD
                     TO TRSM-DISTRIB-YTD
                   MOVE TRVAL-MARKET-YTD
                     TO TRSM-MARKET-YTD
                   MOVE TRVAL-PRINCIPAL-AMT
                     TO TRSM-PRINCIPAL-AMT
                   MOVE TRVAL-REVIEW-FLAG
                     TO TRSM-REVIEW-FLAG
                   MOVE TRVAL-EXEMPT-FLAG
                     TO TRSM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-TRVAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TRS FIELDS TO TRLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-TRLIM.
           INITIALIZE TRLIM-PARMS
           MOVE TRSM-TRUSTEE-FEE-AMT
             TO TRLIM-TRUSTEE-FEE-AMT
           MOVE TRSM-DISTRIB-YTD
             TO TRLIM-DISTRIB-YTD
           MOVE TRSM-DISTRIB-AMT
             TO TRLIM-DISTRIB-AMT
           MOVE TRSM-INCOME-YTD
             TO TRLIM-INCOME-YTD
           MOVE TRSM-INCOME-AMT
             TO TRLIM-INCOME-AMT
           MOVE TRSM-PRINCIPAL-YTD
             TO TRLIM-PRINCIPAL-YTD
           MOVE TRSM-CLOSE-DATE
             TO TRLIM-CLOSE-DATE
           MOVE TRSM-LAST-MAINT-DATE
             TO TRLIM-LAST-MAINT-DATE
           MOVE TRSM-OPEN-DATE
             TO TRLIM-OPEN-DATE
           MOVE TRSM-NOTICE-DATE
             TO TRLIM-NOTICE-DATE
           MOVE TRSM-CYCLE-CD
             TO TRLIM-CYCLE-CD
           MOVE TRSM-TRUST-TYPE-CD
             TO TRLIM-TRUST-TYPE-CD
           MOVE TRSM-REASON-CD
             TO TRLIM-REASON-CD
           MOVE TRSM-STATUS-CD
             TO TRLIM-STATUS-CD
           MOVE TRSM-STOP-FLAG
             TO TRLIM-STOP-FLAG
           MOVE TRSM-EXEMPT-FLAG
             TO TRLIM-EXEMPT-FLAG
           MOVE TRSM-SUSPEND-FLAG
             TO TRLIM-SUSPEND-FLAG
           MOVE TRSM-PAYMENT-CNT
             TO TRLIM-PAYMENT-CNT
           MOVE TRSM-RETRY-CNT
             TO TRLIM-RETRY-CNT
           MOVE TRSM-MARGIN
             TO TRLIM-MARGIN
           MOVE TRSM-RATE
             TO TRLIM-RATE
           MOVE TRSM-NAME
             TO TRLIM-NAME
           MOVE TRSM-CUST-ID
             TO TRLIM-CUST-ID
           CALL 'TRLIM' USING TRLIM-PARMS
           EVALUATE TRLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TRLIM-TRUSTEE-FEE-AMT
                     TO TRSM-TRUSTEE-FEE-AMT
                   MOVE TRLIM-DISTRIB-AMT
                     TO TRSM-DISTRIB-AMT
                   MOVE TRLIM-INCOME-YTD
                     TO TRSM-INCOME-YTD
                   MOVE TRLIM-PRINCIPAL-YTD
                     TO TRSM-PRINCIPAL-YTD
                   MOVE TRLIM-EXEMPT-FLAG
                     TO TRSM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-TRLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE TRST-KEY TO RD-KEY
           MOVE TRST-TRAN-CD TO RD-CODE
           MOVE TRST-INCOME-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE TRST-KEY TO RD-KEY
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
           DISPLAY 'TRSRNEW READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TRSRNEW' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
