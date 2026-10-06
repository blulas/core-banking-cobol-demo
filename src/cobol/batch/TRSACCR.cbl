      *================================================================*
      * PROGRAM  : TRSACCR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PERSONAL TRUST - DAILY ACCRUAL. READS THE TRS       *
      *            TRANSACTIONS, UPDATES THE TRS MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. PERSONAL TRUST ACCOUNTING,      *
      *            INCOME DISTRIBUTION AND TRUSTEE FEES                *
      * INPUT    : TRSMST (KSDS), TRSTRN, BANKCTL                      *
      * OUTPUT   : TRSOUT, TRSACRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TRPEN                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-11-22 TWH  ORIGINAL                                       *
      * 1995-01-14 PQR  INCOME STATUS CODES ADDED INC7854              *
      * 1995-08-26 BHS  TRUSTEE-FEE CUTOFF MOVED INC2536               *
      * 1997-05-03 BHS  PRINCIPAL EXCEPTIONS TO OPS QUEUE INC9406      *
      * 2000-08-23 SKN  PRINCIPAL TABLE EXPANDED INC9562               *
      * 2000-08-23 DPS  MARKET STATUS CODES ADDED CHG8612              *
      * 2001-05-07 PQR  INCOME STATUS CODES ADDED CHG0865              *
      * 2001-06-09 BHS  INCOME TABLE EXPANDED PRB3697                  *
      * 2002-11-27 LMB  DISTRIB RERUN PROTECTION CHG1553               *
      * 2002-08-14 LMB  TAX LIMIT REVISED INC6424                      *
      * 2005-02-24 JLM  PRINCIPAL EXCEPTIONS TO OPS QUEUE REQ0398      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRSACCR.
       AUTHOR. M K LOWE.

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
           SELECT REPORT-FILE ASSIGN TO "TRSACRPT"
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
                                           VALUE 19.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 31.
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
      * WS-DISTRIB-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISTRIB-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000685000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000792500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000149500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000788000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000013000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006350000000634000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016350000000710500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026350000000184500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028850000000683000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029350000000721500'.
       01  WS-DISTRIB-BAND-TBL REDEFINES WS-DISTRIB-BAND-VALUES.
           05  WS-DISTRIB-BAND-ROW OCCURS 10 TIMES.
               10  WS-DISTRIB-BAND-LOW     PIC 9(09)V99.
               10  WS-DISTRIB-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-TAX-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TAX-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000476500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000401000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000570000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000521500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010700000000899000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013200000000371500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018200000000795500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020700000000884000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025700000000527500'.
       01  WS-TAX-SCHED-TBL REDEFINES WS-TAX-SCHED-VALUES.
           05  WS-TAX-SCHED-ROW OCCURS 9 TIMES.
               10  WS-TAX-SCHED-LOW        PIC 9(09)V99.
               10  WS-TAX-SCHED-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TRSACCR  PERSONAL TRUST'.
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
       COPY TRPENP.

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
               PERFORM 3000-CHECK-CLOSE
               PERFORM 3010-COUNT-NOTICE
               PERFORM 3020-DESCRIBE-TRAN
               PERFORM 3030-DESCRIBE-TRAN
               PERFORM 3040-DESCRIBE-TRUST-TYPE
               PERFORM 3050-DESCRIBE-DIST-FREQ
               PERFORM 3060-SET-REGION
               PERFORM 3070-SPLIT-TRUSTEE-FEE
               PERFORM 3080-LIMIT-ITEM
               PERFORM 3090-ASSESS-SUSPEND
               PERFORM 3100-ASSESS-NOTICE-SENT
               PERFORM 3110-COUNT-SEQ
               PERFORM 3120-SELECT-RATE
               PERFORM 3130-CHECK-EFFECTIVE
               PERFORM 3140-COUNT-CYCLE
               PERFORM 3150-DESCRIBE-TRAN
               PERFORM 3160-SET-REGION
               PERFORM 3170-ACCRUE-DISTRIB
               PERFORM 3180-FLAG-TRUSTEE-FEE
               PERFORM 3190-SPLIT-PRINCIPAL
               PERFORM 3200-ACCUM-REASON
               PERFORM 3210-SPLIT-INCOME
               PERFORM 3220-EDIT-OPERATOR
               PERFORM 3230-CHECK-EXPIRY
               PERFORM 3240-COUNT-RETRY
               PERFORM 3250-LIMIT-DISTRIB
               PERFORM 3260-SELECT-DISCOUNT
               PERFORM 3270-ACCRUE-TAX
               PERFORM 3280-SET-REASON
               PERFORM 3290-COUNT-RETRY
               PERFORM 3300-EDIT-REJECT
               PERFORM 3310-SET-TRUST-TYPE
               PERFORM 3320-AGE-EXPIRY
               PERFORM 3330-SET-REASON
               PERFORM 3340-SPLIT-TAX
               PERFORM 3350-ACCRUE-DISTRIB
               PERFORM 3360-LIMIT-PRINCIPAL
               PERFORM 3370-FLAG-TAX
               PERFORM 3380-ASSESS-STOP
               PERFORM 3390-FLAG-TRUSTEE-FEE
               PERFORM 3400-EDIT-REF-NO
               PERFORM 3410-LIMIT-PRINCIPAL
               PERFORM 3420-ACCRUE-DISTRIB
               PERFORM 3430-SPLIT-PRINCIPAL
               PERFORM 3440-FLAG-TAX
               PERFORM 3450-PRORATE-TRUSTEE-FEE
               PERFORM 3460-FLAG-PRINCIPAL
               PERFORM 3470-ASSESS-NOTICE-SENT
               PERFORM 3480-AGE-POST
               PERFORM 3490-FLAG-ITEM
               PERFORM 3500-SET-CHANNEL
               PERFORM 3510-LIMIT-PRINCIPAL
               PERFORM 3520-ASSESS-STOP
               PERFORM 3530-CHECK-LAST-PAY
               PERFORM 3540-DESCRIBE-REGION
               PERFORM 3550-LIMIT-INCOME
               PERFORM 3560-ACCUM-CYCLE
               PERFORM 3570-ASSESS-PREFERRED
               PERFORM 3580-PRORATE-PRINCIPAL
               PERFORM 3590-LIMIT-MARKET
               PERFORM 3600-AGE-LAST-MAINT
               PERFORM 3610-DESCRIBE-REGION
               PERFORM 3620-COUNT-NOTICE
               PERFORM 3630-COUNT-NOTICE
               PERFORM 3640-SELECT-DISCOUNT
               PERFORM 3650-CHECK-POST
               PERFORM 3660-FLAG-INCOME
               PERFORM 3670-PRORATE-DISTRIB
               PERFORM 3680-FLAG-TRUSTEE-FEE
               PERFORM 3690-CHECK-LAST-STMT
               PERFORM 3700-LIMIT-PRINCIPAL
               PERFORM 3710-DESCRIBE-CHANNEL
               PERFORM 3720-ACCUM-TRAN
               PERFORM 3730-FLAG-DISTRIB
               PERFORM 3740-LIMIT-INCOME
               PERFORM 3750-ACCUM-TRUST-TYPE
               PERFORM 3760-PRORATE-INCOME
               PERFORM 3770-SPLIT-INCOME
               PERFORM 3780-ACCRUE-DISTRIB
               PERFORM 3790-CHECK-MATURITY
               PERFORM 3800-ASSESS-SUSPEND
               PERFORM 3810-SELECT-DISCOUNT
               PERFORM 3820-ACCUM-TRAN
               PERFORM 3830-ACCRUE-DISTRIB
               PERFORM 3840-LIMIT-PRINCIPAL
               PERFORM 3850-FLAG-DISTRIB
               PERFORM 3860-EDIT-CONTACT
               PERFORM 3870-EDIT-OPERATOR
               PERFORM 3880-SELECT-RATE
               PERFORM 3890-CHECK-NOTICE
               PERFORM 3900-LIMIT-MARKET
               PERFORM 3910-ASSESS-PREFERRED
               PERFORM 3920-COUNT-PAYMENT
               PERFORM 3930-SET-TRUST-TYPE
               PERFORM 3940-ACCUM-REGION
               PERFORM 3950-ACCRUE-TRUSTEE-FEE
               PERFORM 3960-AGE-LAST-PAY
               PERFORM 3970-SPLIT-PRINCIPAL
               PERFORM 3980-SELECT-MARGIN
               PERFORM 3990-ASSESS-EXEMPT
               PERFORM 4000-ACCRUE-TRUSTEE-FEE
               PERFORM 4010-COUNT-CYCLE
               PERFORM 4020-AGE-OPEN
               PERFORM 4030-AGE-NOTICE
               PERFORM 4040-COUNT-NOTICE
               PERFORM 4050-LIMIT-TAX
               PERFORM 4060-COUNT-SEQ
               PERFORM 4070-ASSESS-STOP
               PERFORM 4080-ACCUM-REASON
               PERFORM 4090-PRORATE-INCOME
               PERFORM 4100-ASSESS-EXEMPT
               PERFORM 4110-ASSESS-STOP
               PERFORM 4120-SET-REASON
               PERFORM 4130-EDIT-MEMO
               PERFORM 4140-DESCRIBE-STATUS
               PERFORM 4150-DESCRIBE-TRAN
               PERFORM 4160-ACCRUE-DISTRIB
               PERFORM 4170-COUNT-NOTICE
               PERFORM 4180-AGE-LAST-MAINT
               PERFORM 4190-LIMIT-PRINCIPAL
               PERFORM 4200-DESCRIBE-REGION
               PERFORM 4210-PRORATE-TRUSTEE-FEE
               PERFORM 4220-SET-CYCLE
               PERFORM 4230-AGE-POST
               PERFORM 4240-SET-REGION
               PERFORM 4250-AGE-NOTICE
               PERFORM 4260-PRORATE-TRUSTEE-FEE
               PERFORM 4270-AGE-EFFECTIVE
               PERFORM 4280-PRORATE-TRUSTEE-FEE
               PERFORM 4290-ACCRUE-PRINCIPAL
               PERFORM 4300-FLAG-MARKET
               PERFORM 4310-FLAG-PRINCIPAL
               PERFORM 4320-AGE-LAST-STMT
               PERFORM 4330-ASSESS-STOP
               PERFORM 4340-FLAG-ITEM
               PERFORM 4350-PRORATE-TRUSTEE-FEE
               PERFORM 4360-ACCRUE-INCOME
               PERFORM 4370-ASSESS-NOTICE-SENT
               PERFORM 4380-SPLIT-TAX
               PERFORM 4390-SPLIT-MARKET
               PERFORM 4400-EDIT-DESC
               PERFORM 4410-LIMIT-ITEM
               PERFORM 4420-DESCRIBE-REGION
               PERFORM 4430-CHECK-LAST-MAINT
               PERFORM 4440-ACCUM-STATUS
               PERFORM 4450-ACCUM-DIST-FREQ
               PERFORM 4460-SET-REGION
               PERFORM 4470-ACCUM-REGION
               PERFORM 4480-SET-DIST-FREQ
               PERFORM 4490-SPLIT-DISTRIB
               PERFORM 4500-SET-REGION
               PERFORM 4510-ACCRUE-INCOME
               PERFORM 4520-CHECK-LAST-PAY
               PERFORM 4530-CHECK-EFF
               PERFORM 4540-PRORATE-MARKET
               PERFORM 4550-ASSESS-EXEMPT
               PERFORM 4560-LIMIT-MARKET
               PERFORM 4570-ACCUM-REASON
               PERFORM 4580-PRORATE-MARKET
               PERFORM 4590-CHECK-EXPIRY
               PERFORM 4600-ASSESS-REVERSAL
               PERFORM 4610-ASSESS-EXEMPT
               PERFORM 4620-FLAG-INCOME
               PERFORM 4630-CHECK-LAST-MAINT
               PERFORM 4640-ACCRUE-DISTRIB
               PERFORM 4650-SPLIT-TAX
               PERFORM 4660-ACCRUE-PRINCIPAL
               PERFORM 4670-PRORATE-MARKET
               PERFORM 4680-DESCRIBE-STATUS
               PERFORM 4690-COUNT-SEQ
               PERFORM 4700-FLAG-MARKET
               PERFORM 4710-ACCRUE-INCOME
               PERFORM 4720-LIMIT-DISTRIB
               PERFORM 4730-AGE-NOTICE
               PERFORM 4740-ACCUM-DIST-FREQ
               PERFORM 4750-CHECK-MATURITY
               PERFORM 4760-AGE-LAST-STMT
               PERFORM 4770-ASSESS-EXEMPT
               PERFORM 4780-PRORATE-PRINCIPAL
               PERFORM 4790-ACCUM-REGION
               PERFORM 4800-LIMIT-PRINCIPAL
               PERFORM 4810-COUNT-RETRY
               PERFORM 4820-FLAG-ITEM
               PERFORM 4830-ASSESS-NOTICE-SENT
               PERFORM 7010-CALL-TRPEN
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
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3000-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3010-COUNT-NOTICE.
           IF TRSM-NOTICE-CNT < 9999999
               ADD 1 TO TRSM-NOTICE-CNT
           END-IF
           IF TRSM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO TRSM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3020-DESCRIBE-TRAN.
           EVALUATE TRST-TRAN-CD
               WHEN 'IN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3030-DESCRIBE-TRAN.
           EVALUATE TRST-TRAN-CD
               WHEN 'IN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRUST TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3040-DESCRIBE-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MNG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VFN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XPL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE DIST FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3050-DESCRIBE-DIST-FREQ.
           EVALUATE TRSM-DIST-FREQ-CD
               WHEN 'E'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'K'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'P'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> D WHEN LAST PAY DATE IS PAST
      * D -> N WHEN LAST PAY DATE IS PAST
      * N -> T WHEN LAST PAY DATE IS PAST
      * T -> A WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3060-SET-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'A' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT TRUSTEE FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3070-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = TRSM-TRUSTEE-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TRSM-TRUSTEE-FEE-AMT - (WS-WORK-AMT *
               2)
           COMPUTE TRSM-INCOME-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3080-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INCOME FEE ON THE DISTRIB AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3090-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TRST-INCOME-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-STOP-FLAG = 'Y'
                   COMPUTE TRST-INCOME-AMT ROUNDED = TRSM-DISTRIB-AMT *
                       TRSM-MARGIN / 200
               WHEN TRSM-REASON-CD = 'H'
                   MOVE 3.00 TO TRST-INCOME-AMT
               WHEN TRSM-REASON-CD = 'M'
                   MOVE 7.00 TO TRST-INCOME-AMT
               WHEN OTHER
                   COMPUTE TRST-INCOME-AMT ROUNDED = TRSM-DISTRIB-AMT *
                       TRSM-MARGIN / 100
           END-EVALUATE
           IF TRST-INCOME-AMT > 0 AND TRST-INCOME-AMT < 1.00
               MOVE 1.00 TO TRST-INCOME-AMT
           END-IF
           IF TRST-INCOME-AMT > 150.00
               MOVE 150.00 TO TRST-INCOME-AMT
           END-IF
           ADD TRST-INCOME-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE INCOME YTD FEE ON THE TRUSTEE FEE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3100-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-INCOME-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   COMPUTE TRSM-INCOME-YTD ROUNDED =
                       TRSM-TRUSTEE-FEE-YTD * TRSM-RATE / 200
               WHEN TRSM-DIST-FREQ-CD = 'E'
                   MOVE 7.00 TO TRSM-INCOME-YTD
               WHEN TRSM-DIST-FREQ-CD = 'K'
                   MOVE 5.00 TO TRSM-INCOME-YTD
               WHEN OTHER
                   COMPUTE TRSM-INCOME-YTD ROUNDED =
                       TRSM-TRUSTEE-FEE-YTD * TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-INCOME-YTD > 0 AND TRSM-INCOME-YTD < 5.00
               MOVE 5.00 TO TRSM-INCOME-YTD
           END-IF
           IF TRSM-INCOME-YTD > 250.00
               MOVE 250.00 TO TRSM-INCOME-YTD
           END-IF
           ADD TRSM-INCOME-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3110-COUNT-SEQ.
           IF TRST-SEQ < 9999999
               ADD 1 TO TRST-SEQ
           END-IF
           IF TRST-SEQ NOT < 5
               MOVE 'Y' TO TRSM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAX SCHED TIER FOR THE TRUSTEE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-SCHED-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TRST-TRUSTEE-FEE-AMT NOT < WS-TAX-SCHED-LOW(WS-TX)
                   MOVE WS-TAX-SCHED-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO TRSM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3140-COUNT-CYCLE.
           IF TRSM-CYCLE-CNT < 9999999
               ADD 1 TO TRSM-CYCLE-CNT
           END-IF
           IF TRSM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3150-DESCRIBE-TRAN.
           EVALUATE TRST-TRAN-CD
               WHEN 'IN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> D WHEN LAST MAINT DATE IS PAST
      * D -> N WHEN LAST MAINT DATE IS PAST
      * N -> T WHEN LAST MAINT DATE IS PAST
      * T -> A WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3160-SET-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'D' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'N' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'A' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3170-ACCRUE-DISTRIB.
           IF TRSM-DISTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-DISTRIB-YTD * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRSM-DISTRIB-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRUSTEE FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3180-FLAG-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-TRUSTEE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3190-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = TRSM-PRINCIPAL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TRSM-PRINCIPAL-AMT - (WS-WORK-AMT *
               3)
           COMPUTE TRSM-TAX-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3200-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-REASON-CD
                   ADD TRSM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INCOME YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3210-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = TRSM-INCOME-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TRSM-INCOME-YTD - (WS-WORK-AMT * 12)
           COMPUTE TRSM-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E201 OPERATOR REQUIRED
      * E152 TRUSTEE FEE AMT NEGATIVE
      * E872 TRUSTEE FEE AMT OVER 1000000
      * E250 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3220-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-OPERATOR = SPACES
                   MOVE 'E201' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-AMT < 0
                   MOVE 'E152' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-AMT > 1000000.00
                   MOVE 'E872' TO WS-REJECT-CODE
               WHEN TRSM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E250' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3230-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TRST-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3240-COUNT-RETRY.
           IF TRSM-RETRY-CNT < 9999999
               ADD 1 TO TRSM-RETRY-CNT
           END-IF
           IF TRSM-RETRY-CNT NOT < 12
               MOVE 'Y' TO TRSM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3250-LIMIT-DISTRIB.
           IF TRST-DISTRIB-AMT < 50.00
               MOVE 50.00 TO TRST-DISTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-DISTRIB-AMT > 50000.00
               MOVE 50000.00 TO TRST-DISTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAX SCHED TIER FOR THE PRINCIPAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3260-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-SCHED-RATE(1) TO TRSM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TRSM-PRINCIPAL-YTD NOT < WS-TAX-SCHED-LOW(WS-TX)
                   MOVE WS-TAX-SCHED-RATE(WS-TX) TO TRSM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3270-ACCRUE-TAX.
           IF TRSM-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TAX-YTD * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRST-INCOME-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * H -> M WHEN POST DATE IS PAST
      * M -> T WHEN POST DATE IS PAST
      * T -> H WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3280-SET-REASON.
           EVALUATE TRSM-REASON-CD
               WHEN 'H'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3290-COUNT-RETRY.
           IF TRSM-RETRY-CNT < 9999999
               ADD 1 TO TRSM-RETRY-CNT
           END-IF
           IF TRSM-RETRY-CNT NOT < 5
               MOVE 'Y' TO TRSM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E138 REJECT CD REQUIRED
      * E175 MARKET AMT NEGATIVE
      * E456 MARKET AMT OVER 250000
      * E442 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3300-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-REJECT-CD = SPACES
                   MOVE 'E138' TO WS-REJECT-CODE
               WHEN TRSM-MARKET-AMT < 0
                   MOVE 'E175' TO WS-REJECT-CODE
               WHEN TRSM-MARKET-AMT > 250000.00
                   MOVE 'E456' TO WS-REJECT-CODE
               WHEN TRSM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E442' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN EXPIRY DATE IS PAST
      * MNG -> VFN WHEN EXPIRY DATE IS PAST
      * VFN -> XPL WHEN EXPIRY DATE IS PAST
      * XPL -> FLJ WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3310-SET-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3320-AGE-EXPIRY.
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
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * H -> M WHEN EXPIRY DATE IS PAST
      * M -> T WHEN EXPIRY DATE IS PAST
      * T -> H WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3330-SET-REASON.
           EVALUATE TRSM-REASON-CD
               WHEN 'H'
                   IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT TAX AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3340-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = TRSM-TAX-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRSM-TAX-AMT - (WS-WORK-AMT * 4)
           COMPUTE TRSM-MARKET-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3350-ACCRUE-DISTRIB.
           IF TRSM-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-DISTRIB-AMT * TRSM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRSM-TRUSTEE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3360-LIMIT-PRINCIPAL.
           IF TRSM-PRINCIPAL-AMT < 0.00
               MOVE 0.00 TO TRSM-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-PRINCIPAL-AMT > 1000.00
               MOVE 1000.00 TO TRSM-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAX AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3370-FLAG-TAX.
           IF TRSM-TAX-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB YTD FEE ON THE INCOME YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3380-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRSM-DISTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   COMPUTE TRSM-DISTRIB-YTD ROUNDED = TRSM-INCOME-YTD *
                       TRSM-MARGIN / 200
               WHEN TRST-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO TRSM-DISTRIB-YTD
               WHEN TRST-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO TRSM-DISTRIB-YTD
               WHEN OTHER
                   COMPUTE TRSM-DISTRIB-YTD ROUNDED = TRSM-INCOME-YTD *
                       TRSM-MARGIN / 100
           END-EVALUATE
           IF TRSM-DISTRIB-YTD > 0 AND TRSM-DISTRIB-YTD < 2.00
               MOVE 2.00 TO TRSM-DISTRIB-YTD
           END-IF
           IF TRSM-DISTRIB-YTD > 250.00
               MOVE 250.00 TO TRSM-DISTRIB-YTD
           END-IF
           ADD TRSM-DISTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRUSTEE FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3390-FLAG-TRUSTEE-FEE.
           IF TRST-TRUSTEE-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRST-TRUSTEE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E362 REF NO REQUIRED
      * E566 MARKET AMT NEGATIVE
      * E939 MARKET AMT OVER 5000
      * E640 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3400-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-REF-NO = SPACES
                   MOVE 'E362' TO WS-REJECT-CODE
               WHEN TRSM-MARKET-AMT < 0
                   MOVE 'E566' TO WS-REJECT-CODE
               WHEN TRSM-MARKET-AMT > 5000.00
                   MOVE 'E939' TO WS-REJECT-CODE
               WHEN TRSM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E640' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3410-LIMIT-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT < 100.00
               MOVE 100.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-PRINCIPAL-AMT > 10000.00
               MOVE 10000.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3420-ACCRUE-DISTRIB.
           IF TRST-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRST-DISTRIB-AMT * TRSM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRSM-DISTRIB-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3430-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE TRSM-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAX AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3440-FLAG-TAX.
           IF TRSM-TAX-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INCOME YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TRUSTEE FEE AMT.
      *----------------------------------------------------------------*
       3450-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRST-TRUSTEE-FEE-AMT ROUNDED =
                   TRSM-INCOME-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRST-TRUSTEE-FEE-AMT
           END-IF
           ADD TRST-TRUSTEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3460-FLAG-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRST-PRINCIPAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE DISTRIB AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3470-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TRST-DISTRIB-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRST-TRAN-CD = 'IN'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN TRST-TRAN-CD = 'DS'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TRST-DISTRIB-AMT *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3480-AGE-POST.
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
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3490-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EXPIRY DATE IS PAST
      * BAT -> MAL WHEN EXPIRY DATE IS PAST
      * MAL -> TEL WHEN EXPIRY DATE IS PAST
      * TEL -> ONL WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3500-SET-CHANNEL.
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
      * PRINCIPAL AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3510-LIMIT-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT < 100.00
               MOVE 100.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-PRINCIPAL-AMT > 1000.00
               MOVE 1000.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE TAX AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3520-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TRSM-TAX-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRSM-REASON-CD = 'H'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN TRSM-REASON-CD = 'M'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TRSM-TAX-AMT *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3530-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3540-DESCRIBE-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'D'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'N'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INCOME YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3550-LIMIT-INCOME.
           IF TRSM-INCOME-YTD < 50.00
               MOVE 50.00 TO TRSM-INCOME-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-INCOME-YTD > 5000.00
               MOVE 5000.00 TO TRSM-INCOME-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3560-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-CYCLE-CD
                   ADD TRSM-DISTRIB-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-DISTRIB-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE PRINCIPAL YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3570-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TRSM-PRINCIPAL-YTD *
                       TRSM-MARGIN / 200
               WHEN TRSM-DIST-FREQ-CD = 'E'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN TRSM-DIST-FREQ-CD = 'K'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TRSM-PRINCIPAL-YTD *
                       TRSM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       3580-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRST-PRINCIPAL-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRST-PRINCIPAL-AMT
           END-IF
           ADD TRST-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * MARKET AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3590-LIMIT-MARKET.
           IF TRSM-MARKET-AMT < 0.00
               MOVE 0.00 TO TRSM-MARKET-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-MARKET-AMT > 500.00
               MOVE 500.00 TO TRSM-MARKET-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3600-AGE-LAST-MAINT.
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
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3610-DESCRIBE-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3620-COUNT-NOTICE.
           IF TRSM-NOTICE-CNT < 9999999
               ADD 1 TO TRSM-NOTICE-CNT
           END-IF
           IF TRSM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO TRSM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3630-COUNT-NOTICE.
           IF TRSM-NOTICE-CNT < 9999999
               ADD 1 TO TRSM-NOTICE-CNT
           END-IF
           IF TRSM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO TRST-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAX SCHED TIER FOR THE DISTRIB YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3640-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-SCHED-RATE(1) TO TRSM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TRSM-DISTRIB-YTD NOT < WS-TAX-SCHED-LOW(WS-TX)
                   MOVE WS-TAX-SCHED-RATE(WS-TX) TO TRSM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3650-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF TRST-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRST-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TRSM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INCOME AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3660-FLAG-INCOME.
           IF TRSM-INCOME-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-INCOME-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MARKET YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISTRIB AMT.
      *----------------------------------------------------------------*
       3670-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRST-DISTRIB-AMT ROUNDED =
                   TRSM-MARKET-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRST-DISTRIB-AMT
           END-IF
           ADD TRST-DISTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRUSTEE FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3680-FLAG-TRUSTEE-FEE.
           IF TRST-TRUSTEE-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRST-TRUSTEE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3690-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3700-LIMIT-PRINCIPAL.
           IF TRSM-PRINCIPAL-YTD < 10.00
               MOVE 10.00 TO TRSM-PRINCIPAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-PRINCIPAL-YTD > 50000.00
               MOVE 50000.00 TO TRSM-PRINCIPAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3710-DESCRIBE-CHANNEL.
           EVALUATE TRST-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3720-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-TRAN-CD
                   ADD TRST-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRST-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3730-FLAG-DISTRIB.
           IF TRSM-DISTRIB-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRSM-DISTRIB-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * INCOME YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3740-LIMIT-INCOME.
           IF TRSM-INCOME-YTD < 10.00
               MOVE 10.00 TO TRSM-INCOME-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-INCOME-YTD > 5000.00
               MOVE 5000.00 TO TRSM-INCOME-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TRUSTEE FEE YTD BY TRUST TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3750-ACCUM-TRUST-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-TRUST-TYPE-CD
                   ADD TRSM-TRUSTEE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-TRUST-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-TRUSTEE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INCOME AMT.
      *----------------------------------------------------------------*
       3760-PRORATE-INCOME.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRST-INCOME-AMT ROUNDED =
                   TRSM-TAX-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRST-INCOME-AMT
           END-IF
           ADD TRST-INCOME-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT INCOME YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3770-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = TRSM-INCOME-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TRSM-INCOME-YTD - (WS-WORK-AMT * 12)
           COMPUTE TRSM-MARKET-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3780-ACCRUE-DISTRIB.
           IF TRSM-DISTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-DISTRIB-YTD * TRSM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3790-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TRST-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRUSTEE FEE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3800-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TRUSTEE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED = WS-WORK-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRSM-CYCLE-CD = 'EN'
                   MOVE 7.00 TO TRSM-TRUSTEE-FEE-YTD
               WHEN TRSM-CYCLE-CD = 'FY'
                   MOVE 12.00 TO TRSM-TRUSTEE-FEE-YTD
               WHEN OTHER
                   COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED = WS-WORK-AMT *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-TRUSTEE-FEE-YTD > 0 AND TRSM-TRUSTEE-FEE-YTD < 10.00
               MOVE 10.00 TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           IF TRSM-TRUSTEE-FEE-YTD > 150.00
               MOVE 150.00 TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           ADD TRSM-TRUSTEE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE TAX SCHED TIER FOR THE TRUSTEE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3810-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-SCHED-RATE(1) TO TRSM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TRSM-TRUSTEE-FEE-AMT NOT < WS-TAX-SCHED-LOW(WS-TX)
                   MOVE WS-TAX-SCHED-RATE(WS-TX) TO TRSM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3820-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-TRAN-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3830-ACCRUE-DISTRIB.
           IF TRST-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRST-DISTRIB-AMT * TRSM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TRSM-INCOME-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3840-LIMIT-PRINCIPAL.
           IF TRSM-PRINCIPAL-YTD < 25.00
               MOVE 25.00 TO TRSM-PRINCIPAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-PRINCIPAL-YTD > 50000.00
               MOVE 50000.00 TO TRSM-PRINCIPAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3850-FLAG-DISTRIB.
           IF TRSM-DISTRIB-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRSM-DISTRIB-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E936 CONTACT REQUIRED
      * E591 DISTRIB AMT NEGATIVE
      * E322 DISTRIB AMT OVER 10000
      * E377 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3860-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-CONTACT = SPACES
                   MOVE 'E936' TO WS-REJECT-CODE
               WHEN TRST-DISTRIB-AMT < 0
                   MOVE 'E591' TO WS-REJECT-CODE
               WHEN TRST-DISTRIB-AMT > 10000.00
                   MOVE 'E322' TO WS-REJECT-CODE
               WHEN TRSM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E377' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E907 OPERATOR REQUIRED
      * E734 PRINCIPAL YTD NEGATIVE
      * E558 PRINCIPAL YTD OVER 50000
      * E537 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3870-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-OPERATOR = SPACES
                   MOVE 'E907' TO WS-REJECT-CODE
               WHEN TRSM-PRINCIPAL-YTD < 0
                   MOVE 'E734' TO WS-REJECT-CODE
               WHEN TRSM-PRINCIPAL-YTD > 50000.00
                   MOVE 'E558' TO WS-REJECT-CODE
               WHEN TRSM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E537' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB BAND TIER FOR THE PRINCIPAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3880-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-BAND-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TRSM-PRINCIPAL-YTD NOT < WS-DISTRIB-BAND-LOW(WS-TX)
                   MOVE WS-DISTRIB-BAND-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3890-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TRSM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARKET AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3900-LIMIT-MARKET.
           IF TRSM-MARKET-AMT < 10.00
               MOVE 10.00 TO TRSM-MARKET-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-MARKET-AMT > 500.00
               MOVE 500.00 TO TRSM-MARKET-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE INCOME AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3910-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TRSM-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-STOP-FLAG = 'Y'
                   COMPUTE TRSM-PRINCIPAL-AMT ROUNDED = TRST-INCOME-AMT
                       * TRSM-RATE / 200
               WHEN TRSM-STATUS-CD = 'EH'
                   MOVE 7.00 TO TRSM-PRINCIPAL-AMT
               WHEN TRSM-STATUS-CD = 'GG'
                   MOVE 15.00 TO TRSM-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE TRSM-PRINCIPAL-AMT ROUNDED = TRST-INCOME-AMT
                       * TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-PRINCIPAL-AMT > 0 AND TRSM-PRINCIPAL-AMT < 2.00
               MOVE 2.00 TO TRSM-PRINCIPAL-AMT
           END-IF
           IF TRSM-PRINCIPAL-AMT > 250.00
               MOVE 250.00 TO TRSM-PRINCIPAL-AMT
           END-IF
           ADD TRSM-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3920-COUNT-PAYMENT.
           IF TRSM-PAYMENT-CNT < 9999999
               ADD 1 TO TRSM-PAYMENT-CNT
           END-IF
           IF TRSM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN OPEN DATE IS PAST
      * MNG -> VFN WHEN OPEN DATE IS PAST
      * VFN -> XPL WHEN OPEN DATE IS PAST
      * XPL -> FLJ WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3930-SET-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE TRUSTEE FEE AMT BY REGION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3940-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-REGION-CD
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-TRUSTEE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TRUSTEE FEE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3950-ACCRUE-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TRUSTEE-FEE-AMT * TRSM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRSM-MARKET-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3960-AGE-LAST-PAY.
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
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3970-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = TRSM-PRINCIPAL-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TRSM-PRINCIPAL-YTD - (WS-WORK-AMT *
               12)
           COMPUTE TRSM-TAX-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAX SCHED TIER FOR THE MARKET AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3980-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-SCHED-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TRSM-MARKET-AMT NOT < WS-TAX-SCHED-LOW(WS-TX)
                   MOVE WS-TAX-SCHED-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE DISTRIB YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3990-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   COMPUTE TRSM-PRINCIPAL-AMT ROUNDED = TRSM-DISTRIB-YTD
                       * TRSM-MARGIN / 200
               WHEN TRST-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO TRSM-PRINCIPAL-AMT
               WHEN TRST-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO TRSM-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE TRSM-PRINCIPAL-AMT ROUNDED = TRSM-DISTRIB-YTD
                       * TRSM-MARGIN / 100
           END-EVALUATE
           IF TRSM-PRINCIPAL-AMT > 0 AND TRSM-PRINCIPAL-AMT < 10.00
               MOVE 10.00 TO TRSM-PRINCIPAL-AMT
           END-IF
           IF TRSM-PRINCIPAL-AMT > 75.00
               MOVE 75.00 TO TRSM-PRINCIPAL-AMT
           END-IF
           ADD TRSM-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TRUSTEE FEE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4000-ACCRUE-TRUSTEE-FEE.
           IF TRST-TRUSTEE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRST-TRUSTEE-FEE-AMT * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4010-COUNT-CYCLE.
           IF TRSM-CYCLE-CNT < 9999999
               ADD 1 TO TRSM-CYCLE-CNT
           END-IF
           IF TRSM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO TRST-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4020-AGE-OPEN.
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
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4030-AGE-NOTICE.
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
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4040-COUNT-NOTICE.
           IF TRSM-NOTICE-CNT < 9999999
               ADD 1 TO TRSM-NOTICE-CNT
           END-IF
           IF TRSM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO TRSM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TAX AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4050-LIMIT-TAX.
           IF TRSM-TAX-AMT < 0.00
               MOVE 0.00 TO TRSM-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-TAX-AMT > 10000.00
               MOVE 10000.00 TO TRSM-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4060-COUNT-SEQ.
           IF TRST-SEQ < 9999999
               ADD 1 TO TRST-SEQ
           END-IF
           IF TRST-SEQ NOT < 5
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE DISTRIB AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4070-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TRST-DISTRIB-AMT *
                       TRSM-RATE / 200
               WHEN TRSM-TRUST-TYPE-CD = 'FLJ'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN TRSM-TRUST-TYPE-CD = 'MNG'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TRST-DISTRIB-AMT *
                       TRSM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4080-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-REASON-CD
                   ADD TRSM-DISTRIB-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-DISTRIB-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MARKET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INCOME YTD.
      *----------------------------------------------------------------*
       4090-PRORATE-INCOME.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-INCOME-YTD ROUNDED =
                   TRSM-MARKET-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-INCOME-YTD
           END-IF
           ADD TRSM-INCOME-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE TAX YTD FEE ON THE TRUSTEE FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4100-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TAX-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-STOP-FLAG = 'Y'
                   COMPUTE TRSM-TAX-YTD ROUNDED = TRST-TRUSTEE-FEE-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRSM-TRUST-TYPE-CD = 'FLJ'
                   MOVE 5.00 TO TRSM-TAX-YTD
               WHEN TRSM-TRUST-TYPE-CD = 'MNG'
                   MOVE 15.00 TO TRSM-TAX-YTD
               WHEN OTHER
                   COMPUTE TRSM-TAX-YTD ROUNDED = TRST-TRUSTEE-FEE-AMT *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-TAX-YTD > 0 AND TRSM-TAX-YTD < 1.00
               MOVE 1.00 TO TRSM-TAX-YTD
           END-IF
           IF TRSM-TAX-YTD > 250.00
               MOVE 250.00 TO TRSM-TAX-YTD
           END-IF
           ADD TRSM-TAX-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE TAX FEE ON THE INCOME AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4110-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRSM-TAX-AMT ROUNDED = TRST-INCOME-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRSM-DIST-FREQ-CD = 'E'
                   MOVE 15.00 TO TRSM-TAX-AMT
               WHEN TRSM-DIST-FREQ-CD = 'K'
                   MOVE 3.00 TO TRSM-TAX-AMT
               WHEN OTHER
                   COMPUTE TRSM-TAX-AMT ROUNDED = TRST-INCOME-AMT *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-TAX-AMT > 0 AND TRSM-TAX-AMT < 2.00
               MOVE 2.00 TO TRSM-TAX-AMT
           END-IF
           IF TRSM-TAX-AMT > 100.00
               MOVE 100.00 TO TRSM-TAX-AMT
           END-IF
           ADD TRSM-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * H -> M WHEN NOTICE DATE IS PAST
      * M -> T WHEN NOTICE DATE IS PAST
      * T -> H WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4120-SET-REASON.
           EVALUATE TRSM-REASON-CD
               WHEN 'H'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E861 MEMO REQUIRED
      * E427 TAX AMT NEGATIVE
      * E133 TAX AMT OVER 99999
      * E141 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4130-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-MEMO = SPACES
                   MOVE 'E861' TO WS-REJECT-CODE
               WHEN TRSM-TAX-AMT < 0
                   MOVE 'E427' TO WS-REJECT-CODE
               WHEN TRSM-TAX-AMT > 99999.00
                   MOVE 'E133' TO WS-REJECT-CODE
               WHEN TRSM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E141' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4140-DESCRIBE-STATUS.
           EVALUATE TRSM-STATUS-CD
               WHEN 'EH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4150-DESCRIBE-TRAN.
           EVALUATE TRST-TRAN-CD
               WHEN 'IN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4160-ACCRUE-DISTRIB.
           IF TRSM-DISTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-DISTRIB-YTD * TRSM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRSM-TAX-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4170-COUNT-NOTICE.
           IF TRSM-NOTICE-CNT < 9999999
               ADD 1 TO TRSM-NOTICE-CNT
           END-IF
           IF TRSM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO TRSM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4180-AGE-LAST-MAINT.
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
      * PRINCIPAL AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4190-LIMIT-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT < 0.00
               MOVE 0.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-PRINCIPAL-AMT > 500.00
               MOVE 500.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4200-DESCRIBE-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'D'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'N'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE TRUSTEE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TRUSTEE FEE AMT.
      *----------------------------------------------------------------*
       4210-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRST-TRUSTEE-FEE-AMT ROUNDED =
                   TRSM-TRUSTEE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRST-TRUSTEE-FEE-AMT
           END-IF
           ADD TRST-TRUSTEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EN -> FY WHEN EFFECTIVE DATE IS PAST
      * FY -> HP WHEN EFFECTIVE DATE IS PAST
      * HP -> JB WHEN EFFECTIVE DATE IS PAST
      * JB -> KZ WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4220-SET-CYCLE.
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
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4230-AGE-POST.
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
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> D WHEN LAST PAY DATE IS PAST
      * D -> N WHEN LAST PAY DATE IS PAST
      * N -> T WHEN LAST PAY DATE IS PAST
      * T -> A WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4240-SET-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'A' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4250-AGE-NOTICE.
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
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE MARKET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TRUSTEE FEE AMT.
      *----------------------------------------------------------------*
       4260-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TRUSTEE-FEE-AMT ROUNDED =
                   TRSM-MARKET-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-TRUSTEE-FEE-AMT
           END-IF
           ADD TRSM-TRUSTEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4270-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TRUSTEE FEE AMT.
      *----------------------------------------------------------------*
       4280-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRST-TRUSTEE-FEE-AMT ROUNDED =
                   TRSM-TAX-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRST-TRUSTEE-FEE-AMT
           END-IF
           ADD TRST-TRUSTEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4290-ACCRUE-PRINCIPAL.
           IF TRSM-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-PRINCIPAL-AMT * TRSM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TRSM-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARKET AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4300-FLAG-MARKET.
           IF TRSM-MARKET-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRSM-MARKET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4310-FLAG-PRINCIPAL.
           IF TRSM-PRINCIPAL-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-PRINCIPAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4320-AGE-LAST-STMT.
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
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE TRUSTEE FEE YTD FEE ON THE TRUSTEE FEE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4330-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TRUSTEE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED =
                       TRSM-TRUSTEE-FEE-AMT * TRSM-RATE / 200
               WHEN TRST-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO TRSM-TRUSTEE-FEE-YTD
               WHEN TRST-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO TRSM-TRUSTEE-FEE-YTD
               WHEN OTHER
                   COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED =
                       TRSM-TRUSTEE-FEE-AMT * TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-TRUSTEE-FEE-YTD > 0 AND TRSM-TRUSTEE-FEE-YTD < 1.00
               MOVE 1.00 TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           IF TRSM-TRUSTEE-FEE-YTD > 250.00
               MOVE 250.00 TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           ADD TRSM-TRUSTEE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4340-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TRUSTEE FEE YTD.
      *----------------------------------------------------------------*
       4350-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED =
                   TRST-PRINCIPAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           ADD TRSM-TRUSTEE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INCOME YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4360-ACCRUE-INCOME.
           IF TRSM-INCOME-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-INCOME-YTD * TRSM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRSM-DISTRIB-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB YTD FEE ON THE MARKET AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4370-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-DISTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRSM-DISTRIB-YTD ROUNDED = TRSM-MARKET-AMT *
                       TRSM-RATE / 200
               WHEN TRSM-CYCLE-CD = 'EN'
                   MOVE 3.00 TO TRSM-DISTRIB-YTD
               WHEN TRSM-CYCLE-CD = 'FY'
                   MOVE 5.00 TO TRSM-DISTRIB-YTD
               WHEN OTHER
                   COMPUTE TRSM-DISTRIB-YTD ROUNDED = TRSM-MARKET-AMT *
                       TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-DISTRIB-YTD > 0 AND TRSM-DISTRIB-YTD < 2.00
               MOVE 2.00 TO TRSM-DISTRIB-YTD
           END-IF
           IF TRSM-DISTRIB-YTD > 250.00
               MOVE 250.00 TO TRSM-DISTRIB-YTD
           END-IF
           ADD TRSM-DISTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT TAX YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4380-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = TRSM-TAX-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TRSM-TAX-YTD - (WS-WORK-AMT * 12)
           COMPUTE TRST-DISTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MARKET AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4390-SPLIT-MARKET.
           COMPUTE WS-WORK-AMT = TRSM-MARKET-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRSM-MARKET-AMT - (WS-WORK-AMT * 4)
           COMPUTE TRSM-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E737 DESC REQUIRED
      * E604 PRINCIPAL AMT NEGATIVE
      * E436 PRINCIPAL AMT OVER 250000
      * E003 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4400-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-DESC = SPACES
                   MOVE 'E737' TO WS-REJECT-CODE
               WHEN TRSM-PRINCIPAL-AMT < 0
                   MOVE 'E604' TO WS-REJECT-CODE
               WHEN TRSM-PRINCIPAL-AMT > 250000.00
                   MOVE 'E436' TO WS-REJECT-CODE
               WHEN TRST-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E003' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4410-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4420-DESCRIBE-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'D'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'N'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4430-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TRSM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INCOME AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4440-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-STATUS-CD
                   ADD TRST-INCOME-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRST-INCOME-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TRUSTEE FEE AMT BY DIST FREQ CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4450-ACCUM-DIST-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-DIST-FREQ-CD
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-DIST-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRST-TRUSTEE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> D WHEN EFFECTIVE DATE IS PAST
      * D -> N WHEN EFFECTIVE DATE IS PAST
      * N -> T WHEN EFFECTIVE DATE IS PAST
      * T -> A WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4460-SET-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'A' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4470-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DIST FREQ CD:
      * E -> K WHEN EFF DATE IS PAST
      * K -> P WHEN EFF DATE IS PAST
      * P -> W WHEN EFF DATE IS PAST
      * W -> Y WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4480-SET-DIST-FREQ.
           EVALUATE TRSM-DIST-FREQ-CD
               WHEN 'E'
                   IF TRST-EFF-DATE > 0 AND TRST-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF TRST-EFF-DATE > 0 AND TRST-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TRST-EFF-DATE > 0 AND TRST-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF TRST-EFF-DATE > 0 AND TRST-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4490-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = TRSM-DISTRIB-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TRSM-DISTRIB-AMT - (WS-WORK-AMT * 2)
           COMPUTE TRST-INCOME-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> D WHEN MATURITY DATE IS PAST
      * D -> N WHEN MATURITY DATE IS PAST
      * N -> T WHEN MATURITY DATE IS PAST
      * T -> A WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4500-SET-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'A' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INCOME YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4510-ACCRUE-INCOME.
           IF TRSM-INCOME-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-INCOME-YTD * TRSM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TRSM-MARKET-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4520-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4530-CHECK-EFF.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MARKET YTD.
      *----------------------------------------------------------------*
       4540-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-MARKET-YTD ROUNDED =
                   TRSM-PRINCIPAL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-MARKET-YTD
           END-IF
           ADD TRSM-MARKET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE INCOME AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4550-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TRST-INCOME-AMT *
                       TRSM-MARGIN / 200
               WHEN TRST-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN TRST-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TRST-INCOME-AMT *
                       TRSM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * MARKET AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4560-LIMIT-MARKET.
           IF TRSM-MARKET-AMT < 50.00
               MOVE 50.00 TO TRSM-MARKET-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-MARKET-AMT > 10000.00
               MOVE 10000.00 TO TRSM-MARKET-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INCOME YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4570-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-REASON-CD
                   ADD TRSM-INCOME-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-INCOME-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MARKET YTD.
      *----------------------------------------------------------------*
       4580-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-MARKET-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-MARKET-YTD
           END-IF
           ADD TRSM-MARKET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4590-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TRST-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE TRUSTEE FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4600-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TRSM-TRUSTEE-FEE-AMT *
                       TRSM-RATE / 200
               WHEN TRST-TRAN-CD = 'IN'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN TRST-TRAN-CD = 'DS'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TRSM-TRUSTEE-FEE-AMT *
                       TRSM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB FEE ON THE DISTRIB AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4610-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRST-DISTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   COMPUTE TRST-DISTRIB-AMT ROUNDED = TRSM-DISTRIB-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRSM-STATUS-CD = 'EH'
                   MOVE 12.00 TO TRST-DISTRIB-AMT
               WHEN TRSM-STATUS-CD = 'GG'
                   MOVE 15.00 TO TRST-DISTRIB-AMT
               WHEN OTHER
                   COMPUTE TRST-DISTRIB-AMT ROUNDED = TRSM-DISTRIB-AMT *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRST-DISTRIB-AMT > 0 AND TRST-DISTRIB-AMT < 5.00
               MOVE 5.00 TO TRST-DISTRIB-AMT
           END-IF
           IF TRST-DISTRIB-AMT > 25.00
               MOVE 25.00 TO TRST-DISTRIB-AMT
           END-IF
           ADD TRST-DISTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * INCOME AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4620-FLAG-INCOME.
           IF TRSM-INCOME-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRSM-INCOME-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4630-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TRSM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4640-ACCRUE-DISTRIB.
           IF TRSM-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-DISTRIB-AMT * TRSM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRSM-INCOME-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAX YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4650-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = TRSM-TAX-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TRSM-TAX-YTD - (WS-WORK-AMT * 12)
           COMPUTE TRSM-TRUSTEE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4660-ACCRUE-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRST-PRINCIPAL-AMT * TRSM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TRSM-DISTRIB-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MARKET YTD.
      *----------------------------------------------------------------*
       4670-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-MARKET-YTD ROUNDED =
                   TRSM-TAX-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-MARKET-YTD
           END-IF
           ADD TRSM-MARKET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4680-DESCRIBE-STATUS.
           EVALUATE TRSM-STATUS-CD
               WHEN 'EH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4690-COUNT-SEQ.
           IF TRST-SEQ < 9999999
               ADD 1 TO TRST-SEQ
           END-IF
           IF TRST-SEQ NOT < 5
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * MARKET YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4700-FLAG-MARKET.
           IF TRSM-MARKET-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-MARKET-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INCOME YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4710-ACCRUE-INCOME.
           IF TRSM-INCOME-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-INCOME-YTD * TRSM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TRSM-MARKET-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4720-LIMIT-DISTRIB.
           IF TRSM-DISTRIB-AMT < 10.00
               MOVE 10.00 TO TRSM-DISTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-DISTRIB-AMT > 1000.00
               MOVE 1000.00 TO TRSM-DISTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4730-AGE-NOTICE.
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
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-MARKET-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY DIST FREQ CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4740-ACCUM-DIST-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-DIST-FREQ-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-DIST-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4750-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TRSM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4760-AGE-LAST-STMT.
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
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE TRUSTEE FEE YTD FEE ON THE DISTRIB AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4770-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TRUSTEE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED =
                       TRST-DISTRIB-AMT * TRSM-MARGIN / 200
               WHEN TRST-TRAN-CD = 'IN'
                   MOVE 3.00 TO TRSM-TRUSTEE-FEE-YTD
               WHEN TRST-TRAN-CD = 'DS'
                   MOVE 10.00 TO TRSM-TRUSTEE-FEE-YTD
               WHEN OTHER
                   COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED =
                       TRST-DISTRIB-AMT * TRSM-MARGIN / 100
           END-EVALUATE
           IF TRSM-TRUSTEE-FEE-YTD > 0 AND TRSM-TRUSTEE-FEE-YTD < 5.00
               MOVE 5.00 TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           IF TRSM-TRUSTEE-FEE-YTD > 25.00
               MOVE 25.00 TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           ADD TRSM-TRUSTEE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE TRUSTEE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINCIPAL YTD.
      *----------------------------------------------------------------*
       4780-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-PRINCIPAL-YTD ROUNDED =
                   TRSM-TRUSTEE-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-PRINCIPAL-YTD
           END-IF
           ADD TRSM-PRINCIPAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4790-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-REGION-CD
                   ADD TRSM-TAX-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-TAX-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4800-LIMIT-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT < 100.00
               MOVE 100.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-PRINCIPAL-AMT > 10000.00
               MOVE 10000.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4810-COUNT-RETRY.
           IF TRSM-RETRY-CNT < 9999999
               ADD 1 TO TRSM-RETRY-CNT
           END-IF
           IF TRSM-RETRY-CNT NOT < 6
               MOVE 'Y' TO TRSM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4820-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAX FEE ON THE PRINCIPAL YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4830-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   COMPUTE TRSM-TAX-AMT ROUNDED = TRSM-PRINCIPAL-YTD *
                       TRSM-DISCOUNT / 200
               WHEN TRST-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO TRSM-TAX-AMT
               WHEN TRST-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO TRSM-TAX-AMT
               WHEN OTHER
                   COMPUTE TRSM-TAX-AMT ROUNDED = TRSM-PRINCIPAL-YTD *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-TAX-AMT > 0 AND TRSM-TAX-AMT < 5.00
               MOVE 5.00 TO TRSM-TAX-AMT
           END-IF
           IF TRSM-TAX-AMT > 35.00
               MOVE 35.00 TO TRSM-TAX-AMT
           END-IF
           ADD TRSM-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PASS THE TRS FIELDS TO TRPEN AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-TRPEN.
           INITIALIZE TRPEN-PARMS
           MOVE TRSM-DISTRIB-YTD
             TO TRPEN-DISTRIB-YTD
           MOVE TRSM-TRUSTEE-FEE-AMT
             TO TRPEN-TRUSTEE-FEE-AMT
           MOVE TRSM-INCOME-AMT
             TO TRPEN-INCOME-AMT
           MOVE TRSM-INCOME-YTD
             TO TRPEN-INCOME-YTD
           MOVE TRSM-TAX-AMT
             TO TRPEN-TAX-AMT
           MOVE TRSM-MARKET-YTD
             TO TRPEN-MARKET-YTD
           MOVE TRSM-CLOSE-DATE
             TO TRPEN-CLOSE-DATE
           MOVE TRSM-NOTICE-DATE
             TO TRPEN-NOTICE-DATE
           MOVE TRSM-LAST-STMT-DATE
             TO TRPEN-LAST-STMT-DATE
           MOVE TRSM-OPEN-DATE
             TO TRPEN-OPEN-DATE
           MOVE TRSM-REGION-CD
             TO TRPEN-REGION-CD
           MOVE TRSM-CYCLE-CD
             TO TRPEN-CYCLE-CD
           MOVE TRSM-STATUS-CD
             TO TRPEN-STATUS-CD
           MOVE TRSM-TRUST-TYPE-CD
             TO TRPEN-TRUST-TYPE-CD
           MOVE TRSM-SUSPEND-FLAG
             TO TRPEN-SUSPEND-FLAG
           MOVE TRSM-EXEMPT-FLAG
             TO TRPEN-EXEMPT-FLAG
           MOVE TRSM-PREFERRED-FLAG
             TO TRPEN-PREFERRED-FLAG
           MOVE TRSM-RETRY-CNT
             TO TRPEN-RETRY-CNT
           MOVE TRSM-PAYMENT-CNT
             TO TRPEN-PAYMENT-CNT
           MOVE TRSM-DISCOUNT
             TO TRPEN-DISCOUNT
           MOVE TRSM-RATE
             TO TRPEN-RATE
           MOVE TRSM-ADDR
             TO TRPEN-ADDR
           MOVE TRSM-REF-NO
             TO TRPEN-REF-NO
           CALL 'TRPEN' USING TRPEN-PARMS
           EVALUATE TRPEN-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TRPEN-DISTRIB-YTD
                     TO TRSM-DISTRIB-YTD
                   MOVE TRPEN-INCOME-AMT
                     TO TRSM-INCOME-AMT
                   MOVE TRPEN-SUSPEND-FLAG
                     TO TRSM-SUSPEND-FLAG
                   MOVE TRPEN-EXEMPT-FLAG
                     TO TRSM-EXEMPT-FLAG
                   MOVE TRPEN-PREFERRED-FLAG
                     TO TRSM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-TRPEN' TO ABND-PARAGRAPH
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
           DISPLAY 'TRSACCR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TRSACCR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
