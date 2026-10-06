      *================================================================*
      * PROGRAM  : TRSANAL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PERSONAL TRUST - ANALYSIS. READS THE TRS            *
      *            TRANSACTIONS, UPDATES THE TRS MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. PERSONAL TRUST ACCOUNTING,      *
      *            INCOME DISTRIBUTION AND TRUSTEE FEES                *
      * INPUT    : TRSMST (KSDS), TRSTRN, BANKCTL                      *
      * OUTPUT   : TRSOUT, TRSANRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TRLIM, TRPEN, TRVAL     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-08-16 TWH  ORIGINAL                                       *
      * 1992-04-04 BHS  INCOME NOTICE TEXT PER LEGAL INC1936           *
      * 1993-01-16 MKL  MARKET CALCULATION CORRECTED PRB2103           *
      * 1994-04-28 GAF  PRINCIPAL STATUS CODES ADDED REQ4152           *
      * 1994-09-21 RJT  DISTRIB LIMIT REVISED REQ8113                  *
      * 1995-10-01 DPS  PRINCIPAL EXCEPTIONS TO OPS QUEUE REQ9047      *
      * 2000-07-13 TWH  PERFORMANCE - TRUSTEE-FEE READ AHEAD REQ1176   *
      * 2001-11-09 PQR  TRUSTEE-FEE LIMIT REVISED CHG9335              *
      * 2003-01-24 LMB  PRINCIPAL ROUNDING - HALF UP REQ7442           *
      * 2004-11-03 SKN  MARKET CALCULATION CORRECTED REQ7230           *
      * 2005-07-01 JLM  PERFORMANCE - TRUSTEE-FEE READ AHEAD CHG7513   *
      * 2007-10-21 SKN  MARKET TABLE EXPANDED PRB4685                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRSANAL.
       AUTHOR. D P SHAW.

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
           SELECT REPORT-FILE ASSIGN TO "TRSANRPT"
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
                                           VALUE 20.000000.
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
      * WS-PRINCIPA-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINCIPA-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000466500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000556000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000423000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000196000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007750000000330500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008000000000492500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008500000000010000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018500000000697500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018600000000541000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018850000000286000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019850000000399500'.
       01  WS-PRINCIPA-TIER-TBL REDEFINES WS-PRINCIPA-TIER-VALUES.
           05  WS-PRINCIPA-TIER-ROW OCCURS 11 TIMES.
               10  WS-PRINCIPA-TIER-LOW    PIC 9(09)V99.
               10  WS-PRINCIPA-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-TAX-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TAX-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000056500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000611500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000214500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000816000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017000000000583000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018000000000316500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018100000000874000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019100000000830000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019200000000749000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021700000000220000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021800000000114500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022050000000287000'.
       01  WS-TAX-GRID-TBL REDEFINES WS-TAX-GRID-VALUES.
           05  WS-TAX-GRID-ROW OCCURS 12 TIMES.
               10  WS-TAX-GRID-LOW         PIC 9(09)V99.
               10  WS-TAX-GRID-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INCOME-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INCOME-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000247000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000775000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000773000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004500000000288500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007000000000216500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007100000000126000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007150000000030000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008150000000352000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008400000000725500'.
       01  WS-INCOME-TIER-TBL REDEFINES WS-INCOME-TIER-VALUES.
           05  WS-INCOME-TIER-ROW OCCURS 9 TIMES.
               10  WS-INCOME-TIER-LOW      PIC 9(09)V99.
               10  WS-INCOME-TIER-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INCOME-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INCOME-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000679500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000492000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000862000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000598500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000127000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009000000000063000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000017500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000361500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000603000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016050000000615000'.
       01  WS-INCOME-SCHED-TBL REDEFINES WS-INCOME-SCHED-VALUES.
           05  WS-INCOME-SCHED-ROW OCCURS 10 TIMES.
               10  WS-INCOME-SCHED-LOW     PIC 9(09)V99.
               10  WS-INCOME-SCHED-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TRSANAL  PERSONAL TRUST'.
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
       COPY TRLIMP.
       COPY TRPENP.
       COPY TRVALP.

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
               PERFORM 3000-FLAG-ITEM
               PERFORM 3010-SET-STATUS
               PERFORM 3020-PRORATE-TRUSTEE-FEE
               PERFORM 3030-PRORATE-TAX
               PERFORM 3040-SPLIT-DISTRIB
               PERFORM 3050-PRORATE-ITEM
               PERFORM 3060-SPLIT-TRUSTEE-FEE
               PERFORM 3070-LIMIT-DISTRIB
               PERFORM 3080-LIMIT-DISTRIB
               PERFORM 3090-CHECK-NOTICE
               PERFORM 3100-EDIT-CONTACT
               PERFORM 3110-FLAG-TAX
               PERFORM 3120-ASSESS-EXEMPT
               PERFORM 3130-PRORATE-TRUSTEE-FEE
               PERFORM 3140-ACCRUE-INCOME
               PERFORM 3150-ASSESS-PREFERRED
               PERFORM 3160-SPLIT-INCOME
               PERFORM 3170-ACCUM-REASON
               PERFORM 3180-FLAG-TAX
               PERFORM 3190-FLAG-PRINCIPAL
               PERFORM 3200-ACCUM-CYCLE
               PERFORM 3210-COUNT-NOTICE
               PERFORM 3220-CHECK-LAST-MAINT
               PERFORM 3230-DESCRIBE-TRUST-TYPE
               PERFORM 3240-FLAG-TAX
               PERFORM 3250-ACCRUE-DISTRIB
               PERFORM 3260-FLAG-ITEM
               PERFORM 3270-ACCUM-DIST-FREQ
               PERFORM 3280-ACCRUE-INCOME
               PERFORM 3290-LIMIT-MARKET
               PERFORM 3300-ACCUM-CYCLE
               PERFORM 3310-EDIT-REJECT
               PERFORM 3320-DESCRIBE-DIST-FREQ
               PERFORM 3330-COUNT-NOTICE
               PERFORM 3340-CHECK-OPEN
               PERFORM 3350-SET-STATUS
               PERFORM 3360-LIMIT-ITEM
               PERFORM 3370-AGE-MATURITY
               PERFORM 3380-ACCUM-REGION
               PERFORM 3390-FLAG-ITEM
               PERFORM 3400-AGE-LAST-MAINT
               PERFORM 3410-SPLIT-INCOME
               PERFORM 3420-DESCRIBE-TRUST-TYPE
               PERFORM 3430-COUNT-CYCLE
               PERFORM 3440-SELECT-DISCOUNT
               PERFORM 3450-DESCRIBE-REGION
               PERFORM 3460-DESCRIBE-REGION
               PERFORM 3470-EDIT-MEMO
               PERFORM 3480-DESCRIBE-TRUST-TYPE
               PERFORM 3490-SELECT-MARGIN
               PERFORM 3500-COUNT-SEQ
               PERFORM 3510-COUNT-RETRY
               PERFORM 3520-SPLIT-TRUSTEE-FEE
               PERFORM 3530-CHECK-MATURITY
               PERFORM 3540-ACCRUE-DISTRIB
               PERFORM 3550-FLAG-TRUSTEE-FEE
               PERFORM 3560-ACCUM-DIST-FREQ
               PERFORM 3570-ASSESS-REVIEW
               PERFORM 3580-AGE-LAST-MAINT
               PERFORM 3590-SET-DIST-FREQ
               PERFORM 3600-ACCUM-CHANNEL
               PERFORM 3610-ASSESS-PREFERRED
               PERFORM 3620-CHECK-NOTICE
               PERFORM 3630-PRORATE-PRINCIPAL
               PERFORM 3640-ACCUM-DIST-FREQ
               PERFORM 3650-SET-STATUS
               PERFORM 3660-LIMIT-TRUSTEE-FEE
               PERFORM 3670-SET-REGION
               PERFORM 3680-ASSESS-REVIEW
               PERFORM 3690-AGE-POST
               PERFORM 3700-COUNT-RETRY
               PERFORM 3710-ACCUM-TRAN
               PERFORM 3720-SET-CYCLE
               PERFORM 3730-AGE-NOTICE
               PERFORM 3740-CHECK-POST
               PERFORM 3750-SPLIT-TRUSTEE-FEE
               PERFORM 3760-ASSESS-REVIEW
               PERFORM 3770-PRORATE-TAX
               PERFORM 3780-ACCUM-TRUST-TYPE
               PERFORM 3790-SET-REGION
               PERFORM 3800-DESCRIBE-TRUST-TYPE
               PERFORM 3810-EDIT-DESC
               PERFORM 3820-SET-TRUST-TYPE
               PERFORM 3830-AGE-NOTICE
               PERFORM 3840-EDIT-NAME
               PERFORM 3850-COUNT-CYCLE
               PERFORM 3860-SELECT-MARGIN
               PERFORM 3870-AGE-CLOSE
               PERFORM 3880-EDIT-CUST-ID
               PERFORM 3890-ACCRUE-TRUSTEE-FEE
               PERFORM 3900-LIMIT-DISTRIB
               PERFORM 3910-SELECT-RATE
               PERFORM 3920-FLAG-PRINCIPAL
               PERFORM 3930-PRORATE-MARKET
               PERFORM 3940-CHECK-EFF
               PERFORM 3950-ACCRUE-MARKET
               PERFORM 3960-AGE-POST
               PERFORM 3970-LIMIT-PRINCIPAL
               PERFORM 3980-ASSESS-REVERSAL
               PERFORM 3990-AGE-OPEN
               PERFORM 4000-AGE-EFFECTIVE
               PERFORM 4010-ASSESS-REVERSAL
               PERFORM 4020-SELECT-RATE
               PERFORM 4030-DESCRIBE-REASON
               PERFORM 4040-COUNT-CYCLE
               PERFORM 4050-AGE-EFF
               PERFORM 4060-FLAG-TRUSTEE-FEE
               PERFORM 4070-AGE-LAST-PAY
               PERFORM 4080-PRORATE-TRUSTEE-FEE
               PERFORM 4090-DESCRIBE-REGION
               PERFORM 4100-DESCRIBE-CYCLE
               PERFORM 4110-ACCRUE-PRINCIPAL
               PERFORM 4120-SET-STATUS
               PERFORM 4130-CHECK-LAST-STMT
               PERFORM 4140-ACCUM-DIST-FREQ
               PERFORM 4150-ACCRUE-TAX
               PERFORM 4160-DESCRIBE-REASON
               PERFORM 4170-CHECK-OPEN
               PERFORM 4180-SPLIT-TRUSTEE-FEE
               PERFORM 4190-EDIT-CONTACT
               PERFORM 4200-PRORATE-INCOME
               PERFORM 4210-DESCRIBE-TRUST-TYPE
               PERFORM 4220-PRORATE-INCOME
               PERFORM 4230-ASSESS-PREFERRED
               PERFORM 4240-EDIT-MEMO
               PERFORM 4250-SPLIT-TRUSTEE-FEE
               PERFORM 4260-SET-CYCLE
               PERFORM 4270-DESCRIBE-REASON
               PERFORM 4280-LIMIT-TRUSTEE-FEE
               PERFORM 4290-SPLIT-TAX
               PERFORM 4300-CHECK-EXPIRY
               PERFORM 4310-ASSESS-EXEMPT
               PERFORM 4320-LIMIT-DISTRIB
               PERFORM 4330-ACCRUE-RATE
               PERFORM 4340-CHECK-MATURITY
               PERFORM 4350-SPLIT-DISTRIB
               PERFORM 4360-ACCRUE-MARGIN
               PERFORM 4370-COUNT-SEQ
               PERFORM 7000-CALL-TRLIM
               PERFORM 7010-CALL-TRPEN
               PERFORM 7020-CALL-TRVAL
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
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3000-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EH -> GG WHEN MATURITY DATE IS PAST
      * GG -> JG WHEN MATURITY DATE IS PAST
      * JG -> EH WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3010-SET-STATUS.
           EVALUATE TRSM-STATUS-CD
               WHEN 'EH'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JG' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JG'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EH' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TRUSTEE FEE AMT.
      *----------------------------------------------------------------*
       3020-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TRUSTEE-FEE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-TRUSTEE-FEE-AMT
           END-IF
           ADD TRSM-TRUSTEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TAX YTD.
      *----------------------------------------------------------------*
       3030-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TAX-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-TAX-YTD
           END-IF
           ADD TRSM-TAX-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3040-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = TRSM-DISTRIB-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TRSM-DISTRIB-AMT - (WS-WORK-AMT * 2)
           COMPUTE TRSM-PRINCIPAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3050-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3060-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE TRSM-TRUSTEE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3070-LIMIT-DISTRIB.
           IF TRSM-DISTRIB-YTD < 25.00
               MOVE 25.00 TO TRSM-DISTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-DISTRIB-YTD > 50000.00
               MOVE 50000.00 TO TRSM-DISTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3080-LIMIT-DISTRIB.
           IF TRSM-DISTRIB-AMT < 10.00
               MOVE 10.00 TO TRSM-DISTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-DISTRIB-AMT > 2500.00
               MOVE 2500.00 TO TRSM-DISTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3090-CHECK-NOTICE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E542 CONTACT REQUIRED
      * E381 TRUSTEE FEE AMT NEGATIVE
      * E073 TRUSTEE FEE AMT OVER 1000000
      * E333 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3100-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-CONTACT = SPACES
                   MOVE 'E542' TO WS-REJECT-CODE
               WHEN TRST-TRUSTEE-FEE-AMT < 0
                   MOVE 'E381' TO WS-REJECT-CODE
               WHEN TRST-TRUSTEE-FEE-AMT > 1000000.00
                   MOVE 'E073' TO WS-REJECT-CODE
               WHEN TRSM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E333' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TAX AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3110-FLAG-TAX.
           IF TRSM-TAX-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE DISTRIB YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3120-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   COMPUTE TRSM-PRINCIPAL-YTD ROUNDED = TRSM-DISTRIB-YTD
                       * TRSM-DISCOUNT / 200
               WHEN TRSM-TRUST-TYPE-CD = 'FLJ'
                   MOVE 10.00 TO TRSM-PRINCIPAL-YTD
               WHEN TRSM-TRUST-TYPE-CD = 'MNG'
                   MOVE 10.00 TO TRSM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE TRSM-PRINCIPAL-YTD ROUNDED = TRSM-DISTRIB-YTD
                       * TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-PRINCIPAL-YTD > 0 AND TRSM-PRINCIPAL-YTD < 10.00
               MOVE 10.00 TO TRSM-PRINCIPAL-YTD
           END-IF
           IF TRSM-PRINCIPAL-YTD > 35.00
               MOVE 35.00 TO TRSM-PRINCIPAL-YTD
           END-IF
           ADD TRSM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TRUSTEE FEE AMT.
      *----------------------------------------------------------------*
       3130-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRST-TRUSTEE-FEE-AMT ROUNDED =
                   TRST-DISTRIB-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRST-TRUSTEE-FEE-AMT
           END-IF
           ADD TRST-TRUSTEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INCOME YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3140-ACCRUE-INCOME.
           IF TRSM-INCOME-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-INCOME-YTD * TRSM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TRST-INCOME-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INCOME FEE ON THE DISTRIB AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3150-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TRSM-INCOME-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   COMPUTE TRSM-INCOME-AMT ROUNDED = TRSM-DISTRIB-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRSM-REASON-CD = 'H'
                   MOVE 12.00 TO TRSM-INCOME-AMT
               WHEN TRSM-REASON-CD = 'M'
                   MOVE 12.00 TO TRSM-INCOME-AMT
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
      * SPLIT INCOME AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3160-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = TRSM-INCOME-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TRSM-INCOME-AMT - (WS-WORK-AMT * 12)
           COMPUTE TRSM-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INCOME YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-REASON.
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
      * TAX AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3180-FLAG-TAX.
           IF TRSM-TAX-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRSM-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3190-FLAG-PRINCIPAL.
           IF TRSM-PRINCIPAL-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRSM-PRINCIPAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MARKET AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3200-ACCUM-CYCLE.
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
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-NOTICE.
           IF TRSM-NOTICE-CNT < 9999999
               ADD 1 TO TRSM-NOTICE-CNT
           END-IF
           IF TRSM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO TRSM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3220-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRUST TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3230-DESCRIBE-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MNG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VFN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XPL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TAX YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3240-FLAG-TAX.
           IF TRSM-TAX-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRSM-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3250-ACCRUE-DISTRIB.
           IF TRSM-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-DISTRIB-AMT * TRSM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRSM-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3260-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX AMT BY DIST FREQ CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3270-ACCUM-DIST-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-DIST-FREQ-CD
                   ADD TRSM-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-DIST-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INCOME AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-INCOME.
           IF TRSM-INCOME-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-INCOME-AMT * TRSM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRSM-TRUSTEE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARKET AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3290-LIMIT-MARKET.
           IF TRSM-MARKET-AMT < 25.00
               MOVE 25.00 TO TRSM-MARKET-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-MARKET-AMT > 1000.00
               MOVE 1000.00 TO TRSM-MARKET-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3300-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-CYCLE-CD
                   ADD TRST-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRST-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E935 REJECT CD REQUIRED
      * E121 DISTRIB AMT NEGATIVE
      * E417 DISTRIB AMT OVER 99999
      * E004 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3310-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-REJECT-CD = SPACES
                   MOVE 'E935' TO WS-REJECT-CODE
               WHEN TRST-DISTRIB-AMT < 0
                   MOVE 'E121' TO WS-REJECT-CODE
               WHEN TRST-DISTRIB-AMT > 99999.00
                   MOVE 'E417' TO WS-REJECT-CODE
               WHEN TRSM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E004' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DIST FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3320-DESCRIBE-DIST-FREQ.
           EVALUATE TRSM-DIST-FREQ-CD
               WHEN 'E'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'K'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'P'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'W'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3330-COUNT-NOTICE.
           IF TRSM-NOTICE-CNT < 9999999
               ADD 1 TO TRSM-NOTICE-CNT
           END-IF
           IF TRSM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO TRSM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TRSM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EH -> GG WHEN LAST MAINT DATE IS PAST
      * GG -> JG WHEN LAST MAINT DATE IS PAST
      * JG -> EH WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3350-SET-STATUS.
           EVALUATE TRSM-STATUS-CD
               WHEN 'EH'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GG' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JG' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JG'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EH' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3360-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-MATURITY.
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
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3380-ACCUM-REGION.
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
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3390-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3400-AGE-LAST-MAINT.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT INCOME YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = TRSM-INCOME-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TRSM-INCOME-YTD - (WS-WORK-AMT * 3)
           COMPUTE TRSM-MARKET-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRUST TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3420-DESCRIBE-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MNG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VFN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XPL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3430-COUNT-CYCLE.
           IF TRSM-CYCLE-CNT < 9999999
               ADD 1 TO TRSM-CYCLE-CNT
           END-IF
           IF TRSM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INCOME TIER TIER FOR THE MARKET AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3440-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-TIER-RATE(1) TO TRSM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TRSM-MARKET-AMT NOT < WS-INCOME-TIER-LOW(WS-TX)
                   MOVE WS-INCOME-TIER-RATE(WS-TX) TO TRSM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3450-DESCRIBE-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3460-DESCRIBE-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'D'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E690 MEMO REQUIRED
      * E057 MARKET AMT NEGATIVE
      * E850 MARKET AMT OVER 1000000
      * E377 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3470-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-MEMO = SPACES
                   MOVE 'E690' TO WS-REJECT-CODE
               WHEN TRSM-MARKET-AMT < 0
                   MOVE 'E057' TO WS-REJECT-CODE
               WHEN TRSM-MARKET-AMT > 1000000.00
                   MOVE 'E850' TO WS-REJECT-CODE
               WHEN TRSM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E377' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRUST TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3480-DESCRIBE-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MNG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VFN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XPL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INCOME SCHED TIER FOR THE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3490-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-SCHED-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TRSM-TAX-AMT NOT < WS-INCOME-SCHED-LOW(WS-TX)
                   MOVE WS-INCOME-SCHED-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3500-COUNT-SEQ.
           IF TRST-SEQ < 9999999
               ADD 1 TO TRST-SEQ
           END-IF
           IF TRST-SEQ NOT < 6
               MOVE 'Y' TO TRSM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-RETRY.
           IF TRSM-RETRY-CNT < 9999999
               ADD 1 TO TRSM-RETRY-CNT
           END-IF
           IF TRSM-RETRY-CNT NOT < 6
               MOVE 'Y' TO TRSM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TRUSTEE FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3520-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = TRSM-TRUSTEE-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TRSM-TRUSTEE-FEE-YTD - (WS-WORK-AMT *
               2)
           COMPUTE TRSM-MARKET-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3530-CHECK-MATURITY.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO TRSM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3540-ACCRUE-DISTRIB.
           IF TRST-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRST-DISTRIB-AMT * TRSM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRSM-TRUSTEE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRUSTEE FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3550-FLAG-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-TRUSTEE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX YTD BY DIST FREQ CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3560-ACCUM-DIST-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-DIST-FREQ-CD
                   ADD TRSM-TAX-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-DIST-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-TAX-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE DISTRIB AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3570-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TRST-DISTRIB-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRST-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN TRST-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO WS-WORK-AMT
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
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3580-AGE-LAST-MAINT.
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
                   ADD TRSM-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRSM-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DIST FREQ CD:
      * E -> K WHEN EXPIRY DATE IS PAST
      * K -> P WHEN EXPIRY DATE IS PAST
      * P -> W WHEN EXPIRY DATE IS PAST
      * W -> Y WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3590-SET-DIST-FREQ.
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
      * ACCUMULATE TAX AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3600-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-CHANNEL-CD
                   ADD TRSM-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE DISTRIB AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3610-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TRST-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   COMPUTE TRST-PRINCIPAL-AMT ROUNDED = TRSM-DISTRIB-AMT
                       * TRSM-MARGIN / 200
               WHEN TRSM-REASON-CD = 'H'
                   MOVE 15.00 TO TRST-PRINCIPAL-AMT
               WHEN TRSM-REASON-CD = 'M'
                   MOVE 7.00 TO TRST-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE TRST-PRINCIPAL-AMT ROUNDED = TRSM-DISTRIB-AMT
                       * TRSM-MARGIN / 100
           END-EVALUATE
           IF TRST-PRINCIPAL-AMT > 0 AND TRST-PRINCIPAL-AMT < 2.00
               MOVE 2.00 TO TRST-PRINCIPAL-AMT
           END-IF
           IF TRST-PRINCIPAL-AMT > 100.00
               MOVE 100.00 TO TRST-PRINCIPAL-AMT
           END-IF
           ADD TRST-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3620-CHECK-NOTICE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       3630-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRST-PRINCIPAL-AMT ROUNDED =
                   TRSM-PRINCIPAL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRST-PRINCIPAL-AMT
           END-IF
           ADD TRST-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB AMT BY DIST FREQ CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3640-ACCUM-DIST-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-DIST-FREQ-CD
                   ADD TRST-DISTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-DIST-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRST-DISTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EH -> GG WHEN MATURITY DATE IS PAST
      * GG -> JG WHEN MATURITY DATE IS PAST
      * JG -> EH WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3650-SET-STATUS.
           EVALUATE TRSM-STATUS-CD
               WHEN 'EH'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JG' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JG'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EH' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRUSTEE FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3660-LIMIT-TRUSTEE-FEE.
           IF TRST-TRUSTEE-FEE-AMT < 100.00
               MOVE 100.00 TO TRST-TRUSTEE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-TRUSTEE-FEE-AMT > 1000.00
               MOVE 1000.00 TO TRST-TRUSTEE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> D WHEN CLOSE DATE IS PAST
      * D -> N WHEN CLOSE DATE IS PAST
      * N -> T WHEN CLOSE DATE IS PAST
      * T -> A WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3670-SET-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'A' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INCOME YTD FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3680-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TRSM-INCOME-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRSM-INCOME-YTD ROUNDED = TRST-PRINCIPAL-AMT
                       * TRSM-DISCOUNT / 200
               WHEN TRST-TRAN-CD = 'IN'
                   MOVE 12.00 TO TRSM-INCOME-YTD
               WHEN TRST-TRAN-CD = 'DS'
                   MOVE 3.00 TO TRSM-INCOME-YTD
               WHEN OTHER
                   COMPUTE TRSM-INCOME-YTD ROUNDED = TRST-PRINCIPAL-AMT
                       * TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-INCOME-YTD > 0 AND TRSM-INCOME-YTD < 5.00
               MOVE 5.00 TO TRSM-INCOME-YTD
           END-IF
           IF TRSM-INCOME-YTD > 250.00
               MOVE 250.00 TO TRSM-INCOME-YTD
           END-IF
           ADD TRSM-INCOME-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3690-AGE-POST.
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
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3700-COUNT-RETRY.
           IF TRSM-RETRY-CNT < 9999999
               ADD 1 TO TRSM-RETRY-CNT
           END-IF
           IF TRSM-RETRY-CNT NOT < 5
               MOVE 'Y' TO TRST-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TRUSTEE FEE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3710-ACCUM-TRAN.
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
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EN -> FY WHEN CLOSE DATE IS PAST
      * FY -> HP WHEN CLOSE DATE IS PAST
      * HP -> JB WHEN CLOSE DATE IS PAST
      * JB -> KZ WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-CYCLE.
           EVALUATE TRSM-CYCLE-CD
               WHEN 'EN'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FY' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FY'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HP' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HP'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JB' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JB'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KZ' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3730-AGE-NOTICE.
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
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-TAX-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-TAX-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-TAX-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3740-CHECK-POST.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO TRST-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TRUSTEE FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3750-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = TRSM-TRUSTEE-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TRSM-TRUSTEE-FEE-YTD - (WS-WORK-AMT *
               3)
           COMPUTE TRSM-INCOME-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAX YTD FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3760-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TAX-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   COMPUTE TRSM-TAX-YTD ROUNDED = TRSM-PRINCIPAL-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRSM-REGION-CD = 'A'
                   MOVE 12.00 TO TRSM-TAX-YTD
               WHEN TRSM-REGION-CD = 'D'
                   MOVE 3.00 TO TRSM-TAX-YTD
               WHEN OTHER
                   COMPUTE TRSM-TAX-YTD ROUNDED = TRSM-PRINCIPAL-AMT *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-TAX-YTD > 0 AND TRSM-TAX-YTD < 2.00
               MOVE 2.00 TO TRSM-TAX-YTD
           END-IF
           IF TRSM-TAX-YTD > 250.00
               MOVE 250.00 TO TRSM-TAX-YTD
           END-IF
           ADD TRSM-TAX-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TAX AMT.
      *----------------------------------------------------------------*
       3770-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TAX-AMT ROUNDED =
                   TRSM-PRINCIPAL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-TAX-AMT
           END-IF
           ADD TRSM-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX AMT BY TRUST TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3780-ACCUM-TRUST-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-TRUST-TYPE-CD
                   ADD TRSM-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-TRUST-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> D WHEN LAST STMT DATE IS PAST
      * D -> N WHEN LAST STMT DATE IS PAST
      * N -> T WHEN LAST STMT DATE IS PAST
      * T -> A WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'A' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRUST TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3800-DESCRIBE-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MNG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VFN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XPL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E489 DESC REQUIRED
      * E459 PRINCIPAL AMT NEGATIVE
      * E879 PRINCIPAL AMT OVER 250000
      * E940 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3810-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-DESC = SPACES
                   MOVE 'E489' TO WS-REJECT-CODE
               WHEN TRST-PRINCIPAL-AMT < 0
                   MOVE 'E459' TO WS-REJECT-CODE
               WHEN TRST-PRINCIPAL-AMT > 250000.00
                   MOVE 'E879' TO WS-REJECT-CODE
               WHEN TRSM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E940' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN LAST MAINT DATE IS PAST
      * MNG -> VFN WHEN LAST MAINT DATE IS PAST
      * VFN -> XPL WHEN LAST MAINT DATE IS PAST
      * XPL -> FLJ WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3820-SET-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3830-AGE-NOTICE.
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
                   ADD TRSM-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E117 NAME REQUIRED
      * E286 DISTRIB AMT NEGATIVE
      * E043 DISTRIB AMT OVER 50000
      * E623 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3840-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-NAME = SPACES
                   MOVE 'E117' TO WS-REJECT-CODE
               WHEN TRSM-DISTRIB-AMT < 0
                   MOVE 'E286' TO WS-REJECT-CODE
               WHEN TRSM-DISTRIB-AMT > 50000.00
                   MOVE 'E043' TO WS-REJECT-CODE
               WHEN TRSM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E623' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3850-COUNT-CYCLE.
           IF TRSM-CYCLE-CNT < 9999999
               ADD 1 TO TRSM-CYCLE-CNT
           END-IF
           IF TRSM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO TRSM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INCOME SCHED TIER FOR THE TRUSTEE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3860-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-SCHED-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TRSM-TRUSTEE-FEE-YTD NOT < WS-INCOME-SCHED-LOW(WS-TX)
                   MOVE WS-INCOME-SCHED-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3870-AGE-CLOSE.
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
               WHEN WS-DAYS NOT > 60
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E196 CUST ID REQUIRED
      * E998 WORK AMT NEGATIVE
      * E275 WORK AMT OVER 1000000
      * E280 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3880-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-CUST-ID = SPACES
                   MOVE 'E196' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E998' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E275' TO WS-REJECT-CODE
               WHEN TRSM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E280' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TRUSTEE FEE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3890-ACCRUE-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TRUSTEE-FEE-AMT * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRST-TRUSTEE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3900-LIMIT-DISTRIB.
           IF TRSM-DISTRIB-AMT < 25.00
               MOVE 25.00 TO TRSM-DISTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-DISTRIB-AMT > 5000.00
               MOVE 5000.00 TO TRSM-DISTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INCOME SCHED TIER FOR THE TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3910-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-SCHED-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TRSM-TAX-YTD NOT < WS-INCOME-SCHED-LOW(WS-TX)
                   MOVE WS-INCOME-SCHED-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3920-FLAG-PRINCIPAL.
           IF TRSM-PRINCIPAL-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRSM-PRINCIPAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INCOME YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MARKET AMT.
      *----------------------------------------------------------------*
       3930-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-MARKET-AMT ROUNDED =
                   TRSM-INCOME-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-MARKET-AMT
           END-IF
           ADD TRSM-MARKET-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3940-CHECK-EFF.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARKET YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3950-ACCRUE-MARKET.
           IF TRSM-MARKET-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-MARKET-YTD * TRSM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRST-INCOME-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3960-AGE-POST.
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
                   ADD TRSM-INCOME-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-INCOME-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-INCOME-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-INCOME-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-INCOME-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3970-LIMIT-PRINCIPAL.
           IF TRSM-PRINCIPAL-YTD < 25.00
               MOVE 25.00 TO TRSM-PRINCIPAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-PRINCIPAL-YTD > 5000.00
               MOVE 5000.00 TO TRSM-PRINCIPAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INCOME FEE ON THE TAX AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3980-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TRSM-INCOME-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   COMPUTE TRSM-INCOME-AMT ROUNDED = TRSM-TAX-AMT *
                       TRSM-MARGIN / 200
               WHEN TRSM-TRUST-TYPE-CD = 'FLJ'
                   MOVE 3.00 TO TRSM-INCOME-AMT
               WHEN TRSM-TRUST-TYPE-CD = 'MNG'
                   MOVE 5.00 TO TRSM-INCOME-AMT
               WHEN OTHER
                   COMPUTE TRSM-INCOME-AMT ROUNDED = TRSM-TAX-AMT *
                       TRSM-MARGIN / 100
           END-EVALUATE
           IF TRSM-INCOME-AMT > 0 AND TRSM-INCOME-AMT < 1.00
               MOVE 1.00 TO TRSM-INCOME-AMT
           END-IF
           IF TRSM-INCOME-AMT > 100.00
               MOVE 100.00 TO TRSM-INCOME-AMT
           END-IF
           ADD TRSM-INCOME-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3990-AGE-OPEN.
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
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4000-AGE-EFFECTIVE.
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
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE TRUSTEE FEE FEE ON THE INCOME AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4010-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TRUSTEE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRSM-TRUSTEE-FEE-AMT ROUNDED =
                       TRSM-INCOME-AMT * TRSM-DISCOUNT / 200
               WHEN TRSM-STATUS-CD = 'EH'
                   MOVE 15.00 TO TRSM-TRUSTEE-FEE-AMT
               WHEN TRSM-STATUS-CD = 'GG'
                   MOVE 10.00 TO TRSM-TRUSTEE-FEE-AMT
               WHEN OTHER
                   COMPUTE TRSM-TRUSTEE-FEE-AMT ROUNDED =
                       TRSM-INCOME-AMT * TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-TRUSTEE-FEE-AMT > 0 AND TRSM-TRUSTEE-FEE-AMT < 2.00
               MOVE 2.00 TO TRSM-TRUSTEE-FEE-AMT
           END-IF
           IF TRSM-TRUSTEE-FEE-AMT > 35.00
               MOVE 35.00 TO TRSM-TRUSTEE-FEE-AMT
           END-IF
           ADD TRSM-TRUSTEE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA TIER TIER FOR THE DISTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4020-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-TIER-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TRST-DISTRIB-AMT NOT < WS-PRINCIPA-TIER-LOW(WS-TX)
                   MOVE WS-PRINCIPA-TIER-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4030-DESCRIBE-REASON.
           EVALUATE TRSM-REASON-CD
               WHEN 'H'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4040-COUNT-CYCLE.
           IF TRSM-CYCLE-CNT < 9999999
               ADD 1 TO TRSM-CYCLE-CNT
           END-IF
           IF TRSM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO TRSM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4050-AGE-EFF.
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
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRUSTEE FEE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4060-FLAG-TRUSTEE-FEE.
           IF TRST-TRUSTEE-FEE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRST-TRUSTEE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4070-AGE-LAST-PAY.
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
      * PRORATE PRINCIPAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TRUSTEE FEE AMT.
      *----------------------------------------------------------------*
       4080-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRST-TRUSTEE-FEE-AMT ROUNDED =
                   TRSM-PRINCIPAL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRST-TRUSTEE-FEE-AMT
           END-IF
           ADD TRST-TRUSTEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4090-DESCRIBE-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4100-DESCRIBE-CYCLE.
           EVALUATE TRSM-CYCLE-CD
               WHEN 'EN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4110-ACCRUE-PRINCIPAL.
           IF TRSM-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-PRINCIPAL-AMT * TRSM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRSM-MARKET-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EH -> GG WHEN POST DATE IS PAST
      * GG -> JG WHEN POST DATE IS PAST
      * JG -> EH WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4120-SET-STATUS.
           EVALUATE TRSM-STATUS-CD
               WHEN 'EH'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JG' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JG'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EH' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4130-CHECK-LAST-STMT.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TRUSTEE FEE AMT BY DIST FREQ CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4140-ACCUM-DIST-FREQ.
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
      * DAILY INTEREST ON TAX YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4150-ACCRUE-TAX.
           IF TRSM-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TAX-YTD * TRSM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4160-DESCRIBE-REASON.
           EVALUATE TRSM-REASON-CD
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4170-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TRSM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TRSM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TRUSTEE FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4180-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = TRST-TRUSTEE-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRST-TRUSTEE-FEE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE TRSM-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E822 CONTACT REQUIRED
      * E621 TRUSTEE FEE AMT NEGATIVE
      * E926 TRUSTEE FEE AMT OVER 250000
      * E136 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4190-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-CONTACT = SPACES
                   MOVE 'E822' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-AMT < 0
                   MOVE 'E621' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-AMT > 250000.00
                   MOVE 'E926' TO WS-REJECT-CODE
               WHEN TRSM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E136' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INCOME YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4200-PRORATE-INCOME.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TRSM-INCOME-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TRUST TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4210-DESCRIBE-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MNG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VFN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XPL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE MARKET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INCOME YTD.
      *----------------------------------------------------------------*
       4220-PRORATE-INCOME.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-INCOME-YTD ROUNDED =
                   TRSM-MARKET-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-INCOME-YTD
           END-IF
           ADD TRSM-INCOME-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB FEE ON THE MARKET AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4230-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TRST-DISTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-STOP-FLAG = 'Y'
                   COMPUTE TRST-DISTRIB-AMT ROUNDED = TRSM-MARKET-AMT *
                       TRSM-MARGIN / 200
               WHEN TRST-TRAN-CD = 'IN'
                   MOVE 10.00 TO TRST-DISTRIB-AMT
               WHEN TRST-TRAN-CD = 'DS'
                   MOVE 3.00 TO TRST-DISTRIB-AMT
               WHEN OTHER
                   COMPUTE TRST-DISTRIB-AMT ROUNDED = TRSM-MARKET-AMT *
                       TRSM-MARGIN / 100
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
      * E899 MEMO REQUIRED
      * E562 PRINCIPAL AMT NEGATIVE
      * E836 PRINCIPAL AMT OVER 1000000
      * E507 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4240-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-MEMO = SPACES
                   MOVE 'E899' TO WS-REJECT-CODE
               WHEN TRSM-PRINCIPAL-AMT < 0
                   MOVE 'E562' TO WS-REJECT-CODE
               WHEN TRSM-PRINCIPAL-AMT > 1000000.00
                   MOVE 'E836' TO WS-REJECT-CODE
               WHEN TRSM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E507' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TRUSTEE FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4250-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = TRSM-TRUSTEE-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TRSM-TRUSTEE-FEE-AMT - (WS-WORK-AMT *
               2)
           COMPUTE TRSM-PRINCIPAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EN -> FY WHEN LAST STMT DATE IS PAST
      * FY -> HP WHEN LAST STMT DATE IS PAST
      * HP -> JB WHEN LAST STMT DATE IS PAST
      * JB -> KZ WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4260-SET-CYCLE.
           EVALUATE TRSM-CYCLE-CD
               WHEN 'EN'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FY' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FY'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HP' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HP'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JB' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JB'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KZ' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4270-DESCRIBE-REASON.
           EVALUATE TRSM-REASON-CD
               WHEN 'H'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'M'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRUSTEE FEE YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4280-LIMIT-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-YTD < 100.00
               MOVE 100.00 TO TRSM-TRUSTEE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-TRUSTEE-FEE-YTD > 50000.00
               MOVE 50000.00 TO TRSM-TRUSTEE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAX YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4290-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = TRSM-TAX-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TRSM-TAX-YTD - (WS-WORK-AMT * 12)
           COMPUTE TRSM-PRINCIPAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4300-CHECK-EXPIRY.
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
      * ASSESS THE PRINCIPAL YTD FEE ON THE DISTRIB AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4310-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   COMPUTE TRSM-PRINCIPAL-YTD ROUNDED = TRSM-DISTRIB-AMT
                       * TRSM-RATE / 200
               WHEN TRSM-REGION-CD = 'A'
                   MOVE 12.00 TO TRSM-PRINCIPAL-YTD
               WHEN TRSM-REGION-CD = 'D'
                   MOVE 5.00 TO TRSM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE TRSM-PRINCIPAL-YTD ROUNDED = TRSM-DISTRIB-AMT
                       * TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-PRINCIPAL-YTD > 0 AND TRSM-PRINCIPAL-YTD < 2.00
               MOVE 2.00 TO TRSM-PRINCIPAL-YTD
           END-IF
           IF TRSM-PRINCIPAL-YTD > 75.00
               MOVE 75.00 TO TRSM-PRINCIPAL-YTD
           END-IF
           ADD TRSM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DISTRIB YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4320-LIMIT-DISTRIB.
           IF TRSM-DISTRIB-YTD < 25.00
               MOVE 25.00 TO TRSM-DISTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-DISTRIB-YTD > 5000.00
               MOVE 5000.00 TO TRSM-DISTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4330-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TRSM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4340-CHECK-MATURITY.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4350-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE TRSM-DISTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4360-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRSM-DISTRIB-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4370-COUNT-SEQ.
           IF TRST-SEQ < 9999999
               ADD 1 TO TRST-SEQ
           END-IF
           IF TRST-SEQ NOT < 6
               MOVE 'Y' TO TRST-REVERSAL-FLAG
           END-IF.

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
                   MOVE TRLIM-DISTRIB-AMT
                     TO TRSM-DISTRIB-AMT
                   MOVE TRLIM-PRINCIPAL-YTD
                     TO TRSM-PRINCIPAL-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-TRLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

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
                   MOVE TRPEN-INCOME-YTD
                     TO TRSM-INCOME-YTD
                   MOVE TRPEN-TAX-AMT
                     TO TRSM-TAX-AMT
                   MOVE TRPEN-MARKET-YTD
                     TO TRSM-MARKET-YTD
                   MOVE TRPEN-EXEMPT-FLAG
                     TO TRSM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-TRPEN' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

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
                   MOVE TRVAL-MARKET-YTD
                     TO TRSM-MARKET-YTD
                   MOVE TRVAL-PRINCIPAL-AMT
                     TO TRSM-PRINCIPAL-AMT
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-TRVAL' TO ABND-PARAGRAPH
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
           DISPLAY 'TRSANAL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TRSANAL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
