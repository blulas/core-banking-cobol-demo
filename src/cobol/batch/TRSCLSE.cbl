      *================================================================*
      * PROGRAM  : TRSCLSE                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PERSONAL TRUST - CLOSURE PROCESSING. READS THE TRS  *
      *            TRANSACTIONS, UPDATES THE TRS MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. PERSONAL TRUST ACCOUNTING,      *
      *            INCOME DISTRIBUTION AND TRUSTEE FEES                *
      * INPUT    : TRSMST (KSDS), TRSTRN, BANKCTL                      *
      * OUTPUT   : TRSOUT, TRSCLRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TRLIM, TRVAL            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-04-08 PQR  ORIGINAL                                       *
      * 1989-02-20 BHS  DISTRIB CUTOFF MOVED INC6566                   *
      * 1990-09-25 GAF  TAX CALCULATION CORRECTED PRB2396              *
      * 1992-01-02 DPS  MARKET CALCULATION CORRECTED PRB1130           *
      * 2001-02-24 TWH  MARKET FEE SCHEDULE UPDATE PRB4515             *
      * 2002-03-05 CRP  TAX LIMIT REVISED INC1906                      *
      * 2007-04-20 CRP  AUDIT FINDING - DISTRIB CONTROL TOTALS CHG2703 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRSCLSE.
       AUTHOR. R J TRAVERS.

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
           SELECT REPORT-FILE ASSIGN TO "TRSCLRPT"
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
      * WS-TRUSTEE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TRUSTEE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000829000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000571000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000537000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000650000000496500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001650000000176500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001900000000248500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001950000000092500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002200000000408000'.
       01  WS-TRUSTEE-TIER-TBL REDEFINES WS-TRUSTEE-TIER-VALUES.
           05  WS-TRUSTEE-TIER-ROW OCCURS 8 TIMES.
               10  WS-TRUSTEE-TIER-LOW     PIC 9(09)V99.
               10  WS-TRUSTEE-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INCOME-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INCOME-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000588000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000213000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000306500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003600000000848500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003650000000563000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003750000000200000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013750000000692000'.
       01  WS-INCOME-GRID-TBL REDEFINES WS-INCOME-GRID-VALUES.
           05  WS-INCOME-GRID-ROW OCCURS 7 TIMES.
               10  WS-INCOME-GRID-LOW      PIC 9(09)V99.
               10  WS-INCOME-GRID-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TRSCLSE  PERSONAL TRUST'.
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
               PERFORM 3000-ASSESS-EXEMPT
               PERFORM 3010-DESCRIBE-TRUST-TYPE
               PERFORM 3020-FLAG-TRUSTEE-FEE
               PERFORM 3030-PRORATE-PRINCIPAL
               PERFORM 3040-EDIT-MEMO
               PERFORM 3050-ACCRUE-DISTRIB
               PERFORM 3060-COUNT-RETRY
               PERFORM 3070-ACCUM-TRUST-TYPE
               PERFORM 3080-SELECT-MARGIN
               PERFORM 3090-ACCUM-REGION
               PERFORM 3100-DESCRIBE-REASON
               PERFORM 3110-PRORATE-DISTRIB
               PERFORM 3120-ACCRUE-PRINCIPAL
               PERFORM 3130-ASSESS-SUSPEND
               PERFORM 3140-ASSESS-PREFERRED
               PERFORM 3150-ASSESS-EXEMPT
               PERFORM 3160-ASSESS-STOP
               PERFORM 3170-DESCRIBE-TRUST-TYPE
               PERFORM 3180-SELECT-RATE
               PERFORM 3190-SET-REASON
               PERFORM 3200-FLAG-INCOME
               PERFORM 3210-SET-TRAN
               PERFORM 3220-SELECT-DISCOUNT
               PERFORM 3230-CHECK-LAST-MAINT
               PERFORM 3240-DESCRIBE-CHANNEL
               PERFORM 3250-ACCRUE-TRUSTEE-FEE
               PERFORM 3260-AGE-EFFECTIVE
               PERFORM 3270-FLAG-PRINCIPAL
               PERFORM 3280-FLAG-DISTRIB
               PERFORM 3290-ACCRUE-TAX
               PERFORM 3300-CHECK-NOTICE
               PERFORM 3310-LIMIT-MARKET
               PERFORM 3320-ACCRUE-TAX
               PERFORM 3330-SELECT-DISCOUNT
               PERFORM 3340-EDIT-NAME
               PERFORM 3350-ACCUM-DIST-FREQ
               PERFORM 3360-DESCRIBE-TRUST-TYPE
               PERFORM 3370-AGE-EXPIRY
               PERFORM 3380-SET-STATUS
               PERFORM 3390-SPLIT-TRUSTEE-FEE
               PERFORM 3400-EDIT-DESC
               PERFORM 3410-ACCUM-STATUS
               PERFORM 3420-SPLIT-TRUSTEE-FEE
               PERFORM 3430-LIMIT-TRUSTEE-FEE
               PERFORM 3440-ACCRUE-DISTRIB
               PERFORM 3450-ASSESS-NOTICE-SENT
               PERFORM 3460-EDIT-REF-NO
               PERFORM 3470-SELECT-MARGIN
               PERFORM 3480-PRORATE-DISTRIB
               PERFORM 3490-SELECT-DISCOUNT
               PERFORM 3500-SPLIT-INCOME
               PERFORM 3510-COUNT-PAYMENT
               PERFORM 3520-ASSESS-EXEMPT
               PERFORM 3530-DESCRIBE-TRAN
               PERFORM 3540-ASSESS-STOP
               PERFORM 3550-ACCRUE-INCOME
               PERFORM 3560-FLAG-MARKET
               PERFORM 3570-ACCUM-REASON
               PERFORM 3580-ACCUM-STATUS
               PERFORM 3590-SPLIT-TRUSTEE-FEE
               PERFORM 3600-FLAG-TRUSTEE-FEE
               PERFORM 3610-SET-CHANNEL
               PERFORM 3620-ACCUM-REGION
               PERFORM 3630-EDIT-REF-NO
               PERFORM 3640-DESCRIBE-REGION
               PERFORM 3650-AGE-LAST-PAY
               PERFORM 3660-ACCRUE-INCOME
               PERFORM 3670-SPLIT-DISTRIB
               PERFORM 3680-ASSESS-SUSPEND
               PERFORM 3690-ACCRUE-INCOME
               PERFORM 3700-FLAG-PRINCIPAL
               PERFORM 3710-ACCRUE-DISTRIB
               PERFORM 3720-CHECK-EXPIRY
               PERFORM 3730-FLAG-INCOME
               PERFORM 3740-ACCRUE-TAX
               PERFORM 3750-AGE-MATURITY
               PERFORM 3760-LIMIT-PRINCIPAL
               PERFORM 3770-EDIT-DESC
               PERFORM 3780-ACCRUE-DISTRIB
               PERFORM 3790-LIMIT-INCOME
               PERFORM 3800-DESCRIBE-STATUS
               PERFORM 3810-ACCUM-STATUS
               PERFORM 3820-SET-CYCLE
               PERFORM 3830-LIMIT-TRUSTEE-FEE
               PERFORM 3840-AGE-NOTICE
               PERFORM 3850-AGE-EFFECTIVE
               PERFORM 3860-COUNT-NOTICE
               PERFORM 3870-DESCRIBE-REASON
               PERFORM 3880-SPLIT-TRUSTEE-FEE
               PERFORM 3890-ASSESS-PREFERRED
               PERFORM 3900-ACCUM-TRUST-TYPE
               PERFORM 3910-ACCRUE-DISTRIB
               PERFORM 3920-FLAG-TRUSTEE-FEE
               PERFORM 3930-LIMIT-PRINCIPAL
               PERFORM 3940-LIMIT-PRINCIPAL
               PERFORM 3950-FLAG-TRUSTEE-FEE
               PERFORM 3960-EDIT-CUST-ID
               PERFORM 3970-SELECT-RATE
               PERFORM 3980-PRORATE-TRUSTEE-FEE
               PERFORM 3990-DESCRIBE-REGION
               PERFORM 4000-SELECT-MARGIN
               PERFORM 4010-ASSESS-REVERSAL
               PERFORM 4020-SELECT-MARGIN
               PERFORM 4030-ACCUM-CHANNEL
               PERFORM 4040-CHECK-EXPIRY
               PERFORM 4050-ASSESS-STOP
               PERFORM 4060-PRORATE-TRUSTEE-FEE
               PERFORM 4070-ASSESS-NOTICE-SENT
               PERFORM 4080-EDIT-REF-NO
               PERFORM 4090-ACCUM-TRAN
               PERFORM 4100-SPLIT-TRUSTEE-FEE
               PERFORM 4110-DESCRIBE-TRAN
               PERFORM 4120-ASSESS-PREFERRED
               PERFORM 4130-SELECT-DISCOUNT
               PERFORM 4140-PRORATE-PRINCIPAL
               PERFORM 4150-COUNT-CYCLE
               PERFORM 4160-CHECK-POST
               PERFORM 4170-EDIT-OPERATOR
               PERFORM 4180-AGE-LAST-STMT
               PERFORM 4190-AGE-POST
               PERFORM 4200-ACCRUE-PRINCIPAL
               PERFORM 4210-SET-TRAN
               PERFORM 4220-FLAG-DISTRIB
               PERFORM 4230-ACCRUE-MARGIN
               PERFORM 4240-ACCRUE-PRINCIPAL
               PERFORM 4250-DESCRIBE-REGION
               PERFORM 4260-DESCRIBE-DIST-FREQ
               PERFORM 4270-DESCRIBE-CYCLE
               PERFORM 4280-AGE-OPEN
               PERFORM 4290-FLAG-INCOME
               PERFORM 4300-CHECK-CLOSE
               PERFORM 4310-SET-REASON
               PERFORM 4320-CHECK-LAST-MAINT
               PERFORM 4330-LIMIT-INCOME
               PERFORM 4340-DESCRIBE-CHANNEL
               PERFORM 4350-SPLIT-TRUSTEE-FEE
               PERFORM 4360-ACCRUE-DISTRIB
               PERFORM 4370-SELECT-MARGIN
               PERFORM 4380-ASSESS-EXEMPT
               PERFORM 4390-DESCRIBE-CYCLE
               PERFORM 4400-ACCUM-TRAN
               PERFORM 4410-DESCRIBE-TRUST-TYPE
               PERFORM 4420-ASSESS-REVIEW
               PERFORM 4430-COUNT-NOTICE
               PERFORM 4440-EDIT-MEMO
               PERFORM 4450-EDIT-MEMO
               PERFORM 4460-PRORATE-TRUSTEE-FEE
               PERFORM 4470-SPLIT-DISTRIB
               PERFORM 4480-ASSESS-STOP
               PERFORM 4490-ASSESS-NOTICE-SENT
               PERFORM 4500-DESCRIBE-STATUS
               PERFORM 4510-SPLIT-DISTRIB
               PERFORM 4520-CHECK-LAST-MAINT
               PERFORM 4530-ACCRUE-RATE
               PERFORM 4540-CHECK-POST
               PERFORM 4550-COUNT-PAYMENT
               PERFORM 4560-FLAG-PRINCIPAL
               PERFORM 4570-ASSESS-NOTICE-SENT
               PERFORM 4580-EDIT-REF-NO
               PERFORM 4590-DESCRIBE-CHANNEL
               PERFORM 4600-DESCRIBE-CYCLE
               PERFORM 4610-PRORATE-MARKET
               PERFORM 4620-ASSESS-REVERSAL
               PERFORM 4630-SELECT-MARGIN
               PERFORM 4640-DESCRIBE-DIST-FREQ
               PERFORM 4650-AGE-POST
               PERFORM 4660-SET-CHANNEL
               PERFORM 4670-ACCRUE-DISTRIB
               PERFORM 4680-DESCRIBE-CYCLE
               PERFORM 4690-SET-CHANNEL
               PERFORM 4700-SELECT-DISCOUNT
               PERFORM 4710-COUNT-NOTICE
               PERFORM 4720-AGE-CLOSE
               PERFORM 4730-ACCUM-TRAN
               PERFORM 4740-SET-CHANNEL
               PERFORM 4750-DESCRIBE-CHANNEL
               PERFORM 4760-COUNT-RETRY
               PERFORM 4770-SPLIT-PRINCIPAL
               PERFORM 4780-EDIT-REJECT
               PERFORM 4790-PRORATE-INCOME
               PERFORM 4800-LIMIT-PRINCIPAL
               PERFORM 4810-PRORATE-PRINCIPAL
               PERFORM 4820-ACCRUE-RATE
               PERFORM 4830-ACCRUE-MARGIN
               PERFORM 4840-SPLIT-TAX
               PERFORM 4850-ACCUM-TRAN
               PERFORM 4860-ACCUM-CYCLE
               PERFORM 4870-DESCRIBE-TRUST-TYPE
               PERFORM 4880-LIMIT-ITEM
               PERFORM 4890-CHECK-CLOSE
               PERFORM 4900-FLAG-PRINCIPAL
               PERFORM 7000-CALL-TRLIM
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
      * ASSESS THE TAX YTD FEE ON THE MARKET YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3000-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TAX-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   COMPUTE TRSM-TAX-YTD ROUNDED = TRSM-MARKET-YTD *
                       TRSM-DISCOUNT / 200
               WHEN TRSM-REASON-CD = 'H'
                   MOVE 10.00 TO TRSM-TAX-YTD
               WHEN TRSM-REASON-CD = 'M'
                   MOVE 12.00 TO TRSM-TAX-YTD
               WHEN OTHER
                   COMPUTE TRSM-TAX-YTD ROUNDED = TRSM-MARKET-YTD *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-TAX-YTD > 0 AND TRSM-TAX-YTD < 10.00
               MOVE 10.00 TO TRSM-TAX-YTD
           END-IF
           IF TRSM-TAX-YTD > 50.00
               MOVE 50.00 TO TRSM-TAX-YTD
           END-IF
           ADD TRSM-TAX-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TRUST TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3010-DESCRIBE-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MNG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VFN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XPL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRUSTEE FEE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3020-FLAG-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-TRUSTEE-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       3030-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-PRINCIPAL-AMT ROUNDED =
                   TRSM-TAX-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-PRINCIPAL-AMT
           END-IF
           ADD TRSM-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E578 MEMO REQUIRED
      * E881 WORK AMT NEGATIVE
      * E450 WORK AMT OVER 50000
      * E613 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3040-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-MEMO = SPACES
                   MOVE 'E578' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E881' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E450' TO WS-REJECT-CODE
               WHEN TRSM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E613' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3050-ACCRUE-DISTRIB.
           IF TRST-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRST-DISTRIB-AMT * TRSM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TRSM-DISTRIB-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3060-COUNT-RETRY.
           IF TRSM-RETRY-CNT < 9999999
               ADD 1 TO TRSM-RETRY-CNT
           END-IF
           IF TRSM-RETRY-CNT NOT < 6
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INCOME YTD BY TRUST TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3070-ACCUM-TRUST-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-TRUST-TYPE-CD
                   ADD TRSM-INCOME-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-TRUST-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-INCOME-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE TIER TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3080-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-TIER-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRST-PRINCIPAL-AMT NOT < WS-TRUSTEE-TIER-LOW(WS-TX)
                   MOVE WS-TRUSTEE-TIER-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3090-ACCUM-REGION.
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
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3100-DESCRIBE-REASON.
           EVALUATE TRSM-REASON-CD
               WHEN 'H'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE MARKET YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISTRIB YTD.
      *----------------------------------------------------------------*
       3110-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-DISTRIB-YTD ROUNDED =
                   TRSM-MARKET-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-DISTRIB-YTD
           END-IF
           ADD TRSM-DISTRIB-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3120-ACCRUE-PRINCIPAL.
           IF TRSM-PRINCIPAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-PRINCIPAL-YTD * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRSM-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRUSTEE FEE FEE ON THE INCOME YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3130-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TRST-TRUSTEE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   COMPUTE TRST-TRUSTEE-FEE-AMT ROUNDED =
                       TRSM-INCOME-YTD * TRSM-MARGIN / 200
               WHEN TRSM-DIST-FREQ-CD = 'E'
                   MOVE 12.00 TO TRST-TRUSTEE-FEE-AMT
               WHEN TRSM-DIST-FREQ-CD = 'K'
                   MOVE 20.00 TO TRST-TRUSTEE-FEE-AMT
               WHEN OTHER
                   COMPUTE TRST-TRUSTEE-FEE-AMT ROUNDED =
                       TRSM-INCOME-YTD * TRSM-MARGIN / 100
           END-EVALUATE
           IF TRST-TRUSTEE-FEE-AMT > 0 AND TRST-TRUSTEE-FEE-AMT < 10.00
               MOVE 10.00 TO TRST-TRUSTEE-FEE-AMT
           END-IF
           IF TRST-TRUSTEE-FEE-AMT > 35.00
               MOVE 35.00 TO TRST-TRUSTEE-FEE-AMT
           END-IF
           ADD TRST-TRUSTEE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE INCOME AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3140-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TRSM-INCOME-AMT *
                       TRSM-MARGIN / 200
               WHEN TRSM-DIST-FREQ-CD = 'E'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN TRSM-DIST-FREQ-CD = 'K'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TRSM-INCOME-AMT *
                       TRSM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE INCOME YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3150-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   COMPUTE TRSM-PRINCIPAL-AMT ROUNDED = TRSM-INCOME-YTD
                       * TRSM-MARGIN / 200
               WHEN TRSM-REASON-CD = 'H'
                   MOVE 5.00 TO TRSM-PRINCIPAL-AMT
               WHEN TRSM-REASON-CD = 'M'
                   MOVE 3.00 TO TRSM-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE TRSM-PRINCIPAL-AMT ROUNDED = TRSM-INCOME-YTD
                       * TRSM-MARGIN / 100
           END-EVALUATE
           IF TRSM-PRINCIPAL-AMT > 0 AND TRSM-PRINCIPAL-AMT < 2.00
               MOVE 2.00 TO TRSM-PRINCIPAL-AMT
           END-IF
           IF TRSM-PRINCIPAL-AMT > 25.00
               MOVE 25.00 TO TRSM-PRINCIPAL-AMT
           END-IF
           ADD TRSM-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE TAX FEE ON THE TRUSTEE FEE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3160-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   COMPUTE TRSM-TAX-AMT ROUNDED = TRSM-TRUSTEE-FEE-YTD *
                       TRSM-MARGIN / 200
               WHEN TRSM-DIST-FREQ-CD = 'E'
                   MOVE 12.00 TO TRSM-TAX-AMT
               WHEN TRSM-DIST-FREQ-CD = 'K'
                   MOVE 7.00 TO TRSM-TAX-AMT
               WHEN OTHER
                   COMPUTE TRSM-TAX-AMT ROUNDED = TRSM-TRUSTEE-FEE-YTD *
                       TRSM-MARGIN / 100
           END-EVALUATE
           IF TRSM-TAX-AMT > 0 AND TRSM-TAX-AMT < 10.00
               MOVE 10.00 TO TRSM-TAX-AMT
           END-IF
           IF TRSM-TAX-AMT > 250.00
               MOVE 250.00 TO TRSM-TAX-AMT
           END-IF
           ADD TRSM-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TRUST TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3170-DESCRIBE-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MNG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VFN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XPL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE TIER TIER FOR THE INCOME YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3180-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-TIER-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRSM-INCOME-YTD NOT < WS-TRUSTEE-TIER-LOW(WS-TX)
                   MOVE WS-TRUSTEE-TIER-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * H -> M WHEN MATURITY DATE IS PAST
      * M -> T WHEN MATURITY DATE IS PAST
      * T -> H WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3190-SET-REASON.
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
      * INCOME AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3200-FLAG-INCOME.
           IF TRSM-INCOME-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRSM-INCOME-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IN -> DS WHEN EXPIRY DATE IS PAST
      * DS -> FE WHEN EXPIRY DATE IS PAST
      * FE -> RB WHEN EXPIRY DATE IS PAST
      * RB -> TX WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3210-SET-TRAN.
           EVALUATE TRST-TRAN-CD
               WHEN 'IN'
                   IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DS' TO TRST-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DS'
                   IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FE' TO TRST-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FE'
                   IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RB' TO TRST-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO TRST-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INCOME GRID TIER FOR THE MARKET AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3220-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-GRID-RATE(1) TO TRSM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TRSM-MARKET-AMT NOT < WS-INCOME-GRID-LOW(WS-TX)
                   MOVE WS-INCOME-GRID-RATE(WS-TX) TO TRSM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3230-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO TRST-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3240-DESCRIBE-CHANNEL.
           EVALUATE TRST-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TRUSTEE FEE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3250-ACCRUE-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TRUSTEE-FEE-AMT * TRSM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRSM-MARKET-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3260-AGE-EFFECTIVE.
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
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3270-FLAG-PRINCIPAL.
           IF TRSM-PRINCIPAL-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRSM-PRINCIPAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3280-FLAG-DISTRIB.
           IF TRSM-DISTRIB-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRSM-DISTRIB-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3290-ACCRUE-TAX.
           IF TRSM-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TAX-YTD * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRSM-DISTRIB-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3300-CHECK-NOTICE.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO TRSM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARKET AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3310-LIMIT-MARKET.
           IF TRSM-MARKET-AMT < 50.00
               MOVE 50.00 TO TRSM-MARKET-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-MARKET-AMT > 500.00
               MOVE 500.00 TO TRSM-MARKET-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3320-ACCRUE-TAX.
           IF TRSM-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TAX-AMT * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRSM-TRUSTEE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE TIER TIER FOR THE TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-TIER-RATE(1) TO TRSM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRSM-TAX-YTD NOT < WS-TRUSTEE-TIER-LOW(WS-TX)
                   MOVE WS-TRUSTEE-TIER-RATE(WS-TX) TO TRSM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E106 NAME REQUIRED
      * E114 WORK AMT NEGATIVE
      * E186 WORK AMT OVER 5000
      * E704 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3340-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-NAME = SPACES
                   MOVE 'E106' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E114' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E186' TO WS-REJECT-CODE
               WHEN TRSM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E704' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TRUSTEE FEE AMT BY DIST FREQ CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3350-ACCUM-DIST-FREQ.
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
      * TRANSLATE TRUST TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3360-DESCRIBE-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MNG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VFN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XPL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-EXPIRY.
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
                   ADD TRSM-INCOME-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRSM-INCOME-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-INCOME-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-INCOME-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-INCOME-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EH -> GG WHEN LAST MAINT DATE IS PAST
      * GG -> JG WHEN LAST MAINT DATE IS PAST
      * JG -> EH WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3380-SET-STATUS.
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
      * SPLIT TRUSTEE FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3390-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = TRSM-TRUSTEE-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRSM-TRUSTEE-FEE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E891 DESC REQUIRED
      * E340 TAX AMT NEGATIVE
      * E772 TAX AMT OVER 10000
      * E356 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3400-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-DESC = SPACES
                   MOVE 'E891' TO WS-REJECT-CODE
               WHEN TRSM-TAX-AMT < 0
                   MOVE 'E340' TO WS-REJECT-CODE
               WHEN TRSM-TAX-AMT > 10000.00
                   MOVE 'E772' TO WS-REJECT-CODE
               WHEN TRSM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E356' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3410-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-STATUS-CD
                   ADD TRSM-DISTRIB-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-DISTRIB-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3420-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE TRST-TRUSTEE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRUSTEE FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3430-LIMIT-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-YTD < 25.00
               MOVE 25.00 TO TRSM-TRUSTEE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-TRUSTEE-FEE-YTD > 2500.00
               MOVE 2500.00 TO TRSM-TRUSTEE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3440-ACCRUE-DISTRIB.
           IF TRSM-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-DISTRIB-AMT * TRSM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRSM-MARKET-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE TAX YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3450-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-STOP-FLAG = 'Y'
                   COMPUTE TRSM-PRINCIPAL-AMT ROUNDED = TRSM-TAX-YTD *
                       TRSM-DISCOUNT / 200
               WHEN TRSM-DIST-FREQ-CD = 'E'
                   MOVE 20.00 TO TRSM-PRINCIPAL-AMT
               WHEN TRSM-DIST-FREQ-CD = 'K'
                   MOVE 5.00 TO TRSM-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE TRSM-PRINCIPAL-AMT ROUNDED = TRSM-TAX-YTD *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-PRINCIPAL-AMT > 0 AND TRSM-PRINCIPAL-AMT < 1.00
               MOVE 1.00 TO TRSM-PRINCIPAL-AMT
           END-IF
           IF TRSM-PRINCIPAL-AMT > 100.00
               MOVE 100.00 TO TRSM-PRINCIPAL-AMT
           END-IF
           ADD TRSM-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E073 REF NO REQUIRED
      * E258 MARKET AMT NEGATIVE
      * E257 MARKET AMT OVER 1000000
      * E573 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3460-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-REF-NO = SPACES
                   MOVE 'E073' TO WS-REJECT-CODE
               WHEN TRSM-MARKET-AMT < 0
                   MOVE 'E258' TO WS-REJECT-CODE
               WHEN TRSM-MARKET-AMT > 1000000.00
                   MOVE 'E257' TO WS-REJECT-CODE
               WHEN TRSM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E573' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INCOME GRID TIER FOR THE TRUSTEE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3470-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-GRID-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TRST-TRUSTEE-FEE-AMT NOT < WS-INCOME-GRID-LOW(WS-TX)
                   MOVE WS-INCOME-GRID-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISTRIB AMT.
      *----------------------------------------------------------------*
       3480-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-DISTRIB-AMT ROUNDED =
                   TRSM-TAX-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-DISTRIB-AMT
           END-IF
           ADD TRSM-DISTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE TIER TIER FOR THE TRUSTEE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3490-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-TIER-RATE(1) TO TRSM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRSM-TRUSTEE-FEE-YTD NOT < WS-TRUSTEE-TIER-LOW(WS-TX)
                   MOVE WS-TRUSTEE-TIER-RATE(WS-TX) TO TRSM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INCOME AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3500-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = TRST-INCOME-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRST-INCOME-AMT - (WS-WORK-AMT * 4)
           COMPUTE TRSM-DISTRIB-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-PAYMENT.
           IF TRSM-PAYMENT-CNT < 9999999
               ADD 1 TO TRSM-PAYMENT-CNT
           END-IF
           IF TRSM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO TRSM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE INCOME YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3520-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-STOP-FLAG = 'Y'
                   COMPUTE TRSM-PRINCIPAL-YTD ROUNDED = TRSM-INCOME-YTD
                       * TRSM-RATE / 200
               WHEN TRSM-STATUS-CD = 'EH'
                   MOVE 10.00 TO TRSM-PRINCIPAL-YTD
               WHEN TRSM-STATUS-CD = 'GG'
                   MOVE 7.00 TO TRSM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE TRSM-PRINCIPAL-YTD ROUNDED = TRSM-INCOME-YTD
                       * TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-PRINCIPAL-YTD > 0 AND TRSM-PRINCIPAL-YTD < 10.00
               MOVE 10.00 TO TRSM-PRINCIPAL-YTD
           END-IF
           IF TRSM-PRINCIPAL-YTD > 25.00
               MOVE 25.00 TO TRSM-PRINCIPAL-YTD
           END-IF
           ADD TRSM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3530-DESCRIBE-TRAN.
           EVALUATE TRST-TRAN-CD
               WHEN 'IN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'DS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE TAX YTD FEE ON THE TRUSTEE FEE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3540-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TAX-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   COMPUTE TRSM-TAX-YTD ROUNDED = TRST-TRUSTEE-FEE-AMT *
                       TRSM-RATE / 200
               WHEN TRSM-STATUS-CD = 'EH'
                   MOVE 20.00 TO TRSM-TAX-YTD
               WHEN TRSM-STATUS-CD = 'GG'
                   MOVE 12.00 TO TRSM-TAX-YTD
               WHEN OTHER
                   COMPUTE TRSM-TAX-YTD ROUNDED = TRST-TRUSTEE-FEE-AMT *
                       TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-TAX-YTD > 0 AND TRSM-TAX-YTD < 2.00
               MOVE 2.00 TO TRSM-TAX-YTD
           END-IF
           IF TRSM-TAX-YTD > 75.00
               MOVE 75.00 TO TRSM-TAX-YTD
           END-IF
           ADD TRSM-TAX-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INCOME AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3550-ACCRUE-INCOME.
           IF TRST-INCOME-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRST-INCOME-AMT * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRSM-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARKET YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3560-FLAG-MARKET.
           IF TRSM-MARKET-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRSM-MARKET-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INCOME AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3570-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-REASON-CD
                   ADD TRST-INCOME-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRST-INCOME-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MARKET YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-STATUS-CD
                   ADD TRSM-MARKET-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-MARKET-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TRUSTEE FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3590-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = TRST-TRUSTEE-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TRST-TRUSTEE-FEE-AMT - (WS-WORK-AMT *
               2)
           COMPUTE TRSM-PRINCIPAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRUSTEE FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3600-FLAG-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRSM-TRUSTEE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3610-SET-CHANNEL.
           EVALUATE TRST-CHANNEL-CD
               WHEN 'BRN'
                   IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INCOME AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3620-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-REGION-CD
                   ADD TRST-INCOME-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRST-INCOME-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E352 REF NO REQUIRED
      * E069 TRUSTEE FEE YTD NEGATIVE
      * E939 TRUSTEE FEE YTD OVER 1000000
      * E588 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3630-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-REF-NO = SPACES
                   MOVE 'E352' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-YTD < 0
                   MOVE 'E069' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-YTD > 1000000.00
                   MOVE 'E939' TO WS-REJECT-CODE
               WHEN TRSM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E588' TO WS-REJECT-CODE
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
       3640-DESCRIBE-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'D'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3650-AGE-LAST-PAY.
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
                   ADD TRSM-TRUSTEE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD TRSM-TRUSTEE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-TRUSTEE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-TRUSTEE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-TRUSTEE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INCOME YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-INCOME.
           IF TRSM-INCOME-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-INCOME-YTD * TRSM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3670-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = TRST-DISTRIB-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TRST-DISTRIB-AMT - (WS-WORK-AMT * 12)
           COMPUTE TRST-TRUSTEE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MARKET YTD FEE ON THE MARKET AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3680-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TRSM-MARKET-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   COMPUTE TRSM-MARKET-YTD ROUNDED = TRSM-MARKET-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRST-TRAN-CD = 'IN'
                   MOVE 7.00 TO TRSM-MARKET-YTD
               WHEN TRST-TRAN-CD = 'DS'
                   MOVE 15.00 TO TRSM-MARKET-YTD
               WHEN OTHER
                   COMPUTE TRSM-MARKET-YTD ROUNDED = TRSM-MARKET-AMT *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-MARKET-YTD > 0 AND TRSM-MARKET-YTD < 1.00
               MOVE 1.00 TO TRSM-MARKET-YTD
           END-IF
           IF TRSM-MARKET-YTD > 25.00
               MOVE 25.00 TO TRSM-MARKET-YTD
           END-IF
           ADD TRSM-MARKET-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INCOME AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3690-ACCRUE-INCOME.
           IF TRST-INCOME-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRST-INCOME-AMT * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRST-TRUSTEE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3700-FLAG-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRST-PRINCIPAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3710-ACCRUE-DISTRIB.
           IF TRSM-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-DISTRIB-AMT * TRSM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TRST-INCOME-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3720-CHECK-EXPIRY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO TRST-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INCOME AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3730-FLAG-INCOME.
           IF TRST-INCOME-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRST-INCOME-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3740-ACCRUE-TAX.
           IF TRSM-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TAX-YTD * TRSM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRSM-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3750-AGE-MATURITY.
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
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3760-LIMIT-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT < 0.00
               MOVE 0.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-PRINCIPAL-AMT > 2500.00
               MOVE 2500.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E747 DESC REQUIRED
      * E826 DISTRIB AMT NEGATIVE
      * E550 DISTRIB AMT OVER 10000
      * E945 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3770-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-DESC = SPACES
                   MOVE 'E747' TO WS-REJECT-CODE
               WHEN TRST-DISTRIB-AMT < 0
                   MOVE 'E826' TO WS-REJECT-CODE
               WHEN TRST-DISTRIB-AMT > 10000.00
                   MOVE 'E550' TO WS-REJECT-CODE
               WHEN TRSM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E945' TO WS-REJECT-CODE
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
       3780-ACCRUE-DISTRIB.
           IF TRSM-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-DISTRIB-AMT * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRSM-DISTRIB-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INCOME AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3790-LIMIT-INCOME.
           IF TRST-INCOME-AMT < 50.00
               MOVE 50.00 TO TRST-INCOME-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-INCOME-AMT > 5000.00
               MOVE 5000.00 TO TRST-INCOME-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3800-DESCRIBE-STATUS.
           EVALUATE TRSM-STATUS-CD
               WHEN 'EH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3810-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-STATUS-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EN -> FY WHEN NOTICE DATE IS PAST
      * FY -> HP WHEN NOTICE DATE IS PAST
      * HP -> JB WHEN NOTICE DATE IS PAST
      * JB -> KZ WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3820-SET-CYCLE.
           EVALUATE TRSM-CYCLE-CD
               WHEN 'EN'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FY' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FY'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HP' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HP'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JB' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JB'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KZ' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRUSTEE FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3830-LIMIT-TRUSTEE-FEE.
           IF TRST-TRUSTEE-FEE-AMT < 0.00
               MOVE 0.00 TO TRST-TRUSTEE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-TRUSTEE-FEE-AMT > 1000.00
               MOVE 1000.00 TO TRST-TRUSTEE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3840-AGE-NOTICE.
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
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3850-AGE-EFFECTIVE.
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
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3860-COUNT-NOTICE.
           IF TRSM-NOTICE-CNT < 9999999
               ADD 1 TO TRSM-NOTICE-CNT
           END-IF
           IF TRSM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO TRSM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3870-DESCRIBE-REASON.
           EVALUATE TRSM-REASON-CD
               WHEN 'H'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3880-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE TRSM-TRUSTEE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE INCOME AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3890-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TRSM-INCOME-AMT *
                       TRSM-RATE / 200
               WHEN TRSM-REGION-CD = 'A'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN TRSM-REGION-CD = 'D'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TRSM-INCOME-AMT *
                       TRSM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE TRUSTEE FEE AMT BY TRUST TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3900-ACCUM-TRUST-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-TRUST-TYPE-CD
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-TRUST-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRST-TRUSTEE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3910-ACCRUE-DISTRIB.
           IF TRST-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRST-DISTRIB-AMT * TRSM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRST-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRUSTEE FEE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3920-FLAG-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRSM-TRUSTEE-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3930-LIMIT-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT < 100.00
               MOVE 100.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-PRINCIPAL-AMT > 5000.00
               MOVE 5000.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3940-LIMIT-PRINCIPAL.
           IF TRSM-PRINCIPAL-YTD < 100.00
               MOVE 100.00 TO TRSM-PRINCIPAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-PRINCIPAL-YTD > 50000.00
               MOVE 50000.00 TO TRSM-PRINCIPAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRUSTEE FEE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3950-FLAG-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRSM-TRUSTEE-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E171 CUST ID REQUIRED
      * E073 MARKET AMT NEGATIVE
      * E887 MARKET AMT OVER 25000
      * E988 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-CUST-ID = SPACES
                   MOVE 'E171' TO WS-REJECT-CODE
               WHEN TRSM-MARKET-AMT < 0
                   MOVE 'E073' TO WS-REJECT-CODE
               WHEN TRSM-MARKET-AMT > 25000.00
                   MOVE 'E887' TO WS-REJECT-CODE
               WHEN TRST-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E988' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE TIER TIER FOR THE TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-TIER-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRSM-TAX-YTD NOT < WS-TRUSTEE-TIER-LOW(WS-TX)
                   MOVE WS-TRUSTEE-TIER-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TRUSTEE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3980-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TRST-TRUSTEE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'D'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE TIER TIER FOR THE TRUSTEE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4000-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-TIER-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRST-TRUSTEE-FEE-AMT NOT < WS-TRUSTEE-TIER-LOW(WS-TX)
                   MOVE WS-TRUSTEE-TIER-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRUSTEE FEE FEE ON THE DISTRIB AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4010-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TRUSTEE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRSM-TRUSTEE-FEE-AMT ROUNDED =
                       TRST-DISTRIB-AMT * TRSM-MARGIN / 200
               WHEN TRSM-REGION-CD = 'A'
                   MOVE 7.00 TO TRSM-TRUSTEE-FEE-AMT
               WHEN TRSM-REGION-CD = 'D'
                   MOVE 10.00 TO TRSM-TRUSTEE-FEE-AMT
               WHEN OTHER
                   COMPUTE TRSM-TRUSTEE-FEE-AMT ROUNDED =
                       TRST-DISTRIB-AMT * TRSM-MARGIN / 100
           END-EVALUATE
           IF TRSM-TRUSTEE-FEE-AMT > 0 AND TRSM-TRUSTEE-FEE-AMT < 2.00
               MOVE 2.00 TO TRSM-TRUSTEE-FEE-AMT
           END-IF
           IF TRSM-TRUSTEE-FEE-AMT > 250.00
               MOVE 250.00 TO TRSM-TRUSTEE-FEE-AMT
           END-IF
           ADD TRSM-TRUSTEE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE INCOME GRID TIER FOR THE TRUSTEE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4020-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-GRID-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TRSM-TRUSTEE-FEE-AMT NOT < WS-INCOME-GRID-LOW(WS-TX)
                   MOVE WS-INCOME-GRID-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INCOME AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4030-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-CHANNEL-CD
                   ADD TRST-INCOME-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRST-INCOME-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4040-CHECK-EXPIRY.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO TRSM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB YTD FEE ON THE TRUSTEE FEE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4050-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRSM-DISTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   COMPUTE TRSM-DISTRIB-YTD ROUNDED =
                       TRST-TRUSTEE-FEE-AMT * TRSM-RATE / 200
               WHEN TRSM-REGION-CD = 'A'
                   MOVE 10.00 TO TRSM-DISTRIB-YTD
               WHEN TRSM-REGION-CD = 'D'
                   MOVE 10.00 TO TRSM-DISTRIB-YTD
               WHEN OTHER
                   COMPUTE TRSM-DISTRIB-YTD ROUNDED =
                       TRST-TRUSTEE-FEE-AMT * TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-DISTRIB-YTD > 0 AND TRSM-DISTRIB-YTD < 1.00
               MOVE 1.00 TO TRSM-DISTRIB-YTD
           END-IF
           IF TRSM-DISTRIB-YTD > 35.00
               MOVE 35.00 TO TRSM-DISTRIB-YTD
           END-IF
           ADD TRSM-DISTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE INCOME AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TRUSTEE FEE AMT.
      *----------------------------------------------------------------*
       4060-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRST-TRUSTEE-FEE-AMT ROUNDED =
                   TRSM-INCOME-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRST-TRUSTEE-FEE-AMT
           END-IF
           ADD TRST-TRUSTEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB YTD FEE ON THE DISTRIB AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4070-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-DISTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   COMPUTE TRSM-DISTRIB-YTD ROUNDED = TRSM-DISTRIB-AMT *
                       TRSM-MARGIN / 200
               WHEN TRSM-TRUST-TYPE-CD = 'FLJ'
                   MOVE 12.00 TO TRSM-DISTRIB-YTD
               WHEN TRSM-TRUST-TYPE-CD = 'MNG'
                   MOVE 3.00 TO TRSM-DISTRIB-YTD
               WHEN OTHER
                   COMPUTE TRSM-DISTRIB-YTD ROUNDED = TRSM-DISTRIB-AMT *
                       TRSM-MARGIN / 100
           END-EVALUATE
           IF TRSM-DISTRIB-YTD > 0 AND TRSM-DISTRIB-YTD < 5.00
               MOVE 5.00 TO TRSM-DISTRIB-YTD
           END-IF
           IF TRSM-DISTRIB-YTD > 35.00
               MOVE 35.00 TO TRSM-DISTRIB-YTD
           END-IF
           ADD TRSM-DISTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E229 REF NO REQUIRED
      * E174 DISTRIB YTD NEGATIVE
      * E813 DISTRIB YTD OVER 1000000
      * E823 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4080-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-REF-NO = SPACES
                   MOVE 'E229' TO WS-REJECT-CODE
               WHEN TRSM-DISTRIB-YTD < 0
                   MOVE 'E174' TO WS-REJECT-CODE
               WHEN TRSM-DISTRIB-YTD > 1000000.00
                   MOVE 'E813' TO WS-REJECT-CODE
               WHEN TRSM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E823' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MARKET YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-TRAN-CD
                   ADD TRSM-MARKET-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-MARKET-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TRUSTEE FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4100-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = TRSM-TRUSTEE-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TRSM-TRUSTEE-FEE-YTD - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4110-DESCRIBE-TRAN.
           EVALUATE TRST-TRAN-CD
               WHEN 'IN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INCOME FEE ON THE TAX YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4120-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TRST-INCOME-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-STOP-FLAG = 'Y'
                   COMPUTE TRST-INCOME-AMT ROUNDED = TRSM-TAX-YTD *
                       TRSM-DISCOUNT / 200
               WHEN TRST-TRAN-CD = 'IN'
                   MOVE 5.00 TO TRST-INCOME-AMT
               WHEN TRST-TRAN-CD = 'DS'
                   MOVE 15.00 TO TRST-INCOME-AMT
               WHEN OTHER
                   COMPUTE TRST-INCOME-AMT ROUNDED = TRSM-TAX-YTD *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRST-INCOME-AMT > 0 AND TRST-INCOME-AMT < 5.00
               MOVE 5.00 TO TRST-INCOME-AMT
           END-IF
           IF TRST-INCOME-AMT > 250.00
               MOVE 250.00 TO TRST-INCOME-AMT
           END-IF
           ADD TRST-INCOME-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE TIER TIER FOR THE TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4130-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-TIER-RATE(1) TO TRSM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRSM-TAX-YTD NOT < WS-TRUSTEE-TIER-LOW(WS-TX)
                   MOVE WS-TRUSTEE-TIER-RATE(WS-TX) TO TRSM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINCIPAL YTD.
      *----------------------------------------------------------------*
       4140-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-PRINCIPAL-YTD ROUNDED =
                   TRSM-DISTRIB-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-PRINCIPAL-YTD
           END-IF
           ADD TRSM-PRINCIPAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4150-COUNT-CYCLE.
           IF TRSM-CYCLE-CNT < 9999999
               ADD 1 TO TRSM-CYCLE-CNT
           END-IF
           IF TRSM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO TRSM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4160-CHECK-POST.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO TRST-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E213 OPERATOR REQUIRED
      * E312 DISTRIB AMT NEGATIVE
      * E816 DISTRIB AMT OVER 1000000
      * E061 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4170-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-OPERATOR = SPACES
                   MOVE 'E213' TO WS-REJECT-CODE
               WHEN TRST-DISTRIB-AMT < 0
                   MOVE 'E312' TO WS-REJECT-CODE
               WHEN TRST-DISTRIB-AMT > 1000000.00
                   MOVE 'E816' TO WS-REJECT-CODE
               WHEN TRSM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E061' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4180-AGE-LAST-STMT.
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
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4190-AGE-POST.
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
               WHEN WS-DAYS NOT > 60
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4200-ACCRUE-PRINCIPAL.
           IF TRSM-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-PRINCIPAL-AMT * TRSM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TRSM-INCOME-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IN -> DS WHEN LAST MAINT DATE IS PAST
      * DS -> FE WHEN LAST MAINT DATE IS PAST
      * FE -> RB WHEN LAST MAINT DATE IS PAST
      * RB -> TX WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4210-SET-TRAN.
           EVALUATE TRST-TRAN-CD
               WHEN 'IN'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DS' TO TRST-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DS'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FE' TO TRST-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FE'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RB' TO TRST-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TX' TO TRST-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DISTRIB AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4220-FLAG-DISTRIB.
           IF TRST-DISTRIB-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRST-DISTRIB-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4230-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TRSM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRSM-MARKET-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4240-ACCRUE-PRINCIPAL.
           IF TRSM-PRINCIPAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-PRINCIPAL-YTD * TRSM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRST-TRUSTEE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4250-DESCRIBE-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'D'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE DIST FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4260-DESCRIBE-DIST-FREQ.
           EVALUATE TRSM-DIST-FREQ-CD
               WHEN 'E'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'P'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4270-DESCRIBE-CYCLE.
           EVALUATE TRSM-CYCLE-CD
               WHEN 'EN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'LD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       4280-AGE-OPEN.
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
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INCOME YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4290-FLAG-INCOME.
           IF TRSM-INCOME-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-INCOME-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4300-CHECK-CLOSE.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO TRSM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * H -> M WHEN NOTICE DATE IS PAST
      * M -> T WHEN NOTICE DATE IS PAST
      * T -> H WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4310-SET-REASON.
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
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4320-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO TRST-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INCOME AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4330-LIMIT-INCOME.
           IF TRSM-INCOME-AMT < 100.00
               MOVE 100.00 TO TRSM-INCOME-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-INCOME-AMT > 500.00
               MOVE 500.00 TO TRSM-INCOME-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4340-DESCRIBE-CHANNEL.
           EVALUATE TRST-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4350-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE TRSM-TRUSTEE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4360-ACCRUE-DISTRIB.
           IF TRSM-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-DISTRIB-AMT * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRSM-TAX-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE TIER TIER FOR THE MARKET AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4370-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-TIER-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRSM-MARKET-AMT NOT < WS-TRUSTEE-TIER-LOW(WS-TX)
                   MOVE WS-TRUSTEE-TIER-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAX YTD FEE ON THE DISTRIB YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4380-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TAX-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   COMPUTE TRSM-TAX-YTD ROUNDED = TRSM-DISTRIB-YTD *
                       TRSM-DISCOUNT / 200
               WHEN TRSM-TRUST-TYPE-CD = 'FLJ'
                   MOVE 15.00 TO TRSM-TAX-YTD
               WHEN TRSM-TRUST-TYPE-CD = 'MNG'
                   MOVE 3.00 TO TRSM-TAX-YTD
               WHEN OTHER
                   COMPUTE TRSM-TAX-YTD ROUNDED = TRSM-DISTRIB-YTD *
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
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4390-DESCRIBE-CYCLE.
           EVALUATE TRSM-CYCLE-CD
               WHEN 'EN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4400-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-TRAN-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRUST TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4410-DESCRIBE-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MNG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VFN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XPL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB FEE ON THE INCOME AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4420-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TRSM-DISTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRSM-DISTRIB-AMT ROUNDED = TRST-INCOME-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRSM-REGION-CD = 'A'
                   MOVE 5.00 TO TRSM-DISTRIB-AMT
               WHEN TRSM-REGION-CD = 'D'
                   MOVE 10.00 TO TRSM-DISTRIB-AMT
               WHEN OTHER
                   COMPUTE TRSM-DISTRIB-AMT ROUNDED = TRST-INCOME-AMT *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-DISTRIB-AMT > 0 AND TRSM-DISTRIB-AMT < 5.00
               MOVE 5.00 TO TRSM-DISTRIB-AMT
           END-IF
           IF TRSM-DISTRIB-AMT > 35.00
               MOVE 35.00 TO TRSM-DISTRIB-AMT
           END-IF
           ADD TRSM-DISTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4430-COUNT-NOTICE.
           IF TRSM-NOTICE-CNT < 9999999
               ADD 1 TO TRSM-NOTICE-CNT
           END-IF
           IF TRSM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E086 MEMO REQUIRED
      * E607 PRINCIPAL YTD NEGATIVE
      * E477 PRINCIPAL YTD OVER 50000
      * E617 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4440-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-MEMO = SPACES
                   MOVE 'E086' TO WS-REJECT-CODE
               WHEN TRSM-PRINCIPAL-YTD < 0
                   MOVE 'E607' TO WS-REJECT-CODE
               WHEN TRSM-PRINCIPAL-YTD > 50000.00
                   MOVE 'E477' TO WS-REJECT-CODE
               WHEN TRSM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E617' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E441 MEMO REQUIRED
      * E985 TRUSTEE FEE AMT NEGATIVE
      * E832 TRUSTEE FEE AMT OVER 25000
      * E992 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4450-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-MEMO = SPACES
                   MOVE 'E441' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-AMT < 0
                   MOVE 'E985' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-AMT > 25000.00
                   MOVE 'E832' TO WS-REJECT-CODE
               WHEN TRSM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E992' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TRUSTEE FEE AMT.
      *----------------------------------------------------------------*
       4460-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRST-TRUSTEE-FEE-AMT ROUNDED =
                   TRST-DISTRIB-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRST-TRUSTEE-FEE-AMT
           END-IF
           ADD TRST-TRUSTEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4470-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE TRST-DISTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRUSTEE FEE YTD FEE ON THE TRUSTEE FEE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4480-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TRUSTEE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED =
                       TRST-TRUSTEE-FEE-AMT * TRSM-DISCOUNT / 200
               WHEN TRSM-DIST-FREQ-CD = 'E'
                   MOVE 15.00 TO TRSM-TRUSTEE-FEE-YTD
               WHEN TRSM-DIST-FREQ-CD = 'K'
                   MOVE 20.00 TO TRSM-TRUSTEE-FEE-YTD
               WHEN OTHER
                   COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED =
                       TRST-TRUSTEE-FEE-AMT * TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-TRUSTEE-FEE-YTD > 0 AND TRSM-TRUSTEE-FEE-YTD < 2.00
               MOVE 2.00 TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           IF TRSM-TRUSTEE-FEE-YTD > 75.00
               MOVE 75.00 TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           ADD TRSM-TRUSTEE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE TRUSTEE FEE YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4490-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TRUSTEE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED = WS-WORK-AMT-2
                       * TRSM-MARGIN / 200
               WHEN TRST-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO TRSM-TRUSTEE-FEE-YTD
               WHEN TRST-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO TRSM-TRUSTEE-FEE-YTD
               WHEN OTHER
                   COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED = WS-WORK-AMT-2
                       * TRSM-MARGIN / 100
           END-EVALUATE
           IF TRSM-TRUSTEE-FEE-YTD > 0 AND TRSM-TRUSTEE-FEE-YTD < 10.00
               MOVE 10.00 TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           IF TRSM-TRUSTEE-FEE-YTD > 25.00
               MOVE 25.00 TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           ADD TRSM-TRUSTEE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4500-DESCRIBE-STATUS.
           EVALUATE TRSM-STATUS-CD
               WHEN 'EH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4510-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = TRST-DISTRIB-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TRST-DISTRIB-AMT - (WS-WORK-AMT * 2)
           COMPUTE TRSM-DISTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4520-CHECK-LAST-MAINT.
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
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4530-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TRSM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRST-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4540-CHECK-POST.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO TRSM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4550-COUNT-PAYMENT.
           IF TRSM-PAYMENT-CNT < 9999999
               ADD 1 TO TRSM-PAYMENT-CNT
           END-IF
           IF TRSM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO TRSM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4560-FLAG-PRINCIPAL.
           IF TRSM-PRINCIPAL-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-PRINCIPAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE DISTRIB YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4570-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRSM-PRINCIPAL-YTD ROUNDED = TRSM-DISTRIB-YTD
                       * TRSM-MARGIN / 200
               WHEN TRSM-DIST-FREQ-CD = 'E'
                   MOVE 5.00 TO TRSM-PRINCIPAL-YTD
               WHEN TRSM-DIST-FREQ-CD = 'K'
                   MOVE 12.00 TO TRSM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE TRSM-PRINCIPAL-YTD ROUNDED = TRSM-DISTRIB-YTD
                       * TRSM-MARGIN / 100
           END-EVALUATE
           IF TRSM-PRINCIPAL-YTD > 0 AND TRSM-PRINCIPAL-YTD < 1.00
               MOVE 1.00 TO TRSM-PRINCIPAL-YTD
           END-IF
           IF TRSM-PRINCIPAL-YTD > 75.00
               MOVE 75.00 TO TRSM-PRINCIPAL-YTD
           END-IF
           ADD TRSM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E922 REF NO REQUIRED
      * E668 INCOME YTD NEGATIVE
      * E552 INCOME YTD OVER 250000
      * E371 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4580-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-REF-NO = SPACES
                   MOVE 'E922' TO WS-REJECT-CODE
               WHEN TRSM-INCOME-YTD < 0
                   MOVE 'E668' TO WS-REJECT-CODE
               WHEN TRSM-INCOME-YTD > 250000.00
                   MOVE 'E552' TO WS-REJECT-CODE
               WHEN TRSM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E371' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4590-DESCRIBE-CHANNEL.
           EVALUATE TRST-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4600-DESCRIBE-CYCLE.
           EVALUATE TRSM-CYCLE-CD
               WHEN 'EN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE MARKET YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4610-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TRSM-MARKET-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE INCOME FEE ON THE TRUSTEE FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4620-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TRST-INCOME-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   COMPUTE TRST-INCOME-AMT ROUNDED =
                       TRSM-TRUSTEE-FEE-AMT * TRSM-DISCOUNT / 200
               WHEN TRST-TRAN-CD = 'IN'
                   MOVE 10.00 TO TRST-INCOME-AMT
               WHEN TRST-TRAN-CD = 'DS'
                   MOVE 15.00 TO TRST-INCOME-AMT
               WHEN OTHER
                   COMPUTE TRST-INCOME-AMT ROUNDED =
                       TRSM-TRUSTEE-FEE-AMT * TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRST-INCOME-AMT > 0 AND TRST-INCOME-AMT < 1.00
               MOVE 1.00 TO TRST-INCOME-AMT
           END-IF
           IF TRST-INCOME-AMT > 250.00
               MOVE 250.00 TO TRST-INCOME-AMT
           END-IF
           ADD TRST-INCOME-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE TIER TIER FOR THE TRUSTEE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4630-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-TIER-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRSM-TRUSTEE-FEE-YTD NOT < WS-TRUSTEE-TIER-LOW(WS-TX)
                   MOVE WS-TRUSTEE-TIER-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DIST FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4640-DESCRIBE-DIST-FREQ.
           EVALUATE TRSM-DIST-FREQ-CD
               WHEN 'E'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4650-AGE-POST.
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
                   ADD TRSM-TRUSTEE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-TRUSTEE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRSM-TRUSTEE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-TRUSTEE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-TRUSTEE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NOTICE DATE IS PAST
      * BAT -> MAL WHEN NOTICE DATE IS PAST
      * MAL -> TEL WHEN NOTICE DATE IS PAST
      * TEL -> ONL WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4660-SET-CHANNEL.
           EVALUATE TRST-CHANNEL-CD
               WHEN 'BRN'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4670-ACCRUE-DISTRIB.
           IF TRST-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRST-DISTRIB-AMT * TRSM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRSM-TRUSTEE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4680-DESCRIBE-CYCLE.
           EVALUATE TRSM-CYCLE-CD
               WHEN 'EN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KZ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST STMT DATE IS PAST
      * BAT -> MAL WHEN LAST STMT DATE IS PAST
      * MAL -> TEL WHEN LAST STMT DATE IS PAST
      * TEL -> ONL WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4690-SET-CHANNEL.
           EVALUATE TRST-CHANNEL-CD
               WHEN 'BRN'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INCOME GRID TIER FOR THE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4700-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-GRID-RATE(1) TO TRSM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TRSM-TAX-AMT NOT < WS-INCOME-GRID-LOW(WS-TX)
                   MOVE WS-INCOME-GRID-RATE(WS-TX) TO TRSM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4710-COUNT-NOTICE.
           IF TRSM-NOTICE-CNT < 9999999
               ADD 1 TO TRSM-NOTICE-CNT
           END-IF
           IF TRSM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO TRSM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4720-AGE-CLOSE.
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
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE MARKET YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4730-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-TRAN-CD
                   ADD TRSM-MARKET-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-MARKET-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4740-SET-CHANNEL.
           EVALUATE TRST-CHANNEL-CD
               WHEN 'BRN'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4750-DESCRIBE-CHANNEL.
           EVALUATE TRST-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4760-COUNT-RETRY.
           IF TRSM-RETRY-CNT < 9999999
               ADD 1 TO TRSM-RETRY-CNT
           END-IF
           IF TRSM-RETRY-CNT NOT < 3
               MOVE 'Y' TO TRSM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4770-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = TRST-PRINCIPAL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRST-PRINCIPAL-AMT - (WS-WORK-AMT *
               4)
           COMPUTE TRSM-INCOME-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E565 REJECT CD REQUIRED
      * E418 TAX AMT NEGATIVE
      * E041 TAX AMT OVER 25000
      * E071 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4780-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-REJECT-CD = SPACES
                   MOVE 'E565' TO WS-REJECT-CODE
               WHEN TRSM-TAX-AMT < 0
                   MOVE 'E418' TO WS-REJECT-CODE
               WHEN TRSM-TAX-AMT > 25000.00
                   MOVE 'E041' TO WS-REJECT-CODE
               WHEN TRSM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E071' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TRUSTEE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INCOME YTD.
      *----------------------------------------------------------------*
       4790-PRORATE-INCOME.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-INCOME-YTD ROUNDED =
                   TRSM-TRUSTEE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-INCOME-YTD
           END-IF
           ADD TRSM-INCOME-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4800-LIMIT-PRINCIPAL.
           IF TRSM-PRINCIPAL-AMT < 0.00
               MOVE 0.00 TO TRSM-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-PRINCIPAL-AMT > 5000.00
               MOVE 5000.00 TO TRSM-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4810-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TRSM-PRINCIPAL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4820-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TRSM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRSM-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4830-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TRSM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRSM-TRUSTEE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAX YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4840-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = TRSM-TAX-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TRSM-TAX-YTD - (WS-WORK-AMT * 12)
           COMPUTE TRST-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4850-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-TRAN-CD
                   ADD TRSM-DISTRIB-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-DISTRIB-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TRUSTEE FEE AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4860-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-CYCLE-CD
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-TRUSTEE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRUST TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4870-DESCRIBE-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MNG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VFN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XPL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4880-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4890-CHECK-CLOSE.
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
               MOVE 'Y' TO TRSM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4900-FLAG-PRINCIPAL.
           IF TRSM-PRINCIPAL-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRSM-PRINCIPAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
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
                   MOVE TRLIM-DISTRIB-YTD
                     TO TRSM-DISTRIB-YTD
                   MOVE TRLIM-STOP-FLAG
                     TO TRSM-STOP-FLAG
                   MOVE TRLIM-EXEMPT-FLAG
                     TO TRSM-EXEMPT-FLAG
                   MOVE TRLIM-SUSPEND-FLAG
                     TO TRSM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-TRLIM' TO ABND-PARAGRAPH
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
                   MOVE TRVAL-TAX-AMT
                     TO TRSM-TAX-AMT
                   MOVE TRVAL-PRINCIPAL-YTD
                     TO TRSM-PRINCIPAL-YTD
                   MOVE TRVAL-DISTRIB-YTD
                     TO TRSM-DISTRIB-YTD
                   MOVE TRVAL-PRINCIPAL-AMT
                     TO TRSM-PRINCIPAL-AMT
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
           DISPLAY 'TRSCLSE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TRSCLSE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
