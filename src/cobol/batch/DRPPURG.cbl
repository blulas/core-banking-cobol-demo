      *================================================================*
      * PROGRAM  : DRPPURG                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : DIVIDEND REINVESTMENT - PURGE AND ARCHIVE. READS THE*
      *            DRP TRANSACTIONS, UPDATES THE DRP MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. BANK HOLDING COMPANY   *
      *            STOCK DIVIDEND REINVESTMENT PLAN                    *
      * INPUT    : DRPMST (KSDS), DRPTRN, BANKCTL                      *
      * OUTPUT   : DRPOUT, DRPPURPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, DRSTAT, DRELG, DRLIM    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-08-09 BHS  ORIGINAL                                       *
      * 1985-04-02 RJT  CASH-PAY TABLE EXPANDED REQ4405                *
      * 1985-05-10 GAF  SHARES-AMT CALCULATION CORRECTED CHG1811       *
      * 1991-06-14 MKL  WITHHOLD FEE SCHEDULE UPDATE PRB3301           *
      * 1996-09-05 PQR  FEE CALCULATION CORRECTED CHG6936              *
      * 1997-11-10 RJT  FEE CALCULATION CORRECTED INC5948              *
      * 1997-08-05 RJT  DIVIDEND RERUN PROTECTION REQ2774              *
      * 1999-10-12 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-09-03 LMB  WITHHOLD STATUS CODES ADDED CHG2165            *
      * 2008-04-18 BHS  FEE STATUS CODES ADDED PRB1164                 *
      * 2009-11-05 MKL  FEE EXCEPTIONS TO OPS QUEUE REQ9167            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DRPPURG.
       AUTHOR. D P SHAW.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "DRPMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS DRPM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "DRPTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "DRPOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "DRPPURPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY DRPMST.
       FD  TRAN-FILE.
       COPY DRPTRN.
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
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 30.
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
      * WS-REINVEST-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REINVEST-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000482000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000581500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000840000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000848000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007100000000863000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007600000000410000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008600000000755000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008700000000519000'.
       01  WS-REINVEST-TIER-TBL REDEFINES WS-REINVEST-TIER-VALUES.
           05  WS-REINVEST-TIER-ROW OCCURS 8 TIMES.
               10  WS-REINVEST-TIER-LOW    PIC 9(09)V99.
               10  WS-REINVEST-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000411000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000548000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000048000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000554500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016500000000694000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016750000000559500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017750000000128500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017850000000874000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018850000000508500'.
       01  WS-FEE-TIER-TBL REDEFINES WS-FEE-TIER-VALUES.
           05  WS-FEE-TIER-ROW OCCURS 9 TIMES.
               10  WS-FEE-TIER-LOW         PIC 9(09)V99.
               10  WS-FEE-TIER-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SHARES-A-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHARES-A-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000348000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000553000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000091000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000607000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012650000000159500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012700000000596500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013200000000251000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013450000000063500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014450000000686500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024450000000440000'.
       01  WS-SHARES-A-SCHED-TBL REDEFINES WS-SHARES-A-SCHED-VALUES.
           05  WS-SHARES-A-SCHED-ROW OCCURS 10 TIMES.
               10  WS-SHARES-A-SCHED-LOW   PIC 9(09)V99.
               10  WS-SHARES-A-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SHARES-A-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHARES-A-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000412000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000152500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000670500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010200000000555500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012700000000461500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017700000000190500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017950000000404000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022950000000215500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023200000000617000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025700000000103000'.
       01  WS-SHARES-A-TIER-TBL REDEFINES WS-SHARES-A-TIER-VALUES.
           05  WS-SHARES-A-TIER-ROW OCCURS 10 TIMES.
               10  WS-SHARES-A-TIER-LOW    PIC 9(09)V99.
               10  WS-SHARES-A-TIER-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  DRPPURG  DIVIDEND REINVESTMENT'.
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
       COPY DRSTATP.
       COPY DRELGP.
       COPY DRLIMP.

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
               MOVE 'DRPMST' TO ABND-FILE-NAME
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
           MOVE DRPT-KEY TO DRPM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-FLAG-FEE
               PERFORM 3010-AGE-LAST-PAY
               PERFORM 3020-ACCUM-REASON
               PERFORM 3030-CHECK-LAST-ACT
               PERFORM 3040-EDIT-CUST-ID
               PERFORM 3050-SET-SOURCE
               PERFORM 3060-FLAG-FEE
               PERFORM 3070-FLAG-SHARES-AMT
               PERFORM 3080-EDIT-DESC
               PERFORM 3090-ASSESS-REVERSAL
               PERFORM 3100-SELECT-RATE
               PERFORM 3110-CHECK-LAST-PAY
               PERFORM 3120-ACCUM-REASON
               PERFORM 3130-SELECT-PENALTY-PCT
               PERFORM 3140-DESCRIBE-TRAN
               PERFORM 3150-SELECT-PENALTY-PCT
               PERFORM 3160-SET-CHANNEL
               PERFORM 3170-DESCRIBE-REASON
               PERFORM 3180-FLAG-CASH-PAY
               PERFORM 3190-PRORATE-SHARES-AMT
               PERFORM 3200-CHECK-NEXT-REVIEW
               PERFORM 3210-SPLIT-SHARES-AMT
               PERFORM 3220-SPLIT-CASH-PAY
               PERFORM 3230-LIMIT-CASH-PAY
               PERFORM 3240-ACCRUE-DISCOUNT
               PERFORM 3250-LIMIT-FEE
               PERFORM 3260-PRORATE-REINVEST
               PERFORM 3270-FLAG-CASH-PAY
               PERFORM 3280-ACCRUE-SHARES-AMT
               PERFORM 3290-EDIT-DESC
               PERFORM 3300-LIMIT-DIVIDEND
               PERFORM 3310-ACCRUE-SHARES-AMT
               PERFORM 3320-EDIT-CUST-ID
               PERFORM 3330-ACCRUE-DIVIDEND
               PERFORM 3340-PRORATE-SHARES-AMT
               PERFORM 3350-PRORATE-REINVEST
               PERFORM 3360-PRORATE-REINVEST
               PERFORM 3370-AGE-NOTICE
               PERFORM 3380-LIMIT-DIVIDEND
               PERFORM 3390-PRORATE-DIVIDEND
               PERFORM 3400-ACCRUE-SHARES-AMT
               PERFORM 3410-ACCUM-CHANNEL
               PERFORM 3420-SET-SOURCE
               PERFORM 3430-SELECT-RATE
               PERFORM 3440-PRORATE-FEE
               PERFORM 3450-EDIT-CONTACT
               PERFORM 3460-DESCRIBE-TRAN
               PERFORM 3470-SPLIT-REINVEST
               PERFORM 3480-CHECK-LAST-ACT
               PERFORM 3490-ACCUM-CYCLE
               PERFORM 3500-SPLIT-REINVEST
               PERFORM 3510-ACCUM-SOURCE
               PERFORM 3520-SET-PLAN-OPTION
               PERFORM 3530-DESCRIBE-TRAN
               PERFORM 3540-COUNT-ITEM
               PERFORM 3550-AGE-NEXT-DUE
               PERFORM 3560-ASSESS-SUSPEND
               PERFORM 3570-SPLIT-CASH-PAY
               PERFORM 3580-SELECT-DISCOUNT
               PERFORM 3590-AGE-NEXT-DUE
               PERFORM 3600-ACCRUE-REINVEST
               PERFORM 3610-PRORATE-FEE
               PERFORM 3620-PRORATE-CASH-PAY
               PERFORM 3630-SET-CHANNEL
               PERFORM 3640-SPLIT-CASH-PAY
               PERFORM 3650-SET-HOLDER-TYPE
               PERFORM 3660-SELECT-RATE
               PERFORM 3670-EDIT-OPERATOR
               PERFORM 3680-AGE-EFF
               PERFORM 3690-ASSESS-AUTO-RENEW
               PERFORM 3700-EDIT-CUST-ID
               PERFORM 3710-SET-CHANNEL
               PERFORM 3720-SELECT-PENALTY-PCT
               PERFORM 3730-ASSESS-AUTO-RENEW
               PERFORM 3740-AGE-EFF
               PERFORM 3750-ACCUM-CHANNEL
               PERFORM 3760-DESCRIBE-TRAN
               PERFORM 3770-CHECK-EFFECTIVE
               PERFORM 3780-EDIT-MEMO
               PERFORM 3790-COUNT-SEQ
               PERFORM 3800-SPLIT-SHARES-AMT
               PERFORM 3810-EDIT-NAME
               PERFORM 3820-ASSESS-PREFERRED
               PERFORM 3830-ACCUM-CHANNEL
               PERFORM 3840-DESCRIBE-PLAN-OPTION
               PERFORM 3850-DESCRIBE-SOURCE
               PERFORM 3860-CHECK-MATURITY
               PERFORM 3870-PRORATE-DIVIDEND
               PERFORM 3880-FLAG-REINVEST
               PERFORM 3890-EDIT-CUST-ID
               PERFORM 3900-SPLIT-FEE
               PERFORM 3910-EDIT-CONTACT
               PERFORM 3920-SET-CYCLE
               PERFORM 3930-EDIT-DESC
               PERFORM 3940-SELECT-RATE
               PERFORM 3950-SELECT-DISCOUNT
               PERFORM 3960-CHECK-LAST-MAINT
               PERFORM 3970-CHECK-LAST-MAINT
               PERFORM 3980-COUNT-NOTICE
               PERFORM 3990-SPLIT-REINVEST
               PERFORM 4000-AGE-OPEN
               PERFORM 4010-CHECK-EFFECTIVE
               PERFORM 4020-FLAG-ITEM
               PERFORM 4030-SET-TRAN
               PERFORM 4040-DESCRIBE-HOLDER-TYPE
               PERFORM 4050-ACCRUE-PENALTY-PCT
               PERFORM 4060-ASSESS-SUSPEND
               PERFORM 4070-EDIT-REJECT
               PERFORM 4080-EDIT-MEMO
               PERFORM 4090-ACCRUE-REINVEST
               PERFORM 4100-ACCUM-PLAN-OPTION
               PERFORM 4110-EDIT-REJECT
               PERFORM 4120-ACCUM-CHANNEL
               PERFORM 4130-FLAG-FEE
               PERFORM 4140-PRORATE-REINVEST
               PERFORM 4150-FLAG-REINVEST
               PERFORM 4160-SELECT-DISCOUNT
               PERFORM 4170-ACCUM-CHANNEL
               PERFORM 4180-SPLIT-FEE
               PERFORM 4190-ACCUM-CHANNEL
               PERFORM 4200-FLAG-FEE
               PERFORM 4210-SPLIT-SHARES-AMT
               PERFORM 4220-SPLIT-FEE
               PERFORM 4230-LIMIT-SHARES-AMT
               PERFORM 4240-ACCUM-CHANNEL
               PERFORM 4250-EDIT-ADDR
               PERFORM 4260-COUNT-PAYMENT
               PERFORM 4270-LIMIT-SHARES-AMT
               PERFORM 4280-PRORATE-DIVIDEND
               PERFORM 4290-AGE-MATURITY
               PERFORM 4300-COUNT-ITEM
               PERFORM 4310-SPLIT-SHARES-AMT
               PERFORM 4320-PRORATE-REINVEST
               PERFORM 4330-DESCRIBE-HOLDER-TYPE
               PERFORM 4340-SET-HOLDER-TYPE
               PERFORM 4350-COUNT-NOTICE
               PERFORM 4360-CHECK-NOTICE
               PERFORM 4370-EDIT-NAME
               PERFORM 4380-DESCRIBE-SOURCE
               PERFORM 4390-ACCUM-CHANNEL
               PERFORM 4400-CHECK-LAST-MAINT
               PERFORM 4410-COUNT-EXCEPT
               PERFORM 4420-CHECK-NEXT-REVIEW
               PERFORM 4430-ACCRUE-SHARES-AMT
               PERFORM 4440-COUNT-ITEM
               PERFORM 4450-SPLIT-REINVEST
               PERFORM 4460-SPLIT-WITHHOLD
               PERFORM 4470-DESCRIBE-CYCLE
               PERFORM 4480-ACCUM-PLAN-OPTION
               PERFORM 4490-SPLIT-FEE
               PERFORM 4500-AGE-NOTICE
               PERFORM 4510-ASSESS-PREFERRED
               PERFORM 4520-AGE-NEXT-REVIEW
               PERFORM 4530-SET-HOLDER-TYPE
               PERFORM 4540-DESCRIBE-CYCLE
               PERFORM 4550-ACCUM-HOLDER-TYPE
               PERFORM 4560-CHECK-NOTICE
               PERFORM 4570-SET-CHANNEL
               PERFORM 4580-COUNT-PAYMENT
               PERFORM 4590-PRORATE-WITHHOLD
               PERFORM 4600-CHECK-LAST-ACT
               PERFORM 4610-COUNT-SEQ
               PERFORM 4620-ASSESS-SUSPEND
               PERFORM 4630-SPLIT-SHARES-AMT
               PERFORM 4640-SPLIT-SHARES-AMT
               PERFORM 4650-COUNT-ITEM
               PERFORM 4660-SPLIT-FEE
               PERFORM 4670-CHECK-LAST-PAY
               PERFORM 4680-LIMIT-SHARES-AMT
               PERFORM 4690-LIMIT-CASH-PAY
               PERFORM 4700-ACCUM-CHANNEL
               PERFORM 4710-ACCUM-TRAN
               PERFORM 4720-SELECT-PENALTY-PCT
               PERFORM 4730-EDIT-REJECT
               PERFORM 4740-SET-CHANNEL
               PERFORM 4750-PRORATE-CASH-PAY
               PERFORM 4760-ACCRUE-WITHHOLD
               PERFORM 7000-CALL-DRSTAT
               PERFORM 7020-CALL-DRELG
               PERFORM 7010-CALL-DRLIM
               REWRITE DRP-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'DRPMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM DRP-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3000-FLAG-FEE.
           IF DRPM-FEE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3010-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-PAY-DATE > 0 AND DRPM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3020-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-REASON-CD
                   ADD DRPM-REINVEST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-REINVEST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3030-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E217 CUST ID REQUIRED
      * E411 FEE AMT NEGATIVE
      * E800 FEE AMT OVER 10000
      * E364 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3040-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CUST-ID = SPACES
                   MOVE 'E217' TO WS-REJECT-CODE
               WHEN DRPM-FEE-AMT < 0
                   MOVE 'E411' TO WS-REJECT-CODE
               WHEN DRPM-FEE-AMT > 10000.00
                   MOVE 'E800' TO WS-REJECT-CODE
               WHEN DRPM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E364' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BT -> FL WHEN NEXT REVIEW DATE IS PAST
      * FL -> NK WHEN NEXT REVIEW DATE IS PAST
      * NK -> YC WHEN NEXT REVIEW DATE IS PAST
      * YC -> BT WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'FL' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'NK' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NK'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'YC' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BT' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3060-FLAG-FEE.
           IF DRPM-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SHARES AMT YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3070-FLAG-SHARES-AMT.
           IF DRPM-SHARES-AMT-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE DRPM-SHARES-AMT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E579 DESC REQUIRED
      * E995 DIVIDEND YTD NEGATIVE
      * E321 DIVIDEND YTD OVER 250000
      * E606 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-DESC = SPACES
                   MOVE 'E579' TO WS-REJECT-CODE
               WHEN DRPM-DIVIDEND-YTD < 0
                   MOVE 'E995' TO WS-REJECT-CODE
               WHEN DRPM-DIVIDEND-YTD > 250000.00
                   MOVE 'E321' TO WS-REJECT-CODE
               WHEN DRPM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND FEE ON THE DIVIDEND YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3090-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO DRPT-DIVIDEND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   COMPUTE DRPT-DIVIDEND-AMT ROUNDED = DRPM-DIVIDEND-YTD
                       * DRPM-DISCOUNT / 200
               WHEN DRPM-HOLDER-TYPE-CD = 'C'
                   MOVE 10.00 TO DRPT-DIVIDEND-AMT
               WHEN DRPM-HOLDER-TYPE-CD = 'H'
                   MOVE 12.00 TO DRPT-DIVIDEND-AMT
               WHEN OTHER
                   COMPUTE DRPT-DIVIDEND-AMT ROUNDED = DRPM-DIVIDEND-YTD
                       * DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPT-DIVIDEND-AMT > 0 AND DRPT-DIVIDEND-AMT < 2.00
               MOVE 2.00 TO DRPT-DIVIDEND-AMT
           END-IF
           IF DRPT-DIVIDEND-AMT > 100.00
               MOVE 100.00 TO DRPT-DIVIDEND-AMT
           END-IF
           ADD DRPT-DIVIDEND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE SHARES A SCHED TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3100-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHARES-A-SCHED-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF DRPM-FEE-AMT NOT < WS-SHARES-A-SCHED-LOW(WS-TX)
                   MOVE WS-SHARES-A-SCHED-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3110-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO DRPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHARES AMT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3120-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-REASON-CD
                   ADD DRPT-SHARES-AMT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPT-SHARES-AMT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3130-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3140-DESCRIBE-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RI'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SHARES A TIER TIER FOR THE DIVIDEND YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHARES-A-TIER-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF DRPM-DIVIDEND-YTD NOT < WS-SHARES-A-TIER-LOW(WS-TX)
                   MOVE WS-SHARES-A-TIER-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT DUE DATE IS PAST
      * BAT -> MAL WHEN NEXT DUE DATE IS PAST
      * MAL -> TEL WHEN NEXT DUE DATE IS PAST
      * TEL -> ONL WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3160-SET-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3170-DESCRIBE-REASON.
           EVALUATE DRPM-REASON-CD
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'G'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'J'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'K'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Y'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH PAY AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3180-FLAG-CASH-PAY.
           IF DRPM-CASH-PAY-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-CASH-PAY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHARES AMT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3190-PRORATE-SHARES-AMT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-SHARES-AMT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3200-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO DRPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3210-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE DRPT-SHARES-AMT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH PAY AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3220-SPLIT-CASH-PAY.
           COMPUTE WS-WORK-AMT = DRPM-CASH-PAY-AMT / 4
           COMPUTE WS-WORK-AMT-2 = DRPM-CASH-PAY-AMT - (WS-WORK-AMT * 4)
           COMPUTE DRPT-REINVEST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH PAY YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3230-LIMIT-CASH-PAY.
           IF DRPM-CASH-PAY-YTD < 100.00
               MOVE 100.00 TO DRPM-CASH-PAY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-CASH-PAY-YTD > 10000.00
               MOVE 10000.00 TO DRPM-CASH-PAY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3240-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * DRPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-SHARES-AMT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3250-LIMIT-FEE.
           IF DRPT-FEE-AMT < 10.00
               MOVE 10.00 TO DRPT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPT-FEE-AMT > 50000.00
               MOVE 50000.00 TO DRPT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REINVEST AMT.
      *----------------------------------------------------------------*
       3260-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-REINVEST-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-REINVEST-AMT
           END-IF
           ADD DRPM-REINVEST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CASH PAY YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3270-FLAG-CASH-PAY.
           IF DRPM-CASH-PAY-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE DRPM-CASH-PAY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHARES AMT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-SHARES-AMT.
           IF DRPM-SHARES-AMT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-SHARES-AMT-AMT * DRPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-WITHHOLD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E358 DESC REQUIRED
      * E902 WORK AMT NEGATIVE
      * E967 WORK AMT OVER 99999
      * E438 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3290-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-DESC = SPACES
                   MOVE 'E358' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E902' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E967' TO WS-REJECT-CODE
               WHEN DRPT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E438' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DIVIDEND YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3300-LIMIT-DIVIDEND.
           IF DRPM-DIVIDEND-YTD < 25.00
               MOVE 25.00 TO DRPM-DIVIDEND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-DIVIDEND-YTD > 10000.00
               MOVE 10000.00 TO DRPM-DIVIDEND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHARES AMT YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3310-ACCRUE-SHARES-AMT.
           IF DRPM-SHARES-AMT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-SHARES-AMT-YTD * DRPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRPT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E175 CUST ID REQUIRED
      * E173 DIVIDEND AMT NEGATIVE
      * E832 DIVIDEND AMT OVER 10000
      * E789 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3320-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CUST-ID = SPACES
                   MOVE 'E175' TO WS-REJECT-CODE
               WHEN DRPM-DIVIDEND-AMT < 0
                   MOVE 'E173' TO WS-REJECT-CODE
               WHEN DRPM-DIVIDEND-AMT > 10000.00
                   MOVE 'E832' TO WS-REJECT-CODE
               WHEN DRPM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E789' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DIVIDEND YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3330-ACCRUE-DIVIDEND.
           IF DRPM-DIVIDEND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-DIVIDEND-YTD * DRPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRPM-CASH-PAY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHARES AMT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHARES AMT YTD.
      *----------------------------------------------------------------*
       3340-PRORATE-SHARES-AMT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-SHARES-AMT-YTD ROUNDED =
                   DRPT-SHARES-AMT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-SHARES-AMT-YTD
           END-IF
           ADD DRPM-SHARES-AMT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE REINVEST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3350-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   DRPM-REINVEST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE DIVIDEND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REINVEST AMT.
      *----------------------------------------------------------------*
       3360-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPT-REINVEST-AMT ROUNDED =
                   DRPM-DIVIDEND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPT-REINVEST-AMT
           END-IF
           ADD DRPT-REINVEST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DIVIDEND AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3380-LIMIT-DIVIDEND.
           IF DRPM-DIVIDEND-AMT < 10.00
               MOVE 10.00 TO DRPM-DIVIDEND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-DIVIDEND-AMT > 5000.00
               MOVE 5000.00 TO DRPM-DIVIDEND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHARES AMT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DIVIDEND YTD.
      *----------------------------------------------------------------*
       3390-PRORATE-DIVIDEND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-DIVIDEND-YTD ROUNDED =
                   DRPM-SHARES-AMT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-DIVIDEND-YTD
           END-IF
           ADD DRPM-DIVIDEND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHARES AMT YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3400-ACCRUE-SHARES-AMT.
           IF DRPM-SHARES-AMT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-SHARES-AMT-YTD * DRPM-RATE / 100 / 360
               ADD WS-WORK-AMT TO DRPM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3410-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CHANNEL-CD
                   ADD DRPM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BT -> FL WHEN NOTICE DATE IS PAST
      * FL -> NK WHEN NOTICE DATE IS PAST
      * NK -> YC WHEN NOTICE DATE IS PAST
      * YC -> BT WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3420-SET-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NK' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NK'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SHARES A TIER TIER FOR THE DIVIDEND YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3430-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHARES-A-TIER-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF DRPM-DIVIDEND-YTD NOT < WS-SHARES-A-TIER-LOW(WS-TX)
                   MOVE WS-SHARES-A-TIER-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WITHHOLD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3440-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-FEE-YTD ROUNDED =
                   DRPM-WITHHOLD-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-FEE-YTD
           END-IF
           ADD DRPM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E472 CONTACT REQUIRED
      * E905 WORK AMT NEGATIVE
      * E736 WORK AMT OVER 25000
      * E179 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3450-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CONTACT = SPACES
                   MOVE 'E472' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E905' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E736' TO WS-REJECT-CODE
               WHEN DRPM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E179' TO WS-REJECT-CODE
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
       3460-DESCRIBE-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RI'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'CP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3470-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPM-REINVEST-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-REINVEST-AMT - (WS-WORK-AMT *
               12)
           COMPUTE DRPT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3480-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO DRPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3490-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CYCLE-CD
                   ADD DRPT-REINVEST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPT-REINVEST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3500-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPM-REINVEST-AMT / 3
           COMPUTE WS-WORK-AMT-2 = DRPM-REINVEST-AMT - (WS-WORK-AMT * 3)
           COMPUTE DRPM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3510-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-SOURCE-CD
                   ADD DRPT-REINVEST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPT-REINVEST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN OPTION CD:
      * E -> R WHEN EFFECTIVE DATE IS PAST
      * R -> T WHEN EFFECTIVE DATE IS PAST
      * T -> Z WHEN EFFECTIVE DATE IS PAST
      * Z -> E WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3520-SET-PLAN-OPTION.
           EVALUATE DRPM-PLAN-OPTION-CD
               WHEN 'E'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3530-DESCRIBE-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RI'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'CP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3540-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 12
               MOVE 'Y' TO DRPM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3550-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND FEE ON THE WORK AMT 2.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3560-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRPT-DIVIDEND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   COMPUTE DRPT-DIVIDEND-AMT ROUNDED = WS-WORK-AMT-2 *
                       DRPM-PENALTY-PCT / 200
               WHEN DRPT-TRAN-CD = 'DV'
                   MOVE 3.00 TO DRPT-DIVIDEND-AMT
               WHEN DRPT-TRAN-CD = 'RI'
                   MOVE 15.00 TO DRPT-DIVIDEND-AMT
               WHEN OTHER
                   COMPUTE DRPT-DIVIDEND-AMT ROUNDED = WS-WORK-AMT-2 *
                       DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPT-DIVIDEND-AMT > 0 AND DRPT-DIVIDEND-AMT < 2.00
               MOVE 2.00 TO DRPT-DIVIDEND-AMT
           END-IF
           IF DRPT-DIVIDEND-AMT > 75.00
               MOVE 75.00 TO DRPT-DIVIDEND-AMT
           END-IF
           ADD DRPT-DIVIDEND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT CASH PAY YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3570-SPLIT-CASH-PAY.
           COMPUTE WS-WORK-AMT = DRPM-CASH-PAY-YTD / 2
           COMPUTE WS-WORK-AMT-2 = DRPM-CASH-PAY-YTD - (WS-WORK-AMT * 2)
           COMPUTE DRPT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE DIVIDEND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3580-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF DRPM-DIVIDEND-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REINVEST AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3600-ACCRUE-REINVEST.
           IF DRPT-REINVEST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPT-REINVEST-AMT * DRPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO DRPM-REINVEST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WITHHOLD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3610-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-FEE-AMT ROUNDED =
                   DRPM-WITHHOLD-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-FEE-AMT
           END-IF
           ADD DRPM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE REINVEST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH PAY YTD.
      *----------------------------------------------------------------*
       3620-PRORATE-CASH-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-CASH-PAY-YTD ROUNDED =
                   DRPT-REINVEST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-CASH-PAY-YTD
           END-IF
           ADD DRPM-CASH-PAY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NOTICE DATE IS PAST
      * BAT -> MAL WHEN NOTICE DATE IS PAST
      * MAL -> TEL WHEN NOTICE DATE IS PAST
      * TEL -> ONL WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CASH PAY AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3640-SPLIT-CASH-PAY.
           COMPUTE WS-WORK-AMT = DRPM-CASH-PAY-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-CASH-PAY-AMT - (WS-WORK-AMT *
               12)
           COMPUTE DRPM-DIVIDEND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR HOLDER TYPE CD:
      * C -> H WHEN MATURITY DATE IS PAST
      * H -> K WHEN MATURITY DATE IS PAST
      * K -> L WHEN MATURITY DATE IS PAST
      * L -> M WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3650-SET-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE REINVEST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3660-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF DRPM-REINVEST-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E295 OPERATOR REQUIRED
      * E915 FEE AMT NEGATIVE
      * E667 FEE AMT OVER 10000
      * E529 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3670-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-OPERATOR = SPACES
                   MOVE 'E295' TO WS-REJECT-CODE
               WHEN DRPT-FEE-AMT < 0
                   MOVE 'E915' TO WS-REJECT-CODE
               WHEN DRPT-FEE-AMT > 10000.00
                   MOVE 'E667' TO WS-REJECT-CODE
               WHEN DRPM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E529' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3680-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPT-SHARES-AMT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPT-SHARES-AMT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD DRPT-SHARES-AMT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPT-SHARES-AMT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPT-SHARES-AMT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND YTD FEE ON THE REINVEST YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3690-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN DRPM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO DRPM-DIVIDEND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-STOP-FLAG = 'Y'
                   COMPUTE DRPM-DIVIDEND-YTD ROUNDED = DRPM-REINVEST-YTD
                       * DRPM-PENALTY-PCT / 200
               WHEN DRPT-TRAN-CD = 'DV'
                   MOVE 3.00 TO DRPM-DIVIDEND-YTD
               WHEN DRPT-TRAN-CD = 'RI'
                   MOVE 20.00 TO DRPM-DIVIDEND-YTD
               WHEN OTHER
                   COMPUTE DRPM-DIVIDEND-YTD ROUNDED = DRPM-REINVEST-YTD
                       * DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-DIVIDEND-YTD > 0 AND DRPM-DIVIDEND-YTD < 10.00
               MOVE 10.00 TO DRPM-DIVIDEND-YTD
           END-IF
           IF DRPM-DIVIDEND-YTD > 35.00
               MOVE 35.00 TO DRPM-DIVIDEND-YTD
           END-IF
           ADD DRPM-DIVIDEND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E523 CUST ID REQUIRED
      * E637 DIVIDEND YTD NEGATIVE
      * E735 DIVIDEND YTD OVER 250000
      * E122 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3700-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CUST-ID = SPACES
                   MOVE 'E523' TO WS-REJECT-CODE
               WHEN DRPM-DIVIDEND-YTD < 0
                   MOVE 'E637' TO WS-REJECT-CODE
               WHEN DRPM-DIVIDEND-YTD > 250000.00
                   MOVE 'E735' TO WS-REJECT-CODE
               WHEN DRPT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E122' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * RM -> VP WHEN EFF DATE IS PAST
      * VP -> YS WHEN EFF DATE IS PAST
      * YS -> RM WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3710-SET-CHANNEL.
           EVALUATE DRPM-CHANNEL-CD
               WHEN 'RM'
                   IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VP' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VP'
                   IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YS' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YS'
                   IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE REINVEST TIER TIER FOR THE SHARES AMT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3720-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REINVEST-TIER-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF DRPM-SHARES-AMT-YTD NOT < WS-REINVEST-TIER-LOW(WS-TX)
                   MOVE WS-REINVEST-TIER-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WITHHOLD FEE ON THE SHARES AMT AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3730-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN DRPM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO DRPM-WITHHOLD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   COMPUTE DRPM-WITHHOLD-AMT ROUNDED =
                       DRPM-SHARES-AMT-AMT * DRPM-DISCOUNT / 200
               WHEN DRPM-SOURCE-CD = 'BT'
                   MOVE 5.00 TO DRPM-WITHHOLD-AMT
               WHEN DRPM-SOURCE-CD = 'FL'
                   MOVE 20.00 TO DRPM-WITHHOLD-AMT
               WHEN OTHER
                   COMPUTE DRPM-WITHHOLD-AMT ROUNDED =
                       DRPM-SHARES-AMT-AMT * DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPM-WITHHOLD-AMT > 0 AND DRPM-WITHHOLD-AMT < 5.00
               MOVE 5.00 TO DRPM-WITHHOLD-AMT
           END-IF
           IF DRPM-WITHHOLD-AMT > 250.00
               MOVE 250.00 TO DRPM-WITHHOLD-AMT
           END-IF
           ADD DRPM-WITHHOLD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3740-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DIVIDEND AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3750-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-CHANNEL-CD
                   ADD DRPM-DIVIDEND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-DIVIDEND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3760-DESCRIBE-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RI'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3770-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E218 MEMO REQUIRED
      * E288 WITHHOLD YTD NEGATIVE
      * E888 WITHHOLD YTD OVER 99999
      * E528 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3780-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-MEMO = SPACES
                   MOVE 'E218' TO WS-REJECT-CODE
               WHEN DRPM-WITHHOLD-YTD < 0
                   MOVE 'E288' TO WS-REJECT-CODE
               WHEN DRPM-WITHHOLD-YTD > 99999.00
                   MOVE 'E888' TO WS-REJECT-CODE
               WHEN DRPT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E528' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3790-COUNT-SEQ.
           IF DRPT-SEQ < 9999999
               ADD 1 TO DRPT-SEQ
           END-IF
           IF DRPT-SEQ NOT < 12
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3800-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE DRPM-SHARES-AMT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E013 NAME REQUIRED
      * E005 SHARES AMT AMT NEGATIVE
      * E408 SHARES AMT AMT OVER 1000000
      * E427 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3810-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-NAME = SPACES
                   MOVE 'E013' TO WS-REJECT-CODE
               WHEN DRPM-SHARES-AMT-AMT < 0
                   MOVE 'E005' TO WS-REJECT-CODE
               WHEN DRPM-SHARES-AMT-AMT > 1000000.00
                   MOVE 'E408' TO WS-REJECT-CODE
               WHEN DRPM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E427' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHARES FEE ON THE REINVEST AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3820-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN DRPM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO DRPM-SHARES-AMT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-STOP-FLAG = 'Y'
                   COMPUTE DRPM-SHARES-AMT-AMT ROUNDED =
                       DRPM-REINVEST-AMT * DRPM-RATE / 200
               WHEN DRPT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO DRPM-SHARES-AMT-AMT
               WHEN DRPT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO DRPM-SHARES-AMT-AMT
               WHEN OTHER
                   COMPUTE DRPM-SHARES-AMT-AMT ROUNDED =
                       DRPM-REINVEST-AMT * DRPM-RATE / 100
           END-EVALUATE
           IF DRPM-SHARES-AMT-AMT > 0 AND DRPM-SHARES-AMT-AMT < 10.00
               MOVE 10.00 TO DRPM-SHARES-AMT-AMT
           END-IF
           IF DRPM-SHARES-AMT-AMT > 75.00
               MOVE 75.00 TO DRPM-SHARES-AMT-AMT
           END-IF
           ADD DRPM-SHARES-AMT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE DIVIDEND YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3830-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-CHANNEL-CD
                   ADD DRPM-DIVIDEND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-DIVIDEND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN OPTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3840-DESCRIBE-PLAN-OPTION.
           EVALUATE DRPM-PLAN-OPTION-CD
               WHEN 'E'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'R'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3850-DESCRIBE-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3860-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH PAY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DIVIDEND AMT.
      *----------------------------------------------------------------*
       3870-PRORATE-DIVIDEND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPT-DIVIDEND-AMT ROUNDED =
                   DRPM-CASH-PAY-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPT-DIVIDEND-AMT
           END-IF
           ADD DRPT-DIVIDEND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * REINVEST AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3880-FLAG-REINVEST.
           IF DRPM-REINVEST-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-REINVEST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E030 CUST ID REQUIRED
      * E615 WORK AMT NEGATIVE
      * E859 WORK AMT OVER 250000
      * E554 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3890-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CUST-ID = SPACES
                   MOVE 'E030' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E615' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E859' TO WS-REJECT-CODE
               WHEN DRPM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E554' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3900-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = DRPT-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = DRPT-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE DRPM-WITHHOLD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E037 CONTACT REQUIRED
      * E608 REINVEST YTD NEGATIVE
      * E165 REINVEST YTD OVER 10000
      * E951 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3910-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CONTACT = SPACES
                   MOVE 'E037' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-YTD < 0
                   MOVE 'E608' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-YTD > 10000.00
                   MOVE 'E165' TO WS-REJECT-CODE
               WHEN DRPM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E951' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DE -> FB WHEN EFFECTIVE DATE IS PAST
      * FB -> PC WHEN EFFECTIVE DATE IS PAST
      * PC -> WY WHEN EFFECTIVE DATE IS PAST
      * WY -> XD WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3920-SET-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WY'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E031 DESC REQUIRED
      * E143 WITHHOLD YTD NEGATIVE
      * E824 WITHHOLD YTD OVER 10000
      * E084 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3930-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-DESC = SPACES
                   MOVE 'E031' TO WS-REJECT-CODE
               WHEN DRPM-WITHHOLD-YTD < 0
                   MOVE 'E143' TO WS-REJECT-CODE
               WHEN DRPM-WITHHOLD-YTD > 10000.00
                   MOVE 'E824' TO WS-REJECT-CODE
               WHEN DRPM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E084' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REINVEST TIER TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3940-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REINVEST-TIER-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF DRPM-FEE-YTD NOT < WS-REINVEST-TIER-LOW(WS-TX)
                   MOVE WS-REINVEST-TIER-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE WITHHOLD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3950-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF DRPM-WITHHOLD-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3960-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3970-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO DRPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-NOTICE.
           IF DRPM-NOTICE-CNT < 9999999
               ADD 1 TO DRPM-NOTICE-CNT
           END-IF
           IF DRPM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3990-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPT-REINVEST-AMT / 2
           COMPUTE WS-WORK-AMT-2 = DRPT-REINVEST-AMT - (WS-WORK-AMT * 2)
           COMPUTE DRPM-CASH-PAY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4000-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4010-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4020-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DV -> RI WHEN POST DATE IS PAST
      * RI -> CP WHEN POST DATE IS PAST
      * CP -> WH WHEN POST DATE IS PAST
      * WH -> SL WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4030-SET-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RI' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RI'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CP'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WH' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WH'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SL' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE HOLDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4040-DESCRIBE-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'K'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4050-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * DRPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-DIVIDEND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE SHARES AMT AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4060-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRPM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   COMPUTE DRPM-FEE-YTD ROUNDED = DRPM-SHARES-AMT-AMT *
                       DRPM-PENALTY-PCT / 200
               WHEN DRPM-REASON-CD = 'B'
                   MOVE 20.00 TO DRPM-FEE-YTD
               WHEN DRPM-REASON-CD = 'G'
                   MOVE 20.00 TO DRPM-FEE-YTD
               WHEN OTHER
                   COMPUTE DRPM-FEE-YTD ROUNDED = DRPM-SHARES-AMT-AMT *
                       DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-FEE-YTD > 0 AND DRPM-FEE-YTD < 1.00
               MOVE 1.00 TO DRPM-FEE-YTD
           END-IF
           IF DRPM-FEE-YTD > 25.00
               MOVE 25.00 TO DRPM-FEE-YTD
           END-IF
           ADD DRPM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E389 REJECT CD REQUIRED
      * E505 FEE AMT NEGATIVE
      * E079 FEE AMT OVER 25000
      * E442 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4070-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-REJECT-CD = SPACES
                   MOVE 'E389' TO WS-REJECT-CODE
               WHEN DRPT-FEE-AMT < 0
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN DRPT-FEE-AMT > 25000.00
                   MOVE 'E079' TO WS-REJECT-CODE
               WHEN DRPM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E442' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E892 MEMO REQUIRED
      * E331 DIVIDEND AMT NEGATIVE
      * E173 DIVIDEND AMT OVER 99999
      * E817 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4080-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-MEMO = SPACES
                   MOVE 'E892' TO WS-REJECT-CODE
               WHEN DRPT-DIVIDEND-AMT < 0
                   MOVE 'E331' TO WS-REJECT-CODE
               WHEN DRPT-DIVIDEND-AMT > 99999.00
                   MOVE 'E173' TO WS-REJECT-CODE
               WHEN DRPM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E817' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REINVEST AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4090-ACCRUE-REINVEST.
           IF DRPM-REINVEST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-REINVEST-AMT * DRPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO DRPM-CASH-PAY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH PAY YTD BY PLAN OPTION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4100-ACCUM-PLAN-OPTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-PLAN-OPTION-CD
                   ADD DRPM-CASH-PAY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-PLAN-OPTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-CASH-PAY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E623 REJECT CD REQUIRED
      * E768 WITHHOLD AMT NEGATIVE
      * E822 WITHHOLD AMT OVER 1000000
      * E797 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4110-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-REJECT-CD = SPACES
                   MOVE 'E623' TO WS-REJECT-CODE
               WHEN DRPM-WITHHOLD-AMT < 0
                   MOVE 'E768' TO WS-REJECT-CODE
               WHEN DRPM-WITHHOLD-AMT > 1000000.00
                   MOVE 'E822' TO WS-REJECT-CODE
               WHEN DRPM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E797' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4120-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4130-FLAG-FEE.
           IF DRPM-FEE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHARES AMT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REINVEST YTD.
      *----------------------------------------------------------------*
       4140-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-REINVEST-YTD ROUNDED =
                   DRPT-SHARES-AMT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-REINVEST-YTD
           END-IF
           ADD DRPM-REINVEST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * REINVEST YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4150-FLAG-REINVEST.
           IF DRPM-REINVEST-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-REINVEST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHARES A TIER TIER FOR THE DIVIDEND YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4160-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SHARES-A-TIER-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF DRPM-DIVIDEND-YTD NOT < WS-SHARES-A-TIER-LOW(WS-TX)
                   MOVE WS-SHARES-A-TIER-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4170-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-CHANNEL-CD
                   ADD DRPM-WITHHOLD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-WITHHOLD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4180-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = DRPM-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = DRPM-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE DRPM-REINVEST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4190-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-CHANNEL-CD
                   ADD DRPM-REINVEST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-REINVEST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4200-FLAG-FEE.
           IF DRPM-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE DRPM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4210-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPT-SHARES-AMT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = DRPT-SHARES-AMT-AMT - (WS-WORK-AMT *
               3)
           COMPUTE DRPM-REINVEST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4220-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = DRPT-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = DRPT-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE DRPM-SHARES-AMT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHARES AMT AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4230-LIMIT-SHARES-AMT.
           IF DRPM-SHARES-AMT-AMT < 50.00
               MOVE 50.00 TO DRPM-SHARES-AMT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-SHARES-AMT-AMT > 2500.00
               MOVE 2500.00 TO DRPM-SHARES-AMT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4240-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CHANNEL-CD
                   ADD DRPM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E368 ADDR REQUIRED
      * E482 WORK AMT NEGATIVE
      * E244 WORK AMT OVER 99999
      * E075 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4250-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-ADDR = SPACES
                   MOVE 'E368' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E482' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E244' TO WS-REJECT-CODE
               WHEN DRPM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E075' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4260-COUNT-PAYMENT.
           IF DRPM-PAYMENT-CNT < 9999999
               ADD 1 TO DRPM-PAYMENT-CNT
           END-IF
           IF DRPM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SHARES AMT YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4270-LIMIT-SHARES-AMT.
           IF DRPM-SHARES-AMT-YTD < 10.00
               MOVE 10.00 TO DRPM-SHARES-AMT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-SHARES-AMT-YTD > 5000.00
               MOVE 5000.00 TO DRPM-SHARES-AMT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DIVIDEND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DIVIDEND YTD.
      *----------------------------------------------------------------*
       4280-PRORATE-DIVIDEND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-DIVIDEND-YTD ROUNDED =
                   DRPT-DIVIDEND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-DIVIDEND-YTD
           END-IF
           ADD DRPM-DIVIDEND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4290-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4300-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 10
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4310-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPM-SHARES-AMT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-SHARES-AMT-YTD - (WS-WORK-AMT *
               12)
           COMPUTE DRPM-CASH-PAY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHARES AMT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REINVEST AMT.
      *----------------------------------------------------------------*
       4320-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPT-REINVEST-AMT ROUNDED =
                   DRPT-SHARES-AMT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPT-REINVEST-AMT
           END-IF
           ADD DRPT-REINVEST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE HOLDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4330-DESCRIBE-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'H'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'L'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'M'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR HOLDER TYPE CD:
      * C -> H WHEN LAST ACT DATE IS PAST
      * H -> K WHEN LAST ACT DATE IS PAST
      * K -> L WHEN LAST ACT DATE IS PAST
      * L -> M WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4340-SET-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4350-COUNT-NOTICE.
           IF DRPM-NOTICE-CNT < 9999999
               ADD 1 TO DRPM-NOTICE-CNT
           END-IF
           IF DRPM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO DRPM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4360-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E783 NAME REQUIRED
      * E523 SHARES AMT YTD NEGATIVE
      * E095 SHARES AMT YTD OVER 50000
      * E688 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4370-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-NAME = SPACES
                   MOVE 'E783' TO WS-REJECT-CODE
               WHEN DRPM-SHARES-AMT-YTD < 0
                   MOVE 'E523' TO WS-REJECT-CODE
               WHEN DRPM-SHARES-AMT-YTD > 50000.00
                   MOVE 'E095' TO WS-REJECT-CODE
               WHEN DRPM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E688' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4380-DESCRIBE-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4390-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CHANNEL-CD
                   ADD DRPM-WITHHOLD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-WITHHOLD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4400-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4410-COUNT-EXCEPT.
           IF DRPM-EXCEPT-CNT < 9999999
               ADD 1 TO DRPM-EXCEPT-CNT
           END-IF
           IF DRPM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4420-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHARES AMT YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4430-ACCRUE-SHARES-AMT.
           IF DRPM-SHARES-AMT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-SHARES-AMT-YTD * DRPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRPM-SHARES-AMT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4440-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 12
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4450-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE DRPM-REINVEST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WITHHOLD YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4460-SPLIT-WITHHOLD.
           COMPUTE WS-WORK-AMT = DRPM-WITHHOLD-YTD / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-WITHHOLD-YTD - (WS-WORK-AMT *
               12)
           COMPUTE DRPM-SHARES-AMT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4470-DESCRIBE-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST AMT BY PLAN OPTION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4480-ACCUM-PLAN-OPTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-PLAN-OPTION-CD
                   ADD DRPT-REINVEST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-PLAN-OPTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPT-REINVEST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4490-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = DRPM-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = DRPM-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE DRPM-SHARES-AMT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4500-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND FEE ON THE REINVEST AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4510-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN DRPM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO DRPT-DIVIDEND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   COMPUTE DRPT-DIVIDEND-AMT ROUNDED = DRPT-REINVEST-AMT
                       * DRPM-RATE / 200
               WHEN DRPT-TRAN-CD = 'DV'
                   MOVE 5.00 TO DRPT-DIVIDEND-AMT
               WHEN DRPT-TRAN-CD = 'RI'
                   MOVE 15.00 TO DRPT-DIVIDEND-AMT
               WHEN OTHER
                   COMPUTE DRPT-DIVIDEND-AMT ROUNDED = DRPT-REINVEST-AMT
                       * DRPM-RATE / 100
           END-EVALUATE
           IF DRPT-DIVIDEND-AMT > 0 AND DRPT-DIVIDEND-AMT < 1.00
               MOVE 1.00 TO DRPT-DIVIDEND-AMT
           END-IF
           IF DRPT-DIVIDEND-AMT > 50.00
               MOVE 50.00 TO DRPT-DIVIDEND-AMT
           END-IF
           ADD DRPT-DIVIDEND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4520-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-REVIEW-DATE > 0 AND DRPM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR HOLDER TYPE CD:
      * C -> H WHEN LAST MAINT DATE IS PAST
      * H -> K WHEN LAST MAINT DATE IS PAST
      * K -> L WHEN LAST MAINT DATE IS PAST
      * L -> M WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4530-SET-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'H' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'K' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'L' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'M' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4540-DESCRIBE-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY HOLDER TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4550-ACCUM-HOLDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-HOLDER-TYPE-CD
                   ADD DRPM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-HOLDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4560-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4570-SET-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4580-COUNT-PAYMENT.
           IF DRPM-PAYMENT-CNT < 9999999
               ADD 1 TO DRPM-PAYMENT-CNT
           END-IF
           IF DRPM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO DRPM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WITHHOLD YTD.
      *----------------------------------------------------------------*
       4590-PRORATE-WITHHOLD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-WITHHOLD-YTD ROUNDED =
                   DRPM-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-WITHHOLD-YTD
           END-IF
           ADD DRPM-WITHHOLD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4600-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4610-COUNT-SEQ.
           IF DRPT-SEQ < 9999999
               ADD 1 TO DRPT-SEQ
           END-IF
           IF DRPT-SEQ NOT < 3
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHARES FEE ON THE CASH PAY YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4620-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRPT-SHARES-AMT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE DRPT-SHARES-AMT-AMT ROUNDED =
                       DRPM-CASH-PAY-YTD * DRPM-RATE / 200
               WHEN DRPM-REASON-CD = 'B'
                   MOVE 7.00 TO DRPT-SHARES-AMT-AMT
               WHEN DRPM-REASON-CD = 'G'
                   MOVE 15.00 TO DRPT-SHARES-AMT-AMT
               WHEN OTHER
                   COMPUTE DRPT-SHARES-AMT-AMT ROUNDED =
                       DRPM-CASH-PAY-YTD * DRPM-RATE / 100
           END-EVALUATE
           IF DRPT-SHARES-AMT-AMT > 0 AND DRPT-SHARES-AMT-AMT < 5.00
               MOVE 5.00 TO DRPT-SHARES-AMT-AMT
           END-IF
           IF DRPT-SHARES-AMT-AMT > 250.00
               MOVE 250.00 TO DRPT-SHARES-AMT-AMT
           END-IF
           ADD DRPT-SHARES-AMT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4630-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPM-SHARES-AMT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = DRPM-SHARES-AMT-YTD - (WS-WORK-AMT *
               2)
           COMPUTE DRPM-REINVEST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4640-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPM-SHARES-AMT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = DRPM-SHARES-AMT-YTD - (WS-WORK-AMT *
               4)
           COMPUTE DRPM-DIVIDEND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4650-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 3
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4660-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = DRPM-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = DRPM-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE DRPM-REINVEST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4670-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHARES AMT YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4680-LIMIT-SHARES-AMT.
           IF DRPM-SHARES-AMT-YTD < 25.00
               MOVE 25.00 TO DRPM-SHARES-AMT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-SHARES-AMT-YTD > 2500.00
               MOVE 2500.00 TO DRPM-SHARES-AMT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH PAY AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4690-LIMIT-CASH-PAY.
           IF DRPM-CASH-PAY-AMT < 25.00
               MOVE 25.00 TO DRPM-CASH-PAY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-CASH-PAY-AMT > 5000.00
               MOVE 5000.00 TO DRPM-CASH-PAY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHARES AMT AMT BY CHANNEL CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4700-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-CHANNEL-CD
                   ADD DRPM-SHARES-AMT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-SHARES-AMT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4710-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-TRAN-CD
                   ADD DRPM-REINVEST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-REINVEST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHARES A SCHED TIER FOR THE WITHHOLD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4720-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHARES-A-SCHED-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF DRPM-WITHHOLD-AMT NOT < WS-SHARES-A-SCHED-LOW(WS-TX)
                   MOVE WS-SHARES-A-SCHED-RATE(WS-TX) TO
                       DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E238 REJECT CD REQUIRED
      * E583 REINVEST YTD NEGATIVE
      * E552 REINVEST YTD OVER 1000000
      * E740 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4730-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-REJECT-CD = SPACES
                   MOVE 'E238' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-YTD < 0
                   MOVE 'E583' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-YTD > 1000000.00
                   MOVE 'E552' TO WS-REJECT-CODE
               WHEN DRPM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E740' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * RM -> VP WHEN NEXT DUE DATE IS PAST
      * VP -> YS WHEN NEXT DUE DATE IS PAST
      * YS -> RM WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4740-SET-CHANNEL.
           EVALUATE DRPM-CHANNEL-CD
               WHEN 'RM'
                   IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VP' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VP'
                   IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YS' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YS'
                   IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH PAY AMT.
      *----------------------------------------------------------------*
       4750-PRORATE-CASH-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-CASH-PAY-AMT ROUNDED =
                   DRPT-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-CASH-PAY-AMT
           END-IF
           ADD DRPM-CASH-PAY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WITHHOLD YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4760-ACCRUE-WITHHOLD.
           IF DRPM-WITHHOLD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-WITHHOLD-YTD * DRPM-RATE / 100 / 365
               ADD WS-WORK-AMT TO DRPM-CASH-PAY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE DRP FIELDS TO DRSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-DRSTAT.
           INITIALIZE DRSTAT-PARMS
           MOVE DRPM-CASH-PAY-AMT
             TO DRSTAT-CASH-PAY-AMT
           MOVE DRPM-FEE-YTD
             TO DRSTAT-FEE-YTD
           MOVE DRPM-DIVIDEND-AMT
             TO DRSTAT-DIVIDEND-AMT
           MOVE DRPM-REINVEST-AMT
             TO DRSTAT-REINVEST-AMT
           MOVE DRPM-SHARES-AMT-AMT
             TO DRSTAT-SHARES-AMT-AMT
           MOVE DRPM-WITHHOLD-YTD
             TO DRSTAT-WITHHOLD-YTD
           MOVE DRPM-LAST-PAY-DATE
             TO DRSTAT-LAST-PAY-DATE
           MOVE DRPM-NEXT-REVIEW-DATE
             TO DRSTAT-NEXT-REVIEW-DATE
           MOVE DRPM-NEXT-DUE-DATE
             TO DRSTAT-NEXT-DUE-DATE
           MOVE DRPM-MATURITY-DATE
             TO DRSTAT-MATURITY-DATE
           MOVE DRPM-PLAN-OPTION-CD
             TO DRSTAT-PLAN-OPTION-CD
           MOVE DRPM-REASON-CD
             TO DRSTAT-REASON-CD
           MOVE DRPM-CHANNEL-CD
             TO DRSTAT-CHANNEL-CD
           MOVE DRPM-HOLDER-TYPE-CD
             TO DRSTAT-HOLDER-TYPE-CD
           MOVE DRPM-OVERRIDE-FLAG
             TO DRSTAT-OVERRIDE-FLAG
           MOVE DRPM-STOP-FLAG
             TO DRSTAT-STOP-FLAG
           MOVE DRPM-AUTO-RENEW-FLAG
             TO DRSTAT-AUTO-RENEW-FLAG
           MOVE DRPM-EXCEPT-CNT
             TO DRSTAT-EXCEPT-CNT
           MOVE DRPM-NOTICE-CNT
             TO DRSTAT-NOTICE-CNT
           MOVE DRPM-RATE
             TO DRSTAT-RATE
           MOVE DRPM-DISCOUNT
             TO DRSTAT-DISCOUNT
           MOVE DRPM-CUST-ID
             TO DRSTAT-CUST-ID
           MOVE DRPM-REF-NO
             TO DRSTAT-REF-NO
           CALL 'DRSTAT' USING DRSTAT-PARMS
           EVALUATE DRSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE DRSTAT-CASH-PAY-AMT
                     TO DRPM-CASH-PAY-AMT
                   MOVE DRSTAT-DIVIDEND-AMT
                     TO DRPM-DIVIDEND-AMT
                   MOVE DRSTAT-WITHHOLD-YTD
                     TO DRPM-WITHHOLD-YTD
                   MOVE DRSTAT-STOP-FLAG
                     TO DRPM-STOP-FLAG
                   MOVE DRSTAT-AUTO-RENEW-FLAG
                     TO DRPM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-DRSTAT' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE DRP FIELDS TO DRELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-DRELG.
           INITIALIZE DRELG-PARMS
           MOVE DRPM-REINVEST-AMT
             TO DRELG-REINVEST-AMT
           MOVE DRPM-WITHHOLD-AMT
             TO DRELG-WITHHOLD-AMT
           MOVE DRPM-FEE-YTD
             TO DRELG-FEE-YTD
           MOVE DRPM-SHARES-AMT-AMT
             TO DRELG-SHARES-AMT-AMT
           MOVE DRPM-CASH-PAY-AMT
             TO DRELG-CASH-PAY-AMT
           MOVE DRPM-CASH-PAY-YTD
             TO DRELG-CASH-PAY-YTD
           MOVE DRPM-LAST-MAINT-DATE
             TO DRELG-LAST-MAINT-DATE
           MOVE DRPM-MATURITY-DATE
             TO DRELG-MATURITY-DATE
           MOVE DRPM-OPEN-DATE
             TO DRELG-OPEN-DATE
           MOVE DRPM-EFFECTIVE-DATE
             TO DRELG-EFFECTIVE-DATE
           MOVE DRPM-SOURCE-CD
             TO DRELG-SOURCE-CD
           MOVE DRPM-PLAN-OPTION-CD
             TO DRELG-PLAN-OPTION-CD
           MOVE DRPM-CYCLE-CD
             TO DRELG-CYCLE-CD
           MOVE DRPM-CHANNEL-CD
             TO DRELG-CHANNEL-CD
           MOVE DRPM-STOP-FLAG
             TO DRELG-STOP-FLAG
           MOVE DRPM-SUSPEND-FLAG
             TO DRELG-SUSPEND-FLAG
           MOVE DRPM-PREFERRED-FLAG
             TO DRELG-PREFERRED-FLAG
           MOVE DRPM-EXCEPT-CNT
             TO DRELG-EXCEPT-CNT
           MOVE DRPM-ITEM-CNT
             TO DRELG-ITEM-CNT
           MOVE DRPM-DISCOUNT
             TO DRELG-DISCOUNT
           MOVE DRPM-RATE
             TO DRELG-RATE
           MOVE DRPM-REF-NO
             TO DRELG-REF-NO
           MOVE DRPM-CUST-ID
             TO DRELG-CUST-ID
           CALL 'DRELG' USING DRELG-PARMS
           EVALUATE DRELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE DRELG-REINVEST-AMT
                     TO DRPM-REINVEST-AMT
                   MOVE DRELG-FEE-YTD
                     TO DRPM-FEE-YTD
                   MOVE DRELG-CASH-PAY-AMT
                     TO DRPM-CASH-PAY-AMT
                   MOVE DRELG-PREFERRED-FLAG
                     TO DRPM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-DRELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE DRP FIELDS TO DRLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-DRLIM.
           INITIALIZE DRLIM-PARMS
           MOVE DRPM-CASH-PAY-AMT
             TO DRLIM-CASH-PAY-AMT
           MOVE DRPM-DIVIDEND-AMT
             TO DRLIM-DIVIDEND-AMT
           MOVE DRPM-CASH-PAY-YTD
             TO DRLIM-CASH-PAY-YTD
           MOVE DRPM-WITHHOLD-YTD
             TO DRLIM-WITHHOLD-YTD
           MOVE DRPM-WITHHOLD-AMT
             TO DRLIM-WITHHOLD-AMT
           MOVE DRPM-REINVEST-AMT
             TO DRLIM-REINVEST-AMT
           MOVE DRPM-NEXT-REVIEW-DATE
             TO DRLIM-NEXT-REVIEW-DATE
           MOVE DRPM-LAST-PAY-DATE
             TO DRLIM-LAST-PAY-DATE
           MOVE DRPM-MATURITY-DATE
             TO DRLIM-MATURITY-DATE
           MOVE DRPM-LAST-MAINT-DATE
             TO DRLIM-LAST-MAINT-DATE
           MOVE DRPM-HOLDER-TYPE-CD
             TO DRLIM-HOLDER-TYPE-CD
           MOVE DRPM-CHANNEL-CD
             TO DRLIM-CHANNEL-CD
           MOVE DRPM-SOURCE-CD
             TO DRLIM-SOURCE-CD
           MOVE DRPM-PLAN-OPTION-CD
             TO DRLIM-PLAN-OPTION-CD
           MOVE DRPM-EXEMPT-FLAG
             TO DRLIM-EXEMPT-FLAG
           MOVE DRPM-PREFERRED-FLAG
             TO DRLIM-PREFERRED-FLAG
           MOVE DRPM-SUSPEND-FLAG
             TO DRLIM-SUSPEND-FLAG
           MOVE DRPM-PAYMENT-CNT
             TO DRLIM-PAYMENT-CNT
           MOVE DRPM-NOTICE-CNT
             TO DRLIM-NOTICE-CNT
           MOVE DRPM-RATE
             TO DRLIM-RATE
           MOVE DRPM-DISCOUNT
             TO DRLIM-DISCOUNT
           MOVE DRPM-CUST-ID
             TO DRLIM-CUST-ID
           MOVE DRPM-DESC
             TO DRLIM-DESC
           CALL 'DRLIM' USING DRLIM-PARMS
           EVALUATE DRLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE DRLIM-CASH-PAY-AMT
                     TO DRPM-CASH-PAY-AMT
                   MOVE DRLIM-DIVIDEND-AMT
                     TO DRPM-DIVIDEND-AMT
                   MOVE DRLIM-CASH-PAY-YTD
                     TO DRPM-CASH-PAY-YTD
                   MOVE DRLIM-WITHHOLD-YTD
                     TO DRPM-WITHHOLD-YTD
                   MOVE DRLIM-REINVEST-AMT
                     TO DRPM-REINVEST-AMT
                   MOVE DRLIM-SUSPEND-FLAG
                     TO DRPM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-DRLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE DRPT-KEY TO RD-KEY
           MOVE DRPT-TRAN-CD TO RD-CODE
           MOVE DRPT-FEE-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE DRPT-KEY TO RD-KEY
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
           DISPLAY 'DRPPURG READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'DRPPURG' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
