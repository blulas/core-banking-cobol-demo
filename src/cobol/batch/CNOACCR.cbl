      *================================================================*
      * PROGRAM  : CNOACCR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : COIN AND CURRENCY - DAILY ACCRUAL. READS THE CCO    *
      *            TRANSACTIONS, UPDATES THE CCO MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. BRANCH COIN AND CURRENCY ORDERS *
      *            AND ARMORED CARRIER SHIPMENT                        *
      * INPUT    : CCOMST (KSDS), CCOTRN, BANKCTL                      *
      * OUTPUT   : CCOOUT, CNOACRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CNPEN, CNCAL, CNFEE     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-09-13 DPS  ORIGINAL                                       *
      * 1989-04-02 RJT  COIN NOTICE TEXT PER LEGAL REQ7551             *
      * 1997-06-03 TWH  CURRENCY EXCEPTIONS TO OPS QUEUE CHG9463       *
      * 1999-05-05 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-08-03 LMB  VARIANCE CALCULATION CORRECTED INC9518         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CNOACCR.
       AUTHOR. B H SANDERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "CCOMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CCOM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "CCOTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "CCOOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "CNOACRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY CCOMST.
       FD  TRAN-FILE.
       COPY CCOTRN.
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
                                           VALUE 16.000000.
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
      * WS-STRAP-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STRAP-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000738000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000237500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000710500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017500000000164000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018500000000218500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018750000000501000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021250000000801000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021300000000588000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021800000000745000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024300000000465500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026800000000398000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027800000000722000'.
       01  WS-STRAP-SCHED-TBL REDEFINES WS-STRAP-SCHED-VALUES.
           05  WS-STRAP-SCHED-ROW OCCURS 12 TIMES.
               10  WS-STRAP-SCHED-LOW      PIC 9(09)V99.
               10  WS-STRAP-SCHED-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-STRAP-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STRAP-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000221000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000414000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000580000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012750000000245000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013250000000040000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018250000000350000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020750000000609500'.
       01  WS-STRAP-GRID-TBL REDEFINES WS-STRAP-GRID-VALUES.
           05  WS-STRAP-GRID-ROW OCCURS 7 TIMES.
               10  WS-STRAP-GRID-LOW       PIC 9(09)V99.
               10  WS-STRAP-GRID-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VARIANCE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VARIANCE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000634000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000794000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000690000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001300000000453500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001800000000884500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002800000000667000'.
       01  WS-VARIANCE-SCHED-TBL REDEFINES WS-VARIANCE-SCHED-VALUES.
           05  WS-VARIANCE-SCHED-ROW OCCURS 6 TIMES.
               10  WS-VARIANCE-SCHED-LOW   PIC 9(09)V99.
               10  WS-VARIANCE-SCHED-RATE  PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CNOACCR  COIN AND CURRENCY'.
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
       COPY CNPENP.
       COPY CNCALP.
       COPY CNFEEP.

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
               MOVE 'CCOMST' TO ABND-FILE-NAME
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
           MOVE CCOT-KEY TO CCOM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-AGE-NEXT-DUE
               PERFORM 3010-FLAG-FEE
               PERFORM 3020-SPLIT-CURRENCY
               PERFORM 3030-ACCUM-REASON
               PERFORM 3040-AGE-POST
               PERFORM 3050-PRORATE-VARIANCE
               PERFORM 3060-LIMIT-SHIPMENT
               PERFORM 3070-SELECT-FEE-PCT
               PERFORM 3080-PRORATE-STRAP
               PERFORM 3090-DESCRIBE-SOURCE
               PERFORM 3100-PRORATE-VARIANCE
               PERFORM 3110-SELECT-FEE-PCT
               PERFORM 3120-SET-DENOM
               PERFORM 3130-PRORATE-COIN
               PERFORM 3140-FLAG-FEE
               PERFORM 3150-AGE-EFF
               PERFORM 3160-ACCUM-STATUS
               PERFORM 3170-SPLIT-VARIANCE
               PERFORM 3180-ACCRUE-VARIANCE
               PERFORM 3190-PRORATE-FEE
               PERFORM 3200-FLAG-COIN
               PERFORM 3210-SELECT-FEE-PCT
               PERFORM 3220-ACCUM-TRAN
               PERFORM 3230-DESCRIBE-CHANNEL
               PERFORM 3240-PRORATE-VARIANCE
               PERFORM 3250-SPLIT-FEE
               PERFORM 3260-AGE-EXPIRY
               PERFORM 3270-SELECT-RATE
               PERFORM 3280-CHECK-LAST-ACT
               PERFORM 3290-COUNT-CYCLE
               PERFORM 3300-PRORATE-STRAP
               PERFORM 3310-PRORATE-FEE
               PERFORM 3320-ASSESS-REVERSAL
               PERFORM 3330-ACCRUE-VARIANCE
               PERFORM 3340-SET-CHANNEL
               PERFORM 3350-DESCRIBE-DENOM
               PERFORM 3360-AGE-EFF
               PERFORM 3370-AGE-OPEN
               PERFORM 3380-EDIT-NAME
               PERFORM 3390-ACCRUE-PENALTY-PCT
               PERFORM 3400-ASSESS-EXEMPT
               PERFORM 3410-EDIT-REJECT
               PERFORM 3420-CHECK-NEXT-DUE
               PERFORM 3430-ACCRUE-STRAP
               PERFORM 3440-PRORATE-SHIPMENT
               PERFORM 3450-ACCUM-REASON
               PERFORM 3460-EDIT-DESC
               PERFORM 3470-COUNT-NOTICE
               PERFORM 3480-SPLIT-SHIPMENT
               PERFORM 3490-SPLIT-STRAP
               PERFORM 3500-ACCRUE-CURRENCY
               PERFORM 3510-DESCRIBE-STATUS
               PERFORM 3520-PRORATE-COIN
               PERFORM 3530-SET-REASON
               PERFORM 3540-ACCUM-CHANNEL
               PERFORM 3550-PRORATE-FEE
               PERFORM 3560-SPLIT-SHIPMENT
               PERFORM 3570-ACCRUE-RATE
               PERFORM 3580-PRORATE-COIN
               PERFORM 3590-CHECK-NEXT-DUE
               PERFORM 3600-SPLIT-SHIPMENT
               PERFORM 3610-ASSESS-NOTICE-SENT
               PERFORM 3620-SELECT-FEE-PCT
               PERFORM 3630-SET-CHANNEL
               PERFORM 3640-LIMIT-COIN
               PERFORM 3650-FLAG-VARIANCE
               PERFORM 3660-SET-CARRIER
               PERFORM 3670-ACCUM-TRAN
               PERFORM 3680-DESCRIBE-STATUS
               PERFORM 3690-EDIT-CUST-ID
               PERFORM 3700-EDIT-REJECT
               PERFORM 3710-LIMIT-SHIPMENT
               PERFORM 3720-SET-DENOM
               PERFORM 3730-SPLIT-COIN
               PERFORM 3740-ASSESS-STOP
               PERFORM 3750-SET-SOURCE
               PERFORM 3760-LIMIT-STRAP
               PERFORM 3770-COUNT-ITEM
               PERFORM 3780-FLAG-CURRENCY
               PERFORM 3790-SET-REASON
               PERFORM 3800-COUNT-NOTICE
               PERFORM 3810-DESCRIBE-TRAN
               PERFORM 3820-DESCRIBE-TRAN
               PERFORM 3830-ASSESS-AUTO-RENEW
               PERFORM 3840-ACCUM-STATUS
               PERFORM 3850-CHECK-LAST-ACT
               PERFORM 3860-SET-STATUS
               PERFORM 3870-ASSESS-AUTO-RENEW
               PERFORM 3880-LIMIT-COIN
               PERFORM 3890-SELECT-PENALTY-PCT
               PERFORM 3900-COUNT-CYCLE
               PERFORM 3910-SELECT-RATE
               PERFORM 3920-LIMIT-VARIANCE
               PERFORM 3930-AGE-OPEN
               PERFORM 3940-COUNT-ITEM
               PERFORM 3950-COUNT-SEQ
               PERFORM 3960-FLAG-CURRENCY
               PERFORM 3970-AGE-NOTICE
               PERFORM 3980-EDIT-CUST-ID
               PERFORM 3990-DESCRIBE-CARRIER
               PERFORM 4000-DESCRIBE-SOURCE
               PERFORM 4010-CHECK-NOTICE
               PERFORM 4020-EDIT-CONTACT
               PERFORM 4030-ACCUM-CHANNEL
               PERFORM 4040-LIMIT-STRAP
               PERFORM 4050-FLAG-FEE
               PERFORM 4060-ASSESS-STOP
               PERFORM 4070-ASSESS-AUTO-RENEW
               PERFORM 4080-LIMIT-COIN
               PERFORM 4090-COUNT-NOTICE
               PERFORM 4100-EDIT-NAME
               PERFORM 4110-SPLIT-CURRENCY
               PERFORM 4120-CHECK-NEXT-DUE
               PERFORM 4130-COUNT-NOTICE
               PERFORM 4140-LIMIT-STRAP
               PERFORM 4150-ACCRUE-COIN
               PERFORM 4160-FLAG-STRAP
               PERFORM 4170-COUNT-NOTICE
               PERFORM 4180-AGE-NOTICE
               PERFORM 4190-SET-CHANNEL
               PERFORM 4200-ACCUM-CHANNEL
               PERFORM 4210-ACCRUE-COIN
               PERFORM 4220-ASSESS-HOLD
               PERFORM 4230-SET-CARRIER
               PERFORM 4240-SPLIT-FEE
               PERFORM 4250-LIMIT-SHIPMENT
               PERFORM 4260-ASSESS-STOP
               PERFORM 4270-SELECT-PENALTY-PCT
               PERFORM 4280-ACCUM-CHANNEL
               PERFORM 4290-AGE-EFFECTIVE
               PERFORM 4300-ASSESS-STOP
               PERFORM 4310-CHECK-NEXT-DUE
               PERFORM 4320-COUNT-SEQ
               PERFORM 4330-SELECT-FEE-PCT
               PERFORM 4340-ASSESS-STOP
               PERFORM 4350-ACCUM-REASON
               PERFORM 4360-ACCUM-CHANNEL
               PERFORM 4370-EDIT-ADDR
               PERFORM 4380-SPLIT-SHIPMENT
               PERFORM 4390-ASSESS-HOLD
               PERFORM 4400-SPLIT-STRAP
               PERFORM 4410-CHECK-NOTICE
               PERFORM 4420-ASSESS-REVERSAL
               PERFORM 4430-ASSESS-EXEMPT
               PERFORM 4440-FLAG-COIN
               PERFORM 4450-FLAG-SHIPMENT
               PERFORM 4460-AGE-NEXT-REVIEW
               PERFORM 4470-ACCRUE-VARIANCE
               PERFORM 4480-CHECK-NEXT-REVIEW
               PERFORM 4490-ACCUM-CARRIER
               PERFORM 4500-PRORATE-COIN
               PERFORM 4510-LIMIT-VARIANCE
               PERFORM 4520-ACCUM-TRAN
               PERFORM 4530-ACCRUE-STRAP
               PERFORM 4540-DESCRIBE-SOURCE
               PERFORM 4550-LIMIT-STRAP
               PERFORM 4560-CHECK-EXPIRY
               PERFORM 4570-COUNT-SEQ
               PERFORM 4580-LIMIT-SHIPMENT
               PERFORM 4590-DESCRIBE-SOURCE
               PERFORM 4600-SPLIT-SHIPMENT
               PERFORM 4610-SELECT-PENALTY-PCT
               PERFORM 4620-AGE-LAST-PAY
               PERFORM 4630-ACCRUE-SHIPMENT
               PERFORM 4640-LIMIT-CURRENCY
               PERFORM 4650-FLAG-ITEM
               PERFORM 4660-SET-DENOM
               PERFORM 4670-ACCRUE-COIN
               PERFORM 4680-SET-CHANNEL
               PERFORM 4690-CHECK-EFF
               PERFORM 7010-CALL-CNPEN
               PERFORM 7020-CALL-CNCAL
               PERFORM 7030-CALL-CNFEE
               REWRITE CCO-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'CCOMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM CCO-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3000-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-CURRENCY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CCOM-CURRENCY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CCOM-CURRENCY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CCOM-CURRENCY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-CURRENCY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3010-FLAG-FEE.
           IF CCOM-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CURRENCY AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3020-SPLIT-CURRENCY.
           COMPUTE WS-WORK-AMT = CCOM-CURRENCY-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CCOM-CURRENCY-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STRAP AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3030-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-REASON-CD
                   ADD CCOM-STRAP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-STRAP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3040-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF CCOT-POST-DATE > 0 AND CCOT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOT-POST-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SHIPMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VARIANCE AMT.
      *----------------------------------------------------------------*
       3050-PRORATE-VARIANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-VARIANCE-AMT ROUNDED =
                   CCOM-SHIPMENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOM-VARIANCE-AMT
           END-IF
           ADD CCOM-VARIANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SHIPMENT AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3060-LIMIT-SHIPMENT.
           IF CCOT-SHIPMENT-AMT < 25.00
               MOVE 25.00 TO CCOT-SHIPMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-SHIPMENT-AMT > 1000.00
               MOVE 1000.00 TO CCOT-SHIPMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VARIANCE SCHED TIER FOR THE CURRENCY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3070-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VARIANCE-SCHED-RATE(1) TO CCOM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF CCOM-CURRENCY-YTD NOT < WS-VARIANCE-SCHED-LOW(WS-TX)
                   MOVE WS-VARIANCE-SCHED-RATE(WS-TX) TO CCOM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STRAP YTD.
      *----------------------------------------------------------------*
       3080-PRORATE-STRAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-STRAP-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOM-STRAP-YTD
           END-IF
           ADD CCOM-STRAP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3090-DESCRIBE-SOURCE.
           EVALUATE CCOM-SOURCE-CD
               WHEN 'CFR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DTX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'EEA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'EUA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TPB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WVN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YTT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SHIPMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VARIANCE YTD.
      *----------------------------------------------------------------*
       3100-PRORATE-VARIANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-VARIANCE-YTD ROUNDED =
                   CCOM-SHIPMENT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-VARIANCE-YTD
           END-IF
           ADD CCOM-VARIANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE STRAP SCHED TIER FOR THE STRAP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3110-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STRAP-SCHED-RATE(1) TO CCOM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CCOM-STRAP-YTD NOT < WS-STRAP-SCHED-LOW(WS-TX)
                   MOVE WS-STRAP-SCHED-RATE(WS-TX) TO CCOM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DENOM CD:
      * B -> D WHEN NEXT DUE DATE IS PAST
      * D -> P WHEN NEXT DUE DATE IS PAST
      * P -> S WHEN NEXT DUE DATE IS PAST
      * S -> W WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3120-SET-DENOM.
           EVALUATE CCOM-DENOM-CD
               WHEN 'B'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CURRENCY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COIN AMT.
      *----------------------------------------------------------------*
       3130-PRORATE-COIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-COIN-AMT ROUNDED =
                   CCOM-CURRENCY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-COIN-AMT
           END-IF
           ADD CCOM-COIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FEE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3140-FLAG-FEE.
           IF CCOM-FEE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3150-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE STRAP AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3160-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-STATUS-CD
                   ADD CCOM-STRAP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-STRAP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VARIANCE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-VARIANCE.
           COMPUTE WS-WORK-AMT = CCOM-VARIANCE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CCOM-VARIANCE-YTD - (WS-WORK-AMT * 3)
           COMPUTE CCOM-COIN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VARIANCE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3180-ACCRUE-VARIANCE.
           IF CCOM-VARIANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-VARIANCE-YTD * CCOM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-COIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VARIANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3190-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-FEE-AMT ROUNDED =
                   CCOT-VARIANCE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-FEE-AMT
           END-IF
           ADD CCOM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * COIN AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3200-FLAG-COIN.
           IF CCOM-COIN-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CCOM-COIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VARIANCE SCHED TIER FOR THE COIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3210-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VARIANCE-SCHED-RATE(1) TO CCOM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF CCOT-COIN-AMT NOT < WS-VARIANCE-SCHED-LOW(WS-TX)
                   MOVE WS-VARIANCE-SCHED-RATE(WS-TX) TO CCOM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3220-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOT-TRAN-CD
                   ADD CCOM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3230-DESCRIBE-CHANNEL.
           EVALUATE CCOT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VARIANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3240-PRORATE-VARIANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-VARIANCE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3250-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = CCOM-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CCOM-FEE-AMT - (WS-WORK-AMT * 12)
           COMPUTE CCOT-VARIANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3260-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE VARIANCE SCHED TIER FOR THE CURRENCY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VARIANCE-SCHED-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF CCOM-CURRENCY-AMT NOT < WS-VARIANCE-SCHED-LOW(WS-TX)
                   MOVE WS-VARIANCE-SCHED-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3280-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CCOM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3290-COUNT-CYCLE.
           IF CCOM-CYCLE-CNT < 9999999
               ADD 1 TO CCOM-CYCLE-CNT
           END-IF
           IF CCOM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STRAP AMT.
      *----------------------------------------------------------------*
       3300-PRORATE-STRAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-STRAP-AMT ROUNDED =
                   CCOM-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-STRAP-AMT
           END-IF
           ADD CCOM-STRAP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE STRAP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3310-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-FEE-AMT ROUNDED =
                   CCOM-STRAP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-FEE-AMT
           END-IF
           ADD CCOM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE STRAP AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3320-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CCOT-STRAP-AMT *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOM-CARRIER-CD = 'J'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN CCOM-CARRIER-CD = 'L'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CCOT-STRAP-AMT *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VARIANCE YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3330-ACCRUE-VARIANCE.
           IF CCOM-VARIANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-VARIANCE-YTD * CCOM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CCOT-SHIPMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AW -> DW WHEN EFFECTIVE DATE IS PAST
      * DW -> GN WHEN EFFECTIVE DATE IS PAST
      * GN -> RS WHEN EFFECTIVE DATE IS PAST
      * RS -> UZ WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3340-SET-CHANNEL.
           EVALUATE CCOM-CHANNEL-CD
               WHEN 'AW'
                   IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DW' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DW'
                   IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GN' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GN'
                   IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RS' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RS'
                   IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UZ' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE DENOM CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3350-DESCRIBE-DENOM.
           EVALUATE CCOM-DENOM-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'D'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'P'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'S'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'W'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3360-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-COIN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CCOM-COIN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CCOM-COIN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOM-COIN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-COIN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CCOM-OPEN-DATE > 0 AND CCOM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-VARIANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CCOM-VARIANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CCOM-VARIANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOM-VARIANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-VARIANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E245 NAME REQUIRED
      * E815 CURRENCY YTD NEGATIVE
      * E430 CURRENCY YTD OVER 99999
      * E672 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3380-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-NAME = SPACES
                   MOVE 'E245' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-YTD < 0
                   MOVE 'E815' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-YTD > 99999.00
                   MOVE 'E430' TO WS-REJECT-CODE
               WHEN CCOM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E672' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3390-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * CCOM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CCOM-VARIANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CURRENCY YTD FEE ON THE COIN YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3400-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CCOM-CURRENCY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-STOP-FLAG = 'Y'
                   COMPUTE CCOM-CURRENCY-YTD ROUNDED = CCOM-COIN-YTD *
                       CCOM-FEE-PCT / 200
               WHEN CCOM-STATUS-CD = 'ADH'
                   MOVE 20.00 TO CCOM-CURRENCY-YTD
               WHEN CCOM-STATUS-CD = 'FVV'
                   MOVE 20.00 TO CCOM-CURRENCY-YTD
               WHEN OTHER
                   COMPUTE CCOM-CURRENCY-YTD ROUNDED = CCOM-COIN-YTD *
                       CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOM-CURRENCY-YTD > 0 AND CCOM-CURRENCY-YTD < 1.00
               MOVE 1.00 TO CCOM-CURRENCY-YTD
           END-IF
           IF CCOM-CURRENCY-YTD > 50.00
               MOVE 50.00 TO CCOM-CURRENCY-YTD
           END-IF
           ADD CCOM-CURRENCY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E626 REJECT CD REQUIRED
      * E943 WORK AMT 2 NEGATIVE
      * E030 WORK AMT 2 OVER 25000
      * E589 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3410-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOT-REJECT-CD = SPACES
                   MOVE 'E626' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E943' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E030' TO WS-REJECT-CODE
               WHEN CCOM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E589' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3420-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO CCOT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STRAP YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3430-ACCRUE-STRAP.
           IF CCOM-STRAP-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-STRAP-YTD * CCOM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-SHIPMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CURRENCY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPMENT AMT.
      *----------------------------------------------------------------*
       3440-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOT-SHIPMENT-AMT ROUNDED =
                   CCOM-CURRENCY-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOT-SHIPMENT-AMT
           END-IF
           ADD CCOT-SHIPMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE VARIANCE AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3450-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-REASON-CD
                   ADD CCOM-VARIANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-VARIANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E377 DESC REQUIRED
      * E636 WORK AMT 2 NEGATIVE
      * E692 WORK AMT 2 OVER 50000
      * E819 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3460-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-DESC = SPACES
                   MOVE 'E377' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E636' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E692' TO WS-REJECT-CODE
               WHEN CCOM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E819' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3470-COUNT-NOTICE.
           IF CCOM-NOTICE-CNT < 9999999
               ADD 1 TO CCOM-NOTICE-CNT
           END-IF
           IF CCOM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO CCOT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHIPMENT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3480-SPLIT-SHIPMENT.
           COMPUTE WS-WORK-AMT = CCOM-SHIPMENT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CCOM-SHIPMENT-AMT - (WS-WORK-AMT * 2)
           COMPUTE CCOM-COIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STRAP AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3490-SPLIT-STRAP.
           COMPUTE WS-WORK-AMT = CCOM-STRAP-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CCOM-STRAP-AMT - (WS-WORK-AMT * 12)
           COMPUTE CCOT-COIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CURRENCY AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3500-ACCRUE-CURRENCY.
           IF CCOM-CURRENCY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-CURRENCY-AMT * CCOM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CCOM-SHIPMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3510-DESCRIBE-STATUS.
           EVALUATE CCOM-STATUS-CD
               WHEN 'ADH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FVV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GCB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UZP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VXB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WTJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XNG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COIN YTD.
      *----------------------------------------------------------------*
       3520-PRORATE-COIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-COIN-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-COIN-YTD
           END-IF
           ADD CCOM-COIN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * JL -> LW WHEN OPEN DATE IS PAST
      * LW -> PS WHEN OPEN DATE IS PAST
      * PS -> JL WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3530-SET-REASON.
           EVALUATE CCOM-REASON-CD
               WHEN 'JL'
                   IF CCOM-OPEN-DATE > 0 AND CCOM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LW' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LW'
                   IF CCOM-OPEN-DATE > 0 AND CCOM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF CCOM-OPEN-DATE > 0 AND CCOM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE COIN YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3540-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOT-CHANNEL-CD
                   ADD CCOM-COIN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-COIN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STRAP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3550-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-FEE-YTD ROUNDED =
                   CCOT-STRAP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-FEE-YTD
           END-IF
           ADD CCOM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT SHIPMENT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3560-SPLIT-SHIPMENT.
           COMPUTE WS-WORK-AMT = CCOT-SHIPMENT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CCOT-SHIPMENT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE CCOM-STRAP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3570-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CCOM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CCOM-CURRENCY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VARIANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COIN AMT.
      *----------------------------------------------------------------*
       3580-PRORATE-COIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOT-COIN-AMT ROUNDED =
                   CCOM-VARIANCE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOT-COIN-AMT
           END-IF
           ADD CCOT-COIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3590-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CCOM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHIPMENT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3600-SPLIT-SHIPMENT.
           COMPUTE WS-WORK-AMT = CCOM-SHIPMENT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CCOM-SHIPMENT-YTD - (WS-WORK-AMT *
               12)
           COMPUTE CCOT-COIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3610-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN CCOM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       CCOM-RATE / 200
               WHEN CCOM-SOURCE-CD = 'CFR'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN CCOM-SOURCE-CD = 'DTX'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       CCOM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE STRAP GRID TIER FOR THE STRAP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3620-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STRAP-GRID-RATE(1) TO CCOM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CCOM-STRAP-YTD NOT < WS-STRAP-GRID-LOW(WS-TX)
                   MOVE WS-STRAP-GRID-RATE(WS-TX) TO CCOM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AW -> DW WHEN NEXT REVIEW DATE IS PAST
      * DW -> GN WHEN NEXT REVIEW DATE IS PAST
      * GN -> RS WHEN NEXT REVIEW DATE IS PAST
      * RS -> UZ WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-CHANNEL.
           EVALUATE CCOM-CHANNEL-CD
               WHEN 'AW'
                   IF CCOM-NEXT-REVIEW-DATE > 0 AND
                       CCOM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DW' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DW'
                   IF CCOM-NEXT-REVIEW-DATE > 0 AND
                       CCOM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GN' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GN'
                   IF CCOM-NEXT-REVIEW-DATE > 0 AND
                       CCOM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'RS' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RS'
                   IF CCOM-NEXT-REVIEW-DATE > 0 AND
                       CCOM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UZ' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COIN AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3640-LIMIT-COIN.
           IF CCOT-COIN-AMT < 25.00
               MOVE 25.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-COIN-AMT > 1000.00
               MOVE 1000.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VARIANCE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3650-FLAG-VARIANCE.
           IF CCOM-VARIANCE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-VARIANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARRIER CD:
      * J -> L WHEN NEXT DUE DATE IS PAST
      * L -> M WHEN NEXT DUE DATE IS PAST
      * M -> X WHEN NEXT DUE DATE IS PAST
      * X -> J WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3660-SET-CARRIER.
           EVALUATE CCOM-CARRIER-CD
               WHEN 'J'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3670-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOT-TRAN-CD
                   ADD CCOM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3680-DESCRIBE-STATUS.
           EVALUATE CCOM-STATUS-CD
               WHEN 'ADH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FVV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GCB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UZP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VXB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WTJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XNG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E230 CUST ID REQUIRED
      * E674 CURRENCY YTD NEGATIVE
      * E680 CURRENCY YTD OVER 250000
      * E492 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3690-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CUST-ID = SPACES
                   MOVE 'E230' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-YTD < 0
                   MOVE 'E674' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-YTD > 250000.00
                   MOVE 'E680' TO WS-REJECT-CODE
               WHEN CCOM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E492' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E343 REJECT CD REQUIRED
      * E501 VARIANCE AMT NEGATIVE
      * E720 VARIANCE AMT OVER 1000000
      * E586 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3700-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOT-REJECT-CD = SPACES
                   MOVE 'E343' TO WS-REJECT-CODE
               WHEN CCOM-VARIANCE-AMT < 0
                   MOVE 'E501' TO WS-REJECT-CODE
               WHEN CCOM-VARIANCE-AMT > 1000000.00
                   MOVE 'E720' TO WS-REJECT-CODE
               WHEN CCOM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E586' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SHIPMENT YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3710-LIMIT-SHIPMENT.
           IF CCOM-SHIPMENT-YTD < 0.00
               MOVE 0.00 TO CCOM-SHIPMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-SHIPMENT-YTD > 10000.00
               MOVE 10000.00 TO CCOM-SHIPMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DENOM CD:
      * B -> D WHEN EFF DATE IS PAST
      * D -> P WHEN EFF DATE IS PAST
      * P -> S WHEN EFF DATE IS PAST
      * S -> W WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-DENOM.
           EVALUATE CCOM-DENOM-CD
               WHEN 'B'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT COIN AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3730-SPLIT-COIN.
           COMPUTE WS-WORK-AMT = CCOT-COIN-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CCOT-COIN-AMT - (WS-WORK-AMT * 12)
           COMPUTE CCOM-VARIANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CURRENCY YTD FEE ON THE SHIPMENT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3740-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CCOM-STOP-FLAG = 'Y'
                   MOVE ZERO TO CCOM-CURRENCY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CCOM-CURRENCY-YTD ROUNDED = CCOM-SHIPMENT-AMT
                       * CCOM-PENALTY-PCT / 200
               WHEN CCOM-SOURCE-CD = 'CFR'
                   MOVE 15.00 TO CCOM-CURRENCY-YTD
               WHEN CCOM-SOURCE-CD = 'DTX'
                   MOVE 10.00 TO CCOM-CURRENCY-YTD
               WHEN OTHER
                   COMPUTE CCOM-CURRENCY-YTD ROUNDED = CCOM-SHIPMENT-AMT
                       * CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-CURRENCY-YTD > 0 AND CCOM-CURRENCY-YTD < 5.00
               MOVE 5.00 TO CCOM-CURRENCY-YTD
           END-IF
           IF CCOM-CURRENCY-YTD > 75.00
               MOVE 75.00 TO CCOM-CURRENCY-YTD
           END-IF
           ADD CCOM-CURRENCY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * CFR -> DTX WHEN POST DATE IS PAST
      * DTX -> EEA WHEN POST DATE IS PAST
      * EEA -> EUA WHEN POST DATE IS PAST
      * EUA -> TPB WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-SOURCE.
           EVALUATE CCOM-SOURCE-CD
               WHEN 'CFR'
                   IF CCOT-POST-DATE > 0 AND CCOT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DTX' TO CCOM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DTX'
                   IF CCOT-POST-DATE > 0 AND CCOT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EEA' TO CCOM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EEA'
                   IF CCOT-POST-DATE > 0 AND CCOT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EUA' TO CCOM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EUA'
                   IF CCOT-POST-DATE > 0 AND CCOT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TPB' TO CCOM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STRAP YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3760-LIMIT-STRAP.
           IF CCOM-STRAP-YTD < 100.00
               MOVE 100.00 TO CCOM-STRAP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-STRAP-YTD > 500.00
               MOVE 500.00 TO CCOM-STRAP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3770-COUNT-ITEM.
           IF CCOM-ITEM-CNT < 9999999
               ADD 1 TO CCOM-ITEM-CNT
           END-IF
           IF CCOM-ITEM-CNT NOT < 10
               MOVE 'Y' TO CCOM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CURRENCY AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3780-FLAG-CURRENCY.
           IF CCOM-CURRENCY-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CCOM-CURRENCY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * JL -> LW WHEN EFF DATE IS PAST
      * LW -> PS WHEN EFF DATE IS PAST
      * PS -> JL WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-REASON.
           EVALUATE CCOM-REASON-CD
               WHEN 'JL'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LW' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LW'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3800-COUNT-NOTICE.
           IF CCOM-NOTICE-CNT < 9999999
               ADD 1 TO CCOM-NOTICE-CNT
           END-IF
           IF CCOM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO CCOM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3810-DESCRIBE-TRAN.
           EVALUATE CCOT-TRAN-CD
               WHEN 'OR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'AD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3820-DESCRIBE-TRAN.
           EVALUATE CCOT-TRAN-CD
               WHEN 'OR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'AD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE COIN YTD FEE ON THE WORK AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3830-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CCOM-COIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-HOLD-FLAG = 'Y'
                   COMPUTE CCOM-COIN-YTD ROUNDED = WS-WORK-AMT *
                       CCOM-FEE-PCT / 200
               WHEN CCOM-CHANNEL-CD = 'AW'
                   MOVE 20.00 TO CCOM-COIN-YTD
               WHEN CCOM-CHANNEL-CD = 'DW'
                   MOVE 10.00 TO CCOM-COIN-YTD
               WHEN OTHER
                   COMPUTE CCOM-COIN-YTD ROUNDED = WS-WORK-AMT *
                       CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOM-COIN-YTD > 0 AND CCOM-COIN-YTD < 1.00
               MOVE 1.00 TO CCOM-COIN-YTD
           END-IF
           IF CCOM-COIN-YTD > 75.00
               MOVE 75.00 TO CCOM-COIN-YTD
           END-IF
           ADD CCOM-COIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3840-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-STATUS-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CCOM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO CCOM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * ADH -> FVV WHEN LAST ACT DATE IS PAST
      * FVV -> GCB WHEN LAST ACT DATE IS PAST
      * GCB -> UZP WHEN LAST ACT DATE IS PAST
      * UZP -> VXB WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3860-SET-STATUS.
           EVALUATE CCOM-STATUS-CD
               WHEN 'ADH'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FVV' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FVV'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GCB' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GCB'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UZP' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UZP'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VXB' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE STRAP AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3870-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CCOT-STRAP-AMT *
                       CCOM-FEE-PCT / 200
               WHEN CCOT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN CCOT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CCOT-STRAP-AMT *
                       CCOM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * COIN AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3880-LIMIT-COIN.
           IF CCOT-COIN-AMT < 0.00
               MOVE 0.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-COIN-AMT > 2500.00
               MOVE 2500.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VARIANCE SCHED TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3890-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VARIANCE-SCHED-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF CCOM-FEE-AMT NOT < WS-VARIANCE-SCHED-LOW(WS-TX)
                   MOVE WS-VARIANCE-SCHED-RATE(WS-TX) TO
                       CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3900-COUNT-CYCLE.
           IF CCOM-CYCLE-CNT < 9999999
               ADD 1 TO CCOM-CYCLE-CNT
           END-IF
           IF CCOM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO CCOM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STRAP SCHED TIER FOR THE COIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3910-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-STRAP-SCHED-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CCOT-COIN-AMT NOT < WS-STRAP-SCHED-LOW(WS-TX)
                   MOVE WS-STRAP-SCHED-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VARIANCE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3920-LIMIT-VARIANCE.
           IF CCOT-VARIANCE-AMT < 10.00
               MOVE 10.00 TO CCOT-VARIANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-VARIANCE-AMT > 50000.00
               MOVE 50000.00 TO CCOT-VARIANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3930-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CCOM-OPEN-DATE > 0 AND CCOM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOT-COIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CCOT-COIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CCOT-COIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOT-COIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOT-COIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3940-COUNT-ITEM.
           IF CCOM-ITEM-CNT < 9999999
               ADD 1 TO CCOM-ITEM-CNT
           END-IF
           IF CCOM-ITEM-CNT NOT < 5
               MOVE 'Y' TO CCOT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3950-COUNT-SEQ.
           IF CCOT-SEQ < 9999999
               ADD 1 TO CCOT-SEQ
           END-IF
           IF CCOT-SEQ NOT < 12
               MOVE 'Y' TO CCOT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CURRENCY YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3960-FLAG-CURRENCY.
           IF CCOM-CURRENCY-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CCOM-CURRENCY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3970-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NOTICE-DATE > 0 AND CCOM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E651 CUST ID REQUIRED
      * E068 FEE YTD NEGATIVE
      * E657 FEE YTD OVER 10000
      * E755 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3980-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CUST-ID = SPACES
                   MOVE 'E651' TO WS-REJECT-CODE
               WHEN CCOM-FEE-YTD < 0
                   MOVE 'E068' TO WS-REJECT-CODE
               WHEN CCOM-FEE-YTD > 10000.00
                   MOVE 'E657' TO WS-REJECT-CODE
               WHEN CCOM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E755' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARRIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-CARRIER.
           EVALUATE CCOM-CARRIER-CD
               WHEN 'J'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'M'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'X'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4000-DESCRIBE-SOURCE.
           EVALUATE CCOM-SOURCE-CD
               WHEN 'CFR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'DTX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'EEA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'EUA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TPB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WVN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YTT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4010-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO CCOM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E786 CONTACT REQUIRED
      * E933 SHIPMENT YTD NEGATIVE
      * E263 SHIPMENT YTD OVER 250000
      * E943 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4020-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CONTACT = SPACES
                   MOVE 'E786' TO WS-REJECT-CODE
               WHEN CCOM-SHIPMENT-YTD < 0
                   MOVE 'E933' TO WS-REJECT-CODE
               WHEN CCOM-SHIPMENT-YTD > 250000.00
                   MOVE 'E263' TO WS-REJECT-CODE
               WHEN CCOT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E943' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COIN YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4030-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-CHANNEL-CD
                   ADD CCOM-COIN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-COIN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STRAP AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4040-LIMIT-STRAP.
           IF CCOT-STRAP-AMT < 25.00
               MOVE 25.00 TO CCOT-STRAP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-STRAP-AMT > 500.00
               MOVE 500.00 TO CCOT-STRAP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4050-FLAG-FEE.
           IF CCOM-FEE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CCOM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPMENT YTD FEE ON THE CURRENCY AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4060-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CCOM-STOP-FLAG = 'Y'
                   MOVE ZERO TO CCOM-SHIPMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CCOM-SHIPMENT-YTD ROUNDED = CCOM-CURRENCY-AMT
                       * CCOM-PENALTY-PCT / 200
               WHEN CCOM-REASON-CD = 'JL'
                   MOVE 7.00 TO CCOM-SHIPMENT-YTD
               WHEN CCOM-REASON-CD = 'LW'
                   MOVE 15.00 TO CCOM-SHIPMENT-YTD
               WHEN OTHER
                   COMPUTE CCOM-SHIPMENT-YTD ROUNDED = CCOM-CURRENCY-AMT
                       * CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-SHIPMENT-YTD > 0 AND CCOM-SHIPMENT-YTD < 1.00
               MOVE 1.00 TO CCOM-SHIPMENT-YTD
           END-IF
           IF CCOM-SHIPMENT-YTD > 150.00
               MOVE 150.00 TO CCOM-SHIPMENT-YTD
           END-IF
           ADD CCOM-SHIPMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE COIN YTD FEE ON THE STRAP AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4070-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CCOM-COIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   COMPUTE CCOM-COIN-YTD ROUNDED = CCOM-STRAP-AMT *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOT-TRAN-CD = 'OR'
                   MOVE 12.00 TO CCOM-COIN-YTD
               WHEN CCOT-TRAN-CD = 'SH'
                   MOVE 10.00 TO CCOM-COIN-YTD
               WHEN OTHER
                   COMPUTE CCOM-COIN-YTD ROUNDED = CCOM-STRAP-AMT *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-COIN-YTD > 0 AND CCOM-COIN-YTD < 5.00
               MOVE 5.00 TO CCOM-COIN-YTD
           END-IF
           IF CCOM-COIN-YTD > 250.00
               MOVE 250.00 TO CCOM-COIN-YTD
           END-IF
           ADD CCOM-COIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * COIN AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4080-LIMIT-COIN.
           IF CCOM-COIN-AMT < 100.00
               MOVE 100.00 TO CCOM-COIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-COIN-AMT > 5000.00
               MOVE 5000.00 TO CCOM-COIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4090-COUNT-NOTICE.
           IF CCOM-NOTICE-CNT < 9999999
               ADD 1 TO CCOM-NOTICE-CNT
           END-IF
           IF CCOM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CCOT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E182 NAME REQUIRED
      * E111 WORK AMT 2 NEGATIVE
      * E494 WORK AMT 2 OVER 99999
      * E508 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4100-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-NAME = SPACES
                   MOVE 'E182' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E111' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E494' TO WS-REJECT-CODE
               WHEN CCOT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E508' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CURRENCY YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4110-SPLIT-CURRENCY.
           COMPUTE WS-WORK-AMT = CCOM-CURRENCY-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CCOM-CURRENCY-YTD - (WS-WORK-AMT * 4)
           COMPUTE CCOM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4120-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CCOM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4130-COUNT-NOTICE.
           IF CCOM-NOTICE-CNT < 9999999
               ADD 1 TO CCOM-NOTICE-CNT
           END-IF
           IF CCOM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO CCOM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STRAP AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4140-LIMIT-STRAP.
           IF CCOT-STRAP-AMT < 50.00
               MOVE 50.00 TO CCOT-STRAP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-STRAP-AMT > 2500.00
               MOVE 2500.00 TO CCOT-STRAP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COIN AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4150-ACCRUE-COIN.
           IF CCOT-COIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-COIN-AMT * CCOM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOT-STRAP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STRAP AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4160-FLAG-STRAP.
           IF CCOM-STRAP-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CCOM-STRAP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4170-COUNT-NOTICE.
           IF CCOM-NOTICE-CNT < 9999999
               ADD 1 TO CCOM-NOTICE-CNT
           END-IF
           IF CCOM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CCOM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4180-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NOTICE-DATE > 0 AND CCOM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4190-SET-CHANNEL.
           EVALUATE CCOT-CHANNEL-CD
               WHEN 'BRN'
                   IF CCOT-POST-DATE > 0 AND CCOT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CCOT-POST-DATE > 0 AND CCOT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CCOT-POST-DATE > 0 AND CCOT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CCOT-POST-DATE > 0 AND CCOT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CURRENCY YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4200-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-CHANNEL-CD
                   ADD CCOM-CURRENCY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-CURRENCY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COIN AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4210-ACCRUE-COIN.
           IF CCOM-COIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-COIN-AMT * CCOM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CCOT-SHIPMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VARIANCE FEE ON THE COIN AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4220-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CCOM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CCOM-VARIANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-STOP-FLAG = 'Y'
                   COMPUTE CCOM-VARIANCE-AMT ROUNDED = CCOM-COIN-AMT *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOM-CHANNEL-CD = 'AW'
                   MOVE 10.00 TO CCOM-VARIANCE-AMT
               WHEN CCOM-CHANNEL-CD = 'DW'
                   MOVE 10.00 TO CCOM-VARIANCE-AMT
               WHEN OTHER
                   COMPUTE CCOM-VARIANCE-AMT ROUNDED = CCOM-COIN-AMT *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-VARIANCE-AMT > 0 AND CCOM-VARIANCE-AMT < 2.00
               MOVE 2.00 TO CCOM-VARIANCE-AMT
           END-IF
           IF CCOM-VARIANCE-AMT > 25.00
               MOVE 25.00 TO CCOM-VARIANCE-AMT
           END-IF
           ADD CCOM-VARIANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARRIER CD:
      * J -> L WHEN NEXT DUE DATE IS PAST
      * L -> M WHEN NEXT DUE DATE IS PAST
      * M -> X WHEN NEXT DUE DATE IS PAST
      * X -> J WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4230-SET-CARRIER.
           EVALUATE CCOM-CARRIER-CD
               WHEN 'J'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4240-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = CCOM-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CCOM-FEE-YTD - (WS-WORK-AMT * 12)
           COMPUTE CCOT-COIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHIPMENT AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4250-LIMIT-SHIPMENT.
           IF CCOT-SHIPMENT-AMT < 25.00
               MOVE 25.00 TO CCOT-SHIPMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-SHIPMENT-AMT > 10000.00
               MOVE 10000.00 TO CCOT-SHIPMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STRAP YTD FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4260-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CCOM-STOP-FLAG = 'Y'
                   MOVE ZERO TO CCOM-STRAP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CCOM-STRAP-YTD ROUNDED = WS-WORK-AMT *
                       CCOM-RATE / 200
               WHEN CCOM-DENOM-CD = 'B'
                   MOVE 3.00 TO CCOM-STRAP-YTD
               WHEN CCOM-DENOM-CD = 'D'
                   MOVE 7.00 TO CCOM-STRAP-YTD
               WHEN OTHER
                   COMPUTE CCOM-STRAP-YTD ROUNDED = WS-WORK-AMT *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-STRAP-YTD > 0 AND CCOM-STRAP-YTD < 2.00
               MOVE 2.00 TO CCOM-STRAP-YTD
           END-IF
           IF CCOM-STRAP-YTD > 35.00
               MOVE 35.00 TO CCOM-STRAP-YTD
           END-IF
           ADD CCOM-STRAP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE VARIANCE SCHED TIER FOR THE CURRENCY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4270-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VARIANCE-SCHED-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF CCOM-CURRENCY-AMT NOT < WS-VARIANCE-SCHED-LOW(WS-TX)
                   MOVE WS-VARIANCE-SCHED-RATE(WS-TX) TO
                       CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VARIANCE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4280-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-CHANNEL-CD
                   ADD CCOM-VARIANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-VARIANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4290-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STRAP YTD FEE ON THE VARIANCE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4300-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CCOM-STOP-FLAG = 'Y'
                   MOVE ZERO TO CCOM-STRAP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CCOM-STRAP-YTD ROUNDED = CCOM-VARIANCE-YTD *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOT-TRAN-CD = 'OR'
                   MOVE 15.00 TO CCOM-STRAP-YTD
               WHEN CCOT-TRAN-CD = 'SH'
                   MOVE 3.00 TO CCOM-STRAP-YTD
               WHEN OTHER
                   COMPUTE CCOM-STRAP-YTD ROUNDED = CCOM-VARIANCE-YTD *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-STRAP-YTD > 0 AND CCOM-STRAP-YTD < 10.00
               MOVE 10.00 TO CCOM-STRAP-YTD
           END-IF
           IF CCOM-STRAP-YTD > 25.00
               MOVE 25.00 TO CCOM-STRAP-YTD
           END-IF
           ADD CCOM-STRAP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4310-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4320-COUNT-SEQ.
           IF CCOT-SEQ < 9999999
               ADD 1 TO CCOT-SEQ
           END-IF
           IF CCOT-SEQ NOT < 6
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STRAP SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4330-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STRAP-SCHED-RATE(1) TO CCOM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-STRAP-SCHED-LOW(WS-TX)
                   MOVE WS-STRAP-SCHED-RATE(WS-TX) TO CCOM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VARIANCE FEE ON THE CURRENCY AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4340-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CCOM-STOP-FLAG = 'Y'
                   MOVE ZERO TO CCOT-VARIANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CCOT-VARIANCE-AMT ROUNDED = CCOM-CURRENCY-AMT
                       * CCOM-PENALTY-PCT / 200
               WHEN CCOM-STATUS-CD = 'ADH'
                   MOVE 20.00 TO CCOT-VARIANCE-AMT
               WHEN CCOM-STATUS-CD = 'FVV'
                   MOVE 10.00 TO CCOT-VARIANCE-AMT
               WHEN OTHER
                   COMPUTE CCOT-VARIANCE-AMT ROUNDED = CCOM-CURRENCY-AMT
                       * CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOT-VARIANCE-AMT > 0 AND CCOT-VARIANCE-AMT < 5.00
               MOVE 5.00 TO CCOT-VARIANCE-AMT
           END-IF
           IF CCOT-VARIANCE-AMT > 150.00
               MOVE 150.00 TO CCOT-VARIANCE-AMT
           END-IF
           ADD CCOT-VARIANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE CURRENCY YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4350-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-REASON-CD
                   ADD CCOM-CURRENCY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-CURRENCY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VARIANCE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4360-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOT-CHANNEL-CD
                   ADD CCOT-VARIANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOT-VARIANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E263 ADDR REQUIRED
      * E740 CURRENCY AMT NEGATIVE
      * E601 CURRENCY AMT OVER 50000
      * E162 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4370-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-ADDR = SPACES
                   MOVE 'E263' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-AMT < 0
                   MOVE 'E740' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-AMT > 50000.00
                   MOVE 'E601' TO WS-REJECT-CODE
               WHEN CCOM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E162' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHIPMENT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4380-SPLIT-SHIPMENT.
           COMPUTE WS-WORK-AMT = CCOT-SHIPMENT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CCOT-SHIPMENT-AMT - (WS-WORK-AMT * 2)
           COMPUTE CCOM-VARIANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE SHIPMENT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4390-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CCOM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CCOM-SHIPMENT-AMT *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOM-CARRIER-CD = 'J'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN CCOM-CARRIER-CD = 'L'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CCOM-SHIPMENT-AMT *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT STRAP AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4400-SPLIT-STRAP.
           COMPUTE WS-WORK-AMT = CCOT-STRAP-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CCOT-STRAP-AMT - (WS-WORK-AMT * 2)
           COMPUTE CCOM-COIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4410-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO CCOM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VARIANCE FEE ON THE CURRENCY AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4420-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CCOT-VARIANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CCOT-VARIANCE-AMT ROUNDED = CCOM-CURRENCY-AMT
                       * CCOM-RATE / 200
               WHEN CCOT-TRAN-CD = 'OR'
                   MOVE 20.00 TO CCOT-VARIANCE-AMT
               WHEN CCOT-TRAN-CD = 'SH'
                   MOVE 5.00 TO CCOT-VARIANCE-AMT
               WHEN OTHER
                   COMPUTE CCOT-VARIANCE-AMT ROUNDED = CCOM-CURRENCY-AMT
                       * CCOM-RATE / 100
           END-EVALUATE
           IF CCOT-VARIANCE-AMT > 0 AND CCOT-VARIANCE-AMT < 10.00
               MOVE 10.00 TO CCOT-VARIANCE-AMT
           END-IF
           IF CCOT-VARIANCE-AMT > 35.00
               MOVE 35.00 TO CCOT-VARIANCE-AMT
           END-IF
           ADD CCOT-VARIANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE SHIPMENT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4430-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CCOM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-HOLD-FLAG = 'Y'
                   COMPUTE CCOM-FEE-AMT ROUNDED = CCOT-SHIPMENT-AMT *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOM-REASON-CD = 'JL'
                   MOVE 5.00 TO CCOM-FEE-AMT
               WHEN CCOM-REASON-CD = 'LW'
                   MOVE 20.00 TO CCOM-FEE-AMT
               WHEN OTHER
                   COMPUTE CCOM-FEE-AMT ROUNDED = CCOT-SHIPMENT-AMT *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-FEE-AMT > 0 AND CCOM-FEE-AMT < 10.00
               MOVE 10.00 TO CCOM-FEE-AMT
           END-IF
           IF CCOM-FEE-AMT > 75.00
               MOVE 75.00 TO CCOM-FEE-AMT
           END-IF
           ADD CCOM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * COIN AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4440-FLAG-COIN.
           IF CCOM-COIN-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-COIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SHIPMENT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4450-FLAG-SHIPMENT.
           IF CCOT-SHIPMENT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CCOT-SHIPMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4460-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NEXT-REVIEW-DATE > 0 AND CCOM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VARIANCE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4470-ACCRUE-VARIANCE.
           IF CCOM-VARIANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-VARIANCE-AMT * CCOM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-STRAP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4480-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO CCOT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CURRENCY YTD BY CARRIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4490-ACCUM-CARRIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-CARRIER-CD
                   ADD CCOM-CURRENCY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-CARRIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-CURRENCY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VARIANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COIN AMT.
      *----------------------------------------------------------------*
       4500-PRORATE-COIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-COIN-AMT ROUNDED =
                   CCOT-VARIANCE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOM-COIN-AMT
           END-IF
           ADD CCOM-COIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * VARIANCE YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4510-LIMIT-VARIANCE.
           IF CCOM-VARIANCE-YTD < 0.00
               MOVE 0.00 TO CCOM-VARIANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-VARIANCE-YTD > 10000.00
               MOVE 10000.00 TO CCOM-VARIANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4520-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOT-TRAN-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STRAP AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4530-ACCRUE-STRAP.
           IF CCOM-STRAP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-STRAP-AMT * CCOM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-COIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4540-DESCRIBE-SOURCE.
           EVALUATE CCOM-SOURCE-CD
               WHEN 'CFR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DTX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'EEA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'EUA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TPB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WVN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YTT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STRAP AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4550-LIMIT-STRAP.
           IF CCOM-STRAP-AMT < 25.00
               MOVE 25.00 TO CCOM-STRAP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-STRAP-AMT > 500.00
               MOVE 500.00 TO CCOM-STRAP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4560-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO CCOM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4570-COUNT-SEQ.
           IF CCOT-SEQ < 9999999
               ADD 1 TO CCOT-SEQ
           END-IF
           IF CCOT-SEQ NOT < 10
               MOVE 'Y' TO CCOM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SHIPMENT YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4580-LIMIT-SHIPMENT.
           IF CCOM-SHIPMENT-YTD < 0.00
               MOVE 0.00 TO CCOM-SHIPMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-SHIPMENT-YTD > 5000.00
               MOVE 5000.00 TO CCOM-SHIPMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4590-DESCRIBE-SOURCE.
           EVALUATE CCOM-SOURCE-CD
               WHEN 'CFR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DTX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'EEA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'EUA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TPB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WVN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YTT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SHIPMENT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4600-SPLIT-SHIPMENT.
           COMPUTE WS-WORK-AMT = CCOM-SHIPMENT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = CCOM-SHIPMENT-YTD - (WS-WORK-AMT * 2)
           COMPUTE CCOT-STRAP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STRAP SCHED TIER FOR THE STRAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4610-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STRAP-SCHED-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CCOM-STRAP-AMT NOT < WS-STRAP-SCHED-LOW(WS-TX)
                   MOVE WS-STRAP-SCHED-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4620-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-SHIPMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CCOM-SHIPMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CCOM-SHIPMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CCOM-SHIPMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-SHIPMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPMENT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4630-ACCRUE-SHIPMENT.
           IF CCOT-SHIPMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-SHIPMENT-AMT * CCOM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CURRENCY AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4640-LIMIT-CURRENCY.
           IF CCOM-CURRENCY-AMT < 25.00
               MOVE 25.00 TO CCOM-CURRENCY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-CURRENCY-AMT > 10000.00
               MOVE 10000.00 TO CCOM-CURRENCY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4650-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DENOM CD:
      * B -> D WHEN MATURITY DATE IS PAST
      * D -> P WHEN MATURITY DATE IS PAST
      * P -> S WHEN MATURITY DATE IS PAST
      * S -> W WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4660-SET-DENOM.
           EVALUATE CCOM-DENOM-CD
               WHEN 'B'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COIN AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4670-ACCRUE-COIN.
           IF CCOT-COIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-COIN-AMT * CCOM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST PAY DATE IS PAST
      * BAT -> MAL WHEN LAST PAY DATE IS PAST
      * MAL -> TEL WHEN LAST PAY DATE IS PAST
      * TEL -> ONL WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4680-SET-CHANNEL.
           EVALUATE CCOT-CHANNEL-CD
               WHEN 'BRN'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4690-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF CCOT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO CCOT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE CCO FIELDS TO CNPEN AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-CNPEN.
           INITIALIZE CNPEN-PARMS
           MOVE CCOM-SHIPMENT-AMT
             TO CNPEN-SHIPMENT-AMT
           MOVE CCOM-COIN-AMT
             TO CNPEN-COIN-AMT
           MOVE CCOM-STRAP-YTD
             TO CNPEN-STRAP-YTD
           MOVE CCOM-SHIPMENT-YTD
             TO CNPEN-SHIPMENT-YTD
           MOVE CCOM-CURRENCY-YTD
             TO CNPEN-CURRENCY-YTD
           MOVE CCOM-FEE-AMT
             TO CNPEN-FEE-AMT
           MOVE CCOM-NOTICE-DATE
             TO CNPEN-NOTICE-DATE
           MOVE CCOM-NEXT-REVIEW-DATE
             TO CNPEN-NEXT-REVIEW-DATE
           MOVE CCOM-LAST-PAY-DATE
             TO CNPEN-LAST-PAY-DATE
           MOVE CCOM-MATURITY-DATE
             TO CNPEN-MATURITY-DATE
           MOVE CCOM-CARRIER-CD
             TO CNPEN-CARRIER-CD
           MOVE CCOM-REASON-CD
             TO CNPEN-REASON-CD
           MOVE CCOM-SOURCE-CD
             TO CNPEN-SOURCE-CD
           MOVE CCOM-CHANNEL-CD
             TO CNPEN-CHANNEL-CD
           MOVE CCOM-STOP-FLAG
             TO CNPEN-STOP-FLAG
           MOVE CCOM-HOLD-FLAG
             TO CNPEN-HOLD-FLAG
           MOVE CCOM-AUTO-RENEW-FLAG
             TO CNPEN-AUTO-RENEW-FLAG
           MOVE CCOM-RETRY-CNT
             TO CNPEN-RETRY-CNT
           MOVE CCOM-ITEM-CNT
             TO CNPEN-ITEM-CNT
           MOVE CCOM-RATE
             TO CNPEN-RATE
           MOVE CCOM-PENALTY-PCT
             TO CNPEN-PENALTY-PCT
           MOVE CCOM-REF-NO
             TO CNPEN-REF-NO
           MOVE CCOM-CONTACT
             TO CNPEN-CONTACT
           CALL 'CNPEN' USING CNPEN-PARMS
           EVALUATE CNPEN-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CNPEN-COIN-AMT
                     TO CCOM-COIN-AMT
                   MOVE CNPEN-STRAP-YTD
                     TO CCOM-STRAP-YTD
                   MOVE CNPEN-STOP-FLAG
                     TO CCOM-STOP-FLAG
                   MOVE CNPEN-HOLD-FLAG
                     TO CCOM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-CNPEN' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE CCO FIELDS TO CNCAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-CNCAL.
           INITIALIZE CNCAL-PARMS
           MOVE CCOM-STRAP-YTD
             TO CNCAL-STRAP-YTD
           MOVE CCOM-VARIANCE-AMT
             TO CNCAL-VARIANCE-AMT
           MOVE CCOM-FEE-YTD
             TO CNCAL-FEE-YTD
           MOVE CCOM-SHIPMENT-AMT
             TO CNCAL-SHIPMENT-AMT
           MOVE CCOM-SHIPMENT-YTD
             TO CNCAL-SHIPMENT-YTD
           MOVE CCOM-COIN-AMT
             TO CNCAL-COIN-AMT
           MOVE CCOM-EXPIRY-DATE
             TO CNCAL-EXPIRY-DATE
           MOVE CCOM-MATURITY-DATE
             TO CNCAL-MATURITY-DATE
           MOVE CCOM-OPEN-DATE
             TO CNCAL-OPEN-DATE
           MOVE CCOM-NEXT-REVIEW-DATE
             TO CNCAL-NEXT-REVIEW-DATE
           MOVE CCOM-STATUS-CD
             TO CNCAL-STATUS-CD
           MOVE CCOM-CARRIER-CD
             TO CNCAL-CARRIER-CD
           MOVE CCOM-SOURCE-CD
             TO CNCAL-SOURCE-CD
           MOVE CCOM-CHANNEL-CD
             TO CNCAL-CHANNEL-CD
           MOVE CCOM-OVERRIDE-FLAG
             TO CNCAL-OVERRIDE-FLAG
           MOVE CCOM-EXEMPT-FLAG
             TO CNCAL-EXEMPT-FLAG
           MOVE CCOM-NOTICE-SENT-FLAG
             TO CNCAL-NOTICE-SENT-FLAG
           MOVE CCOM-NOTICE-CNT
             TO CNCAL-NOTICE-CNT
           MOVE CCOM-RETRY-CNT
             TO CNCAL-RETRY-CNT
           MOVE CCOM-FEE-PCT
             TO CNCAL-FEE-PCT
           MOVE CCOM-RATE
             TO CNCAL-RATE
           MOVE CCOM-NAME
             TO CNCAL-NAME
           MOVE CCOM-DESC
             TO CNCAL-DESC
           CALL 'CNCAL' USING CNCAL-PARMS
           EVALUATE CNCAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CNCAL-STRAP-YTD
                     TO CCOM-STRAP-YTD
                   MOVE CNCAL-VARIANCE-AMT
                     TO CCOM-VARIANCE-AMT
                   MOVE CNCAL-FEE-YTD
                     TO CCOM-FEE-YTD
                   MOVE CNCAL-COIN-AMT
                     TO CCOM-COIN-AMT
                   MOVE CNCAL-OVERRIDE-FLAG
                     TO CCOM-OVERRIDE-FLAG
                   MOVE CNCAL-EXEMPT-FLAG
                     TO CCOM-EXEMPT-FLAG
                   MOVE CNCAL-NOTICE-SENT-FLAG
                     TO CCOM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-CNCAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE CCO FIELDS TO CNFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-CNFEE.
           INITIALIZE CNFEE-PARMS
           MOVE CCOM-STRAP-YTD
             TO CNFEE-STRAP-YTD
           MOVE CCOM-CURRENCY-YTD
             TO CNFEE-CURRENCY-YTD
           MOVE CCOM-STRAP-AMT
             TO CNFEE-STRAP-AMT
           MOVE CCOM-FEE-AMT
             TO CNFEE-FEE-AMT
           MOVE CCOM-VARIANCE-YTD
             TO CNFEE-VARIANCE-YTD
           MOVE CCOM-COIN-YTD
             TO CNFEE-COIN-YTD
           MOVE CCOM-NEXT-REVIEW-DATE
             TO CNFEE-NEXT-REVIEW-DATE
           MOVE CCOM-LAST-PAY-DATE
             TO CNFEE-LAST-PAY-DATE
           MOVE CCOM-NOTICE-DATE
             TO CNFEE-NOTICE-DATE
           MOVE CCOM-EFFECTIVE-DATE
             TO CNFEE-EFFECTIVE-DATE
           MOVE CCOM-REASON-CD
             TO CNFEE-REASON-CD
           MOVE CCOM-SOURCE-CD
             TO CNFEE-SOURCE-CD
           MOVE CCOM-DENOM-CD
             TO CNFEE-DENOM-CD
           MOVE CCOM-CARRIER-CD
             TO CNFEE-CARRIER-CD
           MOVE CCOM-EXEMPT-FLAG
             TO CNFEE-EXEMPT-FLAG
           MOVE CCOM-AUTO-RENEW-FLAG
             TO CNFEE-AUTO-RENEW-FLAG
           MOVE CCOM-HOLD-FLAG
             TO CNFEE-HOLD-FLAG
           MOVE CCOM-CYCLE-CNT
             TO CNFEE-CYCLE-CNT
           MOVE CCOM-NOTICE-CNT
             TO CNFEE-NOTICE-CNT
           MOVE CCOM-RATE
             TO CNFEE-RATE
           MOVE CCOM-FEE-PCT
             TO CNFEE-FEE-PCT
           MOVE CCOM-DESC
             TO CNFEE-DESC
           MOVE CCOM-REF-NO
             TO CNFEE-REF-NO
           CALL 'CNFEE' USING CNFEE-PARMS
           EVALUATE CNFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CNFEE-VARIANCE-YTD
                     TO CCOM-VARIANCE-YTD
                   MOVE CNFEE-COIN-YTD
                     TO CCOM-COIN-YTD
                   MOVE CNFEE-AUTO-RENEW-FLAG
                     TO CCOM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-CNFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE CCOT-KEY TO RD-KEY
           MOVE CCOT-TRAN-CD TO RD-CODE
           MOVE CCOT-COIN-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE CCOT-KEY TO RD-KEY
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
           DISPLAY 'CNOACCR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CNOACCR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
