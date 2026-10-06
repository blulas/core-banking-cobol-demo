      *================================================================*
      * PROGRAM  : LBXAGE                                              *
      * TYPE     : BATCH                                               *
      * PURPOSE  : LOCKBOX REMITTANCE - AGING. READS THE LBX           *
      *            TRANSACTIONS, UPDATES THE LBX MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. WHOLESALE LOCKBOX REMITTANCE    *
      *            CAPTURE AND CREDIT TO CLIENTS                       *
      * INPUT    : LBXMST (KSDS), LBXTRN, BANKCTL                      *
      * OUTPUT   : LBXOUT, LBXAGRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, LBWAIV                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-12-07 JLM  ORIGINAL                                       *
      * 1998-08-24 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-02-04 EWS  FEE NOTICE TEXT PER LEGAL CHG7724              *
      * 2001-06-03 CRP  FLOAT CALCULATION CORRECTED CHG0731            *
      * 2003-04-26 RJT  CHECK FEE SCHEDULE UPDATE CHG7521              *
      * 2003-01-08 BHS  DEPOSIT CUTOFF MOVED CHG4262                   *
      * 2004-09-15 SKN  FEE STATUS CODES ADDED CHG9529                 *
      * 2005-01-28 GAF  EXCEPT TABLE EXPANDED INC3542                  *
      * 2009-01-14 BHS  FEE STATUS CODES ADDED INC0649                 *
      * 2011-01-27 CRP  FLOAT TABLE EXPANDED CHG7745                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBXAGE.
       AUTHOR. C R PATEL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "LBXMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS LBXM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "LBXTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "LBXOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "LBXAGRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY LBXMST.
       FD  TRAN-FILE.
       COPY LBXTRN.
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
                                           VALUE 14.000000.
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
      * WS-DEPOSIT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEPOSIT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000717000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000822000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000771000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000579500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011550000000051500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011800000000395000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014300000000839500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014800000000544000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019800000000637500'.
       01  WS-DEPOSIT-GRID-TBL REDEFINES WS-DEPOSIT-GRID-VALUES.
           05  WS-DEPOSIT-GRID-ROW OCCURS 9 TIMES.
               10  WS-DEPOSIT-GRID-LOW     PIC 9(09)V99.
               10  WS-DEPOSIT-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASH-ON-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-ON-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000779500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000778000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000531500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001750000000348500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011750000000877500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016750000000537000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016850000000566500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019350000000535500'.
       01  WS-CASH-ON-SCHED-TBL REDEFINES WS-CASH-ON-SCHED-VALUES.
           05  WS-CASH-ON-SCHED-ROW OCCURS 8 TIMES.
               10  WS-CASH-ON-SCHED-LOW    PIC 9(09)V99.
               10  WS-CASH-ON-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASH-ON-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-ON-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000589500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000813000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000681000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010350000000396500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010400000000701500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015400000000509500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000758000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015750000000802500'.
       01  WS-CASH-ON-BAND-TBL REDEFINES WS-CASH-ON-BAND-VALUES.
           05  WS-CASH-ON-BAND-ROW OCCURS 8 TIMES.
               10  WS-CASH-ON-BAND-LOW     PIC 9(09)V99.
               10  WS-CASH-ON-BAND-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  LBXAGE  LOCKBOX REMITTANCE'.
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
       COPY LBWAIVP.

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
               MOVE 'LBXMST' TO ABND-FILE-NAME
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
           MOVE LBXT-KEY TO LBXM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ASSESS-NOTICE-SENT
               PERFORM 3010-DESCRIBE-LOCKBOX
               PERFORM 3020-COUNT-ITEM
               PERFORM 3030-COUNT-RETRY
               PERFORM 3040-FLAG-FLOAT
               PERFORM 3050-DESCRIBE-TRAN
               PERFORM 3060-FLAG-FEE
               PERFORM 3070-ACCRUE-FLOAT
               PERFORM 3080-SELECT-PENALTY-PCT
               PERFORM 3090-LIMIT-CASH-ON-HAND
               PERFORM 3100-COUNT-ITEM
               PERFORM 3110-AGE-NOTICE
               PERFORM 3120-CHECK-CLOSE
               PERFORM 3130-EDIT-NAME
               PERFORM 3140-ACCUM-REASON
               PERFORM 3150-DESCRIBE-TRAN
               PERFORM 3160-PRORATE-CHECK
               PERFORM 3170-ACCUM-REGION
               PERFORM 3180-ACCUM-CHANNEL
               PERFORM 3190-AGE-LAST-STMT
               PERFORM 3200-LIMIT-FLOAT
               PERFORM 3210-ACCRUE-CHECK
               PERFORM 3220-COUNT-SEQ
               PERFORM 3230-LIMIT-DEPOSIT
               PERFORM 3240-ACCUM-LOCKBOX
               PERFORM 3250-PRORATE-CASH-ON-HAND
               PERFORM 3260-ACCRUE-FEE
               PERFORM 3270-DESCRIBE-TRAN
               PERFORM 3280-PRORATE-CHECK
               PERFORM 3290-COUNT-SEQ
               PERFORM 3300-DESCRIBE-LOCKBOX
               PERFORM 3310-PRORATE-DEPOSIT
               PERFORM 3320-SET-CHANNEL
               PERFORM 3330-ACCUM-CYCLE
               PERFORM 3340-PRORATE-DEPOSIT
               PERFORM 3350-ACCUM-BATCH-TYPE
               PERFORM 3360-PRORATE-CASH-ON-HAND
               PERFORM 3370-ACCRUE-FEE-PCT
               PERFORM 3380-SET-CHANNEL
               PERFORM 3390-CHECK-EFF
               PERFORM 3400-EDIT-CONTACT
               PERFORM 3410-CHECK-EXPIRY
               PERFORM 3420-ACCRUE-CASH-ON-HAND
               PERFORM 3430-ASSESS-AUTO-RENEW
               PERFORM 3440-ACCRUE-CHECK
               PERFORM 3450-ASSESS-EXEMPT
               PERFORM 3460-DESCRIBE-BATCH-TYPE
               PERFORM 3470-ACCRUE-FLOAT
               PERFORM 3480-LIMIT-EXCEPT
               PERFORM 3490-ASSESS-REVERSAL
               PERFORM 3500-DESCRIBE-CYCLE
               PERFORM 3510-SET-CHANNEL
               PERFORM 3520-CHECK-POST
               PERFORM 3530-LIMIT-EXCEPT
               PERFORM 3540-AGE-OPEN
               PERFORM 3550-ACCUM-CHANNEL
               PERFORM 3560-ACCRUE-REMIT
               PERFORM 3570-SELECT-PENALTY-PCT
               PERFORM 3580-FLAG-FEE
               PERFORM 3590-SPLIT-EXCEPT
               PERFORM 3600-AGE-NEXT-DUE
               PERFORM 3610-EDIT-REJECT
               PERFORM 3620-EDIT-REF-NO
               PERFORM 3630-FLAG-ITEM
               PERFORM 3640-FLAG-FEE
               PERFORM 3650-CHECK-EFFECTIVE
               PERFORM 3660-COUNT-PAYMENT
               PERFORM 3670-LIMIT-CASH-ON-HAND
               PERFORM 3680-ACCRUE-FLOAT
               PERFORM 3690-SELECT-FEE-PCT
               PERFORM 3700-SELECT-DISCOUNT
               PERFORM 3710-PRORATE-REMIT
               PERFORM 3720-LIMIT-FEE
               PERFORM 3730-DESCRIBE-CHANNEL
               PERFORM 3740-FLAG-FLOAT
               PERFORM 3750-CHECK-MATURITY
               PERFORM 3760-LIMIT-EXCEPT
               PERFORM 3770-DESCRIBE-REGION
               PERFORM 3780-ACCRUE-CASH-ON-HAND
               PERFORM 3790-AGE-LAST-STMT
               PERFORM 3800-FLAG-FLOAT
               PERFORM 3810-LIMIT-CHECK
               PERFORM 3820-ACCRUE-REMIT
               PERFORM 3830-ACCUM-TRAN
               PERFORM 3840-COUNT-SEQ
               PERFORM 3850-PRORATE-CHECK
               PERFORM 3860-PRORATE-DEPOSIT
               PERFORM 3870-AGE-POST
               PERFORM 3880-FLAG-CHECK
               PERFORM 3890-ASSESS-PREFERRED
               PERFORM 3900-DESCRIBE-CYCLE
               PERFORM 3910-EDIT-REF-NO
               PERFORM 3920-PRORATE-CASH-ON-HAND
               PERFORM 3930-ACCUM-CHANNEL
               PERFORM 3940-SPLIT-EXCEPT
               PERFORM 3950-PRORATE-CHECK
               PERFORM 3960-SELECT-DISCOUNT
               PERFORM 3970-DESCRIBE-CHANNEL
               PERFORM 3980-AGE-MATURITY
               PERFORM 3990-SELECT-DISCOUNT
               PERFORM 4000-AGE-NOTICE
               PERFORM 4010-CHECK-EFFECTIVE
               PERFORM 4020-FLAG-CASH-ON-HAND
               PERFORM 4030-DESCRIBE-BATCH-TYPE
               PERFORM 4040-LIMIT-FEE
               PERFORM 4050-ASSESS-NOTICE-SENT
               PERFORM 4060-CHECK-EXPIRY
               PERFORM 4070-SET-BATCH-TYPE
               PERFORM 4080-SPLIT-REMIT
               PERFORM 4090-EDIT-REF-NO
               PERFORM 4100-ACCRUE-FEE
               PERFORM 4110-LIMIT-EXCEPT
               PERFORM 4120-PRORATE-CHECK
               PERFORM 4130-SELECT-PENALTY-PCT
               PERFORM 4140-LIMIT-FEE
               PERFORM 4150-ACCUM-REGION
               PERFORM 4160-EDIT-ADDR
               PERFORM 4170-SPLIT-CASH-ON-HAND
               PERFORM 4180-AGE-NOTICE
               PERFORM 4190-CHECK-NOTICE
               PERFORM 4200-SELECT-PENALTY-PCT
               PERFORM 4210-COUNT-PAYMENT
               PERFORM 4220-PRORATE-FEE
               PERFORM 4230-CHECK-NEXT-DUE
               PERFORM 4240-CHECK-LAST-PAY
               PERFORM 4250-LIMIT-CASH-ON-HAND
               PERFORM 4260-FLAG-REMIT
               PERFORM 4270-LIMIT-REMIT
               PERFORM 4280-LIMIT-FLOAT
               PERFORM 4290-SET-REGION
               PERFORM 4300-CHECK-LAST-PAY
               PERFORM 4310-SELECT-DISCOUNT
               PERFORM 4320-EDIT-REF-NO
               PERFORM 4330-PRORATE-FLOAT
               PERFORM 4340-FLAG-ITEM
               PERFORM 4350-LIMIT-REMIT
               PERFORM 4360-ASSESS-REVERSAL
               PERFORM 4370-DESCRIBE-CHANNEL
               PERFORM 4380-AGE-CLOSE
               PERFORM 4390-COUNT-ITEM
               PERFORM 4400-DESCRIBE-BATCH-TYPE
               PERFORM 4410-LIMIT-EXCEPT
               PERFORM 4420-EDIT-REJECT
               PERFORM 4430-SPLIT-CASH-ON-HAND
               PERFORM 4440-ACCRUE-FEE
               PERFORM 4450-LIMIT-FLOAT
               PERFORM 4460-EDIT-DESC
               PERFORM 4470-ASSESS-HOLD
               PERFORM 4480-SELECT-PENALTY-PCT
               PERFORM 4490-LIMIT-FLOAT
               PERFORM 4500-SELECT-PENALTY-PCT
               PERFORM 4510-EDIT-CUST-ID
               PERFORM 4520-SELECT-DISCOUNT
               PERFORM 4530-PRORATE-DEPOSIT
               PERFORM 4540-COUNT-ITEM
               PERFORM 4550-FLAG-CHECK
               PERFORM 4560-SET-CHANNEL
               PERFORM 4570-SELECT-PENALTY-PCT
               PERFORM 4580-COUNT-PAYMENT
               PERFORM 4590-LIMIT-FEE
               PERFORM 7000-CALL-LBWAIV
               REWRITE LBX-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'LBXMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM LBX-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE REMIT YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3000-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN LBXM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = LBXM-REMIT-YTD *
                       LBXM-FEE-PCT / 200
               WHEN LBXM-BATCH-TYPE-CD = 'C'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN LBXM-BATCH-TYPE-CD = 'D'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = LBXM-REMIT-YTD *
                       LBXM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE LOCKBOX CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3010-DESCRIBE-LOCKBOX.
           EVALUATE LBXM-LOCKBOX-CD
               WHEN 'DX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 5
               MOVE 'Y' TO LBXT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3030-COUNT-RETRY.
           IF LBXM-RETRY-CNT < 9999999
               ADD 1 TO LBXM-RETRY-CNT
           END-IF
           IF LBXM-RETRY-CNT NOT < 5
               MOVE 'Y' TO LBXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3040-FLAG-FLOAT.
           IF LBXM-FLOAT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE LBXM-FLOAT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3050-DESCRIBE-TRAN.
           EVALUATE LBXT-TRAN-CD
               WHEN 'RC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'EX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'AD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3060-FLAG-FEE.
           IF LBXT-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE LBXT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FLOAT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3070-ACCRUE-FLOAT.
           IF LBXM-FLOAT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-FLOAT-AMT * LBXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-EXCEPT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH ON BAND TIER FOR THE CHECK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3080-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ON-BAND-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF LBXT-CHECK-AMT NOT < WS-CASH-ON-BAND-LOW(WS-TX)
                   MOVE WS-CASH-ON-BAND-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH ON HAND YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3090-LIMIT-CASH-ON-HAND.
           IF LBXM-CASH-ON-HAND-YTD < 50.00
               MOVE 50.00 TO LBXM-CASH-ON-HAND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-CASH-ON-HAND-YTD > 50000.00
               MOVE 50000.00 TO LBXM-CASH-ON-HAND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3100-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 6
               MOVE 'Y' TO LBXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3110-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-NOTICE-DATE > 0 AND LBXM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3120-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E580 NAME REQUIRED
      * E742 CHECK AMT NEGATIVE
      * E079 CHECK AMT OVER 25000
      * E163 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3130-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-NAME = SPACES
                   MOVE 'E580' TO WS-REJECT-CODE
               WHEN LBXT-CHECK-AMT < 0
                   MOVE 'E742' TO WS-REJECT-CODE
               WHEN LBXT-CHECK-AMT > 25000.00
                   MOVE 'E079' TO WS-REJECT-CODE
               WHEN LBXM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E163' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ON HAND AMT BY REASON CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3140-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REASON-CD
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3150-DESCRIBE-TRAN.
           EVALUATE LBXT-TRAN-CD
               WHEN 'RC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'EX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'AD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CHECK YTD.
      *----------------------------------------------------------------*
       3160-PRORATE-CHECK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-CHECK-YTD ROUNDED =
                   LBXM-DEPOSIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-CHECK-YTD
           END-IF
           ADD LBXM-CHECK-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REGION-CD
                   ADD LBXM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCEPT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3180-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXT-CHANNEL-CD
                   ADD LBXM-EXCEPT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-EXCEPT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3190-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FLOAT AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3200-LIMIT-FLOAT.
           IF LBXM-FLOAT-AMT < 50.00
               MOVE 50.00 TO LBXM-FLOAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-FLOAT-AMT > 500.00
               MOVE 500.00 TO LBXM-FLOAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHECK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3210-ACCRUE-CHECK.
           IF LBXT-CHECK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXT-CHECK-AMT * LBXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO LBXM-FLOAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3220-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 3
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3230-LIMIT-DEPOSIT.
           IF LBXM-DEPOSIT-YTD < 100.00
               MOVE 100.00 TO LBXM-DEPOSIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-DEPOSIT-YTD > 5000.00
               MOVE 5000.00 TO LBXM-DEPOSIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHECK YTD BY LOCKBOX CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3240-ACCUM-LOCKBOX.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-LOCKBOX-CD
                   ADD LBXM-CHECK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-LOCKBOX-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CHECK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH ON HAND YTD.
      *----------------------------------------------------------------*
       3250-PRORATE-CASH-ON-HAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-CASH-ON-HAND-YTD ROUNDED =
                   LBXM-DEPOSIT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-CASH-ON-HAND-YTD
           END-IF
           ADD LBXM-CASH-ON-HAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3260-ACCRUE-FEE.
           IF LBXT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXT-FEE-AMT * LBXM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO LBXM-CASH-ON-HAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3270-DESCRIBE-TRAN.
           EVALUATE LBXT-TRAN-CD
               WHEN 'RC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'EX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'AD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CHECK YTD.
      *----------------------------------------------------------------*
       3280-PRORATE-CHECK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-CHECK-YTD ROUNDED =
                   LBXM-DEPOSIT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-CHECK-YTD
           END-IF
           ADD LBXM-CHECK-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3290-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 6
               MOVE 'Y' TO LBXM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LOCKBOX CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3300-DESCRIBE-LOCKBOX.
           EVALUATE LBXM-LOCKBOX-CD
               WHEN 'DX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CHECK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEPOSIT AMT.
      *----------------------------------------------------------------*
       3310-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-DEPOSIT-AMT ROUNDED =
                   LBXT-CHECK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-DEPOSIT-AMT
           END-IF
           ADD LBXM-DEPOSIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * B -> H WHEN EFF DATE IS PAST
      * H -> Z WHEN EFF DATE IS PAST
      * Z -> B WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3320-SET-CHANNEL.
           EVALUATE LBXM-CHANNEL-CD
               WHEN 'B'
                   IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CHECK YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3330-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-CYCLE-CD
                   ADD LBXM-CHECK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CHECK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEPOSIT YTD.
      *----------------------------------------------------------------*
       3340-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-DEPOSIT-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-DEPOSIT-YTD
           END-IF
           ADD LBXM-DEPOSIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE FLOAT AMT BY BATCH TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3350-ACCUM-BATCH-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-BATCH-TYPE-CD
                   ADD LBXM-FLOAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-BATCH-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-FLOAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH ON HAND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3360-PRORATE-CASH-ON-HAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   LBXM-CASH-ON-HAND-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3370-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * LBXM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO LBXM-EXCEPT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT DUE DATE IS PAST
      * BAT -> MAL WHEN NEXT DUE DATE IS PAST
      * MAL -> TEL WHEN NEXT DUE DATE IS PAST
      * TEL -> ONL WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3380-SET-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3390-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF LBXT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E782 CONTACT REQUIRED
      * E711 CASH ON HAND AMT NEGATIVE
      * E421 CASH ON HAND AMT OVER 10000
      * E026 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3400-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-CONTACT = SPACES
                   MOVE 'E782' TO WS-REJECT-CODE
               WHEN LBXM-CASH-ON-HAND-AMT < 0
                   MOVE 'E711' TO WS-REJECT-CODE
               WHEN LBXM-CASH-ON-HAND-AMT > 10000.00
                   MOVE 'E421' TO WS-REJECT-CODE
               WHEN LBXM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E026' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3410-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO LBXM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH ON HAND YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3420-ACCRUE-CASH-ON-HAND.
           IF LBXM-CASH-ON-HAND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-CASH-ON-HAND-YTD * LBXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO LBXM-CHECK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FLOAT FEE ON THE CHECK YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3430-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN LBXM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO LBXM-FLOAT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXT-REVERSAL-FLAG = 'Y'
                   COMPUTE LBXM-FLOAT-AMT ROUNDED = LBXM-CHECK-YTD *
                       LBXM-DISCOUNT / 200
               WHEN LBXM-CYCLE-CD = 'EDV'
                   MOVE 5.00 TO LBXM-FLOAT-AMT
               WHEN LBXM-CYCLE-CD = 'EPM'
                   MOVE 10.00 TO LBXM-FLOAT-AMT
               WHEN OTHER
                   COMPUTE LBXM-FLOAT-AMT ROUNDED = LBXM-CHECK-YTD *
                       LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXM-FLOAT-AMT > 0 AND LBXM-FLOAT-AMT < 5.00
               MOVE 5.00 TO LBXM-FLOAT-AMT
           END-IF
           IF LBXM-FLOAT-AMT > 150.00
               MOVE 150.00 TO LBXM-FLOAT-AMT
           END-IF
           ADD LBXM-FLOAT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHECK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3440-ACCRUE-CHECK.
           IF LBXM-CHECK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-CHECK-AMT * LBXM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-FLOAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ON HAND FEE ON THE FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3450-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN LBXM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO LBXM-CASH-ON-HAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXT-REVERSAL-FLAG = 'Y'
                   COMPUTE LBXM-CASH-ON-HAND-AMT ROUNDED = LBXM-FEE-AMT
                       * LBXM-FEE-PCT / 200
               WHEN LBXM-REASON-CD = 'GF'
                   MOVE 10.00 TO LBXM-CASH-ON-HAND-AMT
               WHEN LBXM-REASON-CD = 'UK'
                   MOVE 20.00 TO LBXM-CASH-ON-HAND-AMT
               WHEN OTHER
                   COMPUTE LBXM-CASH-ON-HAND-AMT ROUNDED = LBXM-FEE-AMT
                       * LBXM-FEE-PCT / 100
           END-EVALUATE
           IF LBXM-CASH-ON-HAND-AMT > 0 AND LBXM-CASH-ON-HAND-AMT < 1.00
               MOVE 1.00 TO LBXM-CASH-ON-HAND-AMT
           END-IF
           IF LBXM-CASH-ON-HAND-AMT > 150.00
               MOVE 150.00 TO LBXM-CASH-ON-HAND-AMT
           END-IF
           ADD LBXM-CASH-ON-HAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE BATCH TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3460-DESCRIBE-BATCH-TYPE.
           EVALUATE LBXM-BATCH-TYPE-CD
               WHEN 'C'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'D'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'F'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'X'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FLOAT YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3470-ACCRUE-FLOAT.
           IF LBXM-FLOAT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-FLOAT-YTD * LBXM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3480-LIMIT-EXCEPT.
           IF LBXM-EXCEPT-YTD < 0.00
               MOVE 0.00 TO LBXM-EXCEPT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-EXCEPT-YTD > 1000.00
               MOVE 1000.00 TO LBXM-EXCEPT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CHECK YTD FEE ON THE DEPOSIT YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3490-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN LBXT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO LBXM-CHECK-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-HOLD-FLAG = 'Y'
                   COMPUTE LBXM-CHECK-YTD ROUNDED = LBXM-DEPOSIT-YTD *
                       LBXM-PENALTY-PCT / 200
               WHEN LBXM-CYCLE-CD = 'EDV'
                   MOVE 3.00 TO LBXM-CHECK-YTD
               WHEN LBXM-CYCLE-CD = 'EPM'
                   MOVE 3.00 TO LBXM-CHECK-YTD
               WHEN OTHER
                   COMPUTE LBXM-CHECK-YTD ROUNDED = LBXM-DEPOSIT-YTD *
                       LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXM-CHECK-YTD > 0 AND LBXM-CHECK-YTD < 1.00
               MOVE 1.00 TO LBXM-CHECK-YTD
           END-IF
           IF LBXM-CHECK-YTD > 250.00
               MOVE 250.00 TO LBXM-CHECK-YTD
           END-IF
           ADD LBXM-CHECK-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-CYCLE.
           EVALUATE LBXM-CYCLE-CD
               WHEN 'EDV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'EPM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FZY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XUU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YBH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3510-SET-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   IF LBXM-OPEN-DATE > 0 AND LBXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF LBXM-OPEN-DATE > 0 AND LBXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF LBXM-OPEN-DATE > 0 AND LBXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF LBXM-OPEN-DATE > 0 AND LBXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3520-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF LBXT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3530-LIMIT-EXCEPT.
           IF LBXM-EXCEPT-YTD < 50.00
               MOVE 50.00 TO LBXM-EXCEPT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-EXCEPT-YTD > 500.00
               MOVE 500.00 TO LBXM-EXCEPT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3540-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF LBXM-OPEN-DATE > 0 AND LBXM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CHECK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3550-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXT-CHANNEL-CD
                   ADD LBXM-CHECK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CHECK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REMIT YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3560-ACCRUE-REMIT.
           IF LBXM-REMIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-REMIT-YTD * LBXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-FLOAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT GRID TIER FOR THE REMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3570-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-GRID-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF LBXM-REMIT-AMT NOT < WS-DEPOSIT-GRID-LOW(WS-TX)
                   MOVE WS-DEPOSIT-GRID-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3580-FLAG-FEE.
           IF LBXM-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE LBXM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3590-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = LBXM-EXCEPT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = LBXM-EXCEPT-YTD - (WS-WORK-AMT * 2)
           COMPUTE LBXM-REMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3600-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-REMIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD LBXM-REMIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD LBXM-REMIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBXM-REMIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-REMIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E500 REJECT CD REQUIRED
      * E523 FEE AMT NEGATIVE
      * E987 FEE AMT OVER 25000
      * E147 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3610-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-REJECT-CD = SPACES
                   MOVE 'E500' TO WS-REJECT-CODE
               WHEN LBXT-FEE-AMT < 0
                   MOVE 'E523' TO WS-REJECT-CODE
               WHEN LBXT-FEE-AMT > 25000.00
                   MOVE 'E987' TO WS-REJECT-CODE
               WHEN LBXM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E147' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E531 REF NO REQUIRED
      * E976 FEE AMT NEGATIVE
      * E712 FEE AMT OVER 99999
      * E062 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3620-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-REF-NO = SPACES
                   MOVE 'E531' TO WS-REJECT-CODE
               WHEN LBXM-FEE-AMT < 0
                   MOVE 'E976' TO WS-REJECT-CODE
               WHEN LBXM-FEE-AMT > 99999.00
                   MOVE 'E712' TO WS-REJECT-CODE
               WHEN LBXT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E062' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3630-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3640-FLAG-FEE.
           IF LBXM-FEE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBXM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3650-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO LBXM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3660-COUNT-PAYMENT.
           IF LBXM-PAYMENT-CNT < 9999999
               ADD 1 TO LBXM-PAYMENT-CNT
           END-IF
           IF LBXM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO LBXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASH ON HAND AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3670-LIMIT-CASH-ON-HAND.
           IF LBXT-CASH-ON-HAND-AMT < 50.00
               MOVE 50.00 TO LBXT-CASH-ON-HAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-CASH-ON-HAND-AMT > 5000.00
               MOVE 5000.00 TO LBXT-CASH-ON-HAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FLOAT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3680-ACCRUE-FLOAT.
           IF LBXM-FLOAT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-FLOAT-AMT * LBXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-EXCEPT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3690-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-GRID-RATE(1) TO LBXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-DEPOSIT-GRID-LOW(WS-TX)
                   MOVE WS-DEPOSIT-GRID-RATE(WS-TX) TO LBXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH ON BAND TIER FOR THE DEPOSIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3700-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ON-BAND-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF LBXM-DEPOSIT-YTD NOT < WS-CASH-ON-BAND-LOW(WS-TX)
                   MOVE WS-CASH-ON-BAND-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FLOAT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REMIT AMT.
      *----------------------------------------------------------------*
       3710-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-REMIT-AMT ROUNDED =
                   LBXM-FLOAT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-REMIT-AMT
           END-IF
           ADD LBXM-REMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3720-LIMIT-FEE.
           IF LBXM-FEE-AMT < 25.00
               MOVE 25.00 TO LBXM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-FEE-AMT > 5000.00
               MOVE 5000.00 TO LBXM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FLOAT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3740-FLAG-FLOAT.
           IF LBXM-FLOAT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBXM-FLOAT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3750-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3760-LIMIT-EXCEPT.
           IF LBXM-EXCEPT-YTD < 25.00
               MOVE 25.00 TO LBXM-EXCEPT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-EXCEPT-YTD > 5000.00
               MOVE 5000.00 TO LBXM-EXCEPT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-REGION.
           EVALUATE LBXM-REGION-CD
               WHEN 'DVP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FMZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KSL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NYK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH ON HAND AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3780-ACCRUE-CASH-ON-HAND.
           IF LBXM-CASH-ON-HAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-CASH-ON-HAND-AMT * LBXM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO LBXT-CASH-ON-HAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3790-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBXM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD LBXM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FLOAT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3800-FLAG-FLOAT.
           IF LBXT-FLOAT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBXT-FLOAT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CHECK AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3810-LIMIT-CHECK.
           IF LBXT-CHECK-AMT < 0.00
               MOVE 0.00 TO LBXT-CHECK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-CHECK-AMT > 10000.00
               MOVE 10000.00 TO LBXT-CHECK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REMIT YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3820-ACCRUE-REMIT.
           IF LBXM-REMIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-REMIT-YTD * LBXM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO LBXM-CASH-ON-HAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3830-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXT-TRAN-CD
                   ADD LBXM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3840-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 10
               MOVE 'Y' TO LBXM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CHECK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3850-PRORATE-CHECK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXT-CHECK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE CASH ON HAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEPOSIT AMT.
      *----------------------------------------------------------------*
       3860-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-DEPOSIT-AMT ROUNDED =
                   LBXM-CASH-ON-HAND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-DEPOSIT-AMT
           END-IF
           ADD LBXM-DEPOSIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3870-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-EXCEPT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD LBXM-EXCEPT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD LBXM-EXCEPT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-EXCEPT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-EXCEPT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CHECK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3880-FLAG-CHECK.
           IF LBXT-CHECK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBXT-CHECK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3890-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       LBXM-DISCOUNT / 200
               WHEN LBXT-TRAN-CD = 'RC'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN LBXT-TRAN-CD = 'DP'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       LBXM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3900-DESCRIBE-CYCLE.
           EVALUATE LBXM-CYCLE-CD
               WHEN 'EDV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'EPM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FZY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XUU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YBH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E965 REF NO REQUIRED
      * E105 FLOAT YTD NEGATIVE
      * E305 FLOAT YTD OVER 250000
      * E809 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3910-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-REF-NO = SPACES
                   MOVE 'E965' TO WS-REJECT-CODE
               WHEN LBXM-FLOAT-YTD < 0
                   MOVE 'E105' TO WS-REJECT-CODE
               WHEN LBXM-FLOAT-YTD > 250000.00
                   MOVE 'E305' TO WS-REJECT-CODE
               WHEN LBXM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E809' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH ON HAND AMT.
      *----------------------------------------------------------------*
       3920-PRORATE-CASH-ON-HAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXT-CASH-ON-HAND-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXT-CASH-ON-HAND-AMT
           END-IF
           ADD LBXT-CASH-ON-HAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE CHECK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3930-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXT-CHANNEL-CD
                   ADD LBXT-CHECK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXT-CHECK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = LBXM-EXCEPT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = LBXM-EXCEPT-YTD - (WS-WORK-AMT * 3)
           COMPUTE LBXM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CHECK AMT.
      *----------------------------------------------------------------*
       3950-PRORATE-CHECK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-CHECK-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-CHECK-AMT
           END-IF
           ADD LBXM-CHECK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CASH ON SCHED TIER FOR THE CHECK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3960-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ON-SCHED-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF LBXM-CHECK-AMT NOT < WS-CASH-ON-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-ON-SCHED-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3970-DESCRIBE-CHANNEL.
           EVALUATE LBXM-CHANNEL-CD
               WHEN 'B'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3980-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBXT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD LBXT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD LBXT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT GRID TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3990-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-GRID-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF LBXM-DEPOSIT-AMT NOT < WS-DEPOSIT-GRID-LOW(WS-TX)
                   MOVE WS-DEPOSIT-GRID-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4000-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-NOTICE-DATE > 0 AND LBXM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-EXCEPT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD LBXM-EXCEPT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD LBXM-EXCEPT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD LBXM-EXCEPT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-EXCEPT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4010-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO LBXM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH ON HAND AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4020-FLAG-CASH-ON-HAND.
           IF LBXT-CASH-ON-HAND-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE LBXT-CASH-ON-HAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BATCH TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4030-DESCRIBE-BATCH-TYPE.
           EVALUATE LBXM-BATCH-TYPE-CD
               WHEN 'C'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'K'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'X'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4040-LIMIT-FEE.
           IF LBXM-FEE-AMT < 0.00
               MOVE 0.00 TO LBXM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-FEE-AMT > 10000.00
               MOVE 10000.00 TO LBXM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CHECK FEE ON THE DEPOSIT AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4050-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN LBXM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO LBXM-CHECK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE LBXM-CHECK-AMT ROUNDED = LBXM-DEPOSIT-AMT *
                       LBXM-PENALTY-PCT / 200
               WHEN LBXT-TRAN-CD = 'RC'
                   MOVE 12.00 TO LBXM-CHECK-AMT
               WHEN LBXT-TRAN-CD = 'DP'
                   MOVE 20.00 TO LBXM-CHECK-AMT
               WHEN OTHER
                   COMPUTE LBXM-CHECK-AMT ROUNDED = LBXM-DEPOSIT-AMT *
                       LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXM-CHECK-AMT > 0 AND LBXM-CHECK-AMT < 2.00
               MOVE 2.00 TO LBXM-CHECK-AMT
           END-IF
           IF LBXM-CHECK-AMT > 75.00
               MOVE 75.00 TO LBXM-CHECK-AMT
           END-IF
           ADD LBXM-CHECK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4060-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BATCH TYPE CD:
      * C -> D WHEN LAST PAY DATE IS PAST
      * D -> F WHEN LAST PAY DATE IS PAST
      * F -> K WHEN LAST PAY DATE IS PAST
      * K -> U WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4070-SET-BATCH-TYPE.
           EVALUATE LBXM-BATCH-TYPE-CD
               WHEN 'C'
                   IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT REMIT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4080-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = LBXM-REMIT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = LBXM-REMIT-YTD - (WS-WORK-AMT * 2)
           COMPUTE LBXM-EXCEPT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E544 REF NO REQUIRED
      * E223 DEPOSIT YTD NEGATIVE
      * E395 DEPOSIT YTD OVER 10000
      * E657 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4090-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-REF-NO = SPACES
                   MOVE 'E544' TO WS-REJECT-CODE
               WHEN LBXM-DEPOSIT-YTD < 0
                   MOVE 'E223' TO WS-REJECT-CODE
               WHEN LBXM-DEPOSIT-YTD > 10000.00
                   MOVE 'E395' TO WS-REJECT-CODE
               WHEN LBXM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E657' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4100-ACCRUE-FEE.
           IF LBXT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXT-FEE-AMT * LBXM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4110-LIMIT-EXCEPT.
           IF LBXM-EXCEPT-YTD < 10.00
               MOVE 10.00 TO LBXM-EXCEPT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-EXCEPT-YTD > 2500.00
               MOVE 2500.00 TO LBXM-EXCEPT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CHECK AMT.
      *----------------------------------------------------------------*
       4120-PRORATE-CHECK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXT-CHECK-AMT ROUNDED =
                   LBXM-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXT-CHECK-AMT
           END-IF
           ADD LBXT-CHECK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CASH ON BAND TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4130-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ON-BAND-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF LBXM-DEPOSIT-AMT NOT < WS-CASH-ON-BAND-LOW(WS-TX)
                   MOVE WS-CASH-ON-BAND-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4140-LIMIT-FEE.
           IF LBXT-FEE-AMT < 50.00
               MOVE 50.00 TO LBXT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-FEE-AMT > 50000.00
               MOVE 50000.00 TO LBXT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ON HAND YTD BY REGION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4150-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REGION-CD
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CASH-ON-HAND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E917 ADDR REQUIRED
      * E691 FLOAT YTD NEGATIVE
      * E741 FLOAT YTD OVER 10000
      * E002 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4160-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-ADDR = SPACES
                   MOVE 'E917' TO WS-REJECT-CODE
               WHEN LBXM-FLOAT-YTD < 0
                   MOVE 'E691' TO WS-REJECT-CODE
               WHEN LBXM-FLOAT-YTD > 10000.00
                   MOVE 'E741' TO WS-REJECT-CODE
               WHEN LBXM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E002' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4170-SPLIT-CASH-ON-HAND.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE LBXT-CASH-ON-HAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4180-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-NOTICE-DATE > 0 AND LBXM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXT-CHECK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD LBXT-CHECK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD LBXT-CHECK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBXT-CHECK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXT-CHECK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4190-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO LBXM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT GRID TIER FOR THE FLOAT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4200-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-GRID-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF LBXT-FLOAT-AMT NOT < WS-DEPOSIT-GRID-LOW(WS-TX)
                   MOVE WS-DEPOSIT-GRID-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4210-COUNT-PAYMENT.
           IF LBXM-PAYMENT-CNT < 9999999
               ADD 1 TO LBXM-PAYMENT-CNT
           END-IF
           IF LBXM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO LBXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH ON HAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       4220-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-FEE-AMT ROUNDED =
                   LBXM-CASH-ON-HAND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-FEE-AMT
           END-IF
           ADD LBXM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4230-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO LBXM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4240-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO LBXM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH ON HAND YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4250-LIMIT-CASH-ON-HAND.
           IF LBXM-CASH-ON-HAND-YTD < 10.00
               MOVE 10.00 TO LBXM-CASH-ON-HAND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-CASH-ON-HAND-YTD > 50000.00
               MOVE 50000.00 TO LBXM-CASH-ON-HAND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REMIT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4260-FLAG-REMIT.
           IF LBXM-REMIT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE LBXM-REMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * REMIT AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4270-LIMIT-REMIT.
           IF LBXM-REMIT-AMT < 50.00
               MOVE 50.00 TO LBXM-REMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-REMIT-AMT > 5000.00
               MOVE 5000.00 TO LBXM-REMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4280-LIMIT-FLOAT.
           IF LBXT-FLOAT-AMT < 10.00
               MOVE 10.00 TO LBXT-FLOAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-FLOAT-AMT > 500.00
               MOVE 500.00 TO LBXT-FLOAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DVP -> FMZ WHEN NEXT DUE DATE IS PAST
      * FMZ -> KSL WHEN NEXT DUE DATE IS PAST
      * KSL -> NYK WHEN NEXT DUE DATE IS PAST
      * NYK -> DVP WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4290-SET-REGION.
           EVALUATE LBXM-REGION-CD
               WHEN 'DVP'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FMZ' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FMZ'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KSL' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KSL'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NYK' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NYK'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DVP' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4300-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO LBXT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT GRID TIER FOR THE CASH ON HAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4310-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-GRID-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF LBXT-CASH-ON-HAND-AMT NOT < WS-DEPOSIT-GRID-LOW(WS-TX)
                   MOVE WS-DEPOSIT-GRID-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E186 REF NO REQUIRED
      * E148 REMIT YTD NEGATIVE
      * E414 REMIT YTD OVER 99999
      * E283 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4320-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-REF-NO = SPACES
                   MOVE 'E186' TO WS-REJECT-CODE
               WHEN LBXM-REMIT-YTD < 0
                   MOVE 'E148' TO WS-REJECT-CODE
               WHEN LBXM-REMIT-YTD > 99999.00
                   MOVE 'E414' TO WS-REJECT-CODE
               WHEN LBXM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E283' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FLOAT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4330-PRORATE-FLOAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   LBXT-FLOAT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

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
      * REMIT YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4350-LIMIT-REMIT.
           IF LBXM-REMIT-YTD < 25.00
               MOVE 25.00 TO LBXM-REMIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-REMIT-YTD > 1000.00
               MOVE 1000.00 TO LBXM-REMIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT FEE ON THE FLOAT AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4360-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN LBXT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO LBXM-DEPOSIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE LBXM-DEPOSIT-AMT ROUNDED = LBXM-FLOAT-AMT *
                       LBXM-DISCOUNT / 200
               WHEN LBXM-CYCLE-CD = 'EDV'
                   MOVE 12.00 TO LBXM-DEPOSIT-AMT
               WHEN LBXM-CYCLE-CD = 'EPM'
                   MOVE 7.00 TO LBXM-DEPOSIT-AMT
               WHEN OTHER
                   COMPUTE LBXM-DEPOSIT-AMT ROUNDED = LBXM-FLOAT-AMT *
                       LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXM-DEPOSIT-AMT > 0 AND LBXM-DEPOSIT-AMT < 2.00
               MOVE 2.00 TO LBXM-DEPOSIT-AMT
           END-IF
           IF LBXM-DEPOSIT-AMT > 25.00
               MOVE 25.00 TO LBXM-DEPOSIT-AMT
           END-IF
           ADD LBXM-DEPOSIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4370-DESCRIBE-CHANNEL.
           EVALUATE LBXM-CHANNEL-CD
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4380-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXT-CASH-ON-HAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBXT-CASH-ON-HAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD LBXT-CASH-ON-HAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD LBXT-CASH-ON-HAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXT-CASH-ON-HAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4390-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 5
               MOVE 'Y' TO LBXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BATCH TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4400-DESCRIBE-BATCH-TYPE.
           EVALUATE LBXM-BATCH-TYPE-CD
               WHEN 'C'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'D'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'F'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'K'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'X'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4410-LIMIT-EXCEPT.
           IF LBXM-EXCEPT-YTD < 25.00
               MOVE 25.00 TO LBXM-EXCEPT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-EXCEPT-YTD > 50000.00
               MOVE 50000.00 TO LBXM-EXCEPT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E394 REJECT CD REQUIRED
      * E767 CASH ON HAND YTD NEGATIVE
      * E761 CASH ON HAND YTD OVER 1000000
      * E936 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4420-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-REJECT-CD = SPACES
                   MOVE 'E394' TO WS-REJECT-CODE
               WHEN LBXM-CASH-ON-HAND-YTD < 0
                   MOVE 'E767' TO WS-REJECT-CODE
               WHEN LBXM-CASH-ON-HAND-YTD > 1000000.00
                   MOVE 'E761' TO WS-REJECT-CODE
               WHEN LBXM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E936' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH ON HAND AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4430-SPLIT-CASH-ON-HAND.
           COMPUTE WS-WORK-AMT = LBXM-CASH-ON-HAND-AMT / 4
           COMPUTE WS-WORK-AMT-2 = LBXM-CASH-ON-HAND-AMT - (WS-WORK-AMT
               * 4)
           COMPUTE LBXT-CASH-ON-HAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4440-ACCRUE-FEE.
           IF LBXM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-FEE-AMT * LBXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO LBXM-DEPOSIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4450-LIMIT-FLOAT.
           IF LBXM-FLOAT-AMT < 100.00
               MOVE 100.00 TO LBXM-FLOAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-FLOAT-AMT > 50000.00
               MOVE 50000.00 TO LBXM-FLOAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E792 DESC REQUIRED
      * E167 FEE YTD NEGATIVE
      * E901 FEE YTD OVER 250000
      * E518 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4460-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-DESC = SPACES
                   MOVE 'E792' TO WS-REJECT-CODE
               WHEN LBXM-FEE-YTD < 0
                   MOVE 'E167' TO WS-REJECT-CODE
               WHEN LBXM-FEE-YTD > 250000.00
                   MOVE 'E901' TO WS-REJECT-CODE
               WHEN LBXM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E518' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ON HAND FEE ON THE CASH ON HAND AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4470-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN LBXM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO LBXM-CASH-ON-HAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE LBXM-CASH-ON-HAND-AMT ROUNDED =
                       LBXT-CASH-ON-HAND-AMT * LBXM-PENALTY-PCT / 200
               WHEN LBXM-CYCLE-CD = 'EDV'
                   MOVE 12.00 TO LBXM-CASH-ON-HAND-AMT
               WHEN LBXM-CYCLE-CD = 'EPM'
                   MOVE 20.00 TO LBXM-CASH-ON-HAND-AMT
               WHEN OTHER
                   COMPUTE LBXM-CASH-ON-HAND-AMT ROUNDED =
                       LBXT-CASH-ON-HAND-AMT * LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXM-CASH-ON-HAND-AMT > 0 AND LBXM-CASH-ON-HAND-AMT < 5.00
               MOVE 5.00 TO LBXM-CASH-ON-HAND-AMT
           END-IF
           IF LBXM-CASH-ON-HAND-AMT > 150.00
               MOVE 150.00 TO LBXM-CASH-ON-HAND-AMT
           END-IF
           ADD LBXM-CASH-ON-HAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CASH ON SCHED TIER FOR THE FLOAT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4480-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ON-SCHED-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF LBXT-FLOAT-AMT NOT < WS-CASH-ON-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-ON-SCHED-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4490-LIMIT-FLOAT.
           IF LBXM-FLOAT-AMT < 100.00
               MOVE 100.00 TO LBXM-FLOAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-FLOAT-AMT > 2500.00
               MOVE 2500.00 TO LBXM-FLOAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH ON SCHED TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4500-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ON-SCHED-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF LBXM-DEPOSIT-AMT NOT < WS-CASH-ON-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-ON-SCHED-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E461 CUST ID REQUIRED
      * E571 CHECK YTD NEGATIVE
      * E333 CHECK YTD OVER 1000000
      * E498 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4510-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-CUST-ID = SPACES
                   MOVE 'E461' TO WS-REJECT-CODE
               WHEN LBXM-CHECK-YTD < 0
                   MOVE 'E571' TO WS-REJECT-CODE
               WHEN LBXM-CHECK-YTD > 1000000.00
                   MOVE 'E333' TO WS-REJECT-CODE
               WHEN LBXM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E498' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH ON SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4520-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ON-SCHED-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-CASH-ON-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-ON-SCHED-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FLOAT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEPOSIT AMT.
      *----------------------------------------------------------------*
       4530-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-DEPOSIT-AMT ROUNDED =
                   LBXM-FLOAT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-DEPOSIT-AMT
           END-IF
           ADD LBXM-DEPOSIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4540-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 5
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CHECK YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4550-FLAG-CHECK.
           IF LBXM-CHECK-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBXM-CHECK-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EXPIRY DATE IS PAST
      * BAT -> MAL WHEN EXPIRY DATE IS PAST
      * MAL -> TEL WHEN EXPIRY DATE IS PAST
      * TEL -> ONL WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4560-SET-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT GRID TIER FOR THE REMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4570-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-GRID-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF LBXM-REMIT-AMT NOT < WS-DEPOSIT-GRID-LOW(WS-TX)
                   MOVE WS-DEPOSIT-GRID-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4580-COUNT-PAYMENT.
           IF LBXM-PAYMENT-CNT < 9999999
               ADD 1 TO LBXM-PAYMENT-CNT
           END-IF
           IF LBXM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO LBXT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4590-LIMIT-FEE.
           IF LBXM-FEE-AMT < 100.00
               MOVE 100.00 TO LBXM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-FEE-AMT > 1000.00
               MOVE 1000.00 TO LBXM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE LBX FIELDS TO LBWAIV AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-LBWAIV.
           INITIALIZE LBWAIV-PARMS
           MOVE LBXM-EXCEPT-AMT
             TO LBWAIV-EXCEPT-AMT
           MOVE LBXM-FLOAT-AMT
             TO LBWAIV-FLOAT-AMT
           MOVE LBXM-CHECK-YTD
             TO LBWAIV-CHECK-YTD
           MOVE LBXM-CASH-ON-HAND-YTD
             TO LBWAIV-CASH-ON-HAND-YTD
           MOVE LBXM-FLOAT-YTD
             TO LBWAIV-FLOAT-YTD
           MOVE LBXM-REMIT-YTD
             TO LBWAIV-REMIT-YTD
           MOVE LBXM-MATURITY-DATE
             TO LBWAIV-MATURITY-DATE
           MOVE LBXM-EXPIRY-DATE
             TO LBWAIV-EXPIRY-DATE
           MOVE LBXM-LAST-PAY-DATE
             TO LBWAIV-LAST-PAY-DATE
           MOVE LBXM-LAST-STMT-DATE
             TO LBWAIV-LAST-STMT-DATE
           MOVE LBXM-REASON-CD
             TO LBWAIV-REASON-CD
           MOVE LBXM-LOCKBOX-CD
             TO LBWAIV-LOCKBOX-CD
           MOVE LBXM-REGION-CD
             TO LBWAIV-REGION-CD
           MOVE LBXM-CHANNEL-CD
             TO LBWAIV-CHANNEL-CD
           MOVE LBXM-PREFERRED-FLAG
             TO LBWAIV-PREFERRED-FLAG
           MOVE LBXM-EXEMPT-FLAG
             TO LBWAIV-EXEMPT-FLAG
           MOVE LBXM-STOP-FLAG
             TO LBWAIV-STOP-FLAG
           MOVE LBXM-PAYMENT-CNT
             TO LBWAIV-PAYMENT-CNT
           MOVE LBXM-EXCEPT-CNT
             TO LBWAIV-EXCEPT-CNT
           MOVE LBXM-PENALTY-PCT
             TO LBWAIV-PENALTY-PCT
           MOVE LBXM-DISCOUNT
             TO LBWAIV-DISCOUNT
           MOVE LBXM-CONTACT
             TO LBWAIV-CONTACT
           MOVE LBXM-CUST-ID
             TO LBWAIV-CUST-ID
           CALL 'LBWAIV' USING LBWAIV-PARMS
           EVALUATE LBWAIV-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE LBWAIV-EXCEPT-AMT
                     TO LBXM-EXCEPT-AMT
                   MOVE LBWAIV-EXEMPT-FLAG
                     TO LBXM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RAIV' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-LBWAIV' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE LBXT-KEY TO RD-KEY
           MOVE LBXT-TRAN-CD TO RD-CODE
           MOVE LBXT-FLOAT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE LBXT-KEY TO RD-KEY
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
           DISPLAY 'LBXAGE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'LBXAGE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
