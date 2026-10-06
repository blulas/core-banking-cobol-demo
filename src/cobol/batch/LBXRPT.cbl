      *================================================================*
      * PROGRAM  : LBXRPT                                              *
      * TYPE     : BATCH                                               *
      * PURPOSE  : LOCKBOX REMITTANCE - MANAGEMENT REPORTING. READS THE*
      *            LBX TRANSACTIONS, UPDATES THE LBX MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. WHOLESALE LOCKBOX      *
      *            REMITTANCE CAPTURE AND CREDIT TO CLIENTS            *
      * INPUT    : LBXMST (KSDS), LBXTRN, BANKCTL                      *
      * OUTPUT   : LBXOUT, LBXRPRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, LBWAIV, LBFEE, LBCAL    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-09-02 GAF  ORIGINAL                                       *
      * 1998-01-25 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-03-10 PQR  DEPOSIT TABLE EXPANDED CHG6683                 *
      * 2002-04-09 CRP  FLOAT ROUNDING - HALF UP REQ7466               *
      * 2003-03-12 PQR  FLOAT RERUN PROTECTION CHG0581                 *
      * 2005-08-11 RJT  EXCEPT FEE SCHEDULE UPDATE INC5371             *
      * 2005-10-09 CRP  EXCEPT CUTOFF MOVED INC4655                    *
      * 2006-06-10 JLM  FEE ROUNDING - HALF UP INC1196                 *
      * 2007-11-18 RJT  REMIT EXCEPTIONS TO OPS QUEUE PRB7025          *
      * 2008-03-28 EWS  EXCEPT CUTOFF MOVED CHG9156                    *
      * 2008-09-15 GAF  CASH-ON-HAND RERUN PROTECTION REQ7089          *
      * 2013-12-10 LMB  REMIT RERUN PROTECTION CHG7610                 *
      * 2014-07-23 GAF  DEPOSIT CUTOFF MOVED PRB0138                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBXRPT.
       AUTHOR. B H SANDERS.

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
           SELECT REPORT-FILE ASSIGN TO "LBXRPRPT"
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
                                           VALUE 11.000000.
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
      * WS-FLOAT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FLOAT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000233000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000515500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000409500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000457000'.
       01  WS-FLOAT-SCHED-TBL REDEFINES WS-FLOAT-SCHED-VALUES.
           05  WS-FLOAT-SCHED-ROW OCCURS 4 TIMES.
               10  WS-FLOAT-SCHED-LOW      PIC 9(09)V99.
               10  WS-FLOAT-SCHED-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000379000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000524000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000883500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000230500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005350000000088500'.
       01  WS-FEE-TIER-TBL REDEFINES WS-FEE-TIER-VALUES.
           05  WS-FEE-TIER-ROW OCCURS 5 TIMES.
               10  WS-FEE-TIER-LOW         PIC 9(09)V99.
               10  WS-FEE-TIER-RATE        PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  LBXRPT  LOCKBOX REMITTANCE'.
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
       COPY LBFEEP.
       COPY LBCALP.

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
               PERFORM 3000-COUNT-RETRY
               PERFORM 3010-LIMIT-FEE
               PERFORM 3020-COUNT-PAYMENT
               PERFORM 3030-CHECK-EFFECTIVE
               PERFORM 3040-ACCUM-REASON
               PERFORM 3050-SPLIT-DEPOSIT
               PERFORM 3060-COUNT-RETRY
               PERFORM 3070-DESCRIBE-REASON
               PERFORM 3080-ACCRUE-FEE
               PERFORM 3090-LIMIT-FLOAT
               PERFORM 3100-PRORATE-REMIT
               PERFORM 3110-SELECT-FEE-PCT
               PERFORM 3120-COUNT-RETRY
               PERFORM 3130-LIMIT-DEPOSIT
               PERFORM 3140-PRORATE-REMIT
               PERFORM 3150-ACCUM-TRAN
               PERFORM 3160-DESCRIBE-CHANNEL
               PERFORM 3170-LIMIT-CASH-ON-HAND
               PERFORM 3180-ACCUM-TRAN
               PERFORM 3190-SPLIT-FEE
               PERFORM 3200-CHECK-OPEN
               PERFORM 3210-ASSESS-STOP
               PERFORM 3220-DESCRIBE-TRAN
               PERFORM 3230-SPLIT-CHECK
               PERFORM 3240-EDIT-DESC
               PERFORM 3250-ASSESS-STOP
               PERFORM 3260-PRORATE-REMIT
               PERFORM 3270-SELECT-DISCOUNT
               PERFORM 3280-LIMIT-CHECK
               PERFORM 3290-PRORATE-CASH-ON-HAND
               PERFORM 3300-FLAG-CASH-ON-HAND
               PERFORM 3310-AGE-MATURITY
               PERFORM 3320-FLAG-CHECK
               PERFORM 3330-SELECT-FEE-PCT
               PERFORM 3340-CHECK-EFFECTIVE
               PERFORM 3350-SPLIT-FEE
               PERFORM 3360-EDIT-CONTACT
               PERFORM 3370-AGE-OPEN
               PERFORM 3380-EDIT-DESC
               PERFORM 3390-COUNT-ITEM
               PERFORM 3400-CHECK-LAST-PAY
               PERFORM 3410-AGE-POST
               PERFORM 3420-COUNT-SEQ
               PERFORM 3430-AGE-MATURITY
               PERFORM 3440-PRORATE-CASH-ON-HAND
               PERFORM 3450-SPLIT-FEE
               PERFORM 3460-ASSESS-EXEMPT
               PERFORM 3470-SET-CHANNEL
               PERFORM 3480-FLAG-ITEM
               PERFORM 3490-CHECK-POST
               PERFORM 3500-EDIT-ADDR
               PERFORM 3510-SELECT-DISCOUNT
               PERFORM 3520-CHECK-MATURITY
               PERFORM 3530-CHECK-LAST-STMT
               PERFORM 3540-EDIT-MEMO
               PERFORM 3550-ASSESS-EXEMPT
               PERFORM 3560-PRORATE-CASH-ON-HAND
               PERFORM 3570-PRORATE-EXCEPT
               PERFORM 3580-FLAG-REMIT
               PERFORM 3590-ASSESS-REVERSAL
               PERFORM 3600-LIMIT-ITEM
               PERFORM 3610-PRORATE-REMIT
               PERFORM 3620-DESCRIBE-REGION
               PERFORM 3630-CHECK-CLOSE
               PERFORM 3640-DESCRIBE-CHANNEL
               PERFORM 3650-FLAG-FLOAT
               PERFORM 3660-SPLIT-REMIT
               PERFORM 3670-COUNT-EXCEPT
               PERFORM 3680-SPLIT-CASH-ON-HAND
               PERFORM 3690-COUNT-PAYMENT
               PERFORM 3700-ACCRUE-FEE
               PERFORM 3710-COUNT-PAYMENT
               PERFORM 3720-DESCRIBE-LOCKBOX
               PERFORM 3730-DESCRIBE-REASON
               PERFORM 3740-PRORATE-FEE
               PERFORM 3750-SET-CHANNEL
               PERFORM 3760-CHECK-MATURITY
               PERFORM 3770-CHECK-NOTICE
               PERFORM 3780-ASSESS-REVERSAL
               PERFORM 3790-LIMIT-CHECK
               PERFORM 3800-DESCRIBE-REASON
               PERFORM 3810-COUNT-EXCEPT
               PERFORM 3820-EDIT-REJECT
               PERFORM 3830-LIMIT-ITEM
               PERFORM 3840-SELECT-PENALTY-PCT
               PERFORM 3850-ACCRUE-EXCEPT
               PERFORM 3860-COUNT-RETRY
               PERFORM 3870-COUNT-EXCEPT
               PERFORM 3880-EDIT-MEMO
               PERFORM 3890-DESCRIBE-REASON
               PERFORM 3900-SPLIT-EXCEPT
               PERFORM 3910-SPLIT-DEPOSIT
               PERFORM 3920-FLAG-FEE
               PERFORM 3930-FLAG-FEE
               PERFORM 3940-SET-LOCKBOX
               PERFORM 3950-COUNT-EXCEPT
               PERFORM 3960-AGE-LAST-STMT
               PERFORM 3970-SELECT-FEE-PCT
               PERFORM 3980-EDIT-CONTACT
               PERFORM 3990-PRORATE-DEPOSIT
               PERFORM 4000-COUNT-RETRY
               PERFORM 4010-EDIT-MEMO
               PERFORM 4020-ASSESS-EXEMPT
               PERFORM 4030-AGE-NOTICE
               PERFORM 4040-ACCRUE-FLOAT
               PERFORM 4050-FLAG-DEPOSIT
               PERFORM 4060-ACCRUE-FLOAT
               PERFORM 4070-SET-LOCKBOX
               PERFORM 4080-CHECK-MATURITY
               PERFORM 4090-PRORATE-DEPOSIT
               PERFORM 4100-EDIT-MEMO
               PERFORM 4110-ACCUM-REASON
               PERFORM 4120-LIMIT-DEPOSIT
               PERFORM 4130-LIMIT-ITEM
               PERFORM 4140-EDIT-CUST-ID
               PERFORM 4150-DESCRIBE-BATCH-TYPE
               PERFORM 4160-ASSESS-HOLD
               PERFORM 4170-EDIT-ADDR
               PERFORM 4180-CHECK-POST
               PERFORM 4190-PRORATE-EXCEPT
               PERFORM 4200-ACCRUE-DEPOSIT
               PERFORM 4210-SET-CHANNEL
               PERFORM 4220-SELECT-FEE-PCT
               PERFORM 4230-SET-BATCH-TYPE
               PERFORM 4240-SPLIT-CASH-ON-HAND
               PERFORM 4250-COUNT-SEQ
               PERFORM 4260-ASSESS-NOTICE-SENT
               PERFORM 4270-SET-BATCH-TYPE
               PERFORM 4280-LIMIT-FLOAT
               PERFORM 4290-LIMIT-CASH-ON-HAND
               PERFORM 4300-SELECT-FEE-PCT
               PERFORM 4310-EDIT-CUST-ID
               PERFORM 4320-SELECT-DISCOUNT
               PERFORM 4330-DESCRIBE-CHANNEL
               PERFORM 4340-CHECK-EXPIRY
               PERFORM 4350-FLAG-CASH-ON-HAND
               PERFORM 4360-ASSESS-EXEMPT
               PERFORM 4370-AGE-MATURITY
               PERFORM 4380-FLAG-DEPOSIT
               PERFORM 4390-DESCRIBE-CYCLE
               PERFORM 4400-CHECK-EFF
               PERFORM 4410-LIMIT-ITEM
               PERFORM 4420-COUNT-ITEM
               PERFORM 4430-LIMIT-CHECK
               PERFORM 4440-DESCRIBE-LOCKBOX
               PERFORM 4450-ASSESS-EXEMPT
               PERFORM 4460-CHECK-OPEN
               PERFORM 4470-COUNT-ITEM
               PERFORM 4480-AGE-LAST-STMT
               PERFORM 4490-LIMIT-FLOAT
               PERFORM 4500-LIMIT-ITEM
               PERFORM 4510-CHECK-NEXT-DUE
               PERFORM 4520-LIMIT-CHECK
               PERFORM 4530-SELECT-DISCOUNT
               PERFORM 4540-FLAG-ITEM
               PERFORM 4550-LIMIT-FEE
               PERFORM 4560-SELECT-DISCOUNT
               PERFORM 4570-COUNT-SEQ
               PERFORM 4580-SET-CYCLE
               PERFORM 4590-COUNT-SEQ
               PERFORM 4600-ACCRUE-PENALTY-PCT
               PERFORM 4610-CHECK-LAST-PAY
               PERFORM 4620-AGE-MATURITY
               PERFORM 4630-SET-CYCLE
               PERFORM 4640-ASSESS-AUTO-RENEW
               PERFORM 4650-ASSESS-STOP
               PERFORM 4660-COUNT-ITEM
               PERFORM 4670-ACCUM-TRAN
               PERFORM 4680-SELECT-PENALTY-PCT
               PERFORM 7000-CALL-LBWAIV
               PERFORM 7020-CALL-LBFEE
               PERFORM 7010-CALL-LBCAL
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
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3000-COUNT-RETRY.
           IF LBXM-RETRY-CNT < 9999999
               ADD 1 TO LBXM-RETRY-CNT
           END-IF
           IF LBXM-RETRY-CNT NOT < 10
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3010-LIMIT-FEE.
           IF LBXT-FEE-AMT < 25.00
               MOVE 25.00 TO LBXT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-FEE-AMT > 5000.00
               MOVE 5000.00 TO LBXT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-PAYMENT.
           IF LBXM-PAYMENT-CNT < 9999999
               ADD 1 TO LBXM-PAYMENT-CNT
           END-IF
           IF LBXM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO LBXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3030-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FLOAT YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3040-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REASON-CD
                   ADD LBXM-FLOAT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-FLOAT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3050-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = LBXM-DEPOSIT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = LBXM-DEPOSIT-AMT - (WS-WORK-AMT * 2)
           COMPUTE LBXM-EXCEPT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3060-COUNT-RETRY.
           IF LBXM-RETRY-CNT < 9999999
               ADD 1 TO LBXM-RETRY-CNT
           END-IF
           IF LBXM-RETRY-CNT NOT < 3
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3070-DESCRIBE-REASON.
           EVALUATE LBXM-REASON-CD
               WHEN 'GF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3080-ACCRUE-FEE.
           IF LBXT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXT-FEE-AMT * LBXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-REMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3090-LIMIT-FLOAT.
           IF LBXM-FLOAT-AMT < 100.00
               MOVE 100.00 TO LBXM-FLOAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-FLOAT-AMT > 1000.00
               MOVE 1000.00 TO LBXM-FLOAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REMIT AMT.
      *----------------------------------------------------------------*
       3100-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-REMIT-AMT ROUNDED =
                   LBXM-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-REMIT-AMT
           END-IF
           ADD LBXM-REMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3110-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-SCHED-RATE(1) TO LBXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-FLOAT-SCHED-LOW(WS-TX)
                   MOVE WS-FLOAT-SCHED-RATE(WS-TX) TO LBXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3120-COUNT-RETRY.
           IF LBXM-RETRY-CNT < 9999999
               ADD 1 TO LBXM-RETRY-CNT
           END-IF
           IF LBXM-RETRY-CNT NOT < 3
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3130-LIMIT-DEPOSIT.
           IF LBXM-DEPOSIT-AMT < 100.00
               MOVE 100.00 TO LBXM-DEPOSIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-DEPOSIT-AMT > 2500.00
               MOVE 2500.00 TO LBXM-DEPOSIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH ON HAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REMIT YTD.
      *----------------------------------------------------------------*
       3140-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-REMIT-YTD ROUNDED =
                   LBXM-CASH-ON-HAND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-REMIT-YTD
           END-IF
           ADD LBXM-REMIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ON HAND AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3150-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXT-TRAN-CD
                   ADD LBXT-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXT-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3160-DESCRIBE-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH ON HAND AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3170-LIMIT-CASH-ON-HAND.
           IF LBXM-CASH-ON-HAND-AMT < 100.00
               MOVE 100.00 TO LBXM-CASH-ON-HAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-CASH-ON-HAND-AMT > 5000.00
               MOVE 5000.00 TO LBXM-CASH-ON-HAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3180-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXT-TRAN-CD
                   ADD LBXM-REMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-REMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3190-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = LBXM-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = LBXM-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE LBXM-FLOAT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3200-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF LBXM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE REMIT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3210-ASSESS-STOP.
           EVALUATE TRUE
               WHEN LBXM-STOP-FLAG = 'Y'
                   MOVE ZERO TO LBXM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE LBXM-FEE-AMT ROUNDED = LBXM-REMIT-AMT *
                       LBXM-PENALTY-PCT / 200
               WHEN LBXM-CHANNEL-CD = 'B'
                   MOVE 10.00 TO LBXM-FEE-AMT
               WHEN LBXM-CHANNEL-CD = 'H'
                   MOVE 5.00 TO LBXM-FEE-AMT
               WHEN OTHER
                   COMPUTE LBXM-FEE-AMT ROUNDED = LBXM-REMIT-AMT *
                       LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXM-FEE-AMT > 0 AND LBXM-FEE-AMT < 1.00
               MOVE 1.00 TO LBXM-FEE-AMT
           END-IF
           IF LBXM-FEE-AMT > 35.00
               MOVE 35.00 TO LBXM-FEE-AMT
           END-IF
           ADD LBXM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3220-DESCRIBE-TRAN.
           EVALUATE LBXT-TRAN-CD
               WHEN 'RC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'EX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'AD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CHECK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3230-SPLIT-CHECK.
           COMPUTE WS-WORK-AMT = LBXT-CHECK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = LBXT-CHECK-AMT - (WS-WORK-AMT * 2)
           COMPUTE LBXM-CASH-ON-HAND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E056 DESC REQUIRED
      * E166 FEE YTD NEGATIVE
      * E870 FEE YTD OVER 5000
      * E874 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-DESC = SPACES
                   MOVE 'E056' TO WS-REJECT-CODE
               WHEN LBXM-FEE-YTD < 0
                   MOVE 'E166' TO WS-REJECT-CODE
               WHEN LBXM-FEE-YTD > 5000.00
                   MOVE 'E870' TO WS-REJECT-CODE
               WHEN LBXM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E874' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REMIT FEE ON THE CHECK YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3250-ASSESS-STOP.
           EVALUATE TRUE
               WHEN LBXM-STOP-FLAG = 'Y'
                   MOVE ZERO TO LBXM-REMIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   COMPUTE LBXM-REMIT-AMT ROUNDED = LBXM-CHECK-YTD *
                       LBXM-DISCOUNT / 200
               WHEN LBXT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO LBXM-REMIT-AMT
               WHEN LBXT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO LBXM-REMIT-AMT
               WHEN OTHER
                   COMPUTE LBXM-REMIT-AMT ROUNDED = LBXM-CHECK-YTD *
                       LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXM-REMIT-AMT > 0 AND LBXM-REMIT-AMT < 2.00
               MOVE 2.00 TO LBXM-REMIT-AMT
           END-IF
           IF LBXM-REMIT-AMT > 50.00
               MOVE 50.00 TO LBXM-REMIT-AMT
           END-IF
           ADD LBXM-REMIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REMIT AMT.
      *----------------------------------------------------------------*
       3260-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-REMIT-AMT ROUNDED =
                   LBXM-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-REMIT-AMT
           END-IF
           ADD LBXM-REMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT SCHED TIER FOR THE DEPOSIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-SCHED-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF LBXM-DEPOSIT-YTD NOT < WS-FLOAT-SCHED-LOW(WS-TX)
                   MOVE WS-FLOAT-SCHED-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHECK AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3280-LIMIT-CHECK.
           IF LBXT-CHECK-AMT < 10.00
               MOVE 10.00 TO LBXT-CHECK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-CHECK-AMT > 1000.00
               MOVE 1000.00 TO LBXT-CHECK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH ON HAND YTD.
      *----------------------------------------------------------------*
       3290-PRORATE-CASH-ON-HAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-CASH-ON-HAND-YTD ROUNDED =
                   LBXM-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-CASH-ON-HAND-YTD
           END-IF
           ADD LBXM-CASH-ON-HAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CASH ON HAND YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3300-FLAG-CASH-ON-HAND.
           IF LBXM-CASH-ON-HAND-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE LBXM-CASH-ON-HAND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3310-AGE-MATURITY.
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
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CHECK YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3320-FLAG-CHECK.
           IF LBXM-CHECK-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE LBXM-CHECK-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE DEPOSIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO LBXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF LBXM-DEPOSIT-YTD NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO LBXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO LBXM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3350-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = LBXM-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = LBXM-FEE-YTD - (WS-WORK-AMT * 3)
           COMPUTE LBXT-CASH-ON-HAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E504 CONTACT REQUIRED
      * E075 DEPOSIT AMT NEGATIVE
      * E844 DEPOSIT AMT OVER 5000
      * E133 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-CONTACT = SPACES
                   MOVE 'E504' TO WS-REJECT-CODE
               WHEN LBXM-DEPOSIT-AMT < 0
                   MOVE 'E075' TO WS-REJECT-CODE
               WHEN LBXM-DEPOSIT-AMT > 5000.00
                   MOVE 'E844' TO WS-REJECT-CODE
               WHEN LBXM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E133' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-OPEN.
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
                   ADD LBXT-CHECK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBXT-CHECK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD LBXT-CHECK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBXT-CHECK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXT-CHECK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E921 DESC REQUIRED
      * E807 CHECK AMT NEGATIVE
      * E649 CHECK AMT OVER 10000
      * E734 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3380-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-DESC = SPACES
                   MOVE 'E921' TO WS-REJECT-CODE
               WHEN LBXM-CHECK-AMT < 0
                   MOVE 'E807' TO WS-REJECT-CODE
               WHEN LBXM-CHECK-AMT > 10000.00
                   MOVE 'E649' TO WS-REJECT-CODE
               WHEN LBXM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E734' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3390-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 12
               MOVE 'Y' TO LBXM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3400-CHECK-LAST-PAY.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-POST.
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
                   ADD LBXT-CASH-ON-HAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD LBXT-CASH-ON-HAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD LBXT-CASH-ON-HAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD LBXT-CASH-ON-HAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXT-CASH-ON-HAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3420-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 6
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3430-AGE-MATURITY.
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
               WHEN WS-DAYS NOT > 90
                   ADD LBXT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD LBXT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBXT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FLOAT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH ON HAND AMT.
      *----------------------------------------------------------------*
       3440-PRORATE-CASH-ON-HAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-CASH-ON-HAND-AMT ROUNDED =
                   LBXT-FLOAT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-CASH-ON-HAND-AMT
           END-IF
           ADD LBXM-CASH-ON-HAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3450-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = LBXM-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = LBXM-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE LBXM-REMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE CHECK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3460-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN LBXM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO LBXT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   COMPUTE LBXT-FEE-AMT ROUNDED = LBXT-CHECK-AMT *
                       LBXM-DISCOUNT / 200
               WHEN LBXM-BATCH-TYPE-CD = 'C'
                   MOVE 15.00 TO LBXT-FEE-AMT
               WHEN LBXM-BATCH-TYPE-CD = 'D'
                   MOVE 15.00 TO LBXT-FEE-AMT
               WHEN OTHER
                   COMPUTE LBXT-FEE-AMT ROUNDED = LBXT-CHECK-AMT *
                       LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXT-FEE-AMT > 0 AND LBXT-FEE-AMT < 10.00
               MOVE 10.00 TO LBXT-FEE-AMT
           END-IF
           IF LBXT-FEE-AMT > 150.00
               MOVE 150.00 TO LBXT-FEE-AMT
           END-IF
           ADD LBXT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * B -> H WHEN LAST STMT DATE IS PAST
      * H -> Z WHEN LAST STMT DATE IS PAST
      * Z -> B WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3470-SET-CHANNEL.
           EVALUATE LBXM-CHANNEL-CD
               WHEN 'B'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3480-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3490-CHECK-POST.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E722 ADDR REQUIRED
      * E514 CHECK AMT NEGATIVE
      * E918 CHECK AMT OVER 10000
      * E409 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3500-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-ADDR = SPACES
                   MOVE 'E722' TO WS-REJECT-CODE
               WHEN LBXT-CHECK-AMT < 0
                   MOVE 'E514' TO WS-REJECT-CODE
               WHEN LBXT-CHECK-AMT > 10000.00
                   MOVE 'E918' TO WS-REJECT-CODE
               WHEN LBXM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E409' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT SCHED TIER FOR THE CHECK YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3510-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-SCHED-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF LBXM-CHECK-YTD NOT < WS-FLOAT-SCHED-LOW(WS-TX)
                   MOVE WS-FLOAT-SCHED-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3520-CHECK-MATURITY.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO LBXM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3530-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF LBXM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO LBXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E926 MEMO REQUIRED
      * E416 EXCEPT AMT NEGATIVE
      * E644 EXCEPT AMT OVER 25000
      * E966 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3540-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-MEMO = SPACES
                   MOVE 'E926' TO WS-REJECT-CODE
               WHEN LBXM-EXCEPT-AMT < 0
                   MOVE 'E416' TO WS-REJECT-CODE
               WHEN LBXM-EXCEPT-AMT > 25000.00
                   MOVE 'E644' TO WS-REJECT-CODE
               WHEN LBXM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E966' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ON HAND FEE ON THE FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3550-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN LBXM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO LBXT-CASH-ON-HAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE LBXT-CASH-ON-HAND-AMT ROUNDED = LBXM-FEE-AMT
                       * LBXM-PENALTY-PCT / 200
               WHEN LBXM-LOCKBOX-CD = 'DX'
                   MOVE 15.00 TO LBXT-CASH-ON-HAND-AMT
               WHEN LBXM-LOCKBOX-CD = 'JM'
                   MOVE 12.00 TO LBXT-CASH-ON-HAND-AMT
               WHEN OTHER
                   COMPUTE LBXT-CASH-ON-HAND-AMT ROUNDED = LBXM-FEE-AMT
                       * LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXT-CASH-ON-HAND-AMT > 0 AND LBXT-CASH-ON-HAND-AMT < 2.00
               MOVE 2.00 TO LBXT-CASH-ON-HAND-AMT
           END-IF
           IF LBXT-CASH-ON-HAND-AMT > 50.00
               MOVE 50.00 TO LBXT-CASH-ON-HAND-AMT
           END-IF
           ADD LBXT-CASH-ON-HAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH ON HAND AMT.
      *----------------------------------------------------------------*
       3560-PRORATE-CASH-ON-HAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXT-CASH-ON-HAND-AMT ROUNDED =
                   LBXM-EXCEPT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXT-CASH-ON-HAND-AMT
           END-IF
           ADD LBXT-CASH-ON-HAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE CASH ON HAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO EXCEPT YTD.
      *----------------------------------------------------------------*
       3570-PRORATE-EXCEPT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-EXCEPT-YTD ROUNDED =
                   LBXT-CASH-ON-HAND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-EXCEPT-YTD
           END-IF
           ADD LBXM-EXCEPT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * REMIT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3580-FLAG-REMIT.
           IF LBXM-REMIT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBXM-REMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REMIT YTD FEE ON THE CASH ON HAND AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3590-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN LBXT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO LBXM-REMIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   COMPUTE LBXM-REMIT-YTD ROUNDED =
                       LBXT-CASH-ON-HAND-AMT * LBXM-PENALTY-PCT / 200
               WHEN LBXM-LOCKBOX-CD = 'DX'
                   MOVE 7.00 TO LBXM-REMIT-YTD
               WHEN LBXM-LOCKBOX-CD = 'JM'
                   MOVE 12.00 TO LBXM-REMIT-YTD
               WHEN OTHER
                   COMPUTE LBXM-REMIT-YTD ROUNDED =
                       LBXT-CASH-ON-HAND-AMT * LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXM-REMIT-YTD > 0 AND LBXM-REMIT-YTD < 1.00
               MOVE 1.00 TO LBXM-REMIT-YTD
           END-IF
           IF LBXM-REMIT-YTD > 75.00
               MOVE 75.00 TO LBXM-REMIT-YTD
           END-IF
           ADD LBXM-REMIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3600-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FLOAT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REMIT AMT.
      *----------------------------------------------------------------*
       3610-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-REMIT-AMT ROUNDED =
                   LBXM-FLOAT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-REMIT-AMT
           END-IF
           ADD LBXM-REMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3620-DESCRIBE-REGION.
           EVALUATE LBXM-REGION-CD
               WHEN 'DVP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FMZ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KSL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NYK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3630-CHECK-CLOSE.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3640-DESCRIBE-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FLOAT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3650-FLAG-FLOAT.
           IF LBXM-FLOAT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBXM-FLOAT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REMIT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3660-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = LBXM-REMIT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = LBXM-REMIT-YTD - (WS-WORK-AMT * 12)
           COMPUTE LBXM-EXCEPT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-EXCEPT.
           IF LBXM-EXCEPT-CNT < 9999999
               ADD 1 TO LBXM-EXCEPT-CNT
           END-IF
           IF LBXM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO LBXM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH ON HAND AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3680-SPLIT-CASH-ON-HAND.
           COMPUTE WS-WORK-AMT = LBXM-CASH-ON-HAND-AMT / 2
           COMPUTE WS-WORK-AMT-2 = LBXM-CASH-ON-HAND-AMT - (WS-WORK-AMT
               * 2)
           COMPUTE LBXM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3690-COUNT-PAYMENT.
           IF LBXM-PAYMENT-CNT < 9999999
               ADD 1 TO LBXM-PAYMENT-CNT
           END-IF
           IF LBXM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO LBXT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3700-ACCRUE-FEE.
           IF LBXM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-FEE-YTD * LBXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO LBXT-FLOAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3710-COUNT-PAYMENT.
           IF LBXM-PAYMENT-CNT < 9999999
               ADD 1 TO LBXM-PAYMENT-CNT
           END-IF
           IF LBXM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LOCKBOX CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3720-DESCRIBE-LOCKBOX.
           EVALUATE LBXM-LOCKBOX-CD
               WHEN 'DX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-REASON.
           EVALUATE LBXM-REASON-CD
               WHEN 'GF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FLOAT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3740-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-FEE-YTD ROUNDED =
                   LBXM-FLOAT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-FEE-YTD
           END-IF
           ADD LBXM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST STMT DATE IS PAST
      * BAT -> MAL WHEN LAST STMT DATE IS PAST
      * MAL -> TEL WHEN LAST STMT DATE IS PAST
      * TEL -> ONL WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3760-CHECK-MATURITY.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3770-CHECK-NOTICE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REMIT FEE ON THE DEPOSIT YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3780-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN LBXT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO LBXM-REMIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE LBXM-REMIT-AMT ROUNDED = LBXM-DEPOSIT-YTD *
                       LBXM-DISCOUNT / 200
               WHEN LBXT-TRAN-CD = 'RC'
                   MOVE 20.00 TO LBXM-REMIT-AMT
               WHEN LBXT-TRAN-CD = 'DP'
                   MOVE 3.00 TO LBXM-REMIT-AMT
               WHEN OTHER
                   COMPUTE LBXM-REMIT-AMT ROUNDED = LBXM-DEPOSIT-YTD *
                       LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXM-REMIT-AMT > 0 AND LBXM-REMIT-AMT < 2.00
               MOVE 2.00 TO LBXM-REMIT-AMT
           END-IF
           IF LBXM-REMIT-AMT > 150.00
               MOVE 150.00 TO LBXM-REMIT-AMT
           END-IF
           ADD LBXM-REMIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CHECK YTD IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3790-LIMIT-CHECK.
           IF LBXM-CHECK-YTD < 10.00
               MOVE 10.00 TO LBXM-CHECK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-CHECK-YTD > 500.00
               MOVE 500.00 TO LBXM-CHECK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3800-DESCRIBE-REASON.
           EVALUATE LBXM-REASON-CD
               WHEN 'GF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3810-COUNT-EXCEPT.
           IF LBXM-EXCEPT-CNT < 9999999
               ADD 1 TO LBXM-EXCEPT-CNT
           END-IF
           IF LBXM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E307 REJECT CD REQUIRED
      * E038 DEPOSIT YTD NEGATIVE
      * E864 DEPOSIT YTD OVER 10000
      * E220 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3820-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-REJECT-CD = SPACES
                   MOVE 'E307' TO WS-REJECT-CODE
               WHEN LBXM-DEPOSIT-YTD < 0
                   MOVE 'E038' TO WS-REJECT-CODE
               WHEN LBXM-DEPOSIT-YTD > 10000.00
                   MOVE 'E864' TO WS-REJECT-CODE
               WHEN LBXM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E220' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3830-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3840-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCEPT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3850-ACCRUE-EXCEPT.
           IF LBXM-EXCEPT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-EXCEPT-AMT * LBXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3860-COUNT-RETRY.
           IF LBXM-RETRY-CNT < 9999999
               ADD 1 TO LBXM-RETRY-CNT
           END-IF
           IF LBXM-RETRY-CNT NOT < 6
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3870-COUNT-EXCEPT.
           IF LBXM-EXCEPT-CNT < 9999999
               ADD 1 TO LBXM-EXCEPT-CNT
           END-IF
           IF LBXM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO LBXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E458 MEMO REQUIRED
      * E553 WORK AMT 2 NEGATIVE
      * E376 WORK AMT 2 OVER 50000
      * E812 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3880-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-MEMO = SPACES
                   MOVE 'E458' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E553' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E376' TO WS-REJECT-CODE
               WHEN LBXM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E812' TO WS-REJECT-CODE
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
       3890-DESCRIBE-REASON.
           EVALUATE LBXM-REASON-CD
               WHEN 'GF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3900-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = LBXM-EXCEPT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = LBXM-EXCEPT-YTD - (WS-WORK-AMT * 2)
           COMPUTE LBXM-DEPOSIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3910-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = LBXM-DEPOSIT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = LBXM-DEPOSIT-YTD - (WS-WORK-AMT * 2)
           COMPUTE LBXM-EXCEPT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3920-FLAG-FEE.
           IF LBXM-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBXM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3930-FLAG-FEE.
           IF LBXM-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBXM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOCKBOX CD:
      * DX -> JM WHEN EXPIRY DATE IS PAST
      * JM -> WW WHEN EXPIRY DATE IS PAST
      * WW -> XE WHEN EXPIRY DATE IS PAST
      * XE -> DX WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3940-SET-LOCKBOX.
           EVALUATE LBXM-LOCKBOX-CD
               WHEN 'DX'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WW' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WW'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XE' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XE'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3950-COUNT-EXCEPT.
           IF LBXM-EXCEPT-CNT < 9999999
               ADD 1 TO LBXM-EXCEPT-CNT
           END-IF
           IF LBXM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO LBXT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3960-AGE-LAST-STMT.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO LBXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO LBXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E106 CONTACT REQUIRED
      * E494 CHECK AMT NEGATIVE
      * E030 CHECK AMT OVER 1000000
      * E667 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3980-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-CONTACT = SPACES
                   MOVE 'E106' TO WS-REJECT-CODE
               WHEN LBXM-CHECK-AMT < 0
                   MOVE 'E494' TO WS-REJECT-CODE
               WHEN LBXM-CHECK-AMT > 1000000.00
                   MOVE 'E030' TO WS-REJECT-CODE
               WHEN LBXM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E667' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3990-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-DEPOSIT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4000-COUNT-RETRY.
           IF LBXM-RETRY-CNT < 9999999
               ADD 1 TO LBXM-RETRY-CNT
           END-IF
           IF LBXM-RETRY-CNT NOT < 12
               MOVE 'Y' TO LBXT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E674 MEMO REQUIRED
      * E425 WORK AMT 2 NEGATIVE
      * E190 WORK AMT 2 OVER 99999
      * E669 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-MEMO = SPACES
                   MOVE 'E674' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E425' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E190' TO WS-REJECT-CODE
               WHEN LBXM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E669' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CHECK FEE ON THE FLOAT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4020-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN LBXM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO LBXM-CHECK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE LBXM-CHECK-AMT ROUNDED = LBXT-FLOAT-AMT *
                       LBXM-DISCOUNT / 200
               WHEN LBXT-TRAN-CD = 'RC'
                   MOVE 7.00 TO LBXM-CHECK-AMT
               WHEN LBXT-TRAN-CD = 'DP'
                   MOVE 12.00 TO LBXM-CHECK-AMT
               WHEN OTHER
                   COMPUTE LBXM-CHECK-AMT ROUNDED = LBXT-FLOAT-AMT *
                       LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXM-CHECK-AMT > 0 AND LBXM-CHECK-AMT < 5.00
               MOVE 5.00 TO LBXM-CHECK-AMT
           END-IF
           IF LBXM-CHECK-AMT > 25.00
               MOVE 25.00 TO LBXM-CHECK-AMT
           END-IF
           ADD LBXM-CHECK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4030-AGE-NOTICE.
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
                   ADD LBXM-EXCEPT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD LBXM-EXCEPT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-EXCEPT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBXM-EXCEPT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-EXCEPT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FLOAT YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4040-ACCRUE-FLOAT.
           IF LBXM-FLOAT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-FLOAT-YTD * LBXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-CHECK-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4050-FLAG-DEPOSIT.
           IF LBXM-DEPOSIT-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBXM-DEPOSIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FLOAT YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4060-ACCRUE-FLOAT.
           IF LBXM-FLOAT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-FLOAT-YTD * LBXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO LBXT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOCKBOX CD:
      * DX -> JM WHEN OPEN DATE IS PAST
      * JM -> WW WHEN OPEN DATE IS PAST
      * WW -> XE WHEN OPEN DATE IS PAST
      * XE -> DX WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4070-SET-LOCKBOX.
           EVALUATE LBXM-LOCKBOX-CD
               WHEN 'DX'
                   IF LBXM-OPEN-DATE > 0 AND LBXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF LBXM-OPEN-DATE > 0 AND LBXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WW' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WW'
                   IF LBXM-OPEN-DATE > 0 AND LBXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XE' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XE'
                   IF LBXM-OPEN-DATE > 0 AND LBXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4080-CHECK-MATURITY.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO LBXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FLOAT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEPOSIT AMT.
      *----------------------------------------------------------------*
       4090-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-DEPOSIT-AMT ROUNDED =
                   LBXT-FLOAT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-DEPOSIT-AMT
           END-IF
           ADD LBXM-DEPOSIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E571 MEMO REQUIRED
      * E258 FEE AMT NEGATIVE
      * E254 FEE AMT OVER 10000
      * E369 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4100-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-MEMO = SPACES
                   MOVE 'E571' TO WS-REJECT-CODE
               WHEN LBXM-FEE-AMT < 0
                   MOVE 'E258' TO WS-REJECT-CODE
               WHEN LBXM-FEE-AMT > 10000.00
                   MOVE 'E254' TO WS-REJECT-CODE
               WHEN LBXM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E369' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FLOAT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4110-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REASON-CD
                   ADD LBXT-FLOAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXT-FLOAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4120-LIMIT-DEPOSIT.
           IF LBXM-DEPOSIT-YTD < 10.00
               MOVE 10.00 TO LBXM-DEPOSIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-DEPOSIT-YTD > 1000.00
               MOVE 1000.00 TO LBXM-DEPOSIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4130-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E990 CUST ID REQUIRED
      * E253 CHECK AMT NEGATIVE
      * E514 CHECK AMT OVER 25000
      * E112 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4140-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-CUST-ID = SPACES
                   MOVE 'E990' TO WS-REJECT-CODE
               WHEN LBXM-CHECK-AMT < 0
                   MOVE 'E253' TO WS-REJECT-CODE
               WHEN LBXM-CHECK-AMT > 25000.00
                   MOVE 'E514' TO WS-REJECT-CODE
               WHEN LBXM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E112' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BATCH TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4150-DESCRIBE-BATCH-TYPE.
           EVALUATE LBXM-BATCH-TYPE-CD
               WHEN 'C'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'D'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'K'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'U'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CHECK YTD FEE ON THE FEE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4160-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN LBXM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO LBXM-CHECK-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE LBXM-CHECK-YTD ROUNDED = LBXM-FEE-YTD *
                       LBXM-DISCOUNT / 200
               WHEN LBXM-REGION-CD = 'DVP'
                   MOVE 5.00 TO LBXM-CHECK-YTD
               WHEN LBXM-REGION-CD = 'FMZ'
                   MOVE 7.00 TO LBXM-CHECK-YTD
               WHEN OTHER
                   COMPUTE LBXM-CHECK-YTD ROUNDED = LBXM-FEE-YTD *
                       LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXM-CHECK-YTD > 0 AND LBXM-CHECK-YTD < 10.00
               MOVE 10.00 TO LBXM-CHECK-YTD
           END-IF
           IF LBXM-CHECK-YTD > 50.00
               MOVE 50.00 TO LBXM-CHECK-YTD
           END-IF
           ADD LBXM-CHECK-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E442 ADDR REQUIRED
      * E109 EXCEPT AMT NEGATIVE
      * E011 EXCEPT AMT OVER 5000
      * E610 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4170-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-ADDR = SPACES
                   MOVE 'E442' TO WS-REJECT-CODE
               WHEN LBXM-EXCEPT-AMT < 0
                   MOVE 'E109' TO WS-REJECT-CODE
               WHEN LBXM-EXCEPT-AMT > 5000.00
                   MOVE 'E011' TO WS-REJECT-CODE
               WHEN LBXM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E610' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4180-CHECK-POST.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO LBXM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO EXCEPT AMT.
      *----------------------------------------------------------------*
       4190-PRORATE-EXCEPT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-EXCEPT-AMT ROUNDED =
                   LBXM-DEPOSIT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-EXCEPT-AMT
           END-IF
           ADD LBXM-EXCEPT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEPOSIT YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4200-ACCRUE-DEPOSIT.
           IF LBXM-DEPOSIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-DEPOSIT-YTD * LBXM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO LBXM-REMIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * B -> H WHEN OPEN DATE IS PAST
      * H -> Z WHEN OPEN DATE IS PAST
      * Z -> B WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4210-SET-CHANNEL.
           EVALUATE LBXM-CHANNEL-CD
               WHEN 'B'
                   IF LBXM-OPEN-DATE > 0 AND LBXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF LBXM-OPEN-DATE > 0 AND LBXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF LBXM-OPEN-DATE > 0 AND LBXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT SCHED TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4220-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-SCHED-RATE(1) TO LBXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF LBXM-DEPOSIT-AMT NOT < WS-FLOAT-SCHED-LOW(WS-TX)
                   MOVE WS-FLOAT-SCHED-RATE(WS-TX) TO LBXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BATCH TYPE CD:
      * C -> D WHEN LAST PAY DATE IS PAST
      * D -> F WHEN LAST PAY DATE IS PAST
      * F -> K WHEN LAST PAY DATE IS PAST
      * K -> U WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4230-SET-BATCH-TYPE.
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
      * SPLIT CASH ON HAND YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4240-SPLIT-CASH-ON-HAND.
           COMPUTE WS-WORK-AMT = LBXM-CASH-ON-HAND-YTD / 12
           COMPUTE WS-WORK-AMT-2 = LBXM-CASH-ON-HAND-YTD - (WS-WORK-AMT
               * 12)
           COMPUTE LBXM-EXCEPT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4250-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 12
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE CASH ON HAND YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4260-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN LBXM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = LBXM-CASH-ON-HAND-YTD
                       * LBXM-DISCOUNT / 200
               WHEN LBXM-BATCH-TYPE-CD = 'C'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN LBXM-BATCH-TYPE-CD = 'D'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = LBXM-CASH-ON-HAND-YTD
                       * LBXM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BATCH TYPE CD:
      * C -> D WHEN CLOSE DATE IS PAST
      * D -> F WHEN CLOSE DATE IS PAST
      * F -> K WHEN CLOSE DATE IS PAST
      * K -> U WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4270-SET-BATCH-TYPE.
           EVALUATE LBXM-BATCH-TYPE-CD
               WHEN 'C'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FLOAT AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4280-LIMIT-FLOAT.
           IF LBXT-FLOAT-AMT < 100.00
               MOVE 100.00 TO LBXT-FLOAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-FLOAT-AMT > 50000.00
               MOVE 50000.00 TO LBXT-FLOAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH ON HAND AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4290-LIMIT-CASH-ON-HAND.
           IF LBXT-CASH-ON-HAND-AMT < 0.00
               MOVE 0.00 TO LBXT-CASH-ON-HAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-CASH-ON-HAND-AMT > 500.00
               MOVE 500.00 TO LBXT-CASH-ON-HAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE CHECK YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4300-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO LBXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF LBXM-CHECK-YTD NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO LBXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E662 CUST ID REQUIRED
      * E636 WORK AMT NEGATIVE
      * E165 WORK AMT OVER 25000
      * E395 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4310-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-CUST-ID = SPACES
                   MOVE 'E662' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E636' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E165' TO WS-REJECT-CODE
               WHEN LBXM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E395' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE REMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4320-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF LBXM-REMIT-YTD NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO LBXM-DISCOUNT
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
       4330-DESCRIBE-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4340-CHECK-EXPIRY.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH ON HAND YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4350-FLAG-CASH-ON-HAND.
           IF LBXM-CASH-ON-HAND-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBXM-CASH-ON-HAND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FLOAT YTD FEE ON THE REMIT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4360-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN LBXM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO LBXM-FLOAT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   COMPUTE LBXM-FLOAT-YTD ROUNDED = LBXM-REMIT-AMT *
                       LBXM-PENALTY-PCT / 200
               WHEN LBXM-CYCLE-CD = 'EDV'
                   MOVE 3.00 TO LBXM-FLOAT-YTD
               WHEN LBXM-CYCLE-CD = 'EPM'
                   MOVE 15.00 TO LBXM-FLOAT-YTD
               WHEN OTHER
                   COMPUTE LBXM-FLOAT-YTD ROUNDED = LBXM-REMIT-AMT *
                       LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXM-FLOAT-YTD > 0 AND LBXM-FLOAT-YTD < 1.00
               MOVE 1.00 TO LBXM-FLOAT-YTD
           END-IF
           IF LBXM-FLOAT-YTD > 50.00
               MOVE 50.00 TO LBXM-FLOAT-YTD
           END-IF
           ADD LBXM-FLOAT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4370-AGE-MATURITY.
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
                   ADD LBXM-DEPOSIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD LBXM-DEPOSIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD LBXM-DEPOSIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD LBXM-DEPOSIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-DEPOSIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DEPOSIT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4380-FLAG-DEPOSIT.
           IF LBXM-DEPOSIT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBXM-DEPOSIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4390-DESCRIBE-CYCLE.
           EVALUATE LBXM-CYCLE-CD
               WHEN 'EDV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'EPM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FZY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XUU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YBH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4400-CHECK-EFF.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4410-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4420-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 5
               MOVE 'Y' TO LBXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CHECK YTD IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4430-LIMIT-CHECK.
           IF LBXM-CHECK-YTD < 10.00
               MOVE 10.00 TO LBXM-CHECK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-CHECK-YTD > 500.00
               MOVE 500.00 TO LBXM-CHECK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LOCKBOX CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4440-DESCRIBE-LOCKBOX.
           EVALUATE LBXM-LOCKBOX-CD
               WHEN 'DX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT FEE ON THE CHECK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4450-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN LBXM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO LBXM-EXCEPT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE LBXM-EXCEPT-AMT ROUNDED = LBXM-CHECK-AMT *
                       LBXM-DISCOUNT / 200
               WHEN LBXM-CYCLE-CD = 'EDV'
                   MOVE 5.00 TO LBXM-EXCEPT-AMT
               WHEN LBXM-CYCLE-CD = 'EPM'
                   MOVE 3.00 TO LBXM-EXCEPT-AMT
               WHEN OTHER
                   COMPUTE LBXM-EXCEPT-AMT ROUNDED = LBXM-CHECK-AMT *
                       LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXM-EXCEPT-AMT > 0 AND LBXM-EXCEPT-AMT < 1.00
               MOVE 1.00 TO LBXM-EXCEPT-AMT
           END-IF
           IF LBXM-EXCEPT-AMT > 150.00
               MOVE 150.00 TO LBXM-EXCEPT-AMT
           END-IF
           ADD LBXM-EXCEPT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4460-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF LBXM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4470-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 12
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4480-AGE-LAST-STMT.
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
                   ADD LBXM-CHECK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBXM-CHECK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD LBXM-CHECK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD LBXM-CHECK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-CHECK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FLOAT AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4490-LIMIT-FLOAT.
           IF LBXT-FLOAT-AMT < 50.00
               MOVE 50.00 TO LBXT-FLOAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-FLOAT-AMT > 1000.00
               MOVE 1000.00 TO LBXT-FLOAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4500-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4510-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO LBXM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHECK AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4520-LIMIT-CHECK.
           IF LBXT-CHECK-AMT < 0.00
               MOVE 0.00 TO LBXT-CHECK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-CHECK-AMT > 500.00
               MOVE 500.00 TO LBXT-CHECK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT SCHED TIER FOR THE EXCEPT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4530-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-SCHED-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF LBXM-EXCEPT-AMT NOT < WS-FLOAT-SCHED-LOW(WS-TX)
                   MOVE WS-FLOAT-SCHED-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4540-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4550-LIMIT-FEE.
           IF LBXM-FEE-YTD < 100.00
               MOVE 100.00 TO LBXM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-FEE-YTD > 5000.00
               MOVE 5000.00 TO LBXM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT SCHED TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4560-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-SCHED-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF LBXM-FEE-YTD NOT < WS-FLOAT-SCHED-LOW(WS-TX)
                   MOVE WS-FLOAT-SCHED-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4570-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 12
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EDV -> EPM WHEN EXPIRY DATE IS PAST
      * EPM -> FZY WHEN EXPIRY DATE IS PAST
      * FZY -> XUU WHEN EXPIRY DATE IS PAST
      * XUU -> YBH WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4580-SET-CYCLE.
           EVALUATE LBXM-CYCLE-CD
               WHEN 'EDV'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EPM' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EPM'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FZY' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FZY'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XUU' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XUU'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YBH' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4590-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 6
               MOVE 'Y' TO LBXM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4600-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * LBXM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO LBXM-CASH-ON-HAND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4610-CHECK-LAST-PAY.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4620-AGE-MATURITY.
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
                   ADD LBXM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD LBXM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD LBXM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBXM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EDV -> EPM WHEN LAST PAY DATE IS PAST
      * EPM -> FZY WHEN LAST PAY DATE IS PAST
      * FZY -> XUU WHEN LAST PAY DATE IS PAST
      * XUU -> YBH WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4630-SET-CYCLE.
           EVALUATE LBXM-CYCLE-CD
               WHEN 'EDV'
                   IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EPM' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EPM'
                   IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FZY' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FZY'
                   IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XUU' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XUU'
                   IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YBH' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE CASH ON HAND AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4640-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN LBXM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO LBXM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   COMPUTE LBXM-FEE-AMT ROUNDED = LBXM-CASH-ON-HAND-AMT
                       * LBXM-FEE-PCT / 200
               WHEN LBXM-CHANNEL-CD = 'B'
                   MOVE 12.00 TO LBXM-FEE-AMT
               WHEN LBXM-CHANNEL-CD = 'H'
                   MOVE 15.00 TO LBXM-FEE-AMT
               WHEN OTHER
                   COMPUTE LBXM-FEE-AMT ROUNDED = LBXM-CASH-ON-HAND-AMT
                       * LBXM-FEE-PCT / 100
           END-EVALUATE
           IF LBXM-FEE-AMT > 0 AND LBXM-FEE-AMT < 5.00
               MOVE 5.00 TO LBXM-FEE-AMT
           END-IF
           IF LBXM-FEE-AMT > 35.00
               MOVE 35.00 TO LBXM-FEE-AMT
           END-IF
           ADD LBXM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE FLOAT FEE ON THE DEPOSIT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4650-ASSESS-STOP.
           EVALUATE TRUE
               WHEN LBXM-STOP-FLAG = 'Y'
                   MOVE ZERO TO LBXT-FLOAT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXT-REVERSAL-FLAG = 'Y'
                   COMPUTE LBXT-FLOAT-AMT ROUNDED = LBXM-DEPOSIT-AMT *
                       LBXM-FEE-PCT / 200
               WHEN LBXM-BATCH-TYPE-CD = 'C'
                   MOVE 15.00 TO LBXT-FLOAT-AMT
               WHEN LBXM-BATCH-TYPE-CD = 'D'
                   MOVE 15.00 TO LBXT-FLOAT-AMT
               WHEN OTHER
                   COMPUTE LBXT-FLOAT-AMT ROUNDED = LBXM-DEPOSIT-AMT *
                       LBXM-FEE-PCT / 100
           END-EVALUATE
           IF LBXT-FLOAT-AMT > 0 AND LBXT-FLOAT-AMT < 5.00
               MOVE 5.00 TO LBXT-FLOAT-AMT
           END-IF
           IF LBXT-FLOAT-AMT > 50.00
               MOVE 50.00 TO LBXT-FLOAT-AMT
           END-IF
           ADD LBXT-FLOAT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4660-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 12
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4670-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXT-TRAN-CD
                   ADD LBXM-REMIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-REMIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT SCHED TIER FOR THE REMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4680-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-SCHED-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF LBXM-REMIT-AMT NOT < WS-FLOAT-SCHED-LOW(WS-TX)
                   MOVE WS-FLOAT-SCHED-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
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
                   MOVE LBWAIV-FLOAT-AMT
                     TO LBXM-FLOAT-AMT
                   MOVE LBWAIV-CHECK-YTD
                     TO LBXM-CHECK-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RAIV' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-LBWAIV' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE LBX FIELDS TO LBFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-LBFEE.
           INITIALIZE LBFEE-PARMS
           MOVE LBXM-EXCEPT-YTD
             TO LBFEE-EXCEPT-YTD
           MOVE LBXM-DEPOSIT-YTD
             TO LBFEE-DEPOSIT-YTD
           MOVE LBXM-REMIT-YTD
             TO LBFEE-REMIT-YTD
           MOVE LBXM-EXCEPT-AMT
             TO LBFEE-EXCEPT-AMT
           MOVE LBXM-FLOAT-YTD
             TO LBFEE-FLOAT-YTD
           MOVE LBXM-FLOAT-AMT
             TO LBFEE-FLOAT-AMT
           MOVE LBXM-CLOSE-DATE
             TO LBFEE-CLOSE-DATE
           MOVE LBXM-NEXT-DUE-DATE
             TO LBFEE-NEXT-DUE-DATE
           MOVE LBXM-LAST-PAY-DATE
             TO LBFEE-LAST-PAY-DATE
           MOVE LBXM-OPEN-DATE
             TO LBFEE-OPEN-DATE
           MOVE LBXM-LOCKBOX-CD
             TO LBFEE-LOCKBOX-CD
           MOVE LBXM-REASON-CD
             TO LBFEE-REASON-CD
           MOVE LBXM-BATCH-TYPE-CD
             TO LBFEE-BATCH-TYPE-CD
           MOVE LBXM-CHANNEL-CD
             TO LBFEE-CHANNEL-CD
           MOVE LBXM-STOP-FLAG
             TO LBFEE-STOP-FLAG
           MOVE LBXM-EXEMPT-FLAG
             TO LBFEE-EXEMPT-FLAG
           MOVE LBXM-NOTICE-SENT-FLAG
             TO LBFEE-NOTICE-SENT-FLAG
           MOVE LBXM-PAYMENT-CNT
             TO LBFEE-PAYMENT-CNT
           MOVE LBXM-ITEM-CNT
             TO LBFEE-ITEM-CNT
           MOVE LBXM-DISCOUNT
             TO LBFEE-DISCOUNT
           MOVE LBXM-PENALTY-PCT
             TO LBFEE-PENALTY-PCT
           MOVE LBXM-NAME
             TO LBFEE-NAME
           MOVE LBXM-ADDR
             TO LBFEE-ADDR
           CALL 'LBFEE' USING LBFEE-PARMS
           EVALUATE LBFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE LBFEE-EXCEPT-YTD
                     TO LBXM-EXCEPT-YTD
                   MOVE LBFEE-DEPOSIT-YTD
                     TO LBXM-DEPOSIT-YTD
                   MOVE LBFEE-FLOAT-YTD
                     TO LBXM-FLOAT-YTD
                   MOVE LBFEE-EXEMPT-FLAG
                     TO LBXM-EXEMPT-FLAG
                   MOVE LBFEE-NOTICE-SENT-FLAG
                     TO LBXM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-LBFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE LBX FIELDS TO LBCAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-LBCAL.
           INITIALIZE LBCAL-PARMS
           MOVE LBXM-DEPOSIT-AMT
             TO LBCAL-DEPOSIT-AMT
           MOVE LBXM-DEPOSIT-YTD
             TO LBCAL-DEPOSIT-YTD
           MOVE LBXM-CHECK-AMT
             TO LBCAL-CHECK-AMT
           MOVE LBXM-CASH-ON-HAND-YTD
             TO LBCAL-CASH-ON-HAND-YTD
           MOVE LBXM-FLOAT-YTD
             TO LBCAL-FLOAT-YTD
           MOVE LBXM-REMIT-YTD
             TO LBCAL-REMIT-YTD
           MOVE LBXM-NOTICE-DATE
             TO LBCAL-NOTICE-DATE
           MOVE LBXM-NEXT-DUE-DATE
             TO LBCAL-NEXT-DUE-DATE
           MOVE LBXM-OPEN-DATE
             TO LBCAL-OPEN-DATE
           MOVE LBXM-EFFECTIVE-DATE
             TO LBCAL-EFFECTIVE-DATE
           MOVE LBXM-REGION-CD
             TO LBCAL-REGION-CD
           MOVE LBXM-BATCH-TYPE-CD
             TO LBCAL-BATCH-TYPE-CD
           MOVE LBXM-REASON-CD
             TO LBCAL-REASON-CD
           MOVE LBXM-LOCKBOX-CD
             TO LBCAL-LOCKBOX-CD
           MOVE LBXM-HOLD-FLAG
             TO LBCAL-HOLD-FLAG
           MOVE LBXM-PREFERRED-FLAG
             TO LBCAL-PREFERRED-FLAG
           MOVE LBXM-NOTICE-SENT-FLAG
             TO LBCAL-NOTICE-SENT-FLAG
           MOVE LBXM-EXCEPT-CNT
             TO LBCAL-EXCEPT-CNT
           MOVE LBXM-RETRY-CNT
             TO LBCAL-RETRY-CNT
           MOVE LBXM-PENALTY-PCT
             TO LBCAL-PENALTY-PCT
           MOVE LBXM-DISCOUNT
             TO LBCAL-DISCOUNT
           MOVE LBXM-DESC
             TO LBCAL-DESC
           MOVE LBXM-CONTACT
             TO LBCAL-CONTACT
           CALL 'LBCAL' USING LBCAL-PARMS
           EVALUATE LBCAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE LBCAL-DEPOSIT-YTD
                     TO LBXM-DEPOSIT-YTD
                   MOVE LBCAL-FLOAT-YTD
                     TO LBXM-FLOAT-YTD
                   MOVE LBCAL-HOLD-FLAG
                     TO LBXM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-LBCAL' TO ABND-PARAGRAPH
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
           DISPLAY 'LBXRPT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'LBXRPT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
