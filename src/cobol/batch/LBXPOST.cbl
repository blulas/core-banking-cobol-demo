      *================================================================*
      * PROGRAM  : LBXPOST                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : LOCKBOX REMITTANCE - DAILY POSTING. READS THE LBX   *
      *            TRANSACTIONS, UPDATES THE LBX MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. WHOLESALE LOCKBOX REMITTANCE    *
      *            CAPTURE AND CREDIT TO CLIENTS                       *
      * INPUT    : LBXMST (KSDS), LBXTRN, BANKCTL                      *
      * OUTPUT   : LBXOUT, LBXPORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, LBFEE, LBCAL, LBWAIV    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-07-07 MKL  ORIGINAL                                       *
      * 2001-11-28 RJT  REMIT CALCULATION CORRECTED CHG5386            *
      * 2002-04-06 GAF  DEPOSIT CALCULATION CORRECTED INC5616          *
      * 2005-11-02 TWH  FLOAT RERUN PROTECTION REQ6250                 *
      * 2010-11-05 MKL  EXCEPT EXCEPTIONS TO OPS QUEUE REQ2692         *
      * 2011-01-13 RJT  EXCEPT STATUS CODES ADDED INC9218              *
      * 2011-03-24 RJT  EXCEPT ROUNDING - HALF UP CHG5980              *
      * 2013-01-11 EWS  FEE RERUN PROTECTION REQ6640                   *
      * 2014-10-14 LMB  CASH-ON-HAND LIMIT REVISED PRB8824             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBXPOST.
       AUTHOR. D P SHAW.

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
           SELECT REPORT-FILE ASSIGN TO "LBXPORPT"
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
                                           VALUE 13.000000.
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
      * WS-REMIT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REMIT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000647000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000010000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000269000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005750000000770500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015750000000072000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018250000000623500'.
       01  WS-REMIT-BAND-TBL REDEFINES WS-REMIT-BAND-VALUES.
           05  WS-REMIT-BAND-ROW OCCURS 6 TIMES.
               10  WS-REMIT-BAND-LOW       PIC 9(09)V99.
               10  WS-REMIT-BAND-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FLOAT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FLOAT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000154000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000238000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000350000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000757500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012800000000265000'.
       01  WS-FLOAT-GRID-TBL REDEFINES WS-FLOAT-GRID-VALUES.
           05  WS-FLOAT-GRID-ROW OCCURS 5 TIMES.
               10  WS-FLOAT-GRID-LOW       PIC 9(09)V99.
               10  WS-FLOAT-GRID-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  LBXPOST  LOCKBOX REMITTANCE'.
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
       COPY LBFEEP.
       COPY LBCALP.
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
               PERFORM 3000-ACCUM-TRAN
               PERFORM 3010-ACCUM-CYCLE
               PERFORM 3020-COUNT-ITEM
               PERFORM 3030-DESCRIBE-TRAN
               PERFORM 3040-SELECT-DISCOUNT
               PERFORM 3050-EDIT-CUST-ID
               PERFORM 3060-ACCRUE-CHECK
               PERFORM 3070-AGE-MATURITY
               PERFORM 3080-ASSESS-PREFERRED
               PERFORM 3090-DESCRIBE-LOCKBOX
               PERFORM 3100-LIMIT-ITEM
               PERFORM 3110-ACCUM-CHANNEL
               PERFORM 3120-AGE-LAST-PAY
               PERFORM 3130-PRORATE-CHECK
               PERFORM 3140-CHECK-NEXT-DUE
               PERFORM 3150-ASSESS-PREFERRED
               PERFORM 3160-SPLIT-FLOAT
               PERFORM 3170-CHECK-EFFECTIVE
               PERFORM 3180-FLAG-REMIT
               PERFORM 3190-EDIT-REF-NO
               PERFORM 3200-CHECK-NEXT-DUE
               PERFORM 3210-SELECT-DISCOUNT
               PERFORM 3220-CHECK-MATURITY
               PERFORM 3230-ASSESS-PREFERRED
               PERFORM 3240-DESCRIBE-BATCH-TYPE
               PERFORM 3250-LIMIT-DEPOSIT
               PERFORM 3260-DESCRIBE-REASON
               PERFORM 3270-SET-TRAN
               PERFORM 3280-EDIT-REF-NO
               PERFORM 3290-EDIT-CONTACT
               PERFORM 3300-COUNT-EXCEPT
               PERFORM 3310-DESCRIBE-LOCKBOX
               PERFORM 3320-FLAG-CHECK
               PERFORM 3330-EDIT-MEMO
               PERFORM 3340-LIMIT-CASH-ON-HAND
               PERFORM 3350-ACCUM-REGION
               PERFORM 3360-CHECK-EFFECTIVE
               PERFORM 3370-SPLIT-FLOAT
               PERFORM 3380-PRORATE-CASH-ON-HAND
               PERFORM 3390-ACCRUE-FLOAT
               PERFORM 3400-LIMIT-EXCEPT
               PERFORM 3410-COUNT-SEQ
               PERFORM 3420-CHECK-CLOSE
               PERFORM 3430-ACCUM-REASON
               PERFORM 3440-ACCUM-CYCLE
               PERFORM 3450-SELECT-DISCOUNT
               PERFORM 3460-ACCUM-TRAN
               PERFORM 3470-LIMIT-CASH-ON-HAND
               PERFORM 3480-CHECK-EFF
               PERFORM 3490-ACCUM-BATCH-TYPE
               PERFORM 3500-PRORATE-CASH-ON-HAND
               PERFORM 3510-SET-TRAN
               PERFORM 3520-ACCRUE-CHECK
               PERFORM 3530-ASSESS-REVERSAL
               PERFORM 3540-SPLIT-FEE
               PERFORM 3550-SET-LOCKBOX
               PERFORM 3560-CHECK-NEXT-DUE
               PERFORM 3570-COUNT-RETRY
               PERFORM 3580-EDIT-CONTACT
               PERFORM 3590-SPLIT-CASH-ON-HAND
               PERFORM 3600-COUNT-EXCEPT
               PERFORM 3610-DESCRIBE-REASON
               PERFORM 3620-FLAG-CHECK
               PERFORM 3630-CHECK-EXPIRY
               PERFORM 3640-SELECT-DISCOUNT
               PERFORM 3650-SPLIT-EXCEPT
               PERFORM 3660-EDIT-MEMO
               PERFORM 3670-SELECT-DISCOUNT
               PERFORM 3680-AGE-OPEN
               PERFORM 3690-COUNT-EXCEPT
               PERFORM 3700-COUNT-SEQ
               PERFORM 3710-CHECK-LAST-STMT
               PERFORM 3720-CHECK-NOTICE
               PERFORM 3730-ACCUM-LOCKBOX
               PERFORM 3740-LIMIT-REMIT
               PERFORM 3750-ASSESS-AUTO-RENEW
               PERFORM 3760-LIMIT-CASH-ON-HAND
               PERFORM 3770-COUNT-ITEM
               PERFORM 3780-LIMIT-ITEM
               PERFORM 3790-SPLIT-REMIT
               PERFORM 3800-FLAG-DEPOSIT
               PERFORM 3810-SELECT-DISCOUNT
               PERFORM 3820-PRORATE-CHECK
               PERFORM 3830-ASSESS-REVERSAL
               PERFORM 3840-ACCRUE-EXCEPT
               PERFORM 3850-SELECT-FEE-PCT
               PERFORM 3860-AGE-LAST-PAY
               PERFORM 3870-DESCRIBE-TRAN
               PERFORM 3880-LIMIT-FEE
               PERFORM 3890-EDIT-ADDR
               PERFORM 3900-FLAG-FEE
               PERFORM 3910-SELECT-PENALTY-PCT
               PERFORM 3920-CHECK-EFFECTIVE
               PERFORM 3930-SELECT-DISCOUNT
               PERFORM 3940-AGE-NEXT-DUE
               PERFORM 3950-LIMIT-EXCEPT
               PERFORM 3960-LIMIT-CASH-ON-HAND
               PERFORM 3970-SET-CYCLE
               PERFORM 3980-DESCRIBE-TRAN
               PERFORM 3990-COUNT-RETRY
               PERFORM 4000-EDIT-OPERATOR
               PERFORM 4010-ACCRUE-CHECK
               PERFORM 4020-ACCUM-CHANNEL
               PERFORM 4030-COUNT-EXCEPT
               PERFORM 4040-LIMIT-FLOAT
               PERFORM 4050-ACCRUE-FEE-PCT
               PERFORM 4060-LIMIT-EXCEPT
               PERFORM 4070-SELECT-PENALTY-PCT
               PERFORM 4080-CHECK-NEXT-DUE
               PERFORM 4090-COUNT-ITEM
               PERFORM 4100-CHECK-EXPIRY
               PERFORM 4110-ASSESS-AUTO-RENEW
               PERFORM 4120-EDIT-DESC
               PERFORM 4130-SET-REASON
               PERFORM 4140-FLAG-FEE
               PERFORM 4150-SPLIT-DEPOSIT
               PERFORM 4160-LIMIT-FEE
               PERFORM 4170-PRORATE-CHECK
               PERFORM 7020-CALL-LBFEE
               PERFORM 7010-CALL-LBCAL
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
      * ACCUMULATE CASH ON HAND AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3000-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXT-TRAN-CD
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FLOAT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3010-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-CYCLE-CD
                   ADD LBXT-FLOAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXT-FLOAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 3
               MOVE 'Y' TO LBXM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3030-DESCRIBE-TRAN.
           EVALUATE LBXT-TRAN-CD
               WHEN 'RC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'EX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'AD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT GRID TIER FOR THE REMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3040-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-GRID-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF LBXM-REMIT-YTD NOT < WS-FLOAT-GRID-LOW(WS-TX)
                   MOVE WS-FLOAT-GRID-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E378 CUST ID REQUIRED
      * E690 CASH ON HAND AMT NEGATIVE
      * E853 CASH ON HAND AMT OVER 25000
      * E291 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3050-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-CUST-ID = SPACES
                   MOVE 'E378' TO WS-REJECT-CODE
               WHEN LBXM-CASH-ON-HAND-AMT < 0
                   MOVE 'E690' TO WS-REJECT-CODE
               WHEN LBXM-CASH-ON-HAND-AMT > 25000.00
                   MOVE 'E853' TO WS-REJECT-CODE
               WHEN LBXM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E291' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHECK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3060-ACCRUE-CHECK.
           IF LBXM-CHECK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-CHECK-AMT * LBXM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO LBXM-FLOAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3070-AGE-MATURITY.
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
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FLOAT FEE ON THE FLOAT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3080-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO LBXM-FLOAT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE LBXM-FLOAT-AMT ROUNDED = LBXT-FLOAT-AMT *
                       LBXM-PENALTY-PCT / 200
               WHEN LBXM-CHANNEL-CD = 'B'
                   MOVE 15.00 TO LBXM-FLOAT-AMT
               WHEN LBXM-CHANNEL-CD = 'H'
                   MOVE 20.00 TO LBXM-FLOAT-AMT
               WHEN OTHER
                   COMPUTE LBXM-FLOAT-AMT ROUNDED = LBXT-FLOAT-AMT *
                       LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXM-FLOAT-AMT > 0 AND LBXM-FLOAT-AMT < 10.00
               MOVE 10.00 TO LBXM-FLOAT-AMT
           END-IF
           IF LBXM-FLOAT-AMT > 250.00
               MOVE 250.00 TO LBXM-FLOAT-AMT
           END-IF
           ADD LBXM-FLOAT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE LOCKBOX CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3090-DESCRIBE-LOCKBOX.
           EVALUATE LBXM-LOCKBOX-CD
               WHEN 'DX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3100-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3110-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXT-CHANNEL-CD
                   ADD LBXM-DEPOSIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-DEPOSIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3120-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-LAST-PAY-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD LBXM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CHECK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CHECK YTD.
      *----------------------------------------------------------------*
       3130-PRORATE-CHECK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-CHECK-YTD ROUNDED =
                   LBXT-CHECK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-CHECK-YTD
           END-IF
           ADD LBXM-CHECK-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3140-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE DEPOSIT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3150-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO LBXM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXT-REVERSAL-FLAG = 'Y'
                   COMPUTE LBXM-FEE-AMT ROUNDED = LBXM-DEPOSIT-AMT *
                       LBXM-DISCOUNT / 200
               WHEN LBXT-TRAN-CD = 'RC'
                   MOVE 12.00 TO LBXM-FEE-AMT
               WHEN LBXT-TRAN-CD = 'DP'
                   MOVE 15.00 TO LBXM-FEE-AMT
               WHEN OTHER
                   COMPUTE LBXM-FEE-AMT ROUNDED = LBXM-DEPOSIT-AMT *
                       LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXM-FEE-AMT > 0 AND LBXM-FEE-AMT < 2.00
               MOVE 2.00 TO LBXM-FEE-AMT
           END-IF
           IF LBXM-FEE-AMT > 150.00
               MOVE 150.00 TO LBXM-FEE-AMT
           END-IF
           ADD LBXM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3160-SPLIT-FLOAT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE LBXM-FLOAT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3170-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REMIT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3180-FLAG-REMIT.
           IF LBXM-REMIT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBXM-REMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E149 REF NO REQUIRED
      * E019 FEE AMT NEGATIVE
      * E111 FEE AMT OVER 1000000
      * E669 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3190-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-REF-NO = SPACES
                   MOVE 'E149' TO WS-REJECT-CODE
               WHEN LBXT-FEE-AMT < 0
                   MOVE 'E019' TO WS-REJECT-CODE
               WHEN LBXT-FEE-AMT > 1000000.00
                   MOVE 'E111' TO WS-REJECT-CODE
               WHEN LBXM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E669' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3200-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO LBXM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REMIT BAND TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3210-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-BAND-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF LBXM-FEE-YTD NOT < WS-REMIT-BAND-LOW(WS-TX)
                   MOVE WS-REMIT-BAND-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3220-CHECK-MATURITY.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO LBXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3230-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       LBXM-FEE-PCT / 200
               WHEN LBXM-REASON-CD = 'GF'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN LBXM-REASON-CD = 'UK'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       LBXM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE BATCH TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3240-DESCRIBE-BATCH-TYPE.
           EVALUATE LBXM-BATCH-TYPE-CD
               WHEN 'C'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'F'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'K'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'U'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DEPOSIT YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3250-LIMIT-DEPOSIT.
           IF LBXM-DEPOSIT-YTD < 50.00
               MOVE 50.00 TO LBXM-DEPOSIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-DEPOSIT-YTD > 500.00
               MOVE 500.00 TO LBXM-DEPOSIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3260-DESCRIBE-REASON.
           EVALUATE LBXM-REASON-CD
               WHEN 'GF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * RC -> DP WHEN EXPIRY DATE IS PAST
      * DP -> EX WHEN EXPIRY DATE IS PAST
      * EX -> RJ WHEN EXPIRY DATE IS PAST
      * RJ -> AD WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3270-SET-TRAN.
           EVALUATE LBXT-TRAN-CD
               WHEN 'RC'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EX' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E664 REF NO REQUIRED
      * E858 CHECK YTD NEGATIVE
      * E523 CHECK YTD OVER 1000000
      * E478 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3280-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-REF-NO = SPACES
                   MOVE 'E664' TO WS-REJECT-CODE
               WHEN LBXM-CHECK-YTD < 0
                   MOVE 'E858' TO WS-REJECT-CODE
               WHEN LBXM-CHECK-YTD > 1000000.00
                   MOVE 'E523' TO WS-REJECT-CODE
               WHEN LBXM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E478' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E565 CONTACT REQUIRED
      * E509 FEE YTD NEGATIVE
      * E683 FEE YTD OVER 1000000
      * E450 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3290-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-CONTACT = SPACES
                   MOVE 'E565' TO WS-REJECT-CODE
               WHEN LBXM-FEE-YTD < 0
                   MOVE 'E509' TO WS-REJECT-CODE
               WHEN LBXM-FEE-YTD > 1000000.00
                   MOVE 'E683' TO WS-REJECT-CODE
               WHEN LBXT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E450' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-EXCEPT.
           IF LBXM-EXCEPT-CNT < 9999999
               ADD 1 TO LBXM-EXCEPT-CNT
           END-IF
           IF LBXM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO LBXT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LOCKBOX CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3310-DESCRIBE-LOCKBOX.
           EVALUATE LBXM-LOCKBOX-CD
               WHEN 'DX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CHECK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3320-FLAG-CHECK.
           IF LBXM-CHECK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBXM-CHECK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E416 MEMO REQUIRED
      * E572 CASH ON HAND AMT NEGATIVE
      * E924 CASH ON HAND AMT OVER 25000
      * E798 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3330-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-MEMO = SPACES
                   MOVE 'E416' TO WS-REJECT-CODE
               WHEN LBXT-CASH-ON-HAND-AMT < 0
                   MOVE 'E572' TO WS-REJECT-CODE
               WHEN LBXT-CASH-ON-HAND-AMT > 25000.00
                   MOVE 'E924' TO WS-REJECT-CODE
               WHEN LBXM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E798' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASH ON HAND AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3340-LIMIT-CASH-ON-HAND.
           IF LBXT-CASH-ON-HAND-AMT < 25.00
               MOVE 25.00 TO LBXT-CASH-ON-HAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-CASH-ON-HAND-AMT > 50000.00
               MOVE 50000.00 TO LBXT-CASH-ON-HAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3350-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REGION-CD
                   ADD LBXT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3360-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO LBXT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3370-SPLIT-FLOAT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE LBXT-FLOAT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH ON HAND YTD.
      *----------------------------------------------------------------*
       3380-PRORATE-CASH-ON-HAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-CASH-ON-HAND-YTD ROUNDED =
                   LBXM-DEPOSIT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-CASH-ON-HAND-YTD
           END-IF
           ADD LBXM-CASH-ON-HAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FLOAT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3390-ACCRUE-FLOAT.
           IF LBXT-FLOAT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXT-FLOAT-AMT * LBXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO LBXT-CHECK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3400-LIMIT-EXCEPT.
           IF LBXM-EXCEPT-YTD < 25.00
               MOVE 25.00 TO LBXM-EXCEPT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-EXCEPT-YTD > 50000.00
               MOVE 50000.00 TO LBXM-EXCEPT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3410-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 12
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3420-CHECK-CLOSE.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REASON-CD
                   ADD LBXM-DEPOSIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-DEPOSIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3440-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT GRID TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3450-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-GRID-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF LBXM-FEE-AMT NOT < WS-FLOAT-GRID-LOW(WS-TX)
                   MOVE WS-FLOAT-GRID-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3460-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXT-TRAN-CD
                   ADD LBXT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASH ON HAND AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3470-LIMIT-CASH-ON-HAND.
           IF LBXT-CASH-ON-HAND-AMT < 10.00
               MOVE 10.00 TO LBXT-CASH-ON-HAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-CASH-ON-HAND-AMT > 2500.00
               MOVE 2500.00 TO LBXT-CASH-ON-HAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3480-CHECK-EFF.
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
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY BATCH TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3490-ACCUM-BATCH-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-BATCH-TYPE-CD
                   ADD LBXM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-BATCH-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH ON HAND AMT.
      *----------------------------------------------------------------*
       3500-PRORATE-CASH-ON-HAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXT-CASH-ON-HAND-AMT ROUNDED =
                   LBXM-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXT-CASH-ON-HAND-AMT
           END-IF
           ADD LBXT-CASH-ON-HAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * RC -> DP WHEN MATURITY DATE IS PAST
      * DP -> EX WHEN MATURITY DATE IS PAST
      * EX -> RJ WHEN MATURITY DATE IS PAST
      * RJ -> AD WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3510-SET-TRAN.
           EVALUATE LBXT-TRAN-CD
               WHEN 'RC'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EX' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHECK AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3520-ACCRUE-CHECK.
           IF LBXT-CHECK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXT-CHECK-AMT * LBXM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO LBXM-REMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ON HAND FEE ON THE FLOAT YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3530-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN LBXT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO LBXM-CASH-ON-HAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-HOLD-FLAG = 'Y'
                   COMPUTE LBXM-CASH-ON-HAND-AMT ROUNDED =
                       LBXM-FLOAT-YTD * LBXM-FEE-PCT / 200
               WHEN LBXM-CHANNEL-CD = 'B'
                   MOVE 7.00 TO LBXM-CASH-ON-HAND-AMT
               WHEN LBXM-CHANNEL-CD = 'H'
                   MOVE 5.00 TO LBXM-CASH-ON-HAND-AMT
               WHEN OTHER
                   COMPUTE LBXM-CASH-ON-HAND-AMT ROUNDED =
                       LBXM-FLOAT-YTD * LBXM-FEE-PCT / 100
           END-EVALUATE
           IF LBXM-CASH-ON-HAND-AMT > 0 AND LBXM-CASH-ON-HAND-AMT < 2.00
               MOVE 2.00 TO LBXM-CASH-ON-HAND-AMT
           END-IF
           IF LBXM-CASH-ON-HAND-AMT > 25.00
               MOVE 25.00 TO LBXM-CASH-ON-HAND-AMT
           END-IF
           ADD LBXM-CASH-ON-HAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3540-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = LBXM-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = LBXM-FEE-AMT - (WS-WORK-AMT * 12)
           COMPUTE LBXM-EXCEPT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOCKBOX CD:
      * DX -> JM WHEN MATURITY DATE IS PAST
      * JM -> WW WHEN MATURITY DATE IS PAST
      * WW -> XE WHEN MATURITY DATE IS PAST
      * XE -> DX WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3550-SET-LOCKBOX.
           EVALUATE LBXM-LOCKBOX-CD
               WHEN 'DX'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WW' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WW'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XE' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XE'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3560-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3570-COUNT-RETRY.
           IF LBXM-RETRY-CNT < 9999999
               ADD 1 TO LBXM-RETRY-CNT
           END-IF
           IF LBXM-RETRY-CNT NOT < 6
               MOVE 'Y' TO LBXM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E915 CONTACT REQUIRED
      * E295 CHECK AMT NEGATIVE
      * E280 CHECK AMT OVER 5000
      * E949 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3580-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-CONTACT = SPACES
                   MOVE 'E915' TO WS-REJECT-CODE
               WHEN LBXT-CHECK-AMT < 0
                   MOVE 'E295' TO WS-REJECT-CODE
               WHEN LBXT-CHECK-AMT > 5000.00
                   MOVE 'E280' TO WS-REJECT-CODE
               WHEN LBXM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E949' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH ON HAND YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3590-SPLIT-CASH-ON-HAND.
           COMPUTE WS-WORK-AMT = LBXM-CASH-ON-HAND-YTD / 4
           COMPUTE WS-WORK-AMT-2 = LBXM-CASH-ON-HAND-YTD - (WS-WORK-AMT
               * 4)
           COMPUTE LBXM-REMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3600-COUNT-EXCEPT.
           IF LBXM-EXCEPT-CNT < 9999999
               ADD 1 TO LBXM-EXCEPT-CNT
           END-IF
           IF LBXM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3610-DESCRIBE-REASON.
           EVALUATE LBXM-REASON-CD
               WHEN 'GF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CHECK YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3620-FLAG-CHECK.
           IF LBXM-CHECK-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE LBXM-CHECK-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3630-CHECK-EXPIRY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO LBXT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REMIT BAND TIER FOR THE CHECK YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3640-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-BAND-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF LBXM-CHECK-YTD NOT < WS-REMIT-BAND-LOW(WS-TX)
                   MOVE WS-REMIT-BAND-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3650-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = LBXM-EXCEPT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = LBXM-EXCEPT-YTD - (WS-WORK-AMT * 12)
           COMPUTE LBXM-DEPOSIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E755 MEMO REQUIRED
      * E208 EXCEPT AMT NEGATIVE
      * E099 EXCEPT AMT OVER 99999
      * E968 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3660-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-MEMO = SPACES
                   MOVE 'E755' TO WS-REJECT-CODE
               WHEN LBXM-EXCEPT-AMT < 0
                   MOVE 'E208' TO WS-REJECT-CODE
               WHEN LBXM-EXCEPT-AMT > 99999.00
                   MOVE 'E099' TO WS-REJECT-CODE
               WHEN LBXM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E968' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REMIT BAND TIER FOR THE CHECK YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3670-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-BAND-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF LBXM-CHECK-YTD NOT < WS-REMIT-BAND-LOW(WS-TX)
                   MOVE WS-REMIT-BAND-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3680-AGE-OPEN.
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
                   ADD LBXM-CHECK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD LBXM-CHECK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD LBXM-CHECK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD LBXM-CHECK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-CHECK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3690-COUNT-EXCEPT.
           IF LBXM-EXCEPT-CNT < 9999999
               ADD 1 TO LBXM-EXCEPT-CNT
           END-IF
           IF LBXM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO LBXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3700-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 3
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3710-CHECK-LAST-STMT.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3720-CHECK-NOTICE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY LOCKBOX CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3730-ACCUM-LOCKBOX.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-LOCKBOX-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-LOCKBOX-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * REMIT YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3740-LIMIT-REMIT.
           IF LBXM-REMIT-YTD < 25.00
               MOVE 25.00 TO LBXM-REMIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-REMIT-YTD > 2500.00
               MOVE 2500.00 TO LBXM-REMIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FLOAT FEE ON THE EXCEPT YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3750-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN LBXM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO LBXM-FLOAT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-STOP-FLAG = 'Y'
                   COMPUTE LBXM-FLOAT-AMT ROUNDED = LBXM-EXCEPT-YTD *
                       LBXM-DISCOUNT / 200
               WHEN LBXM-BATCH-TYPE-CD = 'C'
                   MOVE 5.00 TO LBXM-FLOAT-AMT
               WHEN LBXM-BATCH-TYPE-CD = 'D'
                   MOVE 15.00 TO LBXM-FLOAT-AMT
               WHEN OTHER
                   COMPUTE LBXM-FLOAT-AMT ROUNDED = LBXM-EXCEPT-YTD *
                       LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXM-FLOAT-AMT > 0 AND LBXM-FLOAT-AMT < 10.00
               MOVE 10.00 TO LBXM-FLOAT-AMT
           END-IF
           IF LBXM-FLOAT-AMT > 50.00
               MOVE 50.00 TO LBXM-FLOAT-AMT
           END-IF
           ADD LBXM-FLOAT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CASH ON HAND AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3760-LIMIT-CASH-ON-HAND.
           IF LBXT-CASH-ON-HAND-AMT < 25.00
               MOVE 25.00 TO LBXT-CASH-ON-HAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-CASH-ON-HAND-AMT > 5000.00
               MOVE 5000.00 TO LBXT-CASH-ON-HAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3770-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 3
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3780-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REMIT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3790-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = LBXM-REMIT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = LBXM-REMIT-YTD - (WS-WORK-AMT * 12)
           COMPUTE LBXM-DEPOSIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3800-FLAG-DEPOSIT.
           IF LBXM-DEPOSIT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBXM-DEPOSIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REMIT BAND TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3810-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-BAND-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF LBXM-FEE-AMT NOT < WS-REMIT-BAND-LOW(WS-TX)
                   MOVE WS-REMIT-BAND-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CHECK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3820-PRORATE-CHECK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   LBXT-CHECK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT FEE ON THE FEE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3830-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN LBXT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO LBXM-EXCEPT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   COMPUTE LBXM-EXCEPT-AMT ROUNDED = LBXM-FEE-YTD *
                       LBXM-FEE-PCT / 200
               WHEN LBXM-REASON-CD = 'GF'
                   MOVE 7.00 TO LBXM-EXCEPT-AMT
               WHEN LBXM-REASON-CD = 'UK'
                   MOVE 10.00 TO LBXM-EXCEPT-AMT
               WHEN OTHER
                   COMPUTE LBXM-EXCEPT-AMT ROUNDED = LBXM-FEE-YTD *
                       LBXM-FEE-PCT / 100
           END-EVALUATE
           IF LBXM-EXCEPT-AMT > 0 AND LBXM-EXCEPT-AMT < 5.00
               MOVE 5.00 TO LBXM-EXCEPT-AMT
           END-IF
           IF LBXM-EXCEPT-AMT > 75.00
               MOVE 75.00 TO LBXM-EXCEPT-AMT
           END-IF
           ADD LBXM-EXCEPT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCEPT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3840-ACCRUE-EXCEPT.
           IF LBXM-EXCEPT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-EXCEPT-AMT * LBXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO LBXT-CHECK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT GRID TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3850-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-GRID-RATE(1) TO LBXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF LBXM-DEPOSIT-AMT NOT < WS-FLOAT-GRID-LOW(WS-TX)
                   MOVE WS-FLOAT-GRID-RATE(WS-TX) TO LBXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3860-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3870-DESCRIBE-TRAN.
           EVALUATE LBXT-TRAN-CD
               WHEN 'RC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'EX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'AD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3880-LIMIT-FEE.
           IF LBXM-FEE-YTD < 25.00
               MOVE 25.00 TO LBXM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-FEE-YTD > 50000.00
               MOVE 50000.00 TO LBXM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E756 ADDR REQUIRED
      * E708 EXCEPT AMT NEGATIVE
      * E121 EXCEPT AMT OVER 50000
      * E310 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3890-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-ADDR = SPACES
                   MOVE 'E756' TO WS-REJECT-CODE
               WHEN LBXM-EXCEPT-AMT < 0
                   MOVE 'E708' TO WS-REJECT-CODE
               WHEN LBXM-EXCEPT-AMT > 50000.00
                   MOVE 'E121' TO WS-REJECT-CODE
               WHEN LBXT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E310' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3900-FLAG-FEE.
           IF LBXM-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBXM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT GRID TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3910-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-GRID-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF LBXM-FEE-YTD NOT < WS-FLOAT-GRID-LOW(WS-TX)
                   MOVE WS-FLOAT-GRID-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3920-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO LBXM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT GRID TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-GRID-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF LBXT-FEE-AMT NOT < WS-FLOAT-GRID-LOW(WS-TX)
                   MOVE WS-FLOAT-GRID-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3940-AGE-NEXT-DUE.
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
      * EXCEPT YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3950-LIMIT-EXCEPT.
           IF LBXM-EXCEPT-YTD < 50.00
               MOVE 50.00 TO LBXM-EXCEPT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-EXCEPT-YTD > 2500.00
               MOVE 2500.00 TO LBXM-EXCEPT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH ON HAND AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3960-LIMIT-CASH-ON-HAND.
           IF LBXM-CASH-ON-HAND-AMT < 100.00
               MOVE 100.00 TO LBXM-CASH-ON-HAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-CASH-ON-HAND-AMT > 1000.00
               MOVE 1000.00 TO LBXM-CASH-ON-HAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EDV -> EPM WHEN CLOSE DATE IS PAST
      * EPM -> FZY WHEN CLOSE DATE IS PAST
      * FZY -> XUU WHEN CLOSE DATE IS PAST
      * XUU -> YBH WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3970-SET-CYCLE.
           EVALUATE LBXM-CYCLE-CD
               WHEN 'EDV'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EPM' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EPM'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FZY' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FZY'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XUU' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XUU'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YBH' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3980-DESCRIBE-TRAN.
           EVALUATE LBXT-TRAN-CD
               WHEN 'RC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'EX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'AD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3990-COUNT-RETRY.
           IF LBXM-RETRY-CNT < 9999999
               ADD 1 TO LBXM-RETRY-CNT
           END-IF
           IF LBXM-RETRY-CNT NOT < 6
               MOVE 'Y' TO LBXM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E854 OPERATOR REQUIRED
      * E332 FLOAT AMT NEGATIVE
      * E855 FLOAT AMT OVER 250000
      * E225 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4000-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-OPERATOR = SPACES
                   MOVE 'E854' TO WS-REJECT-CODE
               WHEN LBXM-FLOAT-AMT < 0
                   MOVE 'E332' TO WS-REJECT-CODE
               WHEN LBXM-FLOAT-AMT > 250000.00
                   MOVE 'E855' TO WS-REJECT-CODE
               WHEN LBXM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E225' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHECK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4010-ACCRUE-CHECK.
           IF LBXM-CHECK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-CHECK-AMT * LBXM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-CASH-ON-HAND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4020-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-CHANNEL-CD
                   ADD LBXM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4030-COUNT-EXCEPT.
           IF LBXM-EXCEPT-CNT < 9999999
               ADD 1 TO LBXM-EXCEPT-CNT
           END-IF
           IF LBXM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4040-LIMIT-FLOAT.
           IF LBXT-FLOAT-AMT < 25.00
               MOVE 25.00 TO LBXT-FLOAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-FLOAT-AMT > 5000.00
               MOVE 5000.00 TO LBXT-FLOAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4050-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * LBXM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO LBXM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4060-LIMIT-EXCEPT.
           IF LBXM-EXCEPT-AMT < 10.00
               MOVE 10.00 TO LBXM-EXCEPT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-EXCEPT-AMT > 5000.00
               MOVE 5000.00 TO LBXM-EXCEPT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT GRID TIER FOR THE CHECK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4070-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-GRID-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF LBXM-CHECK-AMT NOT < WS-FLOAT-GRID-LOW(WS-TX)
                   MOVE WS-FLOAT-GRID-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4080-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO LBXM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4090-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 3
               MOVE 'Y' TO LBXT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4100-CHECK-EXPIRY.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO LBXM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT FEE ON THE CHECK AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4110-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN LBXM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO LBXM-EXCEPT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE LBXM-EXCEPT-AMT ROUNDED = LBXT-CHECK-AMT *
                       LBXM-FEE-PCT / 200
               WHEN LBXM-BATCH-TYPE-CD = 'C'
                   MOVE 10.00 TO LBXM-EXCEPT-AMT
               WHEN LBXM-BATCH-TYPE-CD = 'D'
                   MOVE 20.00 TO LBXM-EXCEPT-AMT
               WHEN OTHER
                   COMPUTE LBXM-EXCEPT-AMT ROUNDED = LBXT-CHECK-AMT *
                       LBXM-FEE-PCT / 100
           END-EVALUATE
           IF LBXM-EXCEPT-AMT > 0 AND LBXM-EXCEPT-AMT < 2.00
               MOVE 2.00 TO LBXM-EXCEPT-AMT
           END-IF
           IF LBXM-EXCEPT-AMT > 75.00
               MOVE 75.00 TO LBXM-EXCEPT-AMT
           END-IF
           ADD LBXM-EXCEPT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E623 DESC REQUIRED
      * E117 DEPOSIT YTD NEGATIVE
      * E858 DEPOSIT YTD OVER 50000
      * E133 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4120-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-DESC = SPACES
                   MOVE 'E623' TO WS-REJECT-CODE
               WHEN LBXM-DEPOSIT-YTD < 0
                   MOVE 'E117' TO WS-REJECT-CODE
               WHEN LBXM-DEPOSIT-YTD > 50000.00
                   MOVE 'E858' TO WS-REJECT-CODE
               WHEN LBXM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E133' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GF -> UK WHEN LAST STMT DATE IS PAST
      * UK -> VR WHEN LAST STMT DATE IS PAST
      * VR -> XG WHEN LAST STMT DATE IS PAST
      * XG -> GF WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4130-SET-REASON.
           EVALUATE LBXM-REASON-CD
               WHEN 'GF'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UK' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VR' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VR'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XG' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XG'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GF' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4140-FLAG-FEE.
           IF LBXT-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE LBXT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4150-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = LBXM-DEPOSIT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = LBXM-DEPOSIT-YTD - (WS-WORK-AMT * 2)
           COMPUTE LBXM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4160-LIMIT-FEE.
           IF LBXM-FEE-YTD < 10.00
               MOVE 10.00 TO LBXM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-FEE-YTD > 5000.00
               MOVE 5000.00 TO LBXM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH ON HAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CHECK YTD.
      *----------------------------------------------------------------*
       4170-PRORATE-CHECK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-CHECK-YTD ROUNDED =
                   LBXT-CASH-ON-HAND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-CHECK-YTD
           END-IF
           ADD LBXM-CHECK-YTD TO WS-TOTAL-PRORATED.

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
                   MOVE LBFEE-FLOAT-YTD
                     TO LBXM-FLOAT-YTD
                   MOVE LBFEE-STOP-FLAG
                     TO LBXM-STOP-FLAG
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
                   MOVE LBCAL-DEPOSIT-AMT
                     TO LBXM-DEPOSIT-AMT
                   MOVE LBCAL-DEPOSIT-YTD
                     TO LBXM-DEPOSIT-YTD
                   MOVE LBCAL-CASH-ON-HAND-YTD
                     TO LBXM-CASH-ON-HAND-YTD
                   MOVE LBCAL-FLOAT-YTD
                     TO LBXM-FLOAT-YTD
                   MOVE LBCAL-REMIT-YTD
                     TO LBXM-REMIT-YTD
                   MOVE LBCAL-NOTICE-SENT-FLAG
                     TO LBXM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-LBCAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

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
                   MOVE LBWAIV-CASH-ON-HAND-YTD
                     TO LBXM-CASH-ON-HAND-YTD
                   MOVE LBWAIV-FLOAT-YTD
                     TO LBXM-FLOAT-YTD
                   MOVE LBWAIV-PREFERRED-FLAG
                     TO LBXM-PREFERRED-FLAG
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
           DISPLAY 'LBXPOST READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'LBXPOST' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
