      *================================================================*
      * PROGRAM  : BPYCONV                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : BILL PAYMENT SERVICE - FILE CONVERSION. READS THE   *
      *            BPY TRANSACTIONS, UPDATES THE BPY MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. CONSUMER BILL PAYMENT  *
      *            PAYEE MAINTENANCE AND CHECK OR EFT DISBURSEMENT     *
      * INPUT    : BPYMST (KSDS), BPYTRN, BANKCTL                      *
      * OUTPUT   : BPYOUT, BPYCORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, BPSTAT                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-11-22 TWH  ORIGINAL                                       *
      * 1995-09-25 GAF  FEE CUTOFF MOVED PRB1193                       *
      * 1998-01-03 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-12-11 EWS  FEE CUTOFF MOVED CHG4475                       *
      * 2000-03-26 MKL  REFUND LIMIT REVISED CHG7664                   *
      * 2002-07-20 DPS  PAYMENT ROUNDING - HALF UP CHG0149             *
      * 2002-01-22 EWS  FEE CALCULATION CORRECTED REQ0848              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BPYCONV.
       AUTHOR. J L MARSH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "BPYMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS BPYM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "BPYTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "BPYOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "BPYCORPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY BPYMST.
       FD  TRAN-FILE.
       COPY BPYTRN.
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
      * WS-REFUND-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REFUND-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000639000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000728500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000527000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004000000000118500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004500000000310000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014500000000720500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014550000000700000'.
       01  WS-REFUND-SCHED-TBL REDEFINES WS-REFUND-SCHED-VALUES.
           05  WS-REFUND-SCHED-ROW OCCURS 7 TIMES.
               10  WS-REFUND-SCHED-LOW     PIC 9(09)V99.
               10  WS-REFUND-SCHED-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYMENT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000607000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000356000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000005000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002850000000610000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002950000000817000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003200000000644000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003250000000188000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013250000000145500'.
       01  WS-PAYMENT-GRID-TBL REDEFINES WS-PAYMENT-GRID-VALUES.
           05  WS-PAYMENT-GRID-ROW OCCURS 8 TIMES.
               10  WS-PAYMENT-GRID-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RETURNED-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RETURNED-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000686500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000629000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000687500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005300000000898000'.
       01  WS-RETURNED-TIER-TBL REDEFINES WS-RETURNED-TIER-VALUES.
           05  WS-RETURNED-TIER-ROW OCCURS 4 TIMES.
               10  WS-RETURNED-TIER-LOW    PIC 9(09)V99.
               10  WS-RETURNED-TIER-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  BPYCONV  BILL PAYMENT SERVICE'.
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
       COPY BPSTATP.

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
               MOVE 'BPYMST' TO ABND-FILE-NAME
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
           MOVE BPYT-KEY TO BPYM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-CHECK-OPEN
               PERFORM 3010-CHECK-OPEN
               PERFORM 3020-ACCRUE-RETURNED
               PERFORM 3030-COUNT-PAYMENT
               PERFORM 3040-SELECT-PENALTY-PCT
               PERFORM 3050-ACCUM-CHANNEL
               PERFORM 3060-SET-SOURCE
               PERFORM 3070-PRORATE-RETURNED
               PERFORM 3080-FLAG-PAYMENT
               PERFORM 3090-COUNT-ITEM
               PERFORM 3100-DESCRIBE-REMIT-METHOD
               PERFORM 3110-EDIT-REJECT
               PERFORM 3120-FLAG-FEE
               PERFORM 3130-FLAG-ITEM
               PERFORM 3140-ACCUM-SOURCE
               PERFORM 3150-CHECK-EXPIRY
               PERFORM 3160-EDIT-CONTACT
               PERFORM 3170-CHECK-EFFECTIVE
               PERFORM 3180-FLAG-REFUND
               PERFORM 3190-SET-REASON
               PERFORM 3200-SET-REASON
               PERFORM 3210-SPLIT-FEE
               PERFORM 3220-ASSESS-AUTO-RENEW
               PERFORM 3230-SPLIT-RETURNED
               PERFORM 3240-PRORATE-REFUND
               PERFORM 3250-PRORATE-PAYMENT
               PERFORM 3260-LIMIT-FEE
               PERFORM 3270-SET-STATUS
               PERFORM 3280-LIMIT-PAYMENT
               PERFORM 3290-ASSESS-OVERRIDE
               PERFORM 3300-PRORATE-REFUND
               PERFORM 3310-EDIT-REJECT
               PERFORM 3320-FLAG-RETURNED
               PERFORM 3330-AGE-MATURITY
               PERFORM 3340-CHECK-EXPIRY
               PERFORM 3350-ACCRUE-RETURNED
               PERFORM 3360-SPLIT-REFUND
               PERFORM 3370-SELECT-PENALTY-PCT
               PERFORM 3380-LIMIT-ITEM
               PERFORM 3390-SET-TRAN
               PERFORM 3400-FLAG-PAYMENT
               PERFORM 3410-CHECK-EXPIRY
               PERFORM 3420-EDIT-MEMO
               PERFORM 3430-EDIT-OPERATOR
               PERFORM 3440-PRORATE-RETURNED
               PERFORM 3450-SELECT-PENALTY-PCT
               PERFORM 3460-ACCRUE-REFUND
               PERFORM 3470-ACCUM-CHANNEL
               PERFORM 3480-SPLIT-FEE
               PERFORM 3490-COUNT-PAYMENT
               PERFORM 3500-DESCRIBE-CHANNEL
               PERFORM 3510-LIMIT-FEE
               PERFORM 3520-CHECK-POST
               PERFORM 3530-SELECT-MARGIN
               PERFORM 3540-ACCRUE-FEE
               PERFORM 3550-ACCRUE-RETURNED
               PERFORM 3560-LIMIT-REFUND
               PERFORM 3570-AGE-LAST-STMT
               PERFORM 3580-ACCUM-CHANNEL
               PERFORM 3590-COUNT-SEQ
               PERFORM 3600-EDIT-OPERATOR
               PERFORM 3610-SET-REMIT-METHOD
               PERFORM 3620-SELECT-MARGIN
               PERFORM 3630-AGE-EFF
               PERFORM 3640-EDIT-CUST-ID
               PERFORM 3650-DESCRIBE-CYCLE
               PERFORM 3660-SELECT-MARGIN
               PERFORM 3670-SPLIT-FEE
               PERFORM 3680-ASSESS-REVERSAL
               PERFORM 3690-COUNT-CYCLE
               PERFORM 3700-ASSESS-HOLD
               PERFORM 3710-ASSESS-REVERSAL
               PERFORM 3720-COUNT-PAYMENT
               PERFORM 3730-AGE-MATURITY
               PERFORM 3740-SET-TRAN
               PERFORM 3750-ACCUM-TRAN
               PERFORM 3760-SELECT-MARGIN
               PERFORM 3770-AGE-NOTICE
               PERFORM 3780-PRORATE-PAYMENT
               PERFORM 3790-AGE-EFF
               PERFORM 3800-ACCUM-STATUS
               PERFORM 3810-COUNT-EXCEPT
               PERFORM 3820-FLAG-FEE
               PERFORM 3830-PRORATE-PAYMENT
               PERFORM 3840-PRORATE-FEE
               PERFORM 3850-LIMIT-PAYMENT
               PERFORM 3860-ASSESS-OVERRIDE
               PERFORM 3870-COUNT-PAYMENT
               PERFORM 3880-COUNT-EXCEPT
               PERFORM 3890-ACCRUE-PAYMENT
               PERFORM 3900-CHECK-EFFECTIVE
               PERFORM 3910-EDIT-ADDR
               PERFORM 3920-PRORATE-RETURNED
               PERFORM 3930-PRORATE-PAYMENT
               PERFORM 3940-ACCUM-REASON
               PERFORM 3950-PRORATE-REFUND
               PERFORM 3960-LIMIT-PAYMENT
               PERFORM 3970-SELECT-PENALTY-PCT
               PERFORM 3980-ASSESS-REVERSAL
               PERFORM 3990-FLAG-FEE
               PERFORM 4000-CHECK-MATURITY
               PERFORM 4010-EDIT-REJECT
               PERFORM 4020-AGE-OPEN
               PERFORM 4030-ASSESS-EXEMPT
               PERFORM 4040-SELECT-RATE
               PERFORM 4050-ASSESS-OVERRIDE
               PERFORM 4060-CHECK-EFF
               PERFORM 4070-DESCRIBE-CHANNEL
               PERFORM 4080-FLAG-PAYMENT
               PERFORM 4090-FLAG-PAYMENT
               PERFORM 4100-FLAG-RETURNED
               PERFORM 4110-PRORATE-REFUND
               PERFORM 4120-ACCRUE-RETURNED
               PERFORM 4130-SELECT-RATE
               PERFORM 4140-SET-STATUS
               PERFORM 4150-SET-CHANNEL
               PERFORM 4160-PRORATE-PAYMENT
               PERFORM 4170-FLAG-REFUND
               PERFORM 4180-SPLIT-PAYMENT
               PERFORM 4190-SELECT-MARGIN
               PERFORM 4200-AGE-LAST-STMT
               PERFORM 4210-EDIT-CUST-ID
               PERFORM 4220-SET-REASON
               PERFORM 4230-FLAG-REFUND
               PERFORM 4240-FLAG-PAYMENT
               PERFORM 4250-ACCUM-PAYEE-TYPE
               PERFORM 4260-ACCUM-REASON
               PERFORM 4270-CHECK-MATURITY
               PERFORM 4280-SET-CYCLE
               PERFORM 4290-AGE-LAST-ACT
               PERFORM 4300-AGE-EFF
               PERFORM 4310-SET-PAYEE-TYPE
               PERFORM 4320-PRORATE-PAYMENT
               PERFORM 4330-PRORATE-PAYMENT
               PERFORM 4340-PRORATE-FEE
               PERFORM 4350-AGE-CLOSE
               PERFORM 4360-SET-REASON
               PERFORM 4370-CHECK-OPEN
               PERFORM 4380-FLAG-PAYMENT
               PERFORM 4390-ACCRUE-RATE
               PERFORM 4400-SPLIT-FEE
               PERFORM 4410-SPLIT-FEE
               PERFORM 4420-DESCRIBE-SOURCE
               PERFORM 4430-ASSESS-HOLD
               PERFORM 4440-ASSESS-HOLD
               PERFORM 4450-SELECT-PENALTY-PCT
               PERFORM 4460-EDIT-CONTACT
               PERFORM 4470-CHECK-EFFECTIVE
               PERFORM 4480-CHECK-MATURITY
               PERFORM 4490-COUNT-CYCLE
               PERFORM 4500-ASSESS-OVERRIDE
               PERFORM 4510-SET-REMIT-METHOD
               PERFORM 4520-FLAG-RETURNED
               PERFORM 4530-COUNT-SEQ
               PERFORM 4540-FLAG-FEE
               PERFORM 4550-ACCRUE-REFUND
               PERFORM 4560-CHECK-NOTICE
               PERFORM 4570-ACCRUE-REFUND
               PERFORM 4580-FLAG-FEE
               PERFORM 4590-AGE-EFFECTIVE
               PERFORM 4600-CHECK-NEXT-DUE
               PERFORM 4610-SELECT-PENALTY-PCT
               PERFORM 4620-SPLIT-REFUND
               PERFORM 4630-FLAG-REFUND
               PERFORM 4640-COUNT-EXCEPT
               PERFORM 4650-ACCRUE-FEE
               PERFORM 4660-SELECT-MARGIN
               PERFORM 4670-DESCRIBE-SOURCE
               PERFORM 4680-ASSESS-SUSPEND
               PERFORM 4690-FLAG-RETURNED
               PERFORM 4700-PRORATE-RETURNED
               PERFORM 4710-SET-CHANNEL
               PERFORM 4720-DESCRIBE-SOURCE
               PERFORM 4730-ACCRUE-REFUND
               PERFORM 4740-PRORATE-RETURNED
               PERFORM 4750-EDIT-MEMO
               PERFORM 4760-SELECT-RATE
               PERFORM 4770-EDIT-NAME
               PERFORM 4780-PRORATE-FEE
               PERFORM 7020-CALL-BPSTAT
               REWRITE BPY-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'BPYMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM BPY-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3000-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF BPYM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO BPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3010-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF BPYM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETURNED AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3020-ACCRUE-RETURNED.
           IF BPYT-RETURNED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-RETURNED-AMT * BPYM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BPYT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3030-COUNT-PAYMENT.
           IF BPYM-PAYMENT-CNT < 9999999
               ADD 1 TO BPYM-PAYMENT-CNT
           END-IF
           IF BPYM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND SCHED TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3040-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-SCHED-RATE(1) TO BPYM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BPYM-PAYMENT-YTD NOT < WS-REFUND-SCHED-LOW(WS-TX)
                   MOVE WS-REFUND-SCHED-RATE(WS-TX) TO BPYM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3050-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYT-CHANNEL-CD
                   ADD BPYM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * CT -> FV WHEN EFFECTIVE DATE IS PAST
      * FV -> HD WHEN EFFECTIVE DATE IS PAST
      * HD -> KD WHEN EFFECTIVE DATE IS PAST
      * KD -> NE WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3060-SET-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FV' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FV'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HD'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NE' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETURNED AMT.
      *----------------------------------------------------------------*
       3070-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-RETURNED-AMT ROUNDED =
                   BPYT-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-RETURNED-AMT
           END-IF
           ADD BPYM-RETURNED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3080-FLAG-PAYMENT.
           IF BPYM-PAYMENT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3090-COUNT-ITEM.
           IF BPYM-ITEM-CNT < 9999999
               ADD 1 TO BPYM-ITEM-CNT
           END-IF
           IF BPYM-ITEM-CNT NOT < 6
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REMIT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3100-DESCRIBE-REMIT-METHOD.
           EVALUATE BPYM-REMIT-METHOD-CD
               WHEN 'CGV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FNS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NMS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TAF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VNK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WUA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XCT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E268 REJECT CD REQUIRED
      * E682 RETURNED YTD NEGATIVE
      * E056 RETURNED YTD OVER 25000
      * E398 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3110-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-REJECT-CD = SPACES
                   MOVE 'E268' TO WS-REJECT-CODE
               WHEN BPYM-RETURNED-YTD < 0
                   MOVE 'E682' TO WS-REJECT-CODE
               WHEN BPYM-RETURNED-YTD > 25000.00
                   MOVE 'E056' TO WS-REJECT-CODE
               WHEN BPYM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E398' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3120-FLAG-FEE.
           IF BPYT-FEE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3130-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3140-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-SOURCE-CD
                   ADD BPYM-REFUND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-REFUND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO BPYM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E116 CONTACT REQUIRED
      * E474 PAYMENT AMT NEGATIVE
      * E226 PAYMENT AMT OVER 250000
      * E809 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3160-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-CONTACT = SPACES
                   MOVE 'E116' TO WS-REJECT-CODE
               WHEN BPYT-PAYMENT-AMT < 0
                   MOVE 'E474' TO WS-REJECT-CODE
               WHEN BPYT-PAYMENT-AMT > 250000.00
                   MOVE 'E226' TO WS-REJECT-CODE
               WHEN BPYM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E809' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3170-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3180-FLAG-REFUND.
           IF BPYM-REFUND-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-REFUND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> BX WHEN EFFECTIVE DATE IS PAST
      * BX -> GM WHEN EFFECTIVE DATE IS PAST
      * GM -> KU WHEN EFFECTIVE DATE IS PAST
      * KU -> LT WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3190-SET-REASON.
           EVALUATE BPYM-REASON-CD
               WHEN 'AX'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BX' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BX'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KU' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KU'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LT' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> BX WHEN EFFECTIVE DATE IS PAST
      * BX -> GM WHEN EFFECTIVE DATE IS PAST
      * GM -> KU WHEN EFFECTIVE DATE IS PAST
      * KU -> LT WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3200-SET-REASON.
           EVALUATE BPYM-REASON-CD
               WHEN 'AX'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BX' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BX'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KU' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KU'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LT' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3210-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPYM-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = BPYM-FEE-YTD - (WS-WORK-AMT * 3)
           COMPUTE BPYT-RETURNED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE REFUND AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3220-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN BPYM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = BPYM-REFUND-AMT *
                       BPYM-MARGIN / 200
               WHEN BPYT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN BPYT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = BPYM-REFUND-AMT *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT RETURNED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3230-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = BPYT-RETURNED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BPYT-RETURNED-AMT - (WS-WORK-AMT * 2)
           COMPUTE BPYM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETURNED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REFUND AMT.
      *----------------------------------------------------------------*
       3240-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-REFUND-AMT ROUNDED =
                   BPYT-RETURNED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-REFUND-AMT
           END-IF
           ADD BPYM-REFUND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3250-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-PAYMENT-AMT ROUNDED =
                   BPYT-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYT-PAYMENT-AMT
           END-IF
           ADD BPYT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3260-LIMIT-FEE.
           IF BPYT-FEE-AMT < 100.00
               MOVE 100.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-FEE-AMT > 50000.00
               MOVE 50000.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * JX -> LR WHEN LAST STMT DATE IS PAST
      * LR -> NZ WHEN LAST STMT DATE IS PAST
      * NZ -> RV WHEN LAST STMT DATE IS PAST
      * RV -> TM WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3270-SET-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LR' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LR'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3280-LIMIT-PAYMENT.
           IF BPYT-PAYMENT-AMT < 0.00
               MOVE 0.00 TO BPYT-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-PAYMENT-AMT > 1000.00
               MOVE 1000.00 TO BPYT-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3290-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BPYM-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE BPYM-REFUND-AMT ROUNDED = WS-WORK-AMT-2 *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYM-CYCLE-CD = 'DA'
                   MOVE 3.00 TO BPYM-REFUND-AMT
               WHEN BPYM-CYCLE-CD = 'FP'
                   MOVE 10.00 TO BPYM-REFUND-AMT
               WHEN OTHER
                   COMPUTE BPYM-REFUND-AMT ROUNDED = WS-WORK-AMT-2 *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYM-REFUND-AMT > 0 AND BPYM-REFUND-AMT < 1.00
               MOVE 1.00 TO BPYM-REFUND-AMT
           END-IF
           IF BPYM-REFUND-AMT > 150.00
               MOVE 150.00 TO BPYM-REFUND-AMT
           END-IF
           ADD BPYM-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REFUND AMT.
      *----------------------------------------------------------------*
       3300-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-REFUND-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-REFUND-AMT
           END-IF
           ADD BPYM-REFUND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E126 REJECT CD REQUIRED
      * E460 FEE YTD NEGATIVE
      * E417 FEE YTD OVER 25000
      * E324 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3310-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-REJECT-CD = SPACES
                   MOVE 'E126' TO WS-REJECT-CODE
               WHEN BPYM-FEE-YTD < 0
                   MOVE 'E460' TO WS-REJECT-CODE
               WHEN BPYM-FEE-YTD > 25000.00
                   MOVE 'E417' TO WS-REJECT-CODE
               WHEN BPYT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E324' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETURNED YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3320-FLAG-RETURNED.
           IF BPYM-RETURNED-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-RETURNED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3330-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BPYM-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BPYM-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETURNED AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3350-ACCRUE-RETURNED.
           IF BPYM-RETURNED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-RETURNED-AMT * BPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REFUND YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3360-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = BPYM-REFUND-YTD / 12
           COMPUTE WS-WORK-AMT-2 = BPYM-REFUND-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED TIER TIER FOR THE REFUND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3370-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-TIER-RATE(1) TO BPYM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF BPYM-REFUND-AMT NOT < WS-RETURNED-TIER-LOW(WS-TX)
                   MOVE WS-RETURNED-TIER-RATE(WS-TX) TO BPYM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3380-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * SC -> PD WHEN POST DATE IS PAST
      * PD -> RT WHEN POST DATE IS PAST
      * RT -> CN WHEN POST DATE IS PAST
      * CN -> RF WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3390-SET-TRAN.
           EVALUATE BPYT-TRAN-CD
               WHEN 'SC'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RT' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RT'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3400-FLAG-PAYMENT.
           IF BPYM-PAYMENT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3410-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO BPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E531 MEMO REQUIRED
      * E763 PAYMENT AMT NEGATIVE
      * E687 PAYMENT AMT OVER 99999
      * E223 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3420-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-MEMO = SPACES
                   MOVE 'E531' TO WS-REJECT-CODE
               WHEN BPYT-PAYMENT-AMT < 0
                   MOVE 'E763' TO WS-REJECT-CODE
               WHEN BPYT-PAYMENT-AMT > 99999.00
                   MOVE 'E687' TO WS-REJECT-CODE
               WHEN BPYM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E223' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E114 OPERATOR REQUIRED
      * E296 FEE YTD NEGATIVE
      * E581 FEE YTD OVER 5000
      * E134 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3430-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-OPERATOR = SPACES
                   MOVE 'E114' TO WS-REJECT-CODE
               WHEN BPYM-FEE-YTD < 0
                   MOVE 'E296' TO WS-REJECT-CODE
               WHEN BPYM-FEE-YTD > 5000.00
                   MOVE 'E581' TO WS-REJECT-CODE
               WHEN BPYM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E134' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REFUND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETURNED AMT.
      *----------------------------------------------------------------*
       3440-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-RETURNED-AMT ROUNDED =
                   BPYM-REFUND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-RETURNED-AMT
           END-IF
           ADD BPYM-RETURNED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED TIER TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3450-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-TIER-RATE(1) TO BPYM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF BPYM-PAYMENT-YTD NOT < WS-RETURNED-TIER-LOW(WS-TX)
                   MOVE WS-RETURNED-TIER-RATE(WS-TX) TO BPYM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3460-ACCRUE-REFUND.
           IF BPYT-REFUND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-REFUND-AMT * BPYM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BPYM-REFUND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3470-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYT-CHANNEL-CD
                   ADD BPYM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3480-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPYT-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = BPYT-FEE-AMT - (WS-WORK-AMT * 12)
           COMPUTE BPYM-REFUND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3490-COUNT-PAYMENT.
           IF BPYM-PAYMENT-CNT < 9999999
               ADD 1 TO BPYM-PAYMENT-CNT
           END-IF
           IF BPYM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3510-LIMIT-FEE.
           IF BPYM-FEE-YTD < 100.00
               MOVE 100.00 TO BPYM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-FEE-YTD > 500.00
               MOVE 500.00 TO BPYM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3520-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF BPYT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3530-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF BPYT-FEE-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3540-ACCRUE-FEE.
           IF BPYM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-FEE-AMT * BPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BPYT-REFUND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETURNED YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3550-ACCRUE-RETURNED.
           IF BPYM-RETURNED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-RETURNED-YTD * BPYM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3560-LIMIT-REFUND.
           IF BPYT-REFUND-AMT < 0.00
               MOVE 0.00 TO BPYT-REFUND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-REFUND-AMT > 5000.00
               MOVE 5000.00 TO BPYT-REFUND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3570-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYT-CHANNEL-CD
                   ADD BPYT-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYT-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3590-COUNT-SEQ.
           IF BPYT-SEQ < 9999999
               ADD 1 TO BPYT-SEQ
           END-IF
           IF BPYT-SEQ NOT < 6
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E070 OPERATOR REQUIRED
      * E482 RETURNED AMT NEGATIVE
      * E916 RETURNED AMT OVER 10000
      * E113 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3600-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-OPERATOR = SPACES
                   MOVE 'E070' TO WS-REJECT-CODE
               WHEN BPYT-RETURNED-AMT < 0
                   MOVE 'E482' TO WS-REJECT-CODE
               WHEN BPYT-RETURNED-AMT > 10000.00
                   MOVE 'E916' TO WS-REJECT-CODE
               WHEN BPYM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E113' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REMIT METHOD CD:
      * CGV -> FNS WHEN LAST STMT DATE IS PAST
      * FNS -> NMS WHEN LAST STMT DATE IS PAST
      * NMS -> TAF WHEN LAST STMT DATE IS PAST
      * TAF -> VNK WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3610-SET-REMIT-METHOD.
           EVALUATE BPYM-REMIT-METHOD-CD
               WHEN 'CGV'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FNS' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNS'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NMS' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NMS'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TAF' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TAF'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VNK' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED TIER TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3620-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-TIER-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF BPYM-FEE-YTD NOT < WS-RETURNED-TIER-LOW(WS-TX)
                   MOVE WS-RETURNED-TIER-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3630-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E128 CUST ID REQUIRED
      * E902 PAYMENT AMT NEGATIVE
      * E263 PAYMENT AMT OVER 5000
      * E745 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-CUST-ID = SPACES
                   MOVE 'E128' TO WS-REJECT-CODE
               WHEN BPYT-PAYMENT-AMT < 0
                   MOVE 'E902' TO WS-REJECT-CODE
               WHEN BPYT-PAYMENT-AMT > 5000.00
                   MOVE 'E263' TO WS-REJECT-CODE
               WHEN BPYT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E745' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3650-DESCRIBE-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3660-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF BPYM-FEE-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3670-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPYM-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = BPYM-FEE-YTD - (WS-WORK-AMT * 12)
           COMPUTE BPYM-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED YTD FEE ON THE FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3680-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BPYM-RETURNED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   COMPUTE BPYM-RETURNED-YTD ROUNDED = BPYT-FEE-AMT *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYM-STATUS-CD = 'JX'
                   MOVE 12.00 TO BPYM-RETURNED-YTD
               WHEN BPYM-STATUS-CD = 'LR'
                   MOVE 15.00 TO BPYM-RETURNED-YTD
               WHEN OTHER
                   COMPUTE BPYM-RETURNED-YTD ROUNDED = BPYT-FEE-AMT *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYM-RETURNED-YTD > 0 AND BPYM-RETURNED-YTD < 10.00
               MOVE 10.00 TO BPYM-RETURNED-YTD
           END-IF
           IF BPYM-RETURNED-YTD > 250.00
               MOVE 250.00 TO BPYM-RETURNED-YTD
           END-IF
           ADD BPYM-RETURNED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3690-COUNT-CYCLE.
           IF BPYM-CYCLE-CNT < 9999999
               ADD 1 TO BPYM-CYCLE-CNT
           END-IF
           IF BPYM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO BPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE FEE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3700-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BPYT-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE BPYT-PAYMENT-AMT ROUNDED = BPYM-FEE-YTD *
                       BPYM-RATE / 200
               WHEN BPYT-TRAN-CD = 'SC'
                   MOVE 20.00 TO BPYT-PAYMENT-AMT
               WHEN BPYT-TRAN-CD = 'PD'
                   MOVE 3.00 TO BPYT-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE BPYT-PAYMENT-AMT ROUNDED = BPYM-FEE-YTD *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYT-PAYMENT-AMT > 0 AND BPYT-PAYMENT-AMT < 1.00
               MOVE 1.00 TO BPYT-PAYMENT-AMT
           END-IF
           IF BPYT-PAYMENT-AMT > 75.00
               MOVE 75.00 TO BPYT-PAYMENT-AMT
           END-IF
           ADD BPYT-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE REFUND YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3710-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BPYM-REFUND-YTD *
                       BPYM-MARGIN / 200
               WHEN BPYM-REASON-CD = 'AX'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN BPYM-REASON-CD = 'BX'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BPYM-REFUND-YTD *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3720-COUNT-PAYMENT.
           IF BPYM-PAYMENT-CNT < 9999999
               ADD 1 TO BPYM-PAYMENT-CNT
           END-IF
           IF BPYM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3730-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * SC -> PD WHEN EXPIRY DATE IS PAST
      * PD -> RT WHEN EXPIRY DATE IS PAST
      * RT -> CN WHEN EXPIRY DATE IS PAST
      * CN -> RF WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3740-SET-TRAN.
           EVALUATE BPYT-TRAN-CD
               WHEN 'SC'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RT' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RT'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3750-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYT-TRAN-CD
                   ADD BPYM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED TIER TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3760-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-TIER-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF BPYM-FEE-YTD NOT < WS-RETURNED-TIER-LOW(WS-TX)
                   MOVE WS-RETURNED-TIER-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3770-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-NOTICE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3780-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-PAYMENT-AMT ROUNDED =
                   BPYM-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-PAYMENT-AMT
           END-IF
           ADD BPYM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3790-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3800-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-STATUS-CD
                   ADD BPYM-REFUND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-REFUND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3810-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO BPYM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3820-FLAG-FEE.
           IF BPYM-FEE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3830-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-PAYMENT-AMT ROUNDED =
                   BPYT-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-PAYMENT-AMT
           END-IF
           ADD BPYM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE RETURNED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3840-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-FEE-YTD ROUNDED =
                   BPYM-RETURNED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-FEE-YTD
           END-IF
           ADD BPYM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3850-LIMIT-PAYMENT.
           IF BPYT-PAYMENT-AMT < 10.00
               MOVE 10.00 TO BPYT-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-PAYMENT-AMT > 1000.00
               MOVE 1000.00 TO BPYT-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE PAYMENT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3860-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BPYM-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-SUSPEND-FLAG = 'Y'
                   COMPUTE BPYM-REFUND-AMT ROUNDED = BPYT-PAYMENT-AMT *
                       BPYM-RATE / 200
               WHEN BPYT-TRAN-CD = 'SC'
                   MOVE 7.00 TO BPYM-REFUND-AMT
               WHEN BPYT-TRAN-CD = 'PD'
                   MOVE 10.00 TO BPYM-REFUND-AMT
               WHEN OTHER
                   COMPUTE BPYM-REFUND-AMT ROUNDED = BPYT-PAYMENT-AMT *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-REFUND-AMT > 0 AND BPYM-REFUND-AMT < 1.00
               MOVE 1.00 TO BPYM-REFUND-AMT
           END-IF
           IF BPYM-REFUND-AMT > 75.00
               MOVE 75.00 TO BPYM-REFUND-AMT
           END-IF
           ADD BPYM-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3870-COUNT-PAYMENT.
           IF BPYM-PAYMENT-CNT < 9999999
               ADD 1 TO BPYM-PAYMENT-CNT
           END-IF
           IF BPYM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3880-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO BPYM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3890-ACCRUE-PAYMENT.
           IF BPYM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-PAYMENT-YTD * BPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BPYM-RETURNED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3900-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E217 ADDR REQUIRED
      * E691 FEE YTD NEGATIVE
      * E835 FEE YTD OVER 10000
      * E383 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3910-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-ADDR = SPACES
                   MOVE 'E217' TO WS-REJECT-CODE
               WHEN BPYM-FEE-YTD < 0
                   MOVE 'E691' TO WS-REJECT-CODE
               WHEN BPYM-FEE-YTD > 10000.00
                   MOVE 'E835' TO WS-REJECT-CODE
               WHEN BPYM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E383' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RETURNED AMT.
      *----------------------------------------------------------------*
       3920-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-RETURNED-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-RETURNED-AMT
           END-IF
           ADD BPYM-RETURNED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3930-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   BPYM-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3940-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-REASON-CD
                   ADD BPYT-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYT-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REFUND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REFUND AMT.
      *----------------------------------------------------------------*
       3950-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-REFUND-AMT ROUNDED =
                   BPYM-REFUND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYT-REFUND-AMT
           END-IF
           ADD BPYT-REFUND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3960-LIMIT-PAYMENT.
           IF BPYM-PAYMENT-AMT < 0.00
               MOVE 0.00 TO BPYM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-PAYMENT-AMT > 500.00
               MOVE 500.00 TO BPYM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND SCHED TIER FOR THE REFUND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-SCHED-RATE(1) TO BPYM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BPYM-REFUND-AMT NOT < WS-REFUND-SCHED-LOW(WS-TX)
                   MOVE WS-REFUND-SCHED-RATE(WS-TX) TO BPYM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE PAYMENT YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3980-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = BPYM-PAYMENT-YTD *
                       BPYM-MARGIN / 200
               WHEN BPYT-TRAN-CD = 'SC'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN BPYT-TRAN-CD = 'PD'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = BPYM-PAYMENT-YTD *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FEE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3990-FLAG-FEE.
           IF BPYM-FEE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4000-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO BPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E335 REJECT CD REQUIRED
      * E130 RETURNED AMT NEGATIVE
      * E700 RETURNED AMT OVER 1000000
      * E497 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-REJECT-CD = SPACES
                   MOVE 'E335' TO WS-REJECT-CODE
               WHEN BPYT-RETURNED-AMT < 0
                   MOVE 'E130' TO WS-REJECT-CODE
               WHEN BPYT-RETURNED-AMT > 1000000.00
                   MOVE 'E700' TO WS-REJECT-CODE
               WHEN BPYM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E497' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4020-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND YTD FEE ON THE FEE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4030-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BPYM-REFUND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE BPYM-REFUND-YTD ROUNDED = BPYM-FEE-YTD *
                       BPYM-MARGIN / 200
               WHEN BPYM-SOURCE-CD = 'CT'
                   MOVE 12.00 TO BPYM-REFUND-YTD
               WHEN BPYM-SOURCE-CD = 'FV'
                   MOVE 15.00 TO BPYM-REFUND-YTD
               WHEN OTHER
                   COMPUTE BPYM-REFUND-YTD ROUNDED = BPYM-FEE-YTD *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF BPYM-REFUND-YTD > 0 AND BPYM-REFUND-YTD < 2.00
               MOVE 2.00 TO BPYM-REFUND-YTD
           END-IF
           IF BPYM-REFUND-YTD > 50.00
               MOVE 50.00 TO BPYM-REFUND-YTD
           END-IF
           ADD BPYM-REFUND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4040-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-TIER-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF BPYM-FEE-AMT NOT < WS-RETURNED-TIER-LOW(WS-TX)
                   MOVE WS-RETURNED-TIER-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED FEE ON THE REFUND AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4050-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BPYM-RETURNED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-SUSPEND-FLAG = 'Y'
                   COMPUTE BPYM-RETURNED-AMT ROUNDED = BPYT-REFUND-AMT *
                       BPYM-MARGIN / 200
               WHEN BPYM-SOURCE-CD = 'CT'
                   MOVE 5.00 TO BPYM-RETURNED-AMT
               WHEN BPYM-SOURCE-CD = 'FV'
                   MOVE 12.00 TO BPYM-RETURNED-AMT
               WHEN OTHER
                   COMPUTE BPYM-RETURNED-AMT ROUNDED = BPYT-REFUND-AMT *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF BPYM-RETURNED-AMT > 0 AND BPYM-RETURNED-AMT < 1.00
               MOVE 1.00 TO BPYM-RETURNED-AMT
           END-IF
           IF BPYM-RETURNED-AMT > 150.00
               MOVE 150.00 TO BPYM-RETURNED-AMT
           END-IF
           ADD BPYM-RETURNED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4060-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF BPYT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4070-DESCRIBE-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4080-FLAG-PAYMENT.
           IF BPYM-PAYMENT-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4090-FLAG-PAYMENT.
           IF BPYM-PAYMENT-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETURNED AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4100-FLAG-RETURNED.
           IF BPYT-RETURNED-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYT-RETURNED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REFUND AMT.
      *----------------------------------------------------------------*
       4110-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-REFUND-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYT-REFUND-AMT
           END-IF
           ADD BPYT-REFUND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETURNED AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4120-ACCRUE-RETURNED.
           IF BPYM-RETURNED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-RETURNED-AMT * BPYM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BPYT-RETURNED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND SCHED TIER FOR THE REFUND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4130-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-SCHED-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BPYM-REFUND-AMT NOT < WS-REFUND-SCHED-LOW(WS-TX)
                   MOVE WS-REFUND-SCHED-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * JX -> LR WHEN NOTICE DATE IS PAST
      * LR -> NZ WHEN NOTICE DATE IS PAST
      * NZ -> RV WHEN NOTICE DATE IS PAST
      * RV -> TM WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4140-SET-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LR' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LR'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4150-SET-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REFUND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       4160-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-PAYMENT-AMT ROUNDED =
                   BPYM-REFUND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-PAYMENT-AMT
           END-IF
           ADD BPYM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * REFUND AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4170-FLAG-REFUND.
           IF BPYT-REFUND-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYT-REFUND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4180-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = BPYM-PAYMENT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BPYM-PAYMENT-AMT - (WS-WORK-AMT * 2)
           COMPUTE BPYM-RETURNED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE REFUND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4190-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF BPYM-REFUND-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4200-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E612 CUST ID REQUIRED
      * E240 PAYMENT AMT NEGATIVE
      * E211 PAYMENT AMT OVER 1000000
      * E831 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4210-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-CUST-ID = SPACES
                   MOVE 'E612' TO WS-REJECT-CODE
               WHEN BPYM-PAYMENT-AMT < 0
                   MOVE 'E240' TO WS-REJECT-CODE
               WHEN BPYM-PAYMENT-AMT > 1000000.00
                   MOVE 'E211' TO WS-REJECT-CODE
               WHEN BPYM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E831' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> BX WHEN OPEN DATE IS PAST
      * BX -> GM WHEN OPEN DATE IS PAST
      * GM -> KU WHEN OPEN DATE IS PAST
      * KU -> LT WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4220-SET-REASON.
           EVALUATE BPYM-REASON-CD
               WHEN 'AX'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BX' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BX'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KU' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KU'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LT' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REFUND YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4230-FLAG-REFUND.
           IF BPYM-REFUND-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-REFUND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4240-FLAG-PAYMENT.
           IF BPYM-PAYMENT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RETURNED AMT BY PAYEE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4250-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-PAYEE-TYPE-CD
                   ADD BPYM-RETURNED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-RETURNED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4260-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-REASON-CD
                   ADD BPYM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4270-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DA -> FP WHEN NOTICE DATE IS PAST
      * FP -> JH WHEN NOTICE DATE IS PAST
      * JH -> JV WHEN NOTICE DATE IS PAST
      * JV -> RM WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4280-SET-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FP' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FP'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JH' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JH'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JV' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JV'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4290-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF BPYM-LAST-ACT-DATE > 0 AND BPYM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4300-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * BT -> FR WHEN NOTICE DATE IS PAST
      * FR -> LL WHEN NOTICE DATE IS PAST
      * LL -> US WHEN NOTICE DATE IS PAST
      * US -> XM WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4310-SET-PAYEE-TYPE.
           EVALUATE BPYM-PAYEE-TYPE-CD
               WHEN 'BT'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FR' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FR'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LL' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LL'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'US' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REFUND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       4320-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-PAYMENT-AMT ROUNDED =
                   BPYT-REFUND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-PAYMENT-AMT
           END-IF
           ADD BPYM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       4330-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-PAYMENT-AMT ROUNDED =
                   BPYM-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYT-PAYMENT-AMT
           END-IF
           ADD BPYT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE REFUND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       4340-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-FEE-AMT ROUNDED =
                   BPYT-REFUND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-FEE-AMT
           END-IF
           ADD BPYM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4350-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> BX WHEN CLOSE DATE IS PAST
      * BX -> GM WHEN CLOSE DATE IS PAST
      * GM -> KU WHEN CLOSE DATE IS PAST
      * KU -> LT WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4360-SET-REASON.
           EVALUATE BPYM-REASON-CD
               WHEN 'AX'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BX' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BX'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KU' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KU'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LT' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4370-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF BPYM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4380-FLAG-PAYMENT.
           IF BPYM-PAYMENT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4390-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * BPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BPYT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4400-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE BPYM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4410-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPYT-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BPYT-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE BPYT-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4420-DESCRIBE-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED FEE ON THE REFUND AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4430-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BPYT-RETURNED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE BPYT-RETURNED-AMT ROUNDED = BPYT-REFUND-AMT *
                       BPYM-MARGIN / 200
               WHEN BPYM-REMIT-METHOD-CD = 'CGV'
                   MOVE 10.00 TO BPYT-RETURNED-AMT
               WHEN BPYM-REMIT-METHOD-CD = 'FNS'
                   MOVE 5.00 TO BPYT-RETURNED-AMT
               WHEN OTHER
                   COMPUTE BPYT-RETURNED-AMT ROUNDED = BPYT-REFUND-AMT *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF BPYT-RETURNED-AMT > 0 AND BPYT-RETURNED-AMT < 5.00
               MOVE 5.00 TO BPYT-RETURNED-AMT
           END-IF
           IF BPYT-RETURNED-AMT > 25.00
               MOVE 25.00 TO BPYT-RETURNED-AMT
           END-IF
           ADD BPYT-RETURNED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE RETURNED AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4440-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BPYM-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE BPYM-PAYMENT-AMT ROUNDED = BPYT-RETURNED-AMT
                       * BPYM-RATE / 200
               WHEN BPYT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO BPYM-PAYMENT-AMT
               WHEN BPYT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO BPYM-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE BPYM-PAYMENT-AMT ROUNDED = BPYT-RETURNED-AMT
                       * BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-PAYMENT-AMT > 0 AND BPYM-PAYMENT-AMT < 2.00
               MOVE 2.00 TO BPYM-PAYMENT-AMT
           END-IF
           IF BPYM-PAYMENT-AMT > 50.00
               MOVE 50.00 TO BPYM-PAYMENT-AMT
           END-IF
           ADD BPYM-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4450-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-TIER-RATE(1) TO BPYM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-RETURNED-TIER-LOW(WS-TX)
                   MOVE WS-RETURNED-TIER-RATE(WS-TX) TO BPYM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E148 CONTACT REQUIRED
      * E980 FEE YTD NEGATIVE
      * E821 FEE YTD OVER 50000
      * E331 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4460-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-CONTACT = SPACES
                   MOVE 'E148' TO WS-REJECT-CODE
               WHEN BPYM-FEE-YTD < 0
                   MOVE 'E980' TO WS-REJECT-CODE
               WHEN BPYM-FEE-YTD > 50000.00
                   MOVE 'E821' TO WS-REJECT-CODE
               WHEN BPYT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E331' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4470-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4480-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4490-COUNT-CYCLE.
           IF BPYM-CYCLE-CNT < 9999999
               ADD 1 TO BPYM-CYCLE-CNT
           END-IF
           IF BPYM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE REFUND YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4500-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BPYT-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-HOLD-FLAG = 'Y'
                   COMPUTE BPYT-PAYMENT-AMT ROUNDED = BPYM-REFUND-YTD *
                       BPYM-RATE / 200
               WHEN BPYT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO BPYT-PAYMENT-AMT
               WHEN BPYT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO BPYT-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE BPYT-PAYMENT-AMT ROUNDED = BPYM-REFUND-YTD *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYT-PAYMENT-AMT > 0 AND BPYT-PAYMENT-AMT < 5.00
               MOVE 5.00 TO BPYT-PAYMENT-AMT
           END-IF
           IF BPYT-PAYMENT-AMT > 75.00
               MOVE 75.00 TO BPYT-PAYMENT-AMT
           END-IF
           ADD BPYT-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REMIT METHOD CD:
      * CGV -> FNS WHEN EFFECTIVE DATE IS PAST
      * FNS -> NMS WHEN EFFECTIVE DATE IS PAST
      * NMS -> TAF WHEN EFFECTIVE DATE IS PAST
      * TAF -> VNK WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4510-SET-REMIT-METHOD.
           EVALUATE BPYM-REMIT-METHOD-CD
               WHEN 'CGV'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FNS' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNS'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NMS' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NMS'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TAF' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TAF'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VNK' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETURNED AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4520-FLAG-RETURNED.
           IF BPYT-RETURNED-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYT-RETURNED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4530-COUNT-SEQ.
           IF BPYT-SEQ < 9999999
               ADD 1 TO BPYT-SEQ
           END-IF
           IF BPYT-SEQ NOT < 12
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4540-FLAG-FEE.
           IF BPYM-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4550-ACCRUE-REFUND.
           IF BPYM-REFUND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-REFUND-YTD * BPYM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BPYM-REFUND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4560-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4570-ACCRUE-REFUND.
           IF BPYM-REFUND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-REFUND-AMT * BPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BPYT-REFUND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4580-FLAG-FEE.
           IF BPYT-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4590-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-EFFECTIVE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4600-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND SCHED TIER FOR THE RETURNED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4610-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-SCHED-RATE(1) TO BPYM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BPYM-RETURNED-AMT NOT < WS-REFUND-SCHED-LOW(WS-TX)
                   MOVE WS-REFUND-SCHED-RATE(WS-TX) TO BPYM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REFUND AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4620-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = BPYT-REFUND-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BPYT-REFUND-AMT - (WS-WORK-AMT * 4)
           COMPUTE BPYT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4630-FLAG-REFUND.
           IF BPYT-REFUND-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYT-REFUND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4640-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4650-ACCRUE-FEE.
           IF BPYT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-FEE-AMT * BPYM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BPYT-RETURNED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4660-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-SCHED-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT-2 NOT < WS-REFUND-SCHED-LOW(WS-TX)
                   MOVE WS-REFUND-SCHED-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4670-DESCRIBE-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE REFUND AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4680-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BPYM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BPYT-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE BPYT-REFUND-AMT ROUNDED = BPYM-REFUND-AMT *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYM-CYCLE-CD = 'DA'
                   MOVE 20.00 TO BPYT-REFUND-AMT
               WHEN BPYM-CYCLE-CD = 'FP'
                   MOVE 15.00 TO BPYT-REFUND-AMT
               WHEN OTHER
                   COMPUTE BPYT-REFUND-AMT ROUNDED = BPYM-REFUND-AMT *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYT-REFUND-AMT > 0 AND BPYT-REFUND-AMT < 10.00
               MOVE 10.00 TO BPYT-REFUND-AMT
           END-IF
           IF BPYT-REFUND-AMT > 25.00
               MOVE 25.00 TO BPYT-REFUND-AMT
           END-IF
           ADD BPYT-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETURNED AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4690-FLAG-RETURNED.
           IF BPYT-RETURNED-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYT-RETURNED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETURNED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4700-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   BPYT-RETURNED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN MATURITY DATE IS PAST
      * BAT -> MAL WHEN MATURITY DATE IS PAST
      * MAL -> TEL WHEN MATURITY DATE IS PAST
      * TEL -> ONL WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4710-SET-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4720-DESCRIBE-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4730-ACCRUE-REFUND.
           IF BPYM-REFUND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-REFUND-YTD * BPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BPYT-REFUND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REFUND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETURNED AMT.
      *----------------------------------------------------------------*
       4740-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-RETURNED-AMT ROUNDED =
                   BPYM-REFUND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYT-RETURNED-AMT
           END-IF
           ADD BPYT-RETURNED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E856 MEMO REQUIRED
      * E821 REFUND AMT NEGATIVE
      * E612 REFUND AMT OVER 5000
      * E380 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4750-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-MEMO = SPACES
                   MOVE 'E856' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-AMT < 0
                   MOVE 'E821' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-AMT > 5000.00
                   MOVE 'E612' TO WS-REJECT-CODE
               WHEN BPYM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E380' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE REFUND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4760-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF BPYM-REFUND-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E090 NAME REQUIRED
      * E170 PAYMENT AMT NEGATIVE
      * E308 PAYMENT AMT OVER 25000
      * E458 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4770-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-NAME = SPACES
                   MOVE 'E090' TO WS-REJECT-CODE
               WHEN BPYM-PAYMENT-AMT < 0
                   MOVE 'E170' TO WS-REJECT-CODE
               WHEN BPYM-PAYMENT-AMT > 25000.00
                   MOVE 'E308' TO WS-REJECT-CODE
               WHEN BPYM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E458' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       4780-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-FEE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-FEE-YTD
           END-IF
           ADD BPYM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PASS THE BPY FIELDS TO BPSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-BPSTAT.
           INITIALIZE BPSTAT-PARMS
           MOVE BPYM-PAYMENT-AMT
             TO BPSTAT-PAYMENT-AMT
           MOVE BPYM-RETURNED-AMT
             TO BPSTAT-RETURNED-AMT
           MOVE BPYM-REFUND-AMT
             TO BPSTAT-REFUND-AMT
           MOVE BPYM-PAYMENT-YTD
             TO BPSTAT-PAYMENT-YTD
           MOVE BPYM-RETURNED-YTD
             TO BPSTAT-RETURNED-YTD
           MOVE BPYM-FEE-AMT
             TO BPSTAT-FEE-AMT
           MOVE BPYM-OPEN-DATE
             TO BPSTAT-OPEN-DATE
           MOVE BPYM-MATURITY-DATE
             TO BPSTAT-MATURITY-DATE
           MOVE BPYM-LAST-STMT-DATE
             TO BPSTAT-LAST-STMT-DATE
           MOVE BPYM-CLOSE-DATE
             TO BPSTAT-CLOSE-DATE
           MOVE BPYM-CYCLE-CD
             TO BPSTAT-CYCLE-CD
           MOVE BPYM-REMIT-METHOD-CD
             TO BPSTAT-REMIT-METHOD-CD
           MOVE BPYM-STATUS-CD
             TO BPSTAT-STATUS-CD
           MOVE BPYM-REASON-CD
             TO BPSTAT-REASON-CD
           MOVE BPYM-AUTO-RENEW-FLAG
             TO BPSTAT-AUTO-RENEW-FLAG
           MOVE BPYM-EXEMPT-FLAG
             TO BPSTAT-EXEMPT-FLAG
           MOVE BPYM-SUSPEND-FLAG
             TO BPSTAT-SUSPEND-FLAG
           MOVE BPYM-PAYMENT-CNT
             TO BPSTAT-PAYMENT-CNT
           MOVE BPYM-EXCEPT-CNT
             TO BPSTAT-EXCEPT-CNT
           MOVE BPYM-PENALTY-PCT
             TO BPSTAT-PENALTY-PCT
           MOVE BPYM-RATE
             TO BPSTAT-RATE
           MOVE BPYM-REF-NO
             TO BPSTAT-REF-NO
           MOVE BPYM-CUST-ID
             TO BPSTAT-CUST-ID
           CALL 'BPSTAT' USING BPSTAT-PARMS
           EVALUATE BPSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE BPSTAT-PAYMENT-AMT
                     TO BPYM-PAYMENT-AMT
                   MOVE BPSTAT-PAYMENT-YTD
                     TO BPYM-PAYMENT-YTD
                   MOVE BPSTAT-RETURNED-YTD
                     TO BPYM-RETURNED-YTD
                   MOVE BPSTAT-SUSPEND-FLAG
                     TO BPYM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-BPSTAT' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE BPYT-KEY TO RD-KEY
           MOVE BPYT-TRAN-CD TO RD-CODE
           MOVE BPYT-PAYMENT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE BPYT-KEY TO RD-KEY
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
           DISPLAY 'BPYCONV READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'BPYCONV' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
