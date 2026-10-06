      *================================================================*
      * PROGRAM  : CRCEXCP                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PROPRIETARY CREDIT CARD - EXCEPTION PROCESSING.     *
      *            READS THE CRC TRANSACTIONS, UPDATES THE CRC MASTER  *
      *            AND PRODUCES THE CONTROL REPORT. IN-HOUSE CREDIT    *
      *            CARD BILLING, FINANCE CHARGE AND DELINQUENCY        *
      * INPUT    : CRCMST (KSDS), CRCTRN, BANKCTL                      *
      * OUTPUT   : CRCOUT, CRCEXRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CRTIER, CRELG, CRCAL    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-09-10 PQR  ORIGINAL                                       *
      * 1996-04-18 PQR  AUDIT FINDING - LATE-FEE CONTROL TOTALS REQ7399*
      * 2001-02-22 TWH  LATE-FEE LIMIT REVISED PRB3418                 *
      * 2002-11-26 LMB  NEW FINANCE REPORT COLUMNS REQ1214             *
      * 2003-04-19 MKL  CASH-ADV EXCEPTIONS TO OPS QUEUE REQ5830       *
      * 2003-11-16 CRP  FINANCE CUTOFF MOVED PRB9208                   *
      * 2007-08-15 PQR  NEW MIN-DUE REPORT COLUMNS CHG6905             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRCEXCP.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "CRCMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CRCM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "CRCTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "CRCOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "CRCEXRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY CRCMST.
       FD  TRAN-FILE.
       COPY CRCTRN.
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
                                           VALUE 21.000000.
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
      * WS-LIMIT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-LIMIT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000311000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000741000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000257500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011050000000189000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021050000000614000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026050000000585500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036050000000228000'.
       01  WS-LIMIT-SCHED-TBL REDEFINES WS-LIMIT-SCHED-VALUES.
           05  WS-LIMIT-SCHED-ROW OCCURS 7 TIMES.
               10  WS-LIMIT-SCHED-LOW      PIC 9(09)V99.
               10  WS-LIMIT-SCHED-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PURCHASE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PURCHASE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000335000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000562500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000046000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000559500'.
       01  WS-PURCHASE-GRID-TBL REDEFINES WS-PURCHASE-GRID-VALUES.
           05  WS-PURCHASE-GRID-ROW OCCURS 4 TIMES.
               10  WS-PURCHASE-GRID-LOW    PIC 9(09)V99.
               10  WS-PURCHASE-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-MIN-DUE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MIN-DUE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000100000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000135000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000705000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002700000000270000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002950000000595000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007950000000321500'.
       01  WS-MIN-DUE-TIER-TBL REDEFINES WS-MIN-DUE-TIER-VALUES.
           05  WS-MIN-DUE-TIER-ROW OCCURS 6 TIMES.
               10  WS-MIN-DUE-TIER-LOW     PIC 9(09)V99.
               10  WS-MIN-DUE-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-MIN-DUE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MIN-DUE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000719500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000590000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000810000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010350000000431000'.
       01  WS-MIN-DUE-SCHED-TBL REDEFINES WS-MIN-DUE-SCHED-VALUES.
           05  WS-MIN-DUE-SCHED-ROW OCCURS 4 TIMES.
               10  WS-MIN-DUE-SCHED-LOW    PIC 9(09)V99.
               10  WS-MIN-DUE-SCHED-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CRCEXCP  PROPRIETARY CREDIT CARD'.
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
       COPY CRTIERP.
       COPY CRELGP.
       COPY CRCALP.

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
               MOVE 'CRCMST' TO ABND-FILE-NAME
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
           MOVE CRCT-KEY TO CRCM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-LIMIT-LATE-FEE
               PERFORM 3010-ACCRUE-FINANCE
               PERFORM 3020-ACCUM-SOURCE
               PERFORM 3030-ASSESS-NOTICE-SENT
               PERFORM 3040-ASSESS-REVERSAL
               PERFORM 3050-LIMIT-PAYMENT
               PERFORM 3060-COUNT-RETRY
               PERFORM 3070-SET-BRANCH
               PERFORM 3080-SPLIT-MIN-DUE
               PERFORM 3090-LIMIT-PURCHASE
               PERFORM 3100-FLAG-LATE-FEE
               PERFORM 3110-DESCRIBE-BILL-CYCLE
               PERFORM 3120-AGE-LAST-PAY
               PERFORM 3130-ACCRUE-PAYMENT
               PERFORM 3140-ACCUM-TRAN
               PERFORM 3150-SELECT-RATE
               PERFORM 3160-LIMIT-CASH-ADV
               PERFORM 3170-FLAG-LATE-FEE
               PERFORM 3180-ACCUM-BRANCH
               PERFORM 3190-COUNT-SEQ
               PERFORM 3200-SET-BILL-CYCLE
               PERFORM 3210-CHECK-EFF
               PERFORM 3220-AGE-NOTICE
               PERFORM 3230-COUNT-RETRY
               PERFORM 3240-CHECK-LAST-ACT
               PERFORM 3250-AGE-LAST-PAY
               PERFORM 3260-PRORATE-LIMIT
               PERFORM 3270-SELECT-DISCOUNT
               PERFORM 3280-DESCRIBE-BILL-CYCLE
               PERFORM 3290-ACCUM-STATUS
               PERFORM 3300-PRORATE-PAYMENT
               PERFORM 3310-CHECK-LAST-PAY
               PERFORM 3320-COUNT-RETRY
               PERFORM 3330-ACCRUE-LIMIT
               PERFORM 3340-LIMIT-LIMIT
               PERFORM 3350-SPLIT-LATE-FEE
               PERFORM 3360-EDIT-NAME
               PERFORM 3370-ACCRUE-PAYMENT
               PERFORM 3380-ACCUM-SOURCE
               PERFORM 3390-LIMIT-CASH-ADV
               PERFORM 3400-PRORATE-PAYMENT
               PERFORM 3410-SET-STATUS
               PERFORM 3420-DESCRIBE-TRAN
               PERFORM 3430-ACCUM-SOURCE
               PERFORM 3440-ASSESS-REVERSAL
               PERFORM 3450-SPLIT-CASH-ADV
               PERFORM 3460-CHECK-NOTICE
               PERFORM 3470-PRORATE-PURCHASE
               PERFORM 3480-LIMIT-MIN-DUE
               PERFORM 3490-PRORATE-PAYMENT
               PERFORM 3500-LIMIT-ITEM
               PERFORM 3510-EDIT-ADDR
               PERFORM 3520-CHECK-OPEN
               PERFORM 3530-PRORATE-LATE-FEE
               PERFORM 3540-LIMIT-PAYMENT
               PERFORM 3550-AGE-LAST-ACT
               PERFORM 3560-ACCRUE-MIN-DUE
               PERFORM 3570-AGE-EXPIRY
               PERFORM 3580-FLAG-MIN-DUE
               PERFORM 3590-DESCRIBE-SOURCE
               PERFORM 3600-AGE-EFFECTIVE
               PERFORM 3610-EDIT-NAME
               PERFORM 3620-SET-CARD-TYPE
               PERFORM 3630-FLAG-MIN-DUE
               PERFORM 3640-PRORATE-LIMIT
               PERFORM 3650-SET-TRAN
               PERFORM 3660-CHECK-CLOSE
               PERFORM 3670-LIMIT-PAYMENT
               PERFORM 3680-FLAG-LATE-FEE
               PERFORM 3690-SET-TYPE
               PERFORM 3700-SET-TRAN
               PERFORM 3710-FLAG-PAYMENT
               PERFORM 3720-PRORATE-PURCHASE
               PERFORM 3730-COUNT-RETRY
               PERFORM 3740-SPLIT-LATE-FEE
               PERFORM 3750-PRORATE-MIN-DUE
               PERFORM 3760-PRORATE-LATE-FEE
               PERFORM 3770-EDIT-DESC
               PERFORM 3780-ACCRUE-MIN-DUE
               PERFORM 3790-EDIT-NAME
               PERFORM 3800-FLAG-CASH-ADV
               PERFORM 3810-SPLIT-LATE-FEE
               PERFORM 3820-COUNT-ITEM
               PERFORM 3830-FLAG-ITEM
               PERFORM 3840-FLAG-CASH-ADV
               PERFORM 3850-EDIT-DESC
               PERFORM 3860-EDIT-REJECT
               PERFORM 3870-EDIT-REJECT
               PERFORM 3880-FLAG-MIN-DUE
               PERFORM 3890-FLAG-LATE-FEE
               PERFORM 3900-ASSESS-STOP
               PERFORM 3910-ACCRUE-RATE
               PERFORM 3920-DESCRIBE-SOURCE
               PERFORM 3930-SELECT-PENALTY-PCT
               PERFORM 3940-SET-SOURCE
               PERFORM 3950-ACCRUE-PAYMENT
               PERFORM 3960-SELECT-DISCOUNT
               PERFORM 3970-SET-STATUS
               PERFORM 3980-FLAG-PAYMENT
               PERFORM 3990-AGE-LAST-ACT
               PERFORM 4000-ACCRUE-LIMIT
               PERFORM 4010-DESCRIBE-TYPE
               PERFORM 4020-FLAG-PAYMENT
               PERFORM 4030-COUNT-SEQ
               PERFORM 4040-ACCUM-CHANNEL
               PERFORM 4050-COUNT-ITEM
               PERFORM 4060-AGE-EFFECTIVE
               PERFORM 4070-FLAG-PAYMENT
               PERFORM 4080-SELECT-RATE
               PERFORM 4090-EDIT-NAME
               PERFORM 4100-FLAG-LIMIT
               PERFORM 4110-CHECK-OPEN
               PERFORM 4120-DESCRIBE-TRAN
               PERFORM 4130-ACCUM-CARD-TYPE
               PERFORM 4140-CHECK-EFFECTIVE
               PERFORM 4150-PRORATE-LIMIT
               PERFORM 4160-ASSESS-AUTO-RENEW
               PERFORM 4170-ASSESS-REVERSAL
               PERFORM 4180-CHECK-LAST-PAY
               PERFORM 4190-AGE-EFF
               PERFORM 4200-EDIT-CUST-ID
               PERFORM 4210-SET-CHANNEL
               PERFORM 4220-EDIT-OPERATOR
               PERFORM 4230-LIMIT-PURCHASE
               PERFORM 4240-LIMIT-LIMIT
               PERFORM 4250-ACCRUE-PURCHASE
               PERFORM 4260-SPLIT-CASH-ADV
               PERFORM 4270-AGE-OPEN
               PERFORM 4280-SET-SOURCE
               PERFORM 4290-DESCRIBE-CARD-TYPE
               PERFORM 4300-SELECT-PENALTY-PCT
               PERFORM 4310-CHECK-OPEN
               PERFORM 4320-CHECK-NOTICE
               PERFORM 4330-AGE-NOTICE
               PERFORM 4340-ASSESS-REVERSAL
               PERFORM 4350-ASSESS-EXEMPT
               PERFORM 4360-FLAG-FINANCE
               PERFORM 4370-ASSESS-NOTICE-SENT
               PERFORM 4380-ACCRUE-FINANCE
               PERFORM 4390-SET-TRAN
               PERFORM 4400-FLAG-CASH-ADV
               PERFORM 4410-LIMIT-ITEM
               PERFORM 4420-AGE-LAST-PAY
               PERFORM 4430-SPLIT-PURCHASE
               PERFORM 4440-EDIT-CONTACT
               PERFORM 4450-AGE-NOTICE
               PERFORM 4460-SET-BILL-CYCLE
               PERFORM 4470-AGE-LAST-ACT
               PERFORM 4480-COUNT-CYCLE
               PERFORM 4490-SET-TRAN
               PERFORM 4500-SPLIT-LIMIT
               PERFORM 4510-ACCRUE-CASH-ADV
               PERFORM 4520-ACCUM-CARD-TYPE
               PERFORM 4530-ACCRUE-CASH-ADV
               PERFORM 4540-PRORATE-PAYMENT
               PERFORM 4550-LIMIT-MIN-DUE
               PERFORM 4560-AGE-EFF
               PERFORM 4570-FLAG-MIN-DUE
               PERFORM 4580-ACCRUE-LATE-FEE
               PERFORM 4590-ACCRUE-DISCOUNT
               PERFORM 4600-SELECT-DISCOUNT
               PERFORM 4610-SET-CARD-TYPE
               PERFORM 4620-PRORATE-PAYMENT
               PERFORM 4630-ACCRUE-MIN-DUE
               PERFORM 4640-EDIT-MEMO
               PERFORM 4650-AGE-OPEN
               PERFORM 4660-FLAG-PAYMENT
               PERFORM 4670-EDIT-REF-NO
               PERFORM 4680-FLAG-ITEM
               PERFORM 4690-SELECT-DISCOUNT
               PERFORM 4700-EDIT-ADDR
               PERFORM 4710-SPLIT-FINANCE
               PERFORM 4720-FLAG-PURCHASE
               PERFORM 7000-CALL-CRTIER
               PERFORM 7030-CALL-CRELG
               PERFORM 7040-CALL-CRCAL
               REWRITE CRC-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'CRCMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM CRC-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * LATE FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3000-LIMIT-LATE-FEE.
           IF CRCM-LATE-FEE-AMT < 0.00
               MOVE 0.00 TO CRCM-LATE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-LATE-FEE-AMT > 50000.00
               MOVE 50000.00 TO CRCM-LATE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FINANCE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-FINANCE.
           IF CRCM-FINANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-FINANCE-YTD * CRCM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3020-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-SOURCE-CD
                   ADD CRCM-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FINANCE YTD FEE ON THE PAYMENT YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3030-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN CRCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO CRCM-FINANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   COMPUTE CRCM-FINANCE-YTD ROUNDED = CRCM-PAYMENT-YTD *
                       CRCM-DISCOUNT / 200
               WHEN CRCM-BRANCH-CD = 'CTB'
                   MOVE 3.00 TO CRCM-FINANCE-YTD
               WHEN CRCM-BRANCH-CD = 'LNX'
                   MOVE 5.00 TO CRCM-FINANCE-YTD
               WHEN OTHER
                   COMPUTE CRCM-FINANCE-YTD ROUNDED = CRCM-PAYMENT-YTD *
                       CRCM-DISCOUNT / 100
           END-EVALUATE
           IF CRCM-FINANCE-YTD > 0 AND CRCM-FINANCE-YTD < 2.00
               MOVE 2.00 TO CRCM-FINANCE-YTD
           END-IF
           IF CRCM-FINANCE-YTD > 75.00
               MOVE 75.00 TO CRCM-FINANCE-YTD
           END-IF
           ADD CRCM-FINANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE LIMIT YTD FEE ON THE FINANCE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3040-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CRCM-LIMIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   COMPUTE CRCM-LIMIT-YTD ROUNDED = CRCM-FINANCE-AMT *
                       CRCM-RATE / 200
               WHEN CRCM-TYPE-CD = 'GP'
                   MOVE 15.00 TO CRCM-LIMIT-YTD
               WHEN CRCM-TYPE-CD = 'UV'
                   MOVE 12.00 TO CRCM-LIMIT-YTD
               WHEN OTHER
                   COMPUTE CRCM-LIMIT-YTD ROUNDED = CRCM-FINANCE-AMT *
                       CRCM-RATE / 100
           END-EVALUATE
           IF CRCM-LIMIT-YTD > 0 AND CRCM-LIMIT-YTD < 1.00
               MOVE 1.00 TO CRCM-LIMIT-YTD
           END-IF
           IF CRCM-LIMIT-YTD > 25.00
               MOVE 25.00 TO CRCM-LIMIT-YTD
           END-IF
           ADD CRCM-LIMIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3050-LIMIT-PAYMENT.
           IF CRCT-PAYMENT-AMT < 25.00
               MOVE 25.00 TO CRCT-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCT-PAYMENT-AMT > 10000.00
               MOVE 10000.00 TO CRCT-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3060-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 3
               MOVE 'Y' TO CRCM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CTB -> LNX WHEN MATURITY DATE IS PAST
      * LNX -> MGS WHEN MATURITY DATE IS PAST
      * MGS -> YNJ WHEN MATURITY DATE IS PAST
      * YNJ -> CTB WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3070-SET-BRANCH.
           EVALUATE CRCM-BRANCH-CD
               WHEN 'CTB'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LNX' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LNX'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MGS' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MGS'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YNJ' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YNJ'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CTB' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT MIN DUE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3080-SPLIT-MIN-DUE.
           COMPUTE WS-WORK-AMT = CRCM-MIN-DUE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CRCM-MIN-DUE-AMT - (WS-WORK-AMT * 2)
           COMPUTE CRCT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3090-LIMIT-PURCHASE.
           IF CRCM-PURCHASE-AMT < 50.00
               MOVE 50.00 TO CRCM-PURCHASE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-PURCHASE-AMT > 50000.00
               MOVE 50000.00 TO CRCM-PURCHASE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LATE FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3100-FLAG-LATE-FEE.
           IF CRCM-LATE-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRCM-LATE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BILL CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3110-DESCRIBE-BILL-CYCLE.
           EVALUATE CRCM-BILL-CYCLE-CD
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'D'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'E'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'F'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3120-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3130-ACCRUE-PAYMENT.
           IF CRCT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCT-PAYMENT-AMT * CRCM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CRCM-PURCHASE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3140-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCT-TRAN-CD
                   ADD CRCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN DUE SCHED TIER FOR THE FINANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-DUE-SCHED-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRCM-FINANCE-AMT NOT < WS-MIN-DUE-SCHED-LOW(WS-TX)
                   MOVE WS-MIN-DUE-SCHED-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH ADV AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3160-LIMIT-CASH-ADV.
           IF CRCT-CASH-ADV-AMT < 100.00
               MOVE 100.00 TO CRCT-CASH-ADV-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCT-CASH-ADV-AMT > 5000.00
               MOVE 5000.00 TO CRCT-CASH-ADV-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LATE FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3170-FLAG-LATE-FEE.
           IF CRCM-LATE-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRCM-LATE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PURCHASE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3180-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-BRANCH-CD
                   ADD CRCM-PURCHASE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-PURCHASE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3190-COUNT-SEQ.
           IF CRCT-SEQ < 9999999
               ADD 1 TO CRCT-SEQ
           END-IF
           IF CRCT-SEQ NOT < 10
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BILL CYCLE CD:
      * B -> D WHEN MATURITY DATE IS PAST
      * D -> E WHEN MATURITY DATE IS PAST
      * E -> F WHEN MATURITY DATE IS PAST
      * F -> H WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3200-SET-BILL-CYCLE.
           EVALUATE CRCM-BILL-CYCLE-CD
               WHEN 'B'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3210-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF CRCT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CRCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3220-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3230-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 3
               MOVE 'Y' TO CRCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3240-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CRCM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3250-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE LIMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3260-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-LIMIT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE MIN DUE SCHED TIER FOR THE LIMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-DUE-SCHED-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRCM-LIMIT-AMT NOT < WS-MIN-DUE-SCHED-LOW(WS-TX)
                   MOVE WS-MIN-DUE-SCHED-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BILL CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3280-DESCRIBE-BILL-CYCLE.
           EVALUATE CRCM-BILL-CYCLE-CD
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'E'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'P'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3290-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-STATUS-CD
                   ADD CRCT-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCT-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH ADV YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3300-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCT-PAYMENT-AMT ROUNDED =
                   CRCM-CASH-ADV-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCT-PAYMENT-AMT
           END-IF
           ADD CRCT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3310-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3320-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 6
               MOVE 'Y' TO CRCM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LIMIT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3330-ACCRUE-LIMIT.
           IF CRCM-LIMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-LIMIT-AMT * CRCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO CRCT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3340-LIMIT-LIMIT.
           IF CRCM-LIMIT-AMT < 0.00
               MOVE 0.00 TO CRCM-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-LIMIT-AMT > 50000.00
               MOVE 50000.00 TO CRCM-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3350-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = CRCM-LATE-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CRCM-LATE-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE CRCM-FINANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E784 NAME REQUIRED
      * E901 PAYMENT AMT NEGATIVE
      * E439 PAYMENT AMT OVER 10000
      * E870 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-NAME = SPACES
                   MOVE 'E784' TO WS-REJECT-CODE
               WHEN CRCM-PAYMENT-AMT < 0
                   MOVE 'E901' TO WS-REJECT-CODE
               WHEN CRCM-PAYMENT-AMT > 10000.00
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN CRCM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E870' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3370-ACCRUE-PAYMENT.
           IF CRCM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PAYMENT-YTD * CRCM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CRCT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3380-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-SOURCE-CD
                   ADD CRCM-LATE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-LATE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASH ADV AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3390-LIMIT-CASH-ADV.
           IF CRCT-CASH-ADV-AMT < 50.00
               MOVE 50.00 TO CRCT-CASH-ADV-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCT-CASH-ADV-AMT > 1000.00
               MOVE 1000.00 TO CRCT-CASH-ADV-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH ADV AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3400-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCT-PAYMENT-AMT ROUNDED =
                   CRCM-CASH-ADV-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCT-PAYMENT-AMT
           END-IF
           ADD CRCT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * BAL -> DDY WHEN EFF DATE IS PAST
      * DDY -> DPB WHEN EFF DATE IS PAST
      * DPB -> UHF WHEN EFF DATE IS PAST
      * UHF -> WLL WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3410-SET-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DDY' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DDY'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DPB' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DPB'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UHF' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UHF'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLL' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3420-DESCRIBE-TRAN.
           EVALUATE CRCT-TRAN-CD
               WHEN 'PU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'CA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-SOURCE-CD
                   ADD CRCM-LIMIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-LIMIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE PAYMENT AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3440-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CRCT-PAYMENT-AMT *
                       CRCM-PENALTY-PCT / 200
               WHEN CRCM-STATUS-CD = 'BAL'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN CRCM-STATUS-CD = 'DDY'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CRCT-PAYMENT-AMT *
                       CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT CASH ADV YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3450-SPLIT-CASH-ADV.
           COMPUTE WS-WORK-AMT = CRCM-CASH-ADV-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CRCM-CASH-ADV-YTD - (WS-WORK-AMT *
               12)
           COMPUTE CRCT-MIN-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3460-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO CRCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PURCHASE AMT.
      *----------------------------------------------------------------*
       3470-PRORATE-PURCHASE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-PURCHASE-AMT ROUNDED =
                   CRCM-LATE-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-PURCHASE-AMT
           END-IF
           ADD CRCM-PURCHASE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * MIN DUE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3480-LIMIT-MIN-DUE.
           IF CRCT-MIN-DUE-AMT < 10.00
               MOVE 10.00 TO CRCT-MIN-DUE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCT-MIN-DUE-AMT > 50000.00
               MOVE 50000.00 TO CRCT-MIN-DUE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       3490-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-PAYMENT-YTD ROUNDED =
                   CRCT-LIMIT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-PAYMENT-YTD
           END-IF
           ADD CRCM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3500-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E660 ADDR REQUIRED
      * E928 MIN DUE AMT NEGATIVE
      * E509 MIN DUE AMT OVER 10000
      * E564 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3510-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-ADDR = SPACES
                   MOVE 'E660' TO WS-REJECT-CODE
               WHEN CRCM-MIN-DUE-AMT < 0
                   MOVE 'E928' TO WS-REJECT-CODE
               WHEN CRCM-MIN-DUE-AMT > 10000.00
                   MOVE 'E509' TO WS-REJECT-CODE
               WHEN CRCM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E564' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3520-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CRCM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CRCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LATE FEE AMT.
      *----------------------------------------------------------------*
       3530-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-LATE-FEE-AMT ROUNDED =
                   CRCM-LATE-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-LATE-FEE-AMT
           END-IF
           ADD CRCM-LATE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3540-LIMIT-PAYMENT.
           IF CRCM-PAYMENT-YTD < 50.00
               MOVE 50.00 TO CRCM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-PAYMENT-YTD > 1000.00
               MOVE 1000.00 TO CRCM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3550-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN DUE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3560-ACCRUE-MIN-DUE.
           IF CRCM-MIN-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-MIN-DUE-AMT * CRCM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CRCM-LIMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3570-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-EXPIRY-DATE > 0 AND CRCM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-MIN-DUE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-MIN-DUE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-MIN-DUE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-MIN-DUE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-MIN-DUE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MIN DUE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3580-FLAG-MIN-DUE.
           IF CRCM-MIN-DUE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRCM-MIN-DUE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3590-DESCRIBE-SOURCE.
           EVALUATE CRCM-SOURCE-CD
               WHEN 'C'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3600-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E843 NAME REQUIRED
      * E728 CASH ADV AMT NEGATIVE
      * E409 CASH ADV AMT OVER 5000
      * E979 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3610-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-NAME = SPACES
                   MOVE 'E843' TO WS-REJECT-CODE
               WHEN CRCT-CASH-ADV-AMT < 0
                   MOVE 'E728' TO WS-REJECT-CODE
               WHEN CRCT-CASH-ADV-AMT > 5000.00
                   MOVE 'E409' TO WS-REJECT-CODE
               WHEN CRCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E979' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARD TYPE CD:
      * PGU -> TRN WHEN LAST PAY DATE IS PAST
      * TRN -> WHR WHEN LAST PAY DATE IS PAST
      * WHR -> ZWB WHEN LAST PAY DATE IS PAST
      * ZWB -> PGU WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3620-SET-CARD-TYPE.
           EVALUATE CRCM-CARD-TYPE-CD
               WHEN 'PGU'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TRN' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TRN'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WHR' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WHR'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZWB' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZWB'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PGU' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MIN DUE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3630-FLAG-MIN-DUE.
           IF CRCT-MIN-DUE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRCT-MIN-DUE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       3640-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCT-LIMIT-AMT ROUNDED =
                   CRCT-PAYMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCT-LIMIT-AMT
           END-IF
           ADD CRCT-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PU -> CA WHEN EFFECTIVE DATE IS PAST
      * CA -> PY WHEN EFFECTIVE DATE IS PAST
      * PY -> FC WHEN EFFECTIVE DATE IS PAST
      * FC -> LF WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3650-SET-TRAN.
           EVALUATE CRCT-TRAN-CD
               WHEN 'PU'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CA' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CA'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FC' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FC'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LF' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3660-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3670-LIMIT-PAYMENT.
           IF CRCM-PAYMENT-YTD < 25.00
               MOVE 25.00 TO CRCM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-PAYMENT-YTD > 5000.00
               MOVE 5000.00 TO CRCM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LATE FEE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3680-FLAG-LATE-FEE.
           IF CRCM-LATE-FEE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRCM-LATE-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * GP -> UV WHEN EXPIRY DATE IS PAST
      * UV -> YK WHEN EXPIRY DATE IS PAST
      * YK -> GP WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3690-SET-TYPE.
           EVALUATE CRCM-TYPE-CD
               WHEN 'GP'
                   IF CRCM-EXPIRY-DATE > 0 AND CRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UV' TO CRCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF CRCM-EXPIRY-DATE > 0 AND CRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YK' TO CRCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YK'
                   IF CRCM-EXPIRY-DATE > 0 AND CRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GP' TO CRCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PU -> CA WHEN EFFECTIVE DATE IS PAST
      * CA -> PY WHEN EFFECTIVE DATE IS PAST
      * PY -> FC WHEN EFFECTIVE DATE IS PAST
      * FC -> LF WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3700-SET-TRAN.
           EVALUATE CRCT-TRAN-CD
               WHEN 'PU'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CA' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CA'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FC' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FC'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LF' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3710-FLAG-PAYMENT.
           IF CRCM-PAYMENT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRCM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PURCHASE YTD.
      *----------------------------------------------------------------*
       3720-PRORATE-PURCHASE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-PURCHASE-YTD ROUNDED =
                   CRCM-LIMIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-PURCHASE-YTD
           END-IF
           ADD CRCM-PURCHASE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3730-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 10
               MOVE 'Y' TO CRCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3740-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = CRCM-LATE-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CRCM-LATE-FEE-YTD - (WS-WORK-AMT * 3)
           COMPUTE CRCT-LIMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MIN DUE YTD.
      *----------------------------------------------------------------*
       3750-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-MIN-DUE-YTD ROUNDED =
                   CRCT-LIMIT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-MIN-DUE-YTD
           END-IF
           ADD CRCM-MIN-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE FINANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LATE FEE AMT.
      *----------------------------------------------------------------*
       3760-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-LATE-FEE-AMT ROUNDED =
                   CRCM-FINANCE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-LATE-FEE-AMT
           END-IF
           ADD CRCM-LATE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E417 DESC REQUIRED
      * E333 LATE FEE YTD NEGATIVE
      * E036 LATE FEE YTD OVER 99999
      * E161 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3770-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-DESC = SPACES
                   MOVE 'E417' TO WS-REJECT-CODE
               WHEN CRCM-LATE-FEE-YTD < 0
                   MOVE 'E333' TO WS-REJECT-CODE
               WHEN CRCM-LATE-FEE-YTD > 99999.00
                   MOVE 'E036' TO WS-REJECT-CODE
               WHEN CRCM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E161' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN DUE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3780-ACCRUE-MIN-DUE.
           IF CRCT-MIN-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCT-MIN-DUE-AMT * CRCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO CRCM-LIMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E607 NAME REQUIRED
      * E167 CASH ADV YTD NEGATIVE
      * E182 CASH ADV YTD OVER 50000
      * E106 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3790-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-NAME = SPACES
                   MOVE 'E607' TO WS-REJECT-CODE
               WHEN CRCM-CASH-ADV-YTD < 0
                   MOVE 'E167' TO WS-REJECT-CODE
               WHEN CRCM-CASH-ADV-YTD > 50000.00
                   MOVE 'E182' TO WS-REJECT-CODE
               WHEN CRCM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E106' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASH ADV YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3800-FLAG-CASH-ADV.
           IF CRCM-CASH-ADV-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCM-CASH-ADV-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3810-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = CRCM-LATE-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CRCM-LATE-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE CRCM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3820-COUNT-ITEM.
           IF CRCM-ITEM-CNT < 9999999
               ADD 1 TO CRCM-ITEM-CNT
           END-IF
           IF CRCM-ITEM-CNT NOT < 5
               MOVE 'Y' TO CRCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3830-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CASH ADV YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3840-FLAG-CASH-ADV.
           IF CRCM-CASH-ADV-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRCM-CASH-ADV-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E142 DESC REQUIRED
      * E575 LIMIT AMT NEGATIVE
      * E447 LIMIT AMT OVER 50000
      * E271 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3850-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-DESC = SPACES
                   MOVE 'E142' TO WS-REJECT-CODE
               WHEN CRCT-LIMIT-AMT < 0
                   MOVE 'E575' TO WS-REJECT-CODE
               WHEN CRCT-LIMIT-AMT > 50000.00
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN CRCM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E271' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E752 REJECT CD REQUIRED
      * E096 PAYMENT AMT NEGATIVE
      * E210 PAYMENT AMT OVER 99999
      * E724 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3860-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCT-REJECT-CD = SPACES
                   MOVE 'E752' TO WS-REJECT-CODE
               WHEN CRCT-PAYMENT-AMT < 0
                   MOVE 'E096' TO WS-REJECT-CODE
               WHEN CRCT-PAYMENT-AMT > 99999.00
                   MOVE 'E210' TO WS-REJECT-CODE
               WHEN CRCM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E724' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E483 REJECT CD REQUIRED
      * E906 CASH ADV YTD NEGATIVE
      * E840 CASH ADV YTD OVER 250000
      * E148 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3870-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCT-REJECT-CD = SPACES
                   MOVE 'E483' TO WS-REJECT-CODE
               WHEN CRCM-CASH-ADV-YTD < 0
                   MOVE 'E906' TO WS-REJECT-CODE
               WHEN CRCM-CASH-ADV-YTD > 250000.00
                   MOVE 'E840' TO WS-REJECT-CODE
               WHEN CRCM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E148' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * MIN DUE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3880-FLAG-MIN-DUE.
           IF CRCM-MIN-DUE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRCM-MIN-DUE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * LATE FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3890-FLAG-LATE-FEE.
           IF CRCM-LATE-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCM-LATE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE YTD FEE ON THE CASH ADV AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3900-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CRCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO CRCM-PURCHASE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CRCM-PURCHASE-YTD ROUNDED = CRCT-CASH-ADV-AMT
                       * CRCM-RATE / 200
               WHEN CRCT-TRAN-CD = 'PU'
                   MOVE 3.00 TO CRCM-PURCHASE-YTD
               WHEN CRCT-TRAN-CD = 'CA'
                   MOVE 20.00 TO CRCM-PURCHASE-YTD
               WHEN OTHER
                   COMPUTE CRCM-PURCHASE-YTD ROUNDED = CRCT-CASH-ADV-AMT
                       * CRCM-RATE / 100
           END-EVALUATE
           IF CRCM-PURCHASE-YTD > 0 AND CRCM-PURCHASE-YTD < 10.00
               MOVE 10.00 TO CRCM-PURCHASE-YTD
           END-IF
           IF CRCM-PURCHASE-YTD > 35.00
               MOVE 35.00 TO CRCM-PURCHASE-YTD
           END-IF
           ADD CRCM-PURCHASE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3910-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * CRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CRCM-PAYMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3920-DESCRIBE-SOURCE.
           EVALUATE CRCM-SOURCE-CD
               WHEN 'C'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'R'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'S'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'T'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE GRID TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-GRID-RATE(1) TO CRCM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRCT-PAYMENT-AMT NOT < WS-PURCHASE-GRID-LOW(WS-TX)
                   MOVE WS-PURCHASE-GRID-RATE(WS-TX) TO CRCM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * C -> H WHEN LAST PAY DATE IS PAST
      * H -> M WHEN LAST PAY DATE IS PAST
      * M -> R WHEN LAST PAY DATE IS PAST
      * R -> S WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3940-SET-SOURCE.
           EVALUATE CRCM-SOURCE-CD
               WHEN 'C'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3950-ACCRUE-PAYMENT.
           IF CRCM-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PAYMENT-AMT * CRCM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CRCM-LATE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3960-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-SCHED-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT NOT < WS-LIMIT-SCHED-LOW(WS-TX)
                   MOVE WS-LIMIT-SCHED-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * BAL -> DDY WHEN CLOSE DATE IS PAST
      * DDY -> DPB WHEN CLOSE DATE IS PAST
      * DPB -> UHF WHEN CLOSE DATE IS PAST
      * UHF -> WLL WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3970-SET-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DDY' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DDY'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DPB' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DPB'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UHF' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UHF'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLL' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3980-FLAG-PAYMENT.
           IF CRCM-PAYMENT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3990-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LIMIT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4000-ACCRUE-LIMIT.
           IF CRCT-LIMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCT-LIMIT-AMT * CRCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO CRCM-LIMIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4010-DESCRIBE-TYPE.
           EVALUATE CRCM-TYPE-CD
               WHEN 'GP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4020-FLAG-PAYMENT.
           IF CRCM-PAYMENT-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRCM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4030-COUNT-SEQ.
           IF CRCT-SEQ < 9999999
               ADD 1 TO CRCT-SEQ
           END-IF
           IF CRCT-SEQ NOT < 12
               MOVE 'Y' TO CRCM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4040-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCT-CHANNEL-CD
                   ADD CRCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4050-COUNT-ITEM.
           IF CRCM-ITEM-CNT < 9999999
               ADD 1 TO CRCM-ITEM-CNT
           END-IF
           IF CRCM-ITEM-CNT NOT < 3
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4060-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4070-FLAG-PAYMENT.
           IF CRCT-PAYMENT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRCT-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT SCHED TIER FOR THE LIMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4080-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-SCHED-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CRCM-LIMIT-AMT NOT < WS-LIMIT-SCHED-LOW(WS-TX)
                   MOVE WS-LIMIT-SCHED-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E012 NAME REQUIRED
      * E764 FINANCE YTD NEGATIVE
      * E195 FINANCE YTD OVER 5000
      * E080 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4090-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-NAME = SPACES
                   MOVE 'E012' TO WS-REJECT-CODE
               WHEN CRCM-FINANCE-YTD < 0
                   MOVE 'E764' TO WS-REJECT-CODE
               WHEN CRCM-FINANCE-YTD > 5000.00
                   MOVE 'E195' TO WS-REJECT-CODE
               WHEN CRCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E080' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4100-FLAG-LIMIT.
           IF CRCT-LIMIT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCT-LIMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4110-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CRCM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO CRCM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4120-DESCRIBE-TRAN.
           EVALUATE CRCT-TRAN-CD
               WHEN 'PU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'CA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PURCHASE YTD BY CARD TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4130-ACCUM-CARD-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-CARD-TYPE-CD
                   ADD CRCM-PURCHASE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-CARD-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-PURCHASE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4140-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MIN DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       4150-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCT-LIMIT-AMT ROUNDED =
                   CRCT-MIN-DUE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCT-LIMIT-AMT
           END-IF
           ADD CRCT-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE MIN DUE FEE ON THE PAYMENT AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4160-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CRCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CRCT-MIN-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-EXEMPT-FLAG = 'Y'
                   COMPUTE CRCT-MIN-DUE-AMT ROUNDED = CRCM-PAYMENT-AMT *
                       CRCM-DISCOUNT / 200
               WHEN CRCM-STATUS-CD = 'BAL'
                   MOVE 20.00 TO CRCT-MIN-DUE-AMT
               WHEN CRCM-STATUS-CD = 'DDY'
                   MOVE 20.00 TO CRCT-MIN-DUE-AMT
               WHEN OTHER
                   COMPUTE CRCT-MIN-DUE-AMT ROUNDED = CRCM-PAYMENT-AMT *
                       CRCM-DISCOUNT / 100
           END-EVALUATE
           IF CRCT-MIN-DUE-AMT > 0 AND CRCT-MIN-DUE-AMT < 5.00
               MOVE 5.00 TO CRCT-MIN-DUE-AMT
           END-IF
           IF CRCT-MIN-DUE-AMT > 50.00
               MOVE 50.00 TO CRCT-MIN-DUE-AMT
           END-IF
           ADD CRCT-MIN-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE LIMIT YTD FEE ON THE LIMIT AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4170-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CRCM-LIMIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-STOP-FLAG = 'Y'
                   COMPUTE CRCM-LIMIT-YTD ROUNDED = CRCM-LIMIT-AMT *
                       CRCM-PENALTY-PCT / 200
               WHEN CRCM-CARD-TYPE-CD = 'PGU'
                   MOVE 7.00 TO CRCM-LIMIT-YTD
               WHEN CRCM-CARD-TYPE-CD = 'TRN'
                   MOVE 12.00 TO CRCM-LIMIT-YTD
               WHEN OTHER
                   COMPUTE CRCM-LIMIT-YTD ROUNDED = CRCM-LIMIT-AMT *
                       CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCM-LIMIT-YTD > 0 AND CRCM-LIMIT-YTD < 2.00
               MOVE 2.00 TO CRCM-LIMIT-YTD
           END-IF
           IF CRCM-LIMIT-YTD > 50.00
               MOVE 50.00 TO CRCM-LIMIT-YTD
           END-IF
           ADD CRCM-LIMIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4180-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO CRCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4190-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-LIMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-LIMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-LIMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCM-LIMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-LIMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E026 CUST ID REQUIRED
      * E327 PAYMENT AMT NEGATIVE
      * E766 PAYMENT AMT OVER 99999
      * E049 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4200-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-CUST-ID = SPACES
                   MOVE 'E026' TO WS-REJECT-CODE
               WHEN CRCT-PAYMENT-AMT < 0
                   MOVE 'E327' TO WS-REJECT-CODE
               WHEN CRCT-PAYMENT-AMT > 99999.00
                   MOVE 'E766' TO WS-REJECT-CODE
               WHEN CRCM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E049' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4210-SET-CHANNEL.
           EVALUATE CRCT-CHANNEL-CD
               WHEN 'BRN'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E471 OPERATOR REQUIRED
      * E048 PURCHASE YTD NEGATIVE
      * E009 PURCHASE YTD OVER 99999
      * E126 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4220-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCT-OPERATOR = SPACES
                   MOVE 'E471' TO WS-REJECT-CODE
               WHEN CRCM-PURCHASE-YTD < 0
                   MOVE 'E048' TO WS-REJECT-CODE
               WHEN CRCM-PURCHASE-YTD > 99999.00
                   MOVE 'E009' TO WS-REJECT-CODE
               WHEN CRCM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E126' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4230-LIMIT-PURCHASE.
           IF CRCM-PURCHASE-AMT < 50.00
               MOVE 50.00 TO CRCM-PURCHASE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-PURCHASE-AMT > 10000.00
               MOVE 10000.00 TO CRCM-PURCHASE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4240-LIMIT-LIMIT.
           IF CRCM-LIMIT-YTD < 50.00
               MOVE 50.00 TO CRCM-LIMIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-LIMIT-YTD > 50000.00
               MOVE 50000.00 TO CRCM-LIMIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PURCHASE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4250-ACCRUE-PURCHASE.
           IF CRCM-PURCHASE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PURCHASE-YTD * CRCM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH ADV AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4260-SPLIT-CASH-ADV.
           COMPUTE WS-WORK-AMT = CRCT-CASH-ADV-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CRCT-CASH-ADV-AMT - (WS-WORK-AMT * 2)
           COMPUTE CRCM-LATE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4270-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * C -> H WHEN MATURITY DATE IS PAST
      * H -> M WHEN MATURITY DATE IS PAST
      * M -> R WHEN MATURITY DATE IS PAST
      * R -> S WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4280-SET-SOURCE.
           EVALUATE CRCM-SOURCE-CD
               WHEN 'C'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CARD TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4290-DESCRIBE-CARD-TYPE.
           EVALUATE CRCM-CARD-TYPE-CD
               WHEN 'PGU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WHR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZWB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE MIN DUE SCHED TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4300-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-DUE-SCHED-RATE(1) TO CRCM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRCM-PAYMENT-YTD NOT < WS-MIN-DUE-SCHED-LOW(WS-TX)
                   MOVE WS-MIN-DUE-SCHED-RATE(WS-TX) TO CRCM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4310-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CRCM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4320-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4330-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE MIN DUE FEE ON THE LIMIT AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4340-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CRCT-MIN-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CRCT-MIN-DUE-AMT ROUNDED = CRCT-LIMIT-AMT *
                       CRCM-PENALTY-PCT / 200
               WHEN CRCM-BRANCH-CD = 'CTB'
                   MOVE 15.00 TO CRCT-MIN-DUE-AMT
               WHEN CRCM-BRANCH-CD = 'LNX'
                   MOVE 15.00 TO CRCT-MIN-DUE-AMT
               WHEN OTHER
                   COMPUTE CRCT-MIN-DUE-AMT ROUNDED = CRCT-LIMIT-AMT *
                       CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCT-MIN-DUE-AMT > 0 AND CRCT-MIN-DUE-AMT < 5.00
               MOVE 5.00 TO CRCT-MIN-DUE-AMT
           END-IF
           IF CRCT-MIN-DUE-AMT > 35.00
               MOVE 35.00 TO CRCT-MIN-DUE-AMT
           END-IF
           ADD CRCT-MIN-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE MIN DUE FEE ON THE PURCHASE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4350-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CRCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CRCM-MIN-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-STOP-FLAG = 'Y'
                   COMPUTE CRCM-MIN-DUE-AMT ROUNDED = CRCM-PURCHASE-YTD
                       * CRCM-PENALTY-PCT / 200
               WHEN CRCM-BILL-CYCLE-CD = 'B'
                   MOVE 7.00 TO CRCM-MIN-DUE-AMT
               WHEN CRCM-BILL-CYCLE-CD = 'D'
                   MOVE 12.00 TO CRCM-MIN-DUE-AMT
               WHEN OTHER
                   COMPUTE CRCM-MIN-DUE-AMT ROUNDED = CRCM-PURCHASE-YTD
                       * CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCM-MIN-DUE-AMT > 0 AND CRCM-MIN-DUE-AMT < 2.00
               MOVE 2.00 TO CRCM-MIN-DUE-AMT
           END-IF
           IF CRCM-MIN-DUE-AMT > 150.00
               MOVE 150.00 TO CRCM-MIN-DUE-AMT
           END-IF
           ADD CRCM-MIN-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FINANCE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4360-FLAG-FINANCE.
           IF CRCM-FINANCE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRCM-FINANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE CASH ADV YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4370-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN CRCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO CRCM-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CRCM-PAYMENT-AMT ROUNDED = CRCM-CASH-ADV-YTD
                       * CRCM-RATE / 200
               WHEN CRCM-TYPE-CD = 'GP'
                   MOVE 5.00 TO CRCM-PAYMENT-AMT
               WHEN CRCM-TYPE-CD = 'UV'
                   MOVE 3.00 TO CRCM-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE CRCM-PAYMENT-AMT ROUNDED = CRCM-CASH-ADV-YTD
                       * CRCM-RATE / 100
           END-EVALUATE
           IF CRCM-PAYMENT-AMT > 0 AND CRCM-PAYMENT-AMT < 10.00
               MOVE 10.00 TO CRCM-PAYMENT-AMT
           END-IF
           IF CRCM-PAYMENT-AMT > 150.00
               MOVE 150.00 TO CRCM-PAYMENT-AMT
           END-IF
           ADD CRCM-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FINANCE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4380-ACCRUE-FINANCE.
           IF CRCM-FINANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-FINANCE-AMT * CRCM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CRCM-LIMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PU -> CA WHEN EFFECTIVE DATE IS PAST
      * CA -> PY WHEN EFFECTIVE DATE IS PAST
      * PY -> FC WHEN EFFECTIVE DATE IS PAST
      * FC -> LF WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4390-SET-TRAN.
           EVALUATE CRCT-TRAN-CD
               WHEN 'PU'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CA' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CA'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FC' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FC'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LF' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH ADV AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4400-FLAG-CASH-ADV.
           IF CRCM-CASH-ADV-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRCM-CASH-ADV-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4410-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4420-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PURCHASE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4430-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = CRCM-PURCHASE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CRCM-PURCHASE-YTD - (WS-WORK-AMT * 3)
           COMPUTE CRCM-LIMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E223 CONTACT REQUIRED
      * E945 WORK AMT 2 NEGATIVE
      * E950 WORK AMT 2 OVER 5000
      * E020 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4440-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-CONTACT = SPACES
                   MOVE 'E223' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E945' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E950' TO WS-REJECT-CODE
               WHEN CRCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E020' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4450-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-NOTICE-DATE TO DTU-DATE-1
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
      * STATUS TRANSITIONS FOR BILL CYCLE CD:
      * B -> D WHEN NOTICE DATE IS PAST
      * D -> E WHEN NOTICE DATE IS PAST
      * E -> F WHEN NOTICE DATE IS PAST
      * F -> H WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4460-SET-BILL-CYCLE.
           EVALUATE CRCM-BILL-CYCLE-CD
               WHEN 'B'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4470-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CRCM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4480-COUNT-CYCLE.
           IF CRCM-CYCLE-CNT < 9999999
               ADD 1 TO CRCM-CYCLE-CNT
           END-IF
           IF CRCM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PU -> CA WHEN LAST ACT DATE IS PAST
      * CA -> PY WHEN LAST ACT DATE IS PAST
      * PY -> FC WHEN LAST ACT DATE IS PAST
      * FC -> LF WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4490-SET-TRAN.
           EVALUATE CRCT-TRAN-CD
               WHEN 'PU'
                   IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CA' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CA'
                   IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FC' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FC'
                   IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LF' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT LIMIT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4500-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = CRCM-LIMIT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CRCM-LIMIT-YTD - (WS-WORK-AMT * 3)
           COMPUTE CRCM-PURCHASE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH ADV AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4510-ACCRUE-CASH-ADV.
           IF CRCM-CASH-ADV-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-CASH-ADV-AMT * CRCM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CRCT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FINANCE YTD BY CARD TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4520-ACCUM-CARD-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-CARD-TYPE-CD
                   ADD CRCM-FINANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-CARD-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-FINANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH ADV YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4530-ACCRUE-CASH-ADV.
           IF CRCM-CASH-ADV-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-CASH-ADV-YTD * CRCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO CRCM-LATE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       4540-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCT-PAYMENT-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCT-PAYMENT-AMT
           END-IF
           ADD CRCT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * MIN DUE AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4550-LIMIT-MIN-DUE.
           IF CRCT-MIN-DUE-AMT < 50.00
               MOVE 50.00 TO CRCT-MIN-DUE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCT-MIN-DUE-AMT > 500.00
               MOVE 500.00 TO CRCT-MIN-DUE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       4560-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-MIN-DUE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-MIN-DUE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CRCM-MIN-DUE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-MIN-DUE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-MIN-DUE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MIN DUE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4570-FLAG-MIN-DUE.
           IF CRCM-MIN-DUE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCM-MIN-DUE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4580-ACCRUE-LATE-FEE.
           IF CRCM-LATE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-LATE-FEE-YTD * CRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4590-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CRCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO CRCM-LATE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT SCHED TIER FOR THE CASH ADV AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4600-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-SCHED-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CRCM-CASH-ADV-AMT NOT < WS-LIMIT-SCHED-LOW(WS-TX)
                   MOVE WS-LIMIT-SCHED-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARD TYPE CD:
      * PGU -> TRN WHEN LAST PAY DATE IS PAST
      * TRN -> WHR WHEN LAST PAY DATE IS PAST
      * WHR -> ZWB WHEN LAST PAY DATE IS PAST
      * ZWB -> PGU WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4610-SET-CARD-TYPE.
           EVALUATE CRCM-CARD-TYPE-CD
               WHEN 'PGU'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TRN' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TRN'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WHR' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WHR'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZWB' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZWB'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PGU' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE MIN DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       4620-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-PAYMENT-YTD ROUNDED =
                   CRCM-MIN-DUE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-PAYMENT-YTD
           END-IF
           ADD CRCM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN DUE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4630-ACCRUE-MIN-DUE.
           IF CRCM-MIN-DUE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-MIN-DUE-YTD * CRCM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CRCM-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E362 MEMO REQUIRED
      * E805 MIN DUE AMT NEGATIVE
      * E250 MIN DUE AMT OVER 1000000
      * E989 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4640-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCT-MEMO = SPACES
                   MOVE 'E362' TO WS-REJECT-CODE
               WHEN CRCM-MIN-DUE-AMT < 0
                   MOVE 'E805' TO WS-REJECT-CODE
               WHEN CRCM-MIN-DUE-AMT > 1000000.00
                   MOVE 'E250' TO WS-REJECT-CODE
               WHEN CRCM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E989' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4650-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4660-FLAG-PAYMENT.
           IF CRCT-PAYMENT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCT-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E302 REF NO REQUIRED
      * E753 LIMIT YTD NEGATIVE
      * E821 LIMIT YTD OVER 50000
      * E353 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4670-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-REF-NO = SPACES
                   MOVE 'E302' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-YTD < 0
                   MOVE 'E753' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-YTD > 50000.00
                   MOVE 'E821' TO WS-REJECT-CODE
               WHEN CRCM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E353' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4680-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN DUE TIER TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4690-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-DUE-TIER-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF CRCM-PAYMENT-AMT NOT < WS-MIN-DUE-TIER-LOW(WS-TX)
                   MOVE WS-MIN-DUE-TIER-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E749 ADDR REQUIRED
      * E519 PURCHASE YTD NEGATIVE
      * E758 PURCHASE YTD OVER 50000
      * E868 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4700-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-ADDR = SPACES
                   MOVE 'E749' TO WS-REJECT-CODE
               WHEN CRCM-PURCHASE-YTD < 0
                   MOVE 'E519' TO WS-REJECT-CODE
               WHEN CRCM-PURCHASE-YTD > 50000.00
                   MOVE 'E758' TO WS-REJECT-CODE
               WHEN CRCM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E868' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FINANCE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4710-SPLIT-FINANCE.
           COMPUTE WS-WORK-AMT = CRCM-FINANCE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CRCM-FINANCE-YTD - (WS-WORK-AMT * 3)
           COMPUTE CRCT-LIMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4720-FLAG-PURCHASE.
           IF CRCM-PURCHASE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCM-PURCHASE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE CRC FIELDS TO CRTIER AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-CRTIER.
           INITIALIZE CRTIER-PARMS
           MOVE CRCM-PAYMENT-AMT
             TO CRTIER-PAYMENT-AMT
           MOVE CRCM-LATE-FEE-AMT
             TO CRTIER-LATE-FEE-AMT
           MOVE CRCM-FINANCE-YTD
             TO CRTIER-FINANCE-YTD
           MOVE CRCM-CASH-ADV-YTD
             TO CRTIER-CASH-ADV-YTD
           MOVE CRCM-LATE-FEE-YTD
             TO CRTIER-LATE-FEE-YTD
           MOVE CRCM-LIMIT-AMT
             TO CRTIER-LIMIT-AMT
           MOVE CRCM-EXPIRY-DATE
             TO CRTIER-EXPIRY-DATE
           MOVE CRCM-MATURITY-DATE
             TO CRTIER-MATURITY-DATE
           MOVE CRCM-LAST-PAY-DATE
             TO CRTIER-LAST-PAY-DATE
           MOVE CRCM-OPEN-DATE
             TO CRTIER-OPEN-DATE
           MOVE CRCM-SOURCE-CD
             TO CRTIER-SOURCE-CD
           MOVE CRCM-STATUS-CD
             TO CRTIER-STATUS-CD
           MOVE CRCM-BRANCH-CD
             TO CRTIER-BRANCH-CD
           MOVE CRCM-CARD-TYPE-CD
             TO CRTIER-CARD-TYPE-CD
           MOVE CRCM-NOTICE-SENT-FLAG
             TO CRTIER-NOTICE-SENT-FLAG
           MOVE CRCM-PREFERRED-FLAG
             TO CRTIER-PREFERRED-FLAG
           MOVE CRCM-AUTO-RENEW-FLAG
             TO CRTIER-AUTO-RENEW-FLAG
           MOVE CRCM-CYCLE-CNT
             TO CRTIER-CYCLE-CNT
           MOVE CRCM-RETRY-CNT
             TO CRTIER-RETRY-CNT
           MOVE CRCM-DISCOUNT
             TO CRTIER-DISCOUNT
           MOVE CRCM-PENALTY-PCT
             TO CRTIER-PENALTY-PCT
           MOVE CRCM-NAME
             TO CRTIER-NAME
           MOVE CRCM-DESC
             TO CRTIER-DESC
           CALL 'CRTIER' USING CRTIER-PARMS
           EVALUATE CRTIER-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CRTIER-FINANCE-YTD
                     TO CRCM-FINANCE-YTD
                   MOVE CRTIER-CASH-ADV-YTD
                     TO CRCM-CASH-ADV-YTD
                   MOVE CRTIER-LATE-FEE-YTD
                     TO CRCM-LATE-FEE-YTD
                   MOVE CRTIER-LIMIT-AMT
                     TO CRCM-LIMIT-AMT
                   MOVE CRTIER-PREFERRED-FLAG
                     TO CRCM-PREFERRED-FLAG
                   MOVE CRTIER-AUTO-RENEW-FLAG
                     TO CRCM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RIER' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-CRTIER' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE CRC FIELDS TO CRELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-CRELG.
           INITIALIZE CRELG-PARMS
           MOVE CRCM-LIMIT-YTD
             TO CRELG-LIMIT-YTD
           MOVE CRCM-PURCHASE-AMT
             TO CRELG-PURCHASE-AMT
           MOVE CRCM-MIN-DUE-YTD
             TO CRELG-MIN-DUE-YTD
           MOVE CRCM-LATE-FEE-AMT
             TO CRELG-LATE-FEE-AMT
           MOVE CRCM-CASH-ADV-AMT
             TO CRELG-CASH-ADV-AMT
           MOVE CRCM-LIMIT-AMT
             TO CRELG-LIMIT-AMT
           MOVE CRCM-LAST-ACT-DATE
             TO CRELG-LAST-ACT-DATE
           MOVE CRCM-OPEN-DATE
             TO CRELG-OPEN-DATE
           MOVE CRCM-LAST-PAY-DATE
             TO CRELG-LAST-PAY-DATE
           MOVE CRCM-MATURITY-DATE
             TO CRELG-MATURITY-DATE
           MOVE CRCM-TYPE-CD
             TO CRELG-TYPE-CD
           MOVE CRCM-CARD-TYPE-CD
             TO CRELG-CARD-TYPE-CD
           MOVE CRCM-BILL-CYCLE-CD
             TO CRELG-BILL-CYCLE-CD
           MOVE CRCM-SOURCE-CD
             TO CRELG-SOURCE-CD
           MOVE CRCM-EXEMPT-FLAG
             TO CRELG-EXEMPT-FLAG
           MOVE CRCM-AUTO-RENEW-FLAG
             TO CRELG-AUTO-RENEW-FLAG
           MOVE CRCM-STOP-FLAG
             TO CRELG-STOP-FLAG
           MOVE CRCM-CYCLE-CNT
             TO CRELG-CYCLE-CNT
           MOVE CRCM-RETRY-CNT
             TO CRELG-RETRY-CNT
           MOVE CRCM-DISCOUNT
             TO CRELG-DISCOUNT
           MOVE CRCM-RATE
             TO CRELG-RATE
           MOVE CRCM-DESC
             TO CRELG-DESC
           MOVE CRCM-ADDR
             TO CRELG-ADDR
           CALL 'CRELG' USING CRELG-PARMS
           EVALUATE CRELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CRELG-LATE-FEE-AMT
                     TO CRCM-LATE-FEE-AMT
                   MOVE CRELG-LIMIT-AMT
                     TO CRCM-LIMIT-AMT
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-CRELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE CRC FIELDS TO CRCAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-CRCAL.
           INITIALIZE CRCAL-PARMS
           MOVE CRCM-LIMIT-AMT
             TO CRCAL-LIMIT-AMT
           MOVE CRCM-CASH-ADV-AMT
             TO CRCAL-CASH-ADV-AMT
           MOVE CRCM-MIN-DUE-YTD
             TO CRCAL-MIN-DUE-YTD
           MOVE CRCM-LATE-FEE-AMT
             TO CRCAL-LATE-FEE-AMT
           MOVE CRCM-FINANCE-YTD
             TO CRCAL-FINANCE-YTD
           MOVE CRCM-CASH-ADV-YTD
             TO CRCAL-CASH-ADV-YTD
           MOVE CRCM-EFFECTIVE-DATE
             TO CRCAL-EFFECTIVE-DATE
           MOVE CRCM-CLOSE-DATE
             TO CRCAL-CLOSE-DATE
           MOVE CRCM-MATURITY-DATE
             TO CRCAL-MATURITY-DATE
           MOVE CRCM-EXPIRY-DATE
             TO CRCAL-EXPIRY-DATE
           MOVE CRCM-STATUS-CD
             TO CRCAL-STATUS-CD
           MOVE CRCM-SOURCE-CD
             TO CRCAL-SOURCE-CD
           MOVE CRCM-CARD-TYPE-CD
             TO CRCAL-CARD-TYPE-CD
           MOVE CRCM-TYPE-CD
             TO CRCAL-TYPE-CD
           MOVE CRCM-EXEMPT-FLAG
             TO CRCAL-EXEMPT-FLAG
           MOVE CRCM-OVERRIDE-FLAG
             TO CRCAL-OVERRIDE-FLAG
           MOVE CRCM-AUTO-RENEW-FLAG
             TO CRCAL-AUTO-RENEW-FLAG
           MOVE CRCM-RETRY-CNT
             TO CRCAL-RETRY-CNT
           MOVE CRCM-CYCLE-CNT
             TO CRCAL-CYCLE-CNT
           MOVE CRCM-PENALTY-PCT
             TO CRCAL-PENALTY-PCT
           MOVE CRCM-RATE
             TO CRCAL-RATE
           MOVE CRCM-DESC
             TO CRCAL-DESC
           MOVE CRCM-CUST-ID
             TO CRCAL-CUST-ID
           CALL 'CRCAL' USING CRCAL-PARMS
           EVALUATE CRCAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CRCAL-CASH-ADV-AMT
                     TO CRCM-CASH-ADV-AMT
                   MOVE CRCAL-OVERRIDE-FLAG
                     TO CRCM-OVERRIDE-FLAG
                   MOVE CRCAL-AUTO-RENEW-FLAG
                     TO CRCM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-CRCAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE CRCT-KEY TO RD-KEY
           MOVE CRCT-TRAN-CD TO RD-CODE
           MOVE CRCT-PAYMENT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE CRCT-KEY TO RD-KEY
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
           DISPLAY 'CRCEXCP READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CRCEXCP' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
