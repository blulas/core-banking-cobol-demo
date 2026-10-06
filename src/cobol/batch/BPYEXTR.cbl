      *================================================================*
      * PROGRAM  : BPYEXTR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : BILL PAYMENT SERVICE - NIGHTLY EXTRACT. READS THE   *
      *            BPY TRANSACTIONS, UPDATES THE BPY MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. CONSUMER BILL PAYMENT  *
      *            PAYEE MAINTENANCE AND CHECK OR EFT DISBURSEMENT     *
      * INPUT    : BPYMST (KSDS), BPYTRN, BANKCTL                      *
      * OUTPUT   : BPYOUT, BPYEXRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, BPPRC, BPSTAT, BPELG    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-11-07 RJT  ORIGINAL                                       *
      * 1996-03-17 SKN  FEE TABLE EXPANDED CHG6368                     *
      * 1996-09-08 LMB  REFUND EXCEPTIONS TO OPS QUEUE INC6205         *
      * 1998-04-04 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-11-03 RJT  RETURNED STATUS CODES ADDED CHG9965            *
      * 2000-10-06 GAF  PAYMENT STATUS CODES ADDED PRB2425             *
      * 2003-06-09 GAF  NEW PAYMENT REPORT COLUMNS PRB5342             *
      * 2003-10-15 EWS  NEW FEE REPORT COLUMNS REQ9539                 *
      * 2004-10-05 SKN  PERFORMANCE - FEE READ AHEAD INC8732           *
      * 2004-11-14 MKL  REFUND CUTOFF MOVED INC7694                    *
      * 2005-03-20 LMB  FEE FEE SCHEDULE UPDATE PRB8193                *
      * 2008-12-09 DPS  REFUND RERUN PROTECTION REQ7561                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BPYEXTR.
       AUTHOR. D P SHAW.

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
           SELECT REPORT-FILE ASSIGN TO "BPYEXRPT"
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
                                           VALUE 12.000000.
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
      * WS-RETURNED-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RETURNED-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000880000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000473000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000338500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015250000000888000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017750000000080000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027750000000227000'.
       01  WS-RETURNED-BAND-TBL REDEFINES WS-RETURNED-BAND-VALUES.
           05  WS-RETURNED-BAND-ROW OCCURS 6 TIMES.
               10  WS-RETURNED-BAND-LOW    PIC 9(09)V99.
               10  WS-RETURNED-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RETURNED-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RETURNED-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000022500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000444000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000179500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001350000000825000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006350000000738500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011350000000470500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011400000000181000'.
       01  WS-RETURNED-SCHED-TBL REDEFINES WS-RETURNED-SCHED-VALUES.
           05  WS-RETURNED-SCHED-ROW OCCURS 7 TIMES.
               10  WS-RETURNED-SCHED-LOW   PIC 9(09)V99.
               10  WS-RETURNED-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYMENT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000834000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000398000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000451000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000711000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001550000000740000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000031500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000012500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022550000000182500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025050000000194500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025150000000869500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035150000000506500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036150000000444000'.
       01  WS-PAYMENT-GRID-TBL REDEFINES WS-PAYMENT-GRID-VALUES.
           05  WS-PAYMENT-GRID-ROW OCCURS 12 TIMES.
               10  WS-PAYMENT-GRID-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RETURNED-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RETURNED-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000826000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000369000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000107500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001300000000506000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002300000000453000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002350000000351500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007350000000803500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007450000000499500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007700000000620000'.
       01  WS-RETURNED-GRID-TBL REDEFINES WS-RETURNED-GRID-VALUES.
           05  WS-RETURNED-GRID-ROW OCCURS 9 TIMES.
               10  WS-RETURNED-GRID-LOW    PIC 9(09)V99.
               10  WS-RETURNED-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  BPYEXTR  BILL PAYMENT SERVICE'.
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
       COPY BPPRCP.
       COPY BPSTATP.
       COPY BPELGP.

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
               PERFORM 3000-COUNT-CYCLE
               PERFORM 3010-EDIT-MEMO
               PERFORM 3020-LIMIT-PAYMENT
               PERFORM 3030-EDIT-REF-NO
               PERFORM 3040-LIMIT-ITEM
               PERFORM 3050-COUNT-EXCEPT
               PERFORM 3060-FLAG-PAYMENT
               PERFORM 3070-ACCRUE-RETURNED
               PERFORM 3080-AGE-MATURITY
               PERFORM 3090-CHECK-NOTICE
               PERFORM 3100-LIMIT-FEE
               PERFORM 3110-ACCRUE-MARGIN
               PERFORM 3120-SPLIT-PAYMENT
               PERFORM 3130-SELECT-MARGIN
               PERFORM 3140-FLAG-PAYMENT
               PERFORM 3150-FLAG-FEE
               PERFORM 3160-AGE-LAST-ACT
               PERFORM 3170-SELECT-RATE
               PERFORM 3180-LIMIT-PAYMENT
               PERFORM 3190-SPLIT-REFUND
               PERFORM 3200-SET-REMIT-METHOD
               PERFORM 3210-FLAG-PAYMENT
               PERFORM 3220-CHECK-EFFECTIVE
               PERFORM 3230-ACCRUE-PENALTY-PCT
               PERFORM 3240-SPLIT-FEE
               PERFORM 3250-PRORATE-REFUND
               PERFORM 3260-SPLIT-RETURNED
               PERFORM 3270-ACCUM-CHANNEL
               PERFORM 3280-DESCRIBE-CHANNEL
               PERFORM 3290-CHECK-MATURITY
               PERFORM 3300-DESCRIBE-STATUS
               PERFORM 3310-AGE-NOTICE
               PERFORM 3320-SELECT-MARGIN
               PERFORM 3330-CHECK-EXPIRY
               PERFORM 3340-EDIT-OPERATOR
               PERFORM 3350-SET-PAYEE-TYPE
               PERFORM 3360-SET-CYCLE
               PERFORM 3370-DESCRIBE-CHANNEL
               PERFORM 3380-ACCRUE-RATE
               PERFORM 3390-SPLIT-RETURNED
               PERFORM 3400-COUNT-ITEM
               PERFORM 3410-EDIT-REF-NO
               PERFORM 3420-PRORATE-REFUND
               PERFORM 3430-LIMIT-REFUND
               PERFORM 3440-ASSESS-PREFERRED
               PERFORM 3450-ACCRUE-PAYMENT
               PERFORM 3460-AGE-NOTICE
               PERFORM 3470-LIMIT-FEE
               PERFORM 3480-ACCUM-SOURCE
               PERFORM 3490-CHECK-EXPIRY
               PERFORM 3500-CHECK-POST
               PERFORM 3510-LIMIT-FEE
               PERFORM 3520-AGE-NOTICE
               PERFORM 3530-AGE-OPEN
               PERFORM 3540-FLAG-ITEM
               PERFORM 3550-SELECT-MARGIN
               PERFORM 3560-FLAG-REFUND
               PERFORM 3570-DESCRIBE-SOURCE
               PERFORM 3580-PRORATE-RETURNED
               PERFORM 3590-LIMIT-FEE
               PERFORM 3600-SET-SOURCE
               PERFORM 3610-PRORATE-PAYMENT
               PERFORM 3620-SPLIT-REFUND
               PERFORM 3630-SET-REMIT-METHOD
               PERFORM 3640-SET-STATUS
               PERFORM 3650-SELECT-RATE
               PERFORM 3660-PRORATE-RETURNED
               PERFORM 3670-ACCUM-CYCLE
               PERFORM 3680-COUNT-SEQ
               PERFORM 3690-LIMIT-FEE
               PERFORM 3700-COUNT-EXCEPT
               PERFORM 3710-AGE-NEXT-DUE
               PERFORM 3720-SET-SOURCE
               PERFORM 3730-ACCUM-REASON
               PERFORM 3740-SET-CYCLE
               PERFORM 3750-SET-TRAN
               PERFORM 3760-SELECT-MARGIN
               PERFORM 3770-EDIT-CUST-ID
               PERFORM 3780-LIMIT-FEE
               PERFORM 3790-DESCRIBE-REASON
               PERFORM 3800-SET-TRAN
               PERFORM 3810-COUNT-CYCLE
               PERFORM 3820-FLAG-PAYMENT
               PERFORM 3830-DESCRIBE-TRAN
               PERFORM 3840-EDIT-OPERATOR
               PERFORM 3850-CHECK-OPEN
               PERFORM 3860-ASSESS-PREFERRED
               PERFORM 3870-AGE-EXPIRY
               PERFORM 3880-AGE-NOTICE
               PERFORM 3890-LIMIT-FEE
               PERFORM 3900-ACCRUE-RETURNED
               PERFORM 3910-AGE-CLOSE
               PERFORM 3920-ACCRUE-RATE
               PERFORM 3930-SPLIT-FEE
               PERFORM 3940-COUNT-SEQ
               PERFORM 3950-SELECT-RATE
               PERFORM 3960-ACCUM-PAYEE-TYPE
               PERFORM 3970-SPLIT-PAYMENT
               PERFORM 3980-ASSESS-HOLD
               PERFORM 3990-SELECT-MARGIN
               PERFORM 4000-DESCRIBE-STATUS
               PERFORM 4010-ACCRUE-REFUND
               PERFORM 4020-COUNT-EXCEPT
               PERFORM 4030-SELECT-MARGIN
               PERFORM 4040-AGE-EXPIRY
               PERFORM 4050-PRORATE-RETURNED
               PERFORM 4060-CHECK-NOTICE
               PERFORM 4070-ACCUM-CYCLE
               PERFORM 4080-SELECT-RATE
               PERFORM 4090-LIMIT-FEE
               PERFORM 4100-COUNT-PAYMENT
               PERFORM 4110-SET-SOURCE
               PERFORM 4120-DESCRIBE-CHANNEL
               PERFORM 4130-CHECK-POST
               PERFORM 4140-EDIT-CONTACT
               PERFORM 4150-PRORATE-REFUND
               PERFORM 4160-PRORATE-RETURNED
               PERFORM 4170-PRORATE-PAYMENT
               PERFORM 4180-DESCRIBE-PAYEE-TYPE
               PERFORM 4190-ACCRUE-FEE
               PERFORM 4200-SET-REASON
               PERFORM 4210-AGE-NEXT-DUE
               PERFORM 4220-SPLIT-FEE
               PERFORM 4230-EDIT-REJECT
               PERFORM 4240-AGE-POST
               PERFORM 4250-LIMIT-RETURNED
               PERFORM 4260-SET-CYCLE
               PERFORM 4270-CHECK-MATURITY
               PERFORM 4280-LIMIT-RETURNED
               PERFORM 4290-SPLIT-RETURNED
               PERFORM 4300-LIMIT-FEE
               PERFORM 4310-LIMIT-PAYMENT
               PERFORM 4320-ASSESS-SUSPEND
               PERFORM 4330-EDIT-ADDR
               PERFORM 4340-CHECK-LAST-STMT
               PERFORM 4350-PRORATE-RETURNED
               PERFORM 4360-ASSESS-SUSPEND
               PERFORM 7010-CALL-BPPRC
               PERFORM 7020-CALL-BPSTAT
               PERFORM 7000-CALL-BPELG
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
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3000-COUNT-CYCLE.
           IF BPYM-CYCLE-CNT < 9999999
               ADD 1 TO BPYM-CYCLE-CNT
           END-IF
           IF BPYM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E710 MEMO REQUIRED
      * E419 FEE AMT NEGATIVE
      * E457 FEE AMT OVER 50000
      * E605 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3010-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-MEMO = SPACES
                   MOVE 'E710' TO WS-REJECT-CODE
               WHEN BPYT-FEE-AMT < 0
                   MOVE 'E419' TO WS-REJECT-CODE
               WHEN BPYT-FEE-AMT > 50000.00
                   MOVE 'E457' TO WS-REJECT-CODE
               WHEN BPYM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3020-LIMIT-PAYMENT.
           IF BPYT-PAYMENT-AMT < 0.00
               MOVE 0.00 TO BPYT-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-PAYMENT-AMT > 500.00
               MOVE 500.00 TO BPYT-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E683 REF NO REQUIRED
      * E111 WORK AMT NEGATIVE
      * E035 WORK AMT OVER 1000000
      * E566 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3030-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-REF-NO = SPACES
                   MOVE 'E683' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E111' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E035' TO WS-REJECT-CODE
               WHEN BPYM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E566' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3040-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3050-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3060-FLAG-PAYMENT.
           IF BPYM-PAYMENT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETURNED AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3070-ACCRUE-RETURNED.
           IF BPYT-RETURNED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-RETURNED-AMT * BPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BPYM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3080-AGE-MATURITY.
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
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3090-CHECK-NOTICE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3100-LIMIT-FEE.
           IF BPYM-FEE-AMT < 25.00
               MOVE 25.00 TO BPYM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-FEE-AMT > 5000.00
               MOVE 5000.00 TO BPYM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3110-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * BPYM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BPYM-RETURNED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3120-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = BPYM-PAYMENT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BPYM-PAYMENT-AMT - (WS-WORK-AMT * 4)
           COMPUTE BPYM-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED BAND TIER FOR THE RETURNED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3130-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-BAND-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF BPYT-RETURNED-AMT NOT < WS-RETURNED-BAND-LOW(WS-TX)
                   MOVE WS-RETURNED-BAND-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3140-FLAG-PAYMENT.
           IF BPYM-PAYMENT-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3150-FLAG-FEE.
           IF BPYM-FEE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3160-AGE-LAST-ACT.
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
                   ADD BPYM-RETURNED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYM-RETURNED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD BPYM-RETURNED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYM-RETURNED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-RETURNED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED GRID TIER FOR THE REFUND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3170-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-GRID-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF BPYT-REFUND-AMT NOT < WS-RETURNED-GRID-LOW(WS-TX)
                   MOVE WS-RETURNED-GRID-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3180-LIMIT-PAYMENT.
           IF BPYM-PAYMENT-AMT < 10.00
               MOVE 10.00 TO BPYM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-PAYMENT-AMT > 5000.00
               MOVE 5000.00 TO BPYM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3190-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE BPYM-REFUND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REMIT METHOD CD:
      * CGV -> FNS WHEN CLOSE DATE IS PAST
      * FNS -> NMS WHEN CLOSE DATE IS PAST
      * NMS -> TAF WHEN CLOSE DATE IS PAST
      * TAF -> VNK WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3200-SET-REMIT-METHOD.
           EVALUATE BPYM-REMIT-METHOD-CD
               WHEN 'CGV'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FNS' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNS'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NMS' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NMS'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TAF' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TAF'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VNK' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3210-FLAG-PAYMENT.
           IF BPYM-PAYMENT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3220-CHECK-EFFECTIVE.
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
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3230-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * BPYM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BPYM-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3240-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPYM-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = BPYM-FEE-YTD - (WS-WORK-AMT * 12)
           COMPUTE BPYM-PAYMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REFUND YTD.
      *----------------------------------------------------------------*
       3250-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-REFUND-YTD ROUNDED =
                   BPYM-PAYMENT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-REFUND-YTD
           END-IF
           ADD BPYM-REFUND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT RETURNED YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3260-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = BPYM-RETURNED-YTD / 12
           COMPUTE WS-WORK-AMT-2 = BPYM-RETURNED-YTD - (WS-WORK-AMT *
               12)
           COMPUTE BPYM-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RETURNED AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3270-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYT-CHANNEL-CD
                   ADD BPYM-RETURNED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-RETURNED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3280-DESCRIBE-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3290-CHECK-MATURITY.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3300-DESCRIBE-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3310-AGE-NOTICE.
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
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3320-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
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
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3330-CHECK-EXPIRY.
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
      * E260 OPERATOR REQUIRED
      * E144 FEE YTD NEGATIVE
      * E910 FEE YTD OVER 1000000
      * E617 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3340-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-OPERATOR = SPACES
                   MOVE 'E260' TO WS-REJECT-CODE
               WHEN BPYM-FEE-YTD < 0
                   MOVE 'E144' TO WS-REJECT-CODE
               WHEN BPYM-FEE-YTD > 1000000.00
                   MOVE 'E910' TO WS-REJECT-CODE
               WHEN BPYM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E617' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * BT -> FR WHEN EXPIRY DATE IS PAST
      * FR -> LL WHEN EXPIRY DATE IS PAST
      * LL -> US WHEN EXPIRY DATE IS PAST
      * US -> XM WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3350-SET-PAYEE-TYPE.
           EVALUATE BPYM-PAYEE-TYPE-CD
               WHEN 'BT'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FR' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FR'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LL' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LL'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'US' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DA -> FP WHEN NOTICE DATE IS PAST
      * FP -> JH WHEN NOTICE DATE IS PAST
      * JH -> JV WHEN NOTICE DATE IS PAST
      * JV -> RM WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3360-SET-CYCLE.
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
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3370-DESCRIBE-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3380-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * BPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BPYM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RETURNED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3390-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = BPYM-RETURNED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BPYM-RETURNED-AMT - (WS-WORK-AMT * 3)
           COMPUTE BPYM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3400-COUNT-ITEM.
           IF BPYM-ITEM-CNT < 9999999
               ADD 1 TO BPYM-ITEM-CNT
           END-IF
           IF BPYM-ITEM-CNT NOT < 3
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E364 REF NO REQUIRED
      * E280 FEE AMT NEGATIVE
      * E898 FEE AMT OVER 99999
      * E493 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3410-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-REF-NO = SPACES
                   MOVE 'E364' TO WS-REJECT-CODE
               WHEN BPYT-FEE-AMT < 0
                   MOVE 'E280' TO WS-REJECT-CODE
               WHEN BPYT-FEE-AMT > 99999.00
                   MOVE 'E898' TO WS-REJECT-CODE
               WHEN BPYM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E493' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REFUND AMT.
      *----------------------------------------------------------------*
       3420-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-REFUND-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-REFUND-AMT
           END-IF
           ADD BPYM-REFUND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * REFUND YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3430-LIMIT-REFUND.
           IF BPYM-REFUND-YTD < 100.00
               MOVE 100.00 TO BPYM-REFUND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-REFUND-YTD > 5000.00
               MOVE 5000.00 TO BPYM-REFUND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3440-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO BPYM-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   COMPUTE BPYM-REFUND-AMT ROUNDED = WS-WORK-AMT-2 *
                       BPYM-RATE / 200
               WHEN BPYM-CYCLE-CD = 'DA'
                   MOVE 15.00 TO BPYM-REFUND-AMT
               WHEN BPYM-CYCLE-CD = 'FP'
                   MOVE 12.00 TO BPYM-REFUND-AMT
               WHEN OTHER
                   COMPUTE BPYM-REFUND-AMT ROUNDED = WS-WORK-AMT-2 *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-REFUND-AMT > 0 AND BPYM-REFUND-AMT < 10.00
               MOVE 10.00 TO BPYM-REFUND-AMT
           END-IF
           IF BPYM-REFUND-AMT > 150.00
               MOVE 150.00 TO BPYM-REFUND-AMT
           END-IF
           ADD BPYM-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3450-ACCRUE-PAYMENT.
           IF BPYM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-PAYMENT-YTD * BPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BPYM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3460-AGE-NOTICE.
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
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3470-LIMIT-FEE.
           IF BPYT-FEE-AMT < 100.00
               MOVE 100.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-FEE-AMT > 500.00
               MOVE 500.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3480-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-SOURCE-CD
                   ADD BPYT-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYT-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3490-CHECK-EXPIRY.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO BPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3500-CHECK-POST.
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
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3510-LIMIT-FEE.
           IF BPYT-FEE-AMT < 100.00
               MOVE 100.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-FEE-AMT > 50000.00
               MOVE 50000.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3520-AGE-NOTICE.
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
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3530-AGE-OPEN.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3540-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3550-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3560-FLAG-REFUND.
           IF BPYM-REFUND-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYM-REFUND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3570-DESCRIBE-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RETURNED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETURNED AMT.
      *----------------------------------------------------------------*
       3580-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-RETURNED-AMT ROUNDED =
                   BPYM-RETURNED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-RETURNED-AMT
           END-IF
           ADD BPYM-RETURNED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3590-LIMIT-FEE.
           IF BPYT-FEE-AMT < 10.00
               MOVE 10.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-FEE-AMT > 2500.00
               MOVE 2500.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * CT -> FV WHEN OPEN DATE IS PAST
      * FV -> HD WHEN OPEN DATE IS PAST
      * HD -> KD WHEN OPEN DATE IS PAST
      * KD -> NE WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3600-SET-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FV' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FV'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HD'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NE' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3610-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-PAYMENT-AMT ROUNDED =
                   BPYM-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYT-PAYMENT-AMT
           END-IF
           ADD BPYT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT REFUND AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3620-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = BPYT-REFUND-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BPYT-REFUND-AMT - (WS-WORK-AMT * 3)
           COMPUTE BPYT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REMIT METHOD CD:
      * CGV -> FNS WHEN CLOSE DATE IS PAST
      * FNS -> NMS WHEN CLOSE DATE IS PAST
      * NMS -> TAF WHEN CLOSE DATE IS PAST
      * TAF -> VNK WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-REMIT-METHOD.
           EVALUATE BPYM-REMIT-METHOD-CD
               WHEN 'CGV'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FNS' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNS'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NMS' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NMS'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TAF' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TAF'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VNK' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * JX -> LR WHEN CLOSE DATE IS PAST
      * LR -> NZ WHEN CLOSE DATE IS PAST
      * NZ -> RV WHEN CLOSE DATE IS PAST
      * RV -> TM WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3640-SET-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LR' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LR'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3650-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BPYT-FEE-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RETURNED AMT.
      *----------------------------------------------------------------*
       3660-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-RETURNED-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-RETURNED-AMT
           END-IF
           ADD BPYM-RETURNED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3670-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-CYCLE-CD
                   ADD BPYM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3680-COUNT-SEQ.
           IF BPYT-SEQ < 9999999
               ADD 1 TO BPYT-SEQ
           END-IF
           IF BPYT-SEQ NOT < 3
               MOVE 'Y' TO BPYM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3690-LIMIT-FEE.
           IF BPYT-FEE-AMT < 25.00
               MOVE 25.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-FEE-AMT > 50000.00
               MOVE 50000.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3700-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO BPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3710-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * CT -> FV WHEN POST DATE IS PAST
      * FV -> HD WHEN POST DATE IS PAST
      * HD -> KD WHEN POST DATE IS PAST
      * KD -> NE WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FV' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FV'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HD'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NE' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3730-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-REASON-CD
                   ADD BPYT-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYT-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DA -> FP WHEN NEXT DUE DATE IS PAST
      * FP -> JH WHEN NEXT DUE DATE IS PAST
      * JH -> JV WHEN NEXT DUE DATE IS PAST
      * JV -> RM WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3740-SET-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FP' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FP'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JH' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JH'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JV' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JV'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * SC -> PD WHEN MATURITY DATE IS PAST
      * PD -> RT WHEN MATURITY DATE IS PAST
      * RT -> CN WHEN MATURITY DATE IS PAST
      * CN -> RF WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-TRAN.
           EVALUATE BPYT-TRAN-CD
               WHEN 'SC'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RT' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RT'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE REFUND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3760-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BPYT-REFUND-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E297 CUST ID REQUIRED
      * E323 WORK AMT NEGATIVE
      * E307 WORK AMT OVER 250000
      * E472 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3770-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-CUST-ID = SPACES
                   MOVE 'E297' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E323' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E307' TO WS-REJECT-CODE
               WHEN BPYM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E472' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3780-LIMIT-FEE.
           IF BPYM-FEE-AMT < 25.00
               MOVE 25.00 TO BPYM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-FEE-AMT > 2500.00
               MOVE 2500.00 TO BPYM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3790-DESCRIBE-REASON.
           EVALUATE BPYM-REASON-CD
               WHEN 'AX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * SC -> PD WHEN OPEN DATE IS PAST
      * PD -> RT WHEN OPEN DATE IS PAST
      * RT -> CN WHEN OPEN DATE IS PAST
      * CN -> RF WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3800-SET-TRAN.
           EVALUATE BPYT-TRAN-CD
               WHEN 'SC'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RT' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RT'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3810-COUNT-CYCLE.
           IF BPYM-CYCLE-CNT < 9999999
               ADD 1 TO BPYM-CYCLE-CNT
           END-IF
           IF BPYM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO BPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3820-FLAG-PAYMENT.
           IF BPYT-PAYMENT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYT-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3830-DESCRIBE-TRAN.
           EVALUATE BPYT-TRAN-CD
               WHEN 'SC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'CN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E092 OPERATOR REQUIRED
      * E013 FEE YTD NEGATIVE
      * E583 FEE YTD OVER 50000
      * E337 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3840-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-OPERATOR = SPACES
                   MOVE 'E092' TO WS-REJECT-CODE
               WHEN BPYM-FEE-YTD < 0
                   MOVE 'E013' TO WS-REJECT-CODE
               WHEN BPYM-FEE-YTD > 50000.00
                   MOVE 'E583' TO WS-REJECT-CODE
               WHEN BPYM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E337' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-OPEN.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO BPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE PAYMENT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3860-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BPYM-PAYMENT-AMT *
                       BPYM-RATE / 200
               WHEN BPYM-SOURCE-CD = 'CT'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN BPYM-SOURCE-CD = 'FV'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BPYM-PAYMENT-AMT *
                       BPYM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3870-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3880-AGE-NOTICE.
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
                   ADD BPYM-RETURNED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYM-RETURNED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BPYM-RETURNED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD BPYM-RETURNED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-RETURNED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3890-LIMIT-FEE.
           IF BPYM-FEE-YTD < 100.00
               MOVE 100.00 TO BPYM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-FEE-YTD > 10000.00
               MOVE 10000.00 TO BPYM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETURNED AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-RETURNED.
           IF BPYT-RETURNED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-RETURNED-AMT * BPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BPYM-PAYMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3910-AGE-CLOSE.
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
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3920-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * BPYM-RATE / 100 / 365
               ADD WS-WORK-AMT TO BPYM-RETURNED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3930-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPYM-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BPYM-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE BPYM-RETURNED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3940-COUNT-SEQ.
           IF BPYT-SEQ < 9999999
               ADD 1 TO BPYT-SEQ
           END-IF
           IF BPYT-SEQ NOT < 6
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED BAND TIER FOR THE RETURNED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3950-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-BAND-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF BPYM-RETURNED-AMT NOT < WS-RETURNED-BAND-LOW(WS-TX)
                   MOVE WS-RETURNED-BAND-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY PAYEE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3960-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-PAYEE-TYPE-CD
                   ADD BPYM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3970-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE BPYM-PAYMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE RETURNED AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3980-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = BPYT-RETURNED-AMT *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYM-STATUS-CD = 'JX'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN BPYM-STATUS-CD = 'LR'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = BPYT-RETURNED-AMT *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED SCHED TIER FOR THE RETURNED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3990-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-SCHED-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BPYM-RETURNED-YTD NOT < WS-RETURNED-SCHED-LOW(WS-TX)
                   MOVE WS-RETURNED-SCHED-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4000-DESCRIBE-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4010-ACCRUE-REFUND.
           IF BPYM-REFUND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-REFUND-AMT * BPYM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BPYM-RETURNED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4020-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4030-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4040-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RETURNED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4050-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-RETURNED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4060-CHECK-NOTICE.
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
      * ACCUMULATE RETURNED YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-CYCLE-CD
                   ADD BPYM-RETURNED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-RETURNED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE REFUND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4080-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BPYT-REFUND-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4090-LIMIT-FEE.
           IF BPYM-FEE-YTD < 0.00
               MOVE 0.00 TO BPYM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-FEE-YTD > 500.00
               MOVE 500.00 TO BPYM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4100-COUNT-PAYMENT.
           IF BPYM-PAYMENT-CNT < 9999999
               ADD 1 TO BPYM-PAYMENT-CNT
           END-IF
           IF BPYM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO BPYM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * CT -> FV WHEN POST DATE IS PAST
      * FV -> HD WHEN POST DATE IS PAST
      * HD -> KD WHEN POST DATE IS PAST
      * KD -> NE WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4110-SET-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FV' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FV'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HD'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NE' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4120-DESCRIBE-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4130-CHECK-POST.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E539 CONTACT REQUIRED
      * E771 PAYMENT YTD NEGATIVE
      * E850 PAYMENT YTD OVER 25000
      * E477 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4140-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-CONTACT = SPACES
                   MOVE 'E539' TO WS-REJECT-CODE
               WHEN BPYM-PAYMENT-YTD < 0
                   MOVE 'E771' TO WS-REJECT-CODE
               WHEN BPYM-PAYMENT-YTD > 25000.00
                   MOVE 'E850' TO WS-REJECT-CODE
               WHEN BPYM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E477' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REFUND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4150-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-REFUND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE RETURNED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4160-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-RETURNED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE RETURNED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       4170-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-PAYMENT-AMT ROUNDED =
                   BPYM-RETURNED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-PAYMENT-AMT
           END-IF
           ADD BPYM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4180-DESCRIBE-PAYEE-TYPE.
           EVALUATE BPYM-PAYEE-TYPE-CD
               WHEN 'BT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'US'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4190-ACCRUE-FEE.
           IF BPYM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-FEE-AMT * BPYM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BPYM-RETURNED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> BX WHEN EXPIRY DATE IS PAST
      * BX -> GM WHEN EXPIRY DATE IS PAST
      * GM -> KU WHEN EXPIRY DATE IS PAST
      * KU -> LT WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4200-SET-REASON.
           EVALUATE BPYM-REASON-CD
               WHEN 'AX'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BX' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BX'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KU' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KU'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LT' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4210-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4220-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPYM-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BPYM-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE BPYM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E185 REJECT CD REQUIRED
      * E533 PAYMENT AMT NEGATIVE
      * E021 PAYMENT AMT OVER 250000
      * E021 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4230-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-REJECT-CD = SPACES
                   MOVE 'E185' TO WS-REJECT-CODE
               WHEN BPYT-PAYMENT-AMT < 0
                   MOVE 'E533' TO WS-REJECT-CODE
               WHEN BPYT-PAYMENT-AMT > 250000.00
                   MOVE 'E021' TO WS-REJECT-CODE
               WHEN BPYT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E021' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4240-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETURNED YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4250-LIMIT-RETURNED.
           IF BPYM-RETURNED-YTD < 25.00
               MOVE 25.00 TO BPYM-RETURNED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-RETURNED-YTD > 1000.00
               MOVE 1000.00 TO BPYM-RETURNED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DA -> FP WHEN POST DATE IS PAST
      * FP -> JH WHEN POST DATE IS PAST
      * JH -> JV WHEN POST DATE IS PAST
      * JV -> RM WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4260-SET-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FP' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FP'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JH' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JH'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JV' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JV'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO BPYM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETURNED YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4280-LIMIT-RETURNED.
           IF BPYM-RETURNED-YTD < 25.00
               MOVE 25.00 TO BPYM-RETURNED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-RETURNED-YTD > 5000.00
               MOVE 5000.00 TO BPYM-RETURNED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RETURNED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4290-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = BPYM-RETURNED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BPYM-RETURNED-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4300-LIMIT-FEE.
           IF BPYT-FEE-AMT < 25.00
               MOVE 25.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-FEE-AMT > 2500.00
               MOVE 2500.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4310-LIMIT-PAYMENT.
           IF BPYM-PAYMENT-YTD < 100.00
               MOVE 100.00 TO BPYM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-PAYMENT-YTD > 50000.00
               MOVE 50000.00 TO BPYM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE PAYMENT AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4320-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BPYM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BPYT-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE BPYT-REFUND-AMT ROUNDED = BPYT-PAYMENT-AMT *
                       BPYM-RATE / 200
               WHEN BPYM-REASON-CD = 'AX'
                   MOVE 12.00 TO BPYT-REFUND-AMT
               WHEN BPYM-REASON-CD = 'BX'
                   MOVE 5.00 TO BPYT-REFUND-AMT
               WHEN OTHER
                   COMPUTE BPYT-REFUND-AMT ROUNDED = BPYT-PAYMENT-AMT *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYT-REFUND-AMT > 0 AND BPYT-REFUND-AMT < 1.00
               MOVE 1.00 TO BPYT-REFUND-AMT
           END-IF
           IF BPYT-REFUND-AMT > 150.00
               MOVE 150.00 TO BPYT-REFUND-AMT
           END-IF
           ADD BPYT-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E476 ADDR REQUIRED
      * E759 REFUND AMT NEGATIVE
      * E351 REFUND AMT OVER 1000000
      * E895 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4330-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-ADDR = SPACES
                   MOVE 'E476' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-AMT < 0
                   MOVE 'E759' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-AMT > 1000000.00
                   MOVE 'E351' TO WS-REJECT-CODE
               WHEN BPYM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E895' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4340-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BPYM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO BPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETURNED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4350-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-RETURNED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE REFUND YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4360-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BPYM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BPYT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE BPYT-FEE-AMT ROUNDED = BPYM-REFUND-YTD *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO BPYT-FEE-AMT
               WHEN BPYT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO BPYT-FEE-AMT
               WHEN OTHER
                   COMPUTE BPYT-FEE-AMT ROUNDED = BPYM-REFUND-YTD *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYT-FEE-AMT > 0 AND BPYT-FEE-AMT < 5.00
               MOVE 5.00 TO BPYT-FEE-AMT
           END-IF
           IF BPYT-FEE-AMT > 150.00
               MOVE 150.00 TO BPYT-FEE-AMT
           END-IF
           ADD BPYT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PASS THE BPY FIELDS TO BPPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-BPPRC.
           INITIALIZE BPPRC-PARMS
           MOVE BPYM-FEE-AMT
             TO BPPRC-FEE-AMT
           MOVE BPYM-PAYMENT-AMT
             TO BPPRC-PAYMENT-AMT
           MOVE BPYM-PAYMENT-YTD
             TO BPPRC-PAYMENT-YTD
           MOVE BPYM-REFUND-YTD
             TO BPPRC-REFUND-YTD
           MOVE BPYM-RETURNED-YTD
             TO BPPRC-RETURNED-YTD
           MOVE BPYM-FEE-YTD
             TO BPPRC-FEE-YTD
           MOVE BPYM-EXPIRY-DATE
             TO BPPRC-EXPIRY-DATE
           MOVE BPYM-LAST-ACT-DATE
             TO BPPRC-LAST-ACT-DATE
           MOVE BPYM-EFFECTIVE-DATE
             TO BPPRC-EFFECTIVE-DATE
           MOVE BPYM-NEXT-DUE-DATE
             TO BPPRC-NEXT-DUE-DATE
           MOVE BPYM-SOURCE-CD
             TO BPPRC-SOURCE-CD
           MOVE BPYM-REMIT-METHOD-CD
             TO BPPRC-REMIT-METHOD-CD
           MOVE BPYM-REASON-CD
             TO BPPRC-REASON-CD
           MOVE BPYM-STATUS-CD
             TO BPPRC-STATUS-CD
           MOVE BPYM-SUSPEND-FLAG
             TO BPPRC-SUSPEND-FLAG
           MOVE BPYM-OVERRIDE-FLAG
             TO BPPRC-OVERRIDE-FLAG
           MOVE BPYM-HOLD-FLAG
             TO BPPRC-HOLD-FLAG
           MOVE BPYM-EXCEPT-CNT
             TO BPPRC-EXCEPT-CNT
           MOVE BPYM-CYCLE-CNT
             TO BPPRC-CYCLE-CNT
           MOVE BPYM-MARGIN
             TO BPPRC-MARGIN
           MOVE BPYM-PENALTY-PCT
             TO BPPRC-PENALTY-PCT
           MOVE BPYM-ADDR
             TO BPPRC-ADDR
           MOVE BPYM-NAME
             TO BPPRC-NAME
           CALL 'BPPRC' USING BPPRC-PARMS
           EVALUATE BPPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE BPPRC-PAYMENT-AMT
                     TO BPYM-PAYMENT-AMT
                   MOVE BPPRC-PAYMENT-YTD
                     TO BPYM-PAYMENT-YTD
                   MOVE BPPRC-RETURNED-YTD
                     TO BPYM-RETURNED-YTD
                   MOVE BPPRC-OVERRIDE-FLAG
                     TO BPYM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-BPPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

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
                   MOVE BPSTAT-REFUND-AMT
                     TO BPYM-REFUND-AMT
                   MOVE BPSTAT-PAYMENT-YTD
                     TO BPYM-PAYMENT-YTD
                   MOVE BPSTAT-RETURNED-YTD
                     TO BPYM-RETURNED-YTD
                   MOVE BPSTAT-FEE-AMT
                     TO BPYM-FEE-AMT
                   MOVE BPSTAT-EXEMPT-FLAG
                     TO BPYM-EXEMPT-FLAG
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

      *----------------------------------------------------------------*
      * PASS THE BPY FIELDS TO BPELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-BPELG.
           INITIALIZE BPELG-PARMS
           MOVE BPYM-PAYMENT-YTD
             TO BPELG-PAYMENT-YTD
           MOVE BPYM-FEE-YTD
             TO BPELG-FEE-YTD
           MOVE BPYM-REFUND-YTD
             TO BPELG-REFUND-YTD
           MOVE BPYM-FEE-AMT
             TO BPELG-FEE-AMT
           MOVE BPYM-PAYMENT-AMT
             TO BPELG-PAYMENT-AMT
           MOVE BPYM-REFUND-AMT
             TO BPELG-REFUND-AMT
           MOVE BPYM-EXPIRY-DATE
             TO BPELG-EXPIRY-DATE
           MOVE BPYM-LAST-STMT-DATE
             TO BPELG-LAST-STMT-DATE
           MOVE BPYM-NEXT-DUE-DATE
             TO BPELG-NEXT-DUE-DATE
           MOVE BPYM-CLOSE-DATE
             TO BPELG-CLOSE-DATE
           MOVE BPYM-CYCLE-CD
             TO BPELG-CYCLE-CD
           MOVE BPYM-STATUS-CD
             TO BPELG-STATUS-CD
           MOVE BPYM-REMIT-METHOD-CD
             TO BPELG-REMIT-METHOD-CD
           MOVE BPYM-PAYEE-TYPE-CD
             TO BPELG-PAYEE-TYPE-CD
           MOVE BPYM-OVERRIDE-FLAG
             TO BPELG-OVERRIDE-FLAG
           MOVE BPYM-HOLD-FLAG
             TO BPELG-HOLD-FLAG
           MOVE BPYM-SUSPEND-FLAG
             TO BPELG-SUSPEND-FLAG
           MOVE BPYM-EXCEPT-CNT
             TO BPELG-EXCEPT-CNT
           MOVE BPYM-CYCLE-CNT
             TO BPELG-CYCLE-CNT
           MOVE BPYM-RATE
             TO BPELG-RATE
           MOVE BPYM-MARGIN
             TO BPELG-MARGIN
           MOVE BPYM-ADDR
             TO BPELG-ADDR
           MOVE BPYM-REF-NO
             TO BPELG-REF-NO
           CALL 'BPELG' USING BPELG-PARMS
           EVALUATE BPELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE BPELG-PAYMENT-AMT
                     TO BPYM-PAYMENT-AMT
                   MOVE BPELG-HOLD-FLAG
                     TO BPYM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-BPELG' TO ABND-PARAGRAPH
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
           DISPLAY 'BPYEXTR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'BPYEXTR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
