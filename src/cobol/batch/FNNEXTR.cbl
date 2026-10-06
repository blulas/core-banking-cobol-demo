      *================================================================*
      * PROGRAM  : FNNEXTR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : FOREIGN BANKNOTES - NIGHTLY EXTRACT. READS THE FXN  *
      *            TRANSACTIONS, UPDATES THE FXN MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. FOREIGN CURRENCY BANKNOTE       *
      *            ORDERS, BUYBACK AND MARGIN                          *
      * INPUT    : FXNMST (KSDS), FXNTRN, BANKCTL                      *
      * OUTPUT   : FXNOUT, FNNEXRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, FNSTAT                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-04-08 PQR  ORIGINAL                                       *
      * 1997-03-18 PQR  MARGIN STATUS CODES ADDED REQ9695              *
      * 1999-12-02 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-09-01 BHS  MARGIN FEE SCHEDULE UPDATE INC5344             *
      * 1999-01-19 BHS  BUYBACK NOTICE TEXT PER LEGAL INC0266          *
      * 2000-03-16 MKL  AUDIT FINDING - SHIPPING CONTROL TOTALS INC6845*
      * 2002-11-28 RJT  MARGIN STATUS CODES ADDED REQ2513              *
      * 2003-07-17 DPS  FOREIGN CUTOFF MOVED PRB0980                   *
      * 2003-02-28 CRP  FOREIGN NOTICE TEXT PER LEGAL REQ0669          *
      * 2004-05-17 SKN  MARGIN EXCEPTIONS TO OPS QUEUE REQ4406         *
      * 2005-03-17 MKL  MARGIN RERUN PROTECTION INC2314                *
      * 2005-07-10 EWS  AUDIT FINDING - FOREIGN CONTROL TOTALS PRB7572 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FNNEXTR.
       AUTHOR. J L MARSH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "FXNMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS FXNM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "FXNTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "FXNOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "FNNEXRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY FXNMST.
       FD  TRAN-FILE.
       COPY FXNTRN.
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
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 28.
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
      * WS-USD-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-USD-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000761000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000797000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000156500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000655500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000101500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000519500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008000000000368000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008050000000239000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018050000000836000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018100000000385000'.
       01  WS-USD-SCHED-TBL REDEFINES WS-USD-SCHED-VALUES.
           05  WS-USD-SCHED-ROW OCCURS 10 TIMES.
               10  WS-USD-SCHED-LOW        PIC 9(09)V99.
               10  WS-USD-SCHED-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-BUYBACK-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BUYBACK-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000082500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000887000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000258500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000577000'.
       01  WS-BUYBACK-TIER-TBL REDEFINES WS-BUYBACK-TIER-VALUES.
           05  WS-BUYBACK-TIER-ROW OCCURS 4 TIMES.
               10  WS-BUYBACK-TIER-LOW     PIC 9(09)V99.
               10  WS-BUYBACK-TIER-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  FNNEXTR  FOREIGN BANKNOTES'.
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
       COPY FNSTATP.

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
               MOVE 'FXNMST' TO ABND-FILE-NAME
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
           MOVE FXNT-KEY TO FXNM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ACCRUE-SHIPPING
               PERFORM 3010-CHECK-MATURITY
               PERFORM 3020-ASSESS-PREFERRED
               PERFORM 3030-AGE-LAST-PAY
               PERFORM 3040-CHECK-NOTICE
               PERFORM 3050-ACCRUE-MARGIN
               PERFORM 3060-SPLIT-USD
               PERFORM 3070-LIMIT-MARGIN
               PERFORM 3080-ACCRUE-MARGIN
               PERFORM 3090-SET-CURRENCY
               PERFORM 3100-ACCUM-STATUS
               PERFORM 3110-SET-BRANCH
               PERFORM 3120-SPLIT-BUYBACK
               PERFORM 3130-CHECK-LAST-ACT
               PERFORM 3140-SELECT-PENALTY-PCT
               PERFORM 3150-ASSESS-REVERSAL
               PERFORM 3160-CHECK-EXPIRY
               PERFORM 3170-LIMIT-BUYBACK
               PERFORM 3180-PRORATE-SHIPPING
               PERFORM 3190-ACCUM-CHANNEL
               PERFORM 3200-DESCRIBE-SOURCE
               PERFORM 3210-SELECT-RATE
               PERFORM 3220-SELECT-MARGIN
               PERFORM 3230-ACCRUE-USD
               PERFORM 3240-SELECT-RATE
               PERFORM 3250-FLAG-MARGIN
               PERFORM 3260-ACCUM-CHANNEL
               PERFORM 3270-SELECT-RATE
               PERFORM 3280-SPLIT-MARGIN
               PERFORM 3290-DESCRIBE-TYPE
               PERFORM 3300-SET-TYPE
               PERFORM 3310-ACCRUE-BUYBACK
               PERFORM 3320-DESCRIBE-SOURCE
               PERFORM 3330-ASSESS-PREFERRED
               PERFORM 3340-SELECT-MARGIN
               PERFORM 3350-ASSESS-OVERRIDE
               PERFORM 3360-LIMIT-USD
               PERFORM 3370-PRORATE-MARGIN
               PERFORM 3380-PRORATE-FOREIGN
               PERFORM 3390-PRORATE-FOREIGN
               PERFORM 3400-PRORATE-BUYBACK
               PERFORM 3410-FLAG-MARGIN
               PERFORM 3420-SPLIT-FOREIGN
               PERFORM 3430-FLAG-BUYBACK
               PERFORM 3440-LIMIT-MARGIN
               PERFORM 3450-EDIT-ADDR
               PERFORM 3460-PRORATE-MARGIN
               PERFORM 3470-ASSESS-NOTICE-SENT
               PERFORM 3480-SPLIT-FOREIGN
               PERFORM 3490-ACCUM-CURRENCY
               PERFORM 3500-DESCRIBE-TRAN
               PERFORM 3510-COUNT-NOTICE
               PERFORM 3520-ASSESS-REVERSAL
               PERFORM 3530-DESCRIBE-TYPE
               PERFORM 3540-SELECT-MARGIN
               PERFORM 3550-ASSESS-EXEMPT
               PERFORM 3560-FLAG-BUYBACK
               PERFORM 3570-AGE-OPEN
               PERFORM 3580-ASSESS-NOTICE-SENT
               PERFORM 3590-DESCRIBE-TYPE
               PERFORM 3600-FLAG-BUYBACK
               PERFORM 3610-ASSESS-NOTICE-SENT
               PERFORM 3620-DESCRIBE-TYPE
               PERFORM 3630-CHECK-CLOSE
               PERFORM 3640-EDIT-MEMO
               PERFORM 3650-ASSESS-NOTICE-SENT
               PERFORM 3660-ACCUM-CURRENCY
               PERFORM 3670-AGE-EXPIRY
               PERFORM 3680-SET-BRANCH
               PERFORM 3690-SPLIT-FOREIGN
               PERFORM 3700-DESCRIBE-BRANCH
               PERFORM 3710-ACCUM-SOURCE
               PERFORM 3720-LIMIT-FOREIGN
               PERFORM 3730-SPLIT-FOREIGN
               PERFORM 3740-SELECT-RATE
               PERFORM 3750-DESCRIBE-TRAN
               PERFORM 3760-SPLIT-MARGIN
               PERFORM 3770-SET-ORDER-TYPE
               PERFORM 3780-AGE-MATURITY
               PERFORM 3790-AGE-LAST-PAY
               PERFORM 3800-FLAG-BUYBACK
               PERFORM 3810-PRORATE-USD
               PERFORM 3820-SPLIT-FOREIGN
               PERFORM 3830-LIMIT-USD
               PERFORM 3840-PRORATE-MARGIN
               PERFORM 3850-ACCRUE-FOREIGN
               PERFORM 3860-COUNT-PAYMENT
               PERFORM 3870-ACCUM-BRANCH
               PERFORM 3880-DESCRIBE-TRAN
               PERFORM 3890-AGE-OPEN
               PERFORM 3900-CHECK-NOTICE
               PERFORM 3910-ASSESS-OVERRIDE
               PERFORM 3920-CHECK-CLOSE
               PERFORM 3930-FLAG-FOREIGN
               PERFORM 3940-SET-CHANNEL
               PERFORM 3950-PRORATE-MARGIN
               PERFORM 3960-EDIT-MEMO
               PERFORM 3970-ACCUM-SOURCE
               PERFORM 3980-DESCRIBE-STATUS
               PERFORM 3990-FLAG-MARGIN
               PERFORM 4000-DESCRIBE-CURRENCY
               PERFORM 4010-SPLIT-MARGIN
               PERFORM 4020-SELECT-MARGIN
               PERFORM 4030-CHECK-EXPIRY
               PERFORM 4040-ACCRUE-USD
               PERFORM 4050-ASSESS-REVERSAL
               PERFORM 4060-COUNT-NOTICE
               PERFORM 4070-SPLIT-FOREIGN
               PERFORM 4080-FLAG-SHIPPING
               PERFORM 4090-SELECT-RATE
               PERFORM 4100-SELECT-RATE
               PERFORM 4110-FLAG-FOREIGN
               PERFORM 4120-SELECT-RATE
               PERFORM 4130-PRORATE-FOREIGN
               PERFORM 4140-ASSESS-AUTO-RENEW
               PERFORM 4150-ASSESS-PREFERRED
               PERFORM 4160-ASSESS-AUTO-RENEW
               PERFORM 4170-COUNT-SEQ
               PERFORM 4180-PRORATE-FOREIGN
               PERFORM 4190-EDIT-NAME
               PERFORM 4200-DESCRIBE-BRANCH
               PERFORM 4210-FLAG-FOREIGN
               PERFORM 4220-ACCRUE-FOREIGN
               PERFORM 4230-SET-STATUS
               PERFORM 4240-SELECT-PENALTY-PCT
               PERFORM 4250-EDIT-REJECT
               PERFORM 4260-ACCUM-TYPE
               PERFORM 4270-ASSESS-AUTO-RENEW
               PERFORM 4280-ACCUM-TRAN
               PERFORM 4290-SET-SOURCE
               PERFORM 4300-LIMIT-SHIPPING
               PERFORM 4310-EDIT-MEMO
               PERFORM 4320-EDIT-REF-NO
               PERFORM 4330-LIMIT-ITEM
               PERFORM 4340-PRORATE-MARGIN
               PERFORM 4350-SET-ORDER-TYPE
               PERFORM 4360-ASSESS-REVERSAL
               PERFORM 4370-SET-SOURCE
               PERFORM 4380-PRORATE-FOREIGN
               PERFORM 4390-ACCRUE-FOREIGN
               PERFORM 4400-PRORATE-FOREIGN
               PERFORM 4410-AGE-POST
               PERFORM 4420-AGE-EFF
               PERFORM 4430-ACCUM-TRAN
               PERFORM 4440-ACCUM-SOURCE
               PERFORM 4450-ACCUM-CURRENCY
               PERFORM 4460-AGE-POST
               PERFORM 4470-SELECT-MARGIN
               PERFORM 4480-PRORATE-USD
               PERFORM 4490-EDIT-CUST-ID
               PERFORM 4500-FLAG-BUYBACK
               PERFORM 4510-ASSESS-EXEMPT
               PERFORM 4520-COUNT-CYCLE
               PERFORM 4530-SPLIT-FOREIGN
               PERFORM 4540-PRORATE-USD
               PERFORM 4550-CHECK-NOTICE
               PERFORM 4560-COUNT-PAYMENT
               PERFORM 4570-ASSESS-EXEMPT
               PERFORM 4580-EDIT-DESC
               PERFORM 4590-ACCUM-SOURCE
               PERFORM 7040-CALL-FNSTAT
               REWRITE FXN-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'FXNMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM FXN-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPPING AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3000-ACCRUE-SHIPPING.
           IF FXNM-SHIPPING-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-SHIPPING-AMT * FXNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3010-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MARGIN YTD FEE ON THE USD AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3020-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FXNM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO FXNM-MARGIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE FXNM-MARGIN-YTD ROUNDED = FXNM-USD-AMT *
                       FXNM-RATE / 200
               WHEN FXNM-ORDER-TYPE-CD = 'DTU'
                   MOVE 20.00 TO FXNM-MARGIN-YTD
               WHEN FXNM-ORDER-TYPE-CD = 'KZV'
                   MOVE 10.00 TO FXNM-MARGIN-YTD
               WHEN OTHER
                   COMPUTE FXNM-MARGIN-YTD ROUNDED = FXNM-USD-AMT *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNM-MARGIN-YTD > 0 AND FXNM-MARGIN-YTD < 2.00
               MOVE 2.00 TO FXNM-MARGIN-YTD
           END-IF
           IF FXNM-MARGIN-YTD > 75.00
               MOVE 75.00 TO FXNM-MARGIN-YTD
           END-IF
           ADD FXNM-MARGIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3030-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3040-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARGIN AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3050-ACCRUE-MARGIN.
           IF FXNT-MARGIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-MARGIN-AMT * FXNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO FXNT-USD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT USD AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3060-SPLIT-USD.
           COMPUTE WS-WORK-AMT = FXNM-USD-AMT / 4
           COMPUTE WS-WORK-AMT-2 = FXNM-USD-AMT - (WS-WORK-AMT * 4)
           COMPUTE FXNT-BUYBACK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3070-LIMIT-MARGIN.
           IF FXNM-MARGIN-AMT < 100.00
               MOVE 100.00 TO FXNM-MARGIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-MARGIN-AMT > 1000.00
               MOVE 1000.00 TO FXNM-MARGIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARGIN YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3080-ACCRUE-MARGIN.
           IF FXNM-MARGIN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-MARGIN-YTD * FXNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO FXNT-MARGIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * C -> E WHEN OPEN DATE IS PAST
      * E -> G WHEN OPEN DATE IS PAST
      * G -> P WHEN OPEN DATE IS PAST
      * P -> W WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3090-SET-CURRENCY.
           EVALUATE FXNM-CURRENCY-CD
               WHEN 'C'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE MARGIN AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-STATUS-CD
                   ADD FXNT-MARGIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNT-MARGIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * APZ -> BBP WHEN LAST PAY DATE IS PAST
      * BBP -> ENL WHEN LAST PAY DATE IS PAST
      * ENL -> EZB WHEN LAST PAY DATE IS PAST
      * EZB -> KHV WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-BRANCH.
           EVALUATE FXNM-BRANCH-CD
               WHEN 'APZ'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BBP' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BBP'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ENL' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ENL'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EZB' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EZB'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHV' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BUYBACK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3120-SPLIT-BUYBACK.
           COMPUTE WS-WORK-AMT = FXNT-BUYBACK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = FXNT-BUYBACK-AMT - (WS-WORK-AMT * 3)
           COMPUTE FXNM-USD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE USD SCHED TIER FOR THE SHIPPING YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3140-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-USD-SCHED-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF FXNM-SHIPPING-YTD NOT < WS-USD-SCHED-LOW(WS-TX)
                   MOVE WS-USD-SCHED-RATE(WS-TX) TO FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE USD YTD FEE ON THE SHIPPING YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3150-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN FXNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO FXNM-USD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE FXNM-USD-YTD ROUNDED = FXNM-SHIPPING-YTD *
                       FXNM-MARGIN / 200
               WHEN FXNM-TYPE-CD = 'CFP'
                   MOVE 7.00 TO FXNM-USD-YTD
               WHEN FXNM-TYPE-CD = 'FFU'
                   MOVE 5.00 TO FXNM-USD-YTD
               WHEN OTHER
                   COMPUTE FXNM-USD-YTD ROUNDED = FXNM-SHIPPING-YTD *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNM-USD-YTD > 0 AND FXNM-USD-YTD < 1.00
               MOVE 1.00 TO FXNM-USD-YTD
           END-IF
           IF FXNM-USD-YTD > 150.00
               MOVE 150.00 TO FXNM-USD-YTD
           END-IF
           ADD FXNM-USD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3160-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3170-LIMIT-BUYBACK.
           IF FXNM-BUYBACK-YTD < 0.00
               MOVE 0.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-BUYBACK-YTD > 50000.00
               MOVE 50000.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE USD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPPING YTD.
      *----------------------------------------------------------------*
       3180-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-SHIPPING-YTD ROUNDED =
                   FXNM-USD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-SHIPPING-YTD
           END-IF
           ADD FXNM-SHIPPING-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3190-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNT-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3200-DESCRIBE-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE USD SCHED TIER FOR THE FOREIGN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3210-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-USD-SCHED-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF FXNM-FOREIGN-YTD NOT < WS-USD-SCHED-LOW(WS-TX)
                   MOVE WS-USD-SCHED-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE USD SCHED TIER FOR THE BUYBACK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3220-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-USD-SCHED-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF FXNT-BUYBACK-AMT NOT < WS-USD-SCHED-LOW(WS-TX)
                   MOVE WS-USD-SCHED-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON USD AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3230-ACCRUE-USD.
           IF FXNM-USD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-USD-AMT * FXNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO FXNM-SHIPPING-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE USD SCHED TIER FOR THE USD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3240-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-USD-SCHED-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF FXNM-USD-YTD NOT < WS-USD-SCHED-LOW(WS-TX)
                   MOVE WS-USD-SCHED-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3250-FLAG-MARGIN.
           IF FXNT-MARGIN-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNT-MARGIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FOREIGN AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3260-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNT-CHANNEL-CD
                   ADD FXNM-FOREIGN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-FOREIGN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK TIER TIER FOR THE BUYBACK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-TIER-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF FXNT-BUYBACK-AMT NOT < WS-BUYBACK-TIER-LOW(WS-TX)
                   MOVE WS-BUYBACK-TIER-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MARGIN AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3280-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = FXNM-MARGIN-AMT / 4
           COMPUTE WS-WORK-AMT-2 = FXNM-MARGIN-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3290-DESCRIBE-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CFP -> FFU WHEN POST DATE IS PAST
      * FFU -> JNR WHEN POST DATE IS PAST
      * JNR -> UGS WHEN POST DATE IS PAST
      * UGS -> XUM WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3300-SET-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FFU' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FFU'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JNR' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JNR'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UGS' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UGS'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XUM' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUYBACK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3310-ACCRUE-BUYBACK.
           IF FXNT-BUYBACK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-BUYBACK-AMT * FXNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO FXNM-FOREIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3320-DESCRIBE-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN YTD FEE ON THE MARGIN YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3330-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FXNM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO FXNM-FOREIGN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE FXNM-FOREIGN-YTD ROUNDED = FXNM-MARGIN-YTD *
                       FXNM-MARGIN / 200
               WHEN FXNM-CURRENCY-CD = 'C'
                   MOVE 12.00 TO FXNM-FOREIGN-YTD
               WHEN FXNM-CURRENCY-CD = 'E'
                   MOVE 3.00 TO FXNM-FOREIGN-YTD
               WHEN OTHER
                   COMPUTE FXNM-FOREIGN-YTD ROUNDED = FXNM-MARGIN-YTD *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNM-FOREIGN-YTD > 0 AND FXNM-FOREIGN-YTD < 5.00
               MOVE 5.00 TO FXNM-FOREIGN-YTD
           END-IF
           IF FXNM-FOREIGN-YTD > 50.00
               MOVE 50.00 TO FXNM-FOREIGN-YTD
           END-IF
           ADD FXNM-FOREIGN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE USD SCHED TIER FOR THE BUYBACK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3340-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-USD-SCHED-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF FXNT-BUYBACK-AMT NOT < WS-USD-SCHED-LOW(WS-TX)
                   MOVE WS-USD-SCHED-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPPING FEE ON THE BUYBACK YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3350-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO FXNM-SHIPPING-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   COMPUTE FXNM-SHIPPING-AMT ROUNDED = FXNM-BUYBACK-YTD
                       * FXNM-PENALTY-PCT / 200
               WHEN FXNT-TRAN-CD = 'OR'
                   MOVE 12.00 TO FXNM-SHIPPING-AMT
               WHEN FXNT-TRAN-CD = 'BB'
                   MOVE 5.00 TO FXNM-SHIPPING-AMT
               WHEN OTHER
                   COMPUTE FXNM-SHIPPING-AMT ROUNDED = FXNM-BUYBACK-YTD
                       * FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNM-SHIPPING-AMT > 0 AND FXNM-SHIPPING-AMT < 10.00
               MOVE 10.00 TO FXNM-SHIPPING-AMT
           END-IF
           IF FXNM-SHIPPING-AMT > 25.00
               MOVE 25.00 TO FXNM-SHIPPING-AMT
           END-IF
           ADD FXNM-SHIPPING-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * USD AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3360-LIMIT-USD.
           IF FXNT-USD-AMT < 0.00
               MOVE 0.00 TO FXNT-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNT-USD-AMT > 2500.00
               MOVE 2500.00 TO FXNT-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MARGIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MARGIN AMT.
      *----------------------------------------------------------------*
       3370-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-MARGIN-AMT ROUNDED =
                   FXNM-MARGIN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNT-MARGIN-AMT
           END-IF
           ADD FXNT-MARGIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE USD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FOREIGN AMT.
      *----------------------------------------------------------------*
       3380-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-FOREIGN-AMT ROUNDED =
                   FXNT-USD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNT-FOREIGN-AMT
           END-IF
           ADD FXNT-FOREIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FOREIGN YTD.
      *----------------------------------------------------------------*
       3390-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-FOREIGN-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-FOREIGN-YTD
           END-IF
           ADD FXNM-FOREIGN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BUYBACK YTD.
      *----------------------------------------------------------------*
       3400-PRORATE-BUYBACK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-BUYBACK-YTD ROUNDED =
                   FXNT-FOREIGN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-BUYBACK-YTD
           END-IF
           ADD FXNM-BUYBACK-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * MARGIN YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3410-FLAG-MARGIN.
           IF FXNM-MARGIN-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNM-MARGIN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FOREIGN YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3420-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = FXNM-FOREIGN-YTD / 3
           COMPUTE WS-WORK-AMT-2 = FXNM-FOREIGN-YTD - (WS-WORK-AMT * 3)
           COMPUTE FXNT-MARGIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3430-FLAG-BUYBACK.
           IF FXNM-BUYBACK-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNM-BUYBACK-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3440-LIMIT-MARGIN.
           IF FXNM-MARGIN-YTD < 100.00
               MOVE 100.00 TO FXNM-MARGIN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-MARGIN-YTD > 2500.00
               MOVE 2500.00 TO FXNM-MARGIN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E177 ADDR REQUIRED
      * E073 BUYBACK AMT NEGATIVE
      * E804 BUYBACK AMT OVER 50000
      * E645 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3450-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-ADDR = SPACES
                   MOVE 'E177' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-AMT < 0
                   MOVE 'E073' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-AMT > 50000.00
                   MOVE 'E804' TO WS-REJECT-CODE
               WHEN FXNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E645' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MARGIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3460-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-MARGIN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE MARGIN YTD FEE ON THE USD AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3470-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO FXNM-MARGIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-PREFERRED-FLAG = 'Y'
                   COMPUTE FXNM-MARGIN-YTD ROUNDED = FXNM-USD-AMT *
                       FXNM-MARGIN / 200
               WHEN FXNM-BRANCH-CD = 'APZ'
                   MOVE 5.00 TO FXNM-MARGIN-YTD
               WHEN FXNM-BRANCH-CD = 'BBP'
                   MOVE 10.00 TO FXNM-MARGIN-YTD
               WHEN OTHER
                   COMPUTE FXNM-MARGIN-YTD ROUNDED = FXNM-USD-AMT *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNM-MARGIN-YTD > 0 AND FXNM-MARGIN-YTD < 5.00
               MOVE 5.00 TO FXNM-MARGIN-YTD
           END-IF
           IF FXNM-MARGIN-YTD > 100.00
               MOVE 100.00 TO FXNM-MARGIN-YTD
           END-IF
           ADD FXNM-MARGIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT FOREIGN AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3480-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = FXNT-FOREIGN-AMT / 4
           COMPUTE WS-WORK-AMT-2 = FXNT-FOREIGN-AMT - (WS-WORK-AMT * 4)
           COMPUTE FXNM-BUYBACK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3490-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-CURRENCY-CD
                   ADD FXNT-BUYBACK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNT-BUYBACK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'CN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK FEE ON THE USD AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3520-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN FXNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO FXNT-BUYBACK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = FXNT-USD-AMT *
                       FXNM-RATE / 200
               WHEN FXNM-BRANCH-CD = 'APZ'
                   MOVE 12.00 TO FXNT-BUYBACK-AMT
               WHEN FXNM-BRANCH-CD = 'BBP'
                   MOVE 15.00 TO FXNT-BUYBACK-AMT
               WHEN OTHER
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = FXNT-USD-AMT *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNT-BUYBACK-AMT > 0 AND FXNT-BUYBACK-AMT < 1.00
               MOVE 1.00 TO FXNT-BUYBACK-AMT
           END-IF
           IF FXNT-BUYBACK-AMT > 150.00
               MOVE 150.00 TO FXNT-BUYBACK-AMT
           END-IF
           ADD FXNT-BUYBACK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3530-DESCRIBE-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3540-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-TIER-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-BUYBACK-TIER-LOW(WS-TX)
                   MOVE WS-BUYBACK-TIER-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3550-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO FXNM-BUYBACK-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE FXNM-BUYBACK-YTD ROUNDED = WS-WORK-AMT-2 *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNM-SOURCE-CD = 'DG'
                   MOVE 5.00 TO FXNM-BUYBACK-YTD
               WHEN FXNM-SOURCE-CD = 'FS'
                   MOVE 12.00 TO FXNM-BUYBACK-YTD
               WHEN OTHER
                   COMPUTE FXNM-BUYBACK-YTD ROUNDED = WS-WORK-AMT-2 *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNM-BUYBACK-YTD > 0 AND FXNM-BUYBACK-YTD < 5.00
               MOVE 5.00 TO FXNM-BUYBACK-YTD
           END-IF
           IF FXNM-BUYBACK-YTD > 35.00
               MOVE 35.00 TO FXNM-BUYBACK-YTD
           END-IF
           ADD FXNM-BUYBACK-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * BUYBACK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3560-FLAG-BUYBACK.
           IF FXNT-BUYBACK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNT-BUYBACK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3570-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPPING YTD FEE ON THE MARGIN AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3580-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO FXNM-SHIPPING-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE FXNM-SHIPPING-YTD ROUNDED = FXNT-MARGIN-AMT *
                       FXNM-MARGIN / 200
               WHEN FXNM-ORDER-TYPE-CD = 'DTU'
                   MOVE 20.00 TO FXNM-SHIPPING-YTD
               WHEN FXNM-ORDER-TYPE-CD = 'KZV'
                   MOVE 3.00 TO FXNM-SHIPPING-YTD
               WHEN OTHER
                   COMPUTE FXNM-SHIPPING-YTD ROUNDED = FXNT-MARGIN-AMT *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNM-SHIPPING-YTD > 0 AND FXNM-SHIPPING-YTD < 1.00
               MOVE 1.00 TO FXNM-SHIPPING-YTD
           END-IF
           IF FXNM-SHIPPING-YTD > 25.00
               MOVE 25.00 TO FXNM-SHIPPING-YTD
           END-IF
           ADD FXNM-SHIPPING-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3590-DESCRIBE-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BUYBACK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3600-FLAG-BUYBACK.
           IF FXNT-BUYBACK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNT-BUYBACK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN YTD FEE ON THE USD AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3610-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO FXNM-FOREIGN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE FXNM-FOREIGN-YTD ROUNDED = FXNT-USD-AMT *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNM-STATUS-CD = 'CXH'
                   MOVE 7.00 TO FXNM-FOREIGN-YTD
               WHEN FXNM-STATUS-CD = 'ESS'
                   MOVE 10.00 TO FXNM-FOREIGN-YTD
               WHEN OTHER
                   COMPUTE FXNM-FOREIGN-YTD ROUNDED = FXNT-USD-AMT *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNM-FOREIGN-YTD > 0 AND FXNM-FOREIGN-YTD < 1.00
               MOVE 1.00 TO FXNM-FOREIGN-YTD
           END-IF
           IF FXNM-FOREIGN-YTD > 50.00
               MOVE 50.00 TO FXNM-FOREIGN-YTD
           END-IF
           ADD FXNM-FOREIGN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3620-DESCRIBE-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3630-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E990 MEMO REQUIRED
      * E862 BUYBACK AMT NEGATIVE
      * E649 BUYBACK AMT OVER 1000000
      * E215 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-MEMO = SPACES
                   MOVE 'E990' TO WS-REJECT-CODE
               WHEN FXNT-BUYBACK-AMT < 0
                   MOVE 'E862' TO WS-REJECT-CODE
               WHEN FXNT-BUYBACK-AMT > 1000000.00
                   MOVE 'E649' TO WS-REJECT-CODE
               WHEN FXNM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E215' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE USD YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3650-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = FXNM-USD-YTD *
                       FXNM-MARGIN / 200
               WHEN FXNM-CURRENCY-CD = 'C'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN FXNM-CURRENCY-CD = 'E'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = FXNM-USD-YTD *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3660-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-CURRENCY-CD
                   ADD FXNT-BUYBACK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNT-BUYBACK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3670-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-EXPIRY-DATE > 0 AND FXNM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * APZ -> BBP WHEN CLOSE DATE IS PAST
      * BBP -> ENL WHEN CLOSE DATE IS PAST
      * ENL -> EZB WHEN CLOSE DATE IS PAST
      * EZB -> KHV WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3680-SET-BRANCH.
           EVALUATE FXNM-BRANCH-CD
               WHEN 'APZ'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BBP' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BBP'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ENL' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ENL'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EZB' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EZB'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHV' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FOREIGN YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3690-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = FXNM-FOREIGN-YTD / 3
           COMPUTE WS-WORK-AMT-2 = FXNM-FOREIGN-YTD - (WS-WORK-AMT * 3)
           COMPUTE FXNT-BUYBACK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3700-DESCRIBE-BRANCH.
           EVALUATE FXNM-BRANCH-CD
               WHEN 'APZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BBP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ENL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'EZB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KHV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TDN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE USD AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3710-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-SOURCE-CD
                   ADD FXNM-USD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-USD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3720-LIMIT-FOREIGN.
           IF FXNM-FOREIGN-YTD < 25.00
               MOVE 25.00 TO FXNM-FOREIGN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-FOREIGN-YTD > 2500.00
               MOVE 2500.00 TO FXNM-FOREIGN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FOREIGN AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3730-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = FXNM-FOREIGN-AMT / 12
           COMPUTE WS-WORK-AMT-2 = FXNM-FOREIGN-AMT - (WS-WORK-AMT * 12)
           COMPUTE FXNM-USD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE USD SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3740-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-USD-SCHED-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-USD-SCHED-LOW(WS-TX)
                   MOVE WS-USD-SCHED-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3750-DESCRIBE-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'CN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT MARGIN AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3760-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = FXNT-MARGIN-AMT / 3
           COMPUTE WS-WORK-AMT-2 = FXNT-MARGIN-AMT - (WS-WORK-AMT * 3)
           COMPUTE FXNM-BUYBACK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ORDER TYPE CD:
      * DTU -> KZV WHEN EFFECTIVE DATE IS PAST
      * KZV -> PHV WHEN EFFECTIVE DATE IS PAST
      * PHV -> PZZ WHEN EFFECTIVE DATE IS PAST
      * PZZ -> WMC WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3770-SET-ORDER-TYPE.
           EVALUATE FXNM-ORDER-TYPE-CD
               WHEN 'DTU'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KZV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KZV'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PHV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PHV'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PZZ' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PZZ'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WMC' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3780-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3790-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BUYBACK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3800-FLAG-BUYBACK.
           IF FXNT-BUYBACK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNT-BUYBACK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO USD YTD.
      *----------------------------------------------------------------*
       3810-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-USD-YTD ROUNDED =
                   FXNM-FOREIGN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-USD-YTD
           END-IF
           ADD FXNM-USD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT FOREIGN YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3820-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = FXNM-FOREIGN-YTD / 3
           COMPUTE WS-WORK-AMT-2 = FXNM-FOREIGN-YTD - (WS-WORK-AMT * 3)
           COMPUTE FXNT-MARGIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * USD AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3830-LIMIT-USD.
           IF FXNT-USD-AMT < 100.00
               MOVE 100.00 TO FXNT-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNT-USD-AMT > 2500.00
               MOVE 2500.00 TO FXNT-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MARGIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3840-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   FXNT-MARGIN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3850-ACCRUE-FOREIGN.
           IF FXNM-FOREIGN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-FOREIGN-YTD * FXNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3860-COUNT-PAYMENT.
           IF FXNM-PAYMENT-CNT < 9999999
               ADD 1 TO FXNM-PAYMENT-CNT
           END-IF
           IF FXNM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FOREIGN YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3870-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-BRANCH-CD
                   ADD FXNM-FOREIGN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-FOREIGN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3880-DESCRIBE-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'CN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3890-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3900-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK FEE ON THE FOREIGN AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3910-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO FXNT-BUYBACK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = FXNM-FOREIGN-AMT *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNM-STATUS-CD = 'CXH'
                   MOVE 5.00 TO FXNT-BUYBACK-AMT
               WHEN FXNM-STATUS-CD = 'ESS'
                   MOVE 10.00 TO FXNT-BUYBACK-AMT
               WHEN OTHER
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = FXNM-FOREIGN-AMT *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNT-BUYBACK-AMT > 0 AND FXNT-BUYBACK-AMT < 1.00
               MOVE 1.00 TO FXNT-BUYBACK-AMT
           END-IF
           IF FXNT-BUYBACK-AMT > 150.00
               MOVE 150.00 TO FXNT-BUYBACK-AMT
           END-IF
           ADD FXNT-BUYBACK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3920-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3930-FLAG-FOREIGN.
           IF FXNM-FOREIGN-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNM-FOREIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFFECTIVE DATE IS PAST
      * BAT -> MAL WHEN EFFECTIVE DATE IS PAST
      * MAL -> TEL WHEN EFFECTIVE DATE IS PAST
      * TEL -> ONL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3940-SET-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE MARGIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MARGIN AMT.
      *----------------------------------------------------------------*
       3950-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-MARGIN-AMT ROUNDED =
                   FXNM-MARGIN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNT-MARGIN-AMT
           END-IF
           ADD FXNT-MARGIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E346 MEMO REQUIRED
      * E323 FOREIGN AMT NEGATIVE
      * E555 FOREIGN AMT OVER 50000
      * E066 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-MEMO = SPACES
                   MOVE 'E346' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-AMT < 0
                   MOVE 'E323' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-AMT > 50000.00
                   MOVE 'E555' TO WS-REJECT-CODE
               WHEN FXNM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E066' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MARGIN AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3970-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-SOURCE-CD
                   ADD FXNM-MARGIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-MARGIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3980-DESCRIBE-STATUS.
           EVALUATE FXNM-STATUS-CD
               WHEN 'CXH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ESS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XDE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XFD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MARGIN YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3990-FLAG-MARGIN.
           IF FXNM-MARGIN-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNM-MARGIN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CURRENCY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4000-DESCRIBE-CURRENCY.
           EVALUATE FXNM-CURRENCY-CD
               WHEN 'C'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'E'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'G'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'P'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT MARGIN AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4010-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = FXNT-MARGIN-AMT / 3
           COMPUTE WS-WORK-AMT-2 = FXNT-MARGIN-AMT - (WS-WORK-AMT * 3)
           COMPUTE FXNM-BUYBACK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK TIER TIER FOR THE BUYBACK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4020-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-TIER-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF FXNM-BUYBACK-AMT NOT < WS-BUYBACK-TIER-LOW(WS-TX)
                   MOVE WS-BUYBACK-TIER-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4030-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON USD AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4040-ACCRUE-USD.
           IF FXNT-USD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-USD-AMT * FXNM-RATE / 100 / 365
               ADD WS-WORK-AMT TO FXNT-BUYBACK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPPING FEE ON THE BUYBACK YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4050-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN FXNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO FXNM-SHIPPING-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE FXNM-SHIPPING-AMT ROUNDED = FXNM-BUYBACK-YTD
                       * FXNM-PENALTY-PCT / 200
               WHEN FXNT-TRAN-CD = 'OR'
                   MOVE 7.00 TO FXNM-SHIPPING-AMT
               WHEN FXNT-TRAN-CD = 'BB'
                   MOVE 3.00 TO FXNM-SHIPPING-AMT
               WHEN OTHER
                   COMPUTE FXNM-SHIPPING-AMT ROUNDED = FXNM-BUYBACK-YTD
                       * FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNM-SHIPPING-AMT > 0 AND FXNM-SHIPPING-AMT < 5.00
               MOVE 5.00 TO FXNM-SHIPPING-AMT
           END-IF
           IF FXNM-SHIPPING-AMT > 25.00
               MOVE 25.00 TO FXNM-SHIPPING-AMT
           END-IF
           ADD FXNM-SHIPPING-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4060-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FOREIGN YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4070-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = FXNM-FOREIGN-YTD / 3
           COMPUTE WS-WORK-AMT-2 = FXNM-FOREIGN-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHIPPING AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4080-FLAG-SHIPPING.
           IF FXNM-SHIPPING-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNM-SHIPPING-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE USD SCHED TIER FOR THE SHIPPING AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4090-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-USD-SCHED-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF FXNM-SHIPPING-AMT NOT < WS-USD-SCHED-LOW(WS-TX)
                   MOVE WS-USD-SCHED-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE USD SCHED TIER FOR THE FOREIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4100-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-USD-SCHED-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF FXNT-FOREIGN-AMT NOT < WS-USD-SCHED-LOW(WS-TX)
                   MOVE WS-USD-SCHED-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4110-FLAG-FOREIGN.
           IF FXNM-FOREIGN-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNM-FOREIGN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK TIER TIER FOR THE USD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4120-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-TIER-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF FXNM-USD-YTD NOT < WS-BUYBACK-TIER-LOW(WS-TX)
                   MOVE WS-BUYBACK-TIER-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MARGIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FOREIGN AMT.
      *----------------------------------------------------------------*
       4130-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-FOREIGN-AMT ROUNDED =
                   FXNT-MARGIN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-FOREIGN-AMT
           END-IF
           ADD FXNM-FOREIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN FEE ON THE MARGIN AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4140-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO FXNT-FOREIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   COMPUTE FXNT-FOREIGN-AMT ROUNDED = FXNT-MARGIN-AMT *
                       FXNM-MARGIN / 200
               WHEN FXNM-STATUS-CD = 'CXH'
                   MOVE 7.00 TO FXNT-FOREIGN-AMT
               WHEN FXNM-STATUS-CD = 'ESS'
                   MOVE 5.00 TO FXNT-FOREIGN-AMT
               WHEN OTHER
                   COMPUTE FXNT-FOREIGN-AMT ROUNDED = FXNT-MARGIN-AMT *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNT-FOREIGN-AMT > 0 AND FXNT-FOREIGN-AMT < 1.00
               MOVE 1.00 TO FXNT-FOREIGN-AMT
           END-IF
           IF FXNT-FOREIGN-AMT > 35.00
               MOVE 35.00 TO FXNT-FOREIGN-AMT
           END-IF
           ADD FXNT-FOREIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE USD FEE ON THE SHIPPING AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4150-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FXNM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO FXNT-USD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   COMPUTE FXNT-USD-AMT ROUNDED = FXNM-SHIPPING-AMT *
                       FXNM-RATE / 200
               WHEN FXNM-CURRENCY-CD = 'C'
                   MOVE 5.00 TO FXNT-USD-AMT
               WHEN FXNM-CURRENCY-CD = 'E'
                   MOVE 12.00 TO FXNT-USD-AMT
               WHEN OTHER
                   COMPUTE FXNT-USD-AMT ROUNDED = FXNM-SHIPPING-AMT *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNT-USD-AMT > 0 AND FXNT-USD-AMT < 10.00
               MOVE 10.00 TO FXNT-USD-AMT
           END-IF
           IF FXNT-USD-AMT > 25.00
               MOVE 25.00 TO FXNT-USD-AMT
           END-IF
           ADD FXNT-USD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK FEE ON THE USD YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4160-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO FXNT-BUYBACK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNT-REVERSAL-FLAG = 'Y'
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = FXNM-USD-YTD *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNM-SOURCE-CD = 'DG'
                   MOVE 10.00 TO FXNT-BUYBACK-AMT
               WHEN FXNM-SOURCE-CD = 'FS'
                   MOVE 7.00 TO FXNT-BUYBACK-AMT
               WHEN OTHER
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = FXNM-USD-YTD *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNT-BUYBACK-AMT > 0 AND FXNT-BUYBACK-AMT < 1.00
               MOVE 1.00 TO FXNT-BUYBACK-AMT
           END-IF
           IF FXNT-BUYBACK-AMT > 35.00
               MOVE 35.00 TO FXNT-BUYBACK-AMT
           END-IF
           ADD FXNT-BUYBACK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4170-COUNT-SEQ.
           IF FXNT-SEQ < 9999999
               ADD 1 TO FXNT-SEQ
           END-IF
           IF FXNT-SEQ NOT < 5
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FOREIGN AMT.
      *----------------------------------------------------------------*
       4180-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-FOREIGN-AMT ROUNDED =
                   FXNM-FOREIGN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNT-FOREIGN-AMT
           END-IF
           ADD FXNT-FOREIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E348 NAME REQUIRED
      * E309 WORK AMT 2 NEGATIVE
      * E160 WORK AMT 2 OVER 25000
      * E865 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4190-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-NAME = SPACES
                   MOVE 'E348' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E309' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E160' TO WS-REJECT-CODE
               WHEN FXNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E865' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4200-DESCRIBE-BRANCH.
           EVALUATE FXNM-BRANCH-CD
               WHEN 'APZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BBP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ENL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'EZB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KHV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TDN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FOREIGN YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4210-FLAG-FOREIGN.
           IF FXNM-FOREIGN-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNM-FOREIGN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4220-ACCRUE-FOREIGN.
           IF FXNT-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-FOREIGN-AMT * FXNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO FXNM-MARGIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CXH -> ESS WHEN EFF DATE IS PAST
      * ESS -> XDE WHEN EFF DATE IS PAST
      * XDE -> XFD WHEN EFF DATE IS PAST
      * XFD -> CXH WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4230-SET-STATUS.
           EVALUATE FXNM-STATUS-CD
               WHEN 'CXH'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ESS' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ESS'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XDE' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XDE'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XFD' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XFD'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CXH' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK TIER TIER FOR THE SHIPPING YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4240-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-TIER-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF FXNM-SHIPPING-YTD NOT < WS-BUYBACK-TIER-LOW(WS-TX)
                   MOVE WS-BUYBACK-TIER-RATE(WS-TX) TO FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E623 REJECT CD REQUIRED
      * E833 USD YTD NEGATIVE
      * E119 USD YTD OVER 1000000
      * E287 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4250-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-REJECT-CD = SPACES
                   MOVE 'E623' TO WS-REJECT-CODE
               WHEN FXNM-USD-YTD < 0
                   MOVE 'E833' TO WS-REJECT-CODE
               WHEN FXNM-USD-YTD > 1000000.00
                   MOVE 'E119' TO WS-REJECT-CODE
               WHEN FXNM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E287' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE USD AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4260-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-TYPE-CD
                   ADD FXNT-USD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNT-USD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE USD FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4270-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO FXNM-USD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE FXNM-USD-AMT ROUNDED = WS-WORK-AMT-2 *
                       FXNM-MARGIN / 200
               WHEN FXNM-TYPE-CD = 'CFP'
                   MOVE 10.00 TO FXNM-USD-AMT
               WHEN FXNM-TYPE-CD = 'FFU'
                   MOVE 5.00 TO FXNM-USD-AMT
               WHEN OTHER
                   COMPUTE FXNM-USD-AMT ROUNDED = WS-WORK-AMT-2 *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNM-USD-AMT > 0 AND FXNM-USD-AMT < 2.00
               MOVE 2.00 TO FXNM-USD-AMT
           END-IF
           IF FXNM-USD-AMT > 100.00
               MOVE 100.00 TO FXNM-USD-AMT
           END-IF
           ADD FXNM-USD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE USD AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4280-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNT-TRAN-CD
                   ADD FXNM-USD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-USD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN LAST PAY DATE IS PAST
      * FS -> HL WHEN LAST PAY DATE IS PAST
      * HL -> HS WHEN LAST PAY DATE IS PAST
      * HS -> NW WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4290-SET-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FS'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HS'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NW' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHIPPING AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4300-LIMIT-SHIPPING.
           IF FXNM-SHIPPING-AMT < 50.00
               MOVE 50.00 TO FXNM-SHIPPING-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-SHIPPING-AMT > 5000.00
               MOVE 5000.00 TO FXNM-SHIPPING-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E261 MEMO REQUIRED
      * E333 BUYBACK AMT NEGATIVE
      * E625 BUYBACK AMT OVER 5000
      * E798 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4310-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-MEMO = SPACES
                   MOVE 'E261' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-AMT < 0
                   MOVE 'E333' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-AMT > 5000.00
                   MOVE 'E625' TO WS-REJECT-CODE
               WHEN FXNM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E798' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E887 REF NO REQUIRED
      * E623 WORK AMT NEGATIVE
      * E432 WORK AMT OVER 1000000
      * E466 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4320-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-REF-NO = SPACES
                   MOVE 'E887' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E623' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E432' TO WS-REJECT-CODE
               WHEN FXNM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E466' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4330-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MARGIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4340-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   FXNM-MARGIN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ORDER TYPE CD:
      * DTU -> KZV WHEN MATURITY DATE IS PAST
      * KZV -> PHV WHEN MATURITY DATE IS PAST
      * PHV -> PZZ WHEN MATURITY DATE IS PAST
      * PZZ -> WMC WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4350-SET-ORDER-TYPE.
           EVALUATE FXNM-ORDER-TYPE-CD
               WHEN 'DTU'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KZV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KZV'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PHV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PHV'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PZZ' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PZZ'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WMC' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN YTD FEE ON THE USD YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4360-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN FXNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO FXNM-FOREIGN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE FXNM-FOREIGN-YTD ROUNDED = FXNM-USD-YTD *
                       FXNM-MARGIN / 200
               WHEN FXNT-TRAN-CD = 'OR'
                   MOVE 15.00 TO FXNM-FOREIGN-YTD
               WHEN FXNT-TRAN-CD = 'BB'
                   MOVE 15.00 TO FXNM-FOREIGN-YTD
               WHEN OTHER
                   COMPUTE FXNM-FOREIGN-YTD ROUNDED = FXNM-USD-YTD *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNM-FOREIGN-YTD > 0 AND FXNM-FOREIGN-YTD < 2.00
               MOVE 2.00 TO FXNM-FOREIGN-YTD
           END-IF
           IF FXNM-FOREIGN-YTD > 150.00
               MOVE 150.00 TO FXNM-FOREIGN-YTD
           END-IF
           ADD FXNM-FOREIGN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN MATURITY DATE IS PAST
      * FS -> HL WHEN MATURITY DATE IS PAST
      * HL -> HS WHEN MATURITY DATE IS PAST
      * HS -> NW WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4370-SET-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FS'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HS'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NW' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SHIPPING YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FOREIGN AMT.
      *----------------------------------------------------------------*
       4380-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-FOREIGN-AMT ROUNDED =
                   FXNM-SHIPPING-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNT-FOREIGN-AMT
           END-IF
           ADD FXNT-FOREIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4390-ACCRUE-FOREIGN.
           IF FXNM-FOREIGN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-FOREIGN-YTD * FXNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO FXNM-USD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUYBACK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FOREIGN AMT.
      *----------------------------------------------------------------*
       4400-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-FOREIGN-AMT ROUNDED =
                   FXNM-BUYBACK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-FOREIGN-AMT
           END-IF
           ADD FXNM-FOREIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4410-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-POST-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4420-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE USD AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4430-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNT-TRAN-CD
                   ADD FXNM-USD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-USD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FOREIGN YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4440-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-SOURCE-CD
                   ADD FXNM-FOREIGN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-FOREIGN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4450-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-CURRENCY-CD
                   ADD FXNT-BUYBACK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNT-BUYBACK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4460-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE USD SCHED TIER FOR THE SHIPPING AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4470-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-USD-SCHED-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF FXNM-SHIPPING-AMT NOT < WS-USD-SCHED-LOW(WS-TX)
                   MOVE WS-USD-SCHED-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE USD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4480-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   FXNM-USD-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E234 CUST ID REQUIRED
      * E852 MARGIN AMT NEGATIVE
      * E445 MARGIN AMT OVER 25000
      * E980 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4490-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-CUST-ID = SPACES
                   MOVE 'E234' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-AMT < 0
                   MOVE 'E852' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-AMT > 25000.00
                   MOVE 'E445' TO WS-REJECT-CODE
               WHEN FXNM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E980' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4500-FLAG-BUYBACK.
           IF FXNM-BUYBACK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNM-BUYBACK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MARGIN FEE ON THE SHIPPING AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4510-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO FXNM-MARGIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE FXNM-MARGIN-AMT ROUNDED = FXNM-SHIPPING-AMT *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNM-CURRENCY-CD = 'C'
                   MOVE 5.00 TO FXNM-MARGIN-AMT
               WHEN FXNM-CURRENCY-CD = 'E'
                   MOVE 3.00 TO FXNM-MARGIN-AMT
               WHEN OTHER
                   COMPUTE FXNM-MARGIN-AMT ROUNDED = FXNM-SHIPPING-AMT *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNM-MARGIN-AMT > 0 AND FXNM-MARGIN-AMT < 10.00
               MOVE 10.00 TO FXNM-MARGIN-AMT
           END-IF
           IF FXNM-MARGIN-AMT > 100.00
               MOVE 100.00 TO FXNM-MARGIN-AMT
           END-IF
           ADD FXNM-MARGIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4520-COUNT-CYCLE.
           IF FXNM-CYCLE-CNT < 9999999
               ADD 1 TO FXNM-CYCLE-CNT
           END-IF
           IF FXNM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FOREIGN AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4530-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = FXNM-FOREIGN-AMT / 12
           COMPUTE WS-WORK-AMT-2 = FXNM-FOREIGN-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE USD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4540-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-USD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4550-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4560-COUNT-PAYMENT.
           IF FXNM-PAYMENT-CNT < 9999999
               ADD 1 TO FXNM-PAYMENT-CNT
           END-IF
           IF FXNM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN FEE ON THE MARGIN AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4570-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO FXNT-FOREIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNT-REVERSAL-FLAG = 'Y'
                   COMPUTE FXNT-FOREIGN-AMT ROUNDED = FXNT-MARGIN-AMT *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNM-STATUS-CD = 'CXH'
                   MOVE 15.00 TO FXNT-FOREIGN-AMT
               WHEN FXNM-STATUS-CD = 'ESS'
                   MOVE 10.00 TO FXNT-FOREIGN-AMT
               WHEN OTHER
                   COMPUTE FXNT-FOREIGN-AMT ROUNDED = FXNT-MARGIN-AMT *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNT-FOREIGN-AMT > 0 AND FXNT-FOREIGN-AMT < 1.00
               MOVE 1.00 TO FXNT-FOREIGN-AMT
           END-IF
           IF FXNT-FOREIGN-AMT > 25.00
               MOVE 25.00 TO FXNT-FOREIGN-AMT
           END-IF
           ADD FXNT-FOREIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E258 DESC REQUIRED
      * E848 USD AMT NEGATIVE
      * E330 USD AMT OVER 1000000
      * E970 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4580-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-DESC = SPACES
                   MOVE 'E258' TO WS-REJECT-CODE
               WHEN FXNT-USD-AMT < 0
                   MOVE 'E848' TO WS-REJECT-CODE
               WHEN FXNT-USD-AMT > 1000000.00
                   MOVE 'E330' TO WS-REJECT-CODE
               WHEN FXNM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E970' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE USD AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4590-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-SOURCE-CD
                   ADD FXNM-USD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-USD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE FXN FIELDS TO FNSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-FNSTAT.
           INITIALIZE FNSTAT-PARMS
           MOVE FXNM-BUYBACK-AMT
             TO FNSTAT-BUYBACK-AMT
           MOVE FXNM-USD-YTD
             TO FNSTAT-USD-YTD
           MOVE FXNM-MARGIN-AMT
             TO FNSTAT-MARGIN-AMT
           MOVE FXNM-SHIPPING-YTD
             TO FNSTAT-SHIPPING-YTD
           MOVE FXNM-USD-AMT
             TO FNSTAT-USD-AMT
           MOVE FXNM-FOREIGN-AMT
             TO FNSTAT-FOREIGN-AMT
           MOVE FXNM-EXPIRY-DATE
             TO FNSTAT-EXPIRY-DATE
           MOVE FXNM-OPEN-DATE
             TO FNSTAT-OPEN-DATE
           MOVE FXNM-MATURITY-DATE
             TO FNSTAT-MATURITY-DATE
           MOVE FXNM-LAST-ACT-DATE
             TO FNSTAT-LAST-ACT-DATE
           MOVE FXNM-SOURCE-CD
             TO FNSTAT-SOURCE-CD
           MOVE FXNM-ORDER-TYPE-CD
             TO FNSTAT-ORDER-TYPE-CD
           MOVE FXNM-TYPE-CD
             TO FNSTAT-TYPE-CD
           MOVE FXNM-STATUS-CD
             TO FNSTAT-STATUS-CD
           MOVE FXNM-REVIEW-FLAG
             TO FNSTAT-REVIEW-FLAG
           MOVE FXNM-OVERRIDE-FLAG
             TO FNSTAT-OVERRIDE-FLAG
           MOVE FXNM-PREFERRED-FLAG
             TO FNSTAT-PREFERRED-FLAG
           MOVE FXNM-PAYMENT-CNT
             TO FNSTAT-PAYMENT-CNT
           MOVE FXNM-NOTICE-CNT
             TO FNSTAT-NOTICE-CNT
           MOVE FXNM-RATE
             TO FNSTAT-RATE
           MOVE FXNM-PENALTY-PCT
             TO FNSTAT-PENALTY-PCT
           MOVE FXNM-ADDR
             TO FNSTAT-ADDR
           MOVE FXNM-CONTACT
             TO FNSTAT-CONTACT
           CALL 'FNSTAT' USING FNSTAT-PARMS
           EVALUATE FNSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE FNSTAT-USD-AMT
                     TO FXNM-USD-AMT
                   MOVE FNSTAT-FOREIGN-AMT
                     TO FXNM-FOREIGN-AMT
                   MOVE FNSTAT-REVIEW-FLAG
                     TO FXNM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-FNSTAT' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE FXNT-KEY TO RD-KEY
           MOVE FXNT-TRAN-CD TO RD-CODE
           MOVE FXNT-MARGIN-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE FXNT-KEY TO RD-KEY
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
           DISPLAY 'FNNEXTR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'FNNEXTR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
