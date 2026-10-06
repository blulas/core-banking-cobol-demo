      *================================================================*
      * PROGRAM  : TLLANAL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TELLER BALANCING - ANALYSIS. READS THE TBL          *
      *            TRANSACTIONS, UPDATES THE TBL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. TELLER CASH DRAWER BALANCING,   *
      *            BATCH PROOF AND OVER/SHORT                          *
      * INPUT    : TBLMST (KSDS), TBLTRN, BANKCTL                      *
      * OUTPUT   : TBLOUT, TLLANRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TLFEE, TLELG, TLWAIV    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-08-04 TWH  ORIGINAL                                       *
      * 1990-04-23 MKL  CASH-OUT CALCULATION CORRECTED REQ0914         *
      * 1998-11-21 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-03-25 MKL  CASH-OUT EXCEPTIONS TO OPS QUEUE PRB9693       *
      * 2005-05-21 SKN  AUDIT FINDING - BATCH CONTROL TOTALS INC9461   *
      * 2006-05-27 BHS  VAULT-BUY FEE SCHEDULE UPDATE INC3741          *
      * 2007-09-11 JLM  OVERSHORT EXCEPTIONS TO OPS QUEUE REQ7117      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLLANAL.
       AUTHOR. J L MARSH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "TBLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TBLM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "TBLTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "TBLOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "TLLANRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY TBLMST.
       FD  TRAN-FILE.
       COPY TBLTRN.
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
                                           VALUE 19.000000.
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
      * WS-VAULT-BU-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VAULT-BU-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000743500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000503000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000498500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000522500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001350000000423000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001850000000606000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002350000000547500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002850000000598000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007850000000637000'.
       01  WS-VAULT-BU-TIER-TBL REDEFINES WS-VAULT-BU-TIER-VALUES.
           05  WS-VAULT-BU-TIER-ROW OCCURS 9 TIMES.
               10  WS-VAULT-BU-TIER-LOW    PIC 9(09)V99.
               10  WS-VAULT-BU-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VAULT-BU-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VAULT-BU-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000881000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000811000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000239000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000654000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007100000000753000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017100000000474000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017150000000528500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017650000000191000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017900000000900000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017950000000345000'.
       01  WS-VAULT-BU-SCHED-TBL REDEFINES WS-VAULT-BU-SCHED-VALUES.
           05  WS-VAULT-BU-SCHED-ROW OCCURS 10 TIMES.
               10  WS-VAULT-BU-SCHED-LOW   PIC 9(09)V99.
               10  WS-VAULT-BU-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASH-IN-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-IN-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000621000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000259500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000189500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000400000000204000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002900000000126000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012900000000509000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013150000000481000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013250000000078500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013300000000502000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013550000000493000'.
       01  WS-CASH-IN-BAND-TBL REDEFINES WS-CASH-IN-BAND-VALUES.
           05  WS-CASH-IN-BAND-ROW OCCURS 10 TIMES.
               10  WS-CASH-IN-BAND-LOW     PIC 9(09)V99.
               10  WS-CASH-IN-BAND-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TLLANAL  TELLER BALANCING'.
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
       COPY TLFEEP.
       COPY TLELGP.
       COPY TLWAIVP.

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
               MOVE 'TBLMST' TO ABND-FILE-NAME
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
           MOVE TBLT-KEY TO TBLM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SELECT-DISCOUNT
               PERFORM 3010-LIMIT-VAULT-SELL
               PERFORM 3020-PRORATE-VAULT-SELL
               PERFORM 3030-SET-TRAN
               PERFORM 3040-ACCRUE-BATCH
               PERFORM 3050-ASSESS-OVERRIDE
               PERFORM 3060-SELECT-DISCOUNT
               PERFORM 3070-CHECK-LAST-MAINT
               PERFORM 3080-CHECK-EFF
               PERFORM 3090-SPLIT-CASH-OUT
               PERFORM 3100-LIMIT-BATCH
               PERFORM 3110-LIMIT-VAULT-SELL
               PERFORM 3120-COUNT-EXCEPT
               PERFORM 3130-ACCUM-CHANNEL
               PERFORM 3140-EDIT-REF-NO
               PERFORM 3150-SELECT-FEE-PCT
               PERFORM 3160-ASSESS-OVERRIDE
               PERFORM 3170-SPLIT-BATCH
               PERFORM 3180-AGE-LAST-PAY
               PERFORM 3190-FLAG-ITEM
               PERFORM 3200-ACCUM-CHANNEL
               PERFORM 3210-PRORATE-VAULT-BUY
               PERFORM 3220-PRORATE-CASH-OUT
               PERFORM 3230-SPLIT-VAULT-BUY
               PERFORM 3240-CHECK-NEXT-REVIEW
               PERFORM 3250-FLAG-BATCH
               PERFORM 3260-EDIT-NAME
               PERFORM 3270-ACCRUE-CASH-OUT
               PERFORM 3280-COUNT-RETRY
               PERFORM 3290-FLAG-VAULT-BUY
               PERFORM 3300-ACCUM-CYCLE
               PERFORM 3310-EDIT-OPERATOR
               PERFORM 3320-PRORATE-CASH-IN
               PERFORM 3330-EDIT-REJECT
               PERFORM 3340-LIMIT-VAULT-BUY
               PERFORM 3350-ASSESS-STOP
               PERFORM 3360-PRORATE-VAULT-SELL
               PERFORM 3370-SELECT-RATE
               PERFORM 3380-PRORATE-CASH-OUT
               PERFORM 3390-SELECT-RATE
               PERFORM 3400-FLAG-OVERSHORT
               PERFORM 3410-FLAG-BATCH
               PERFORM 3420-CHECK-EFFECTIVE
               PERFORM 3430-CHECK-LAST-ACT
               PERFORM 3440-ACCRUE-VAULT-SELL
               PERFORM 3450-AGE-EXPIRY
               PERFORM 3460-ACCUM-CHANNEL
               PERFORM 3470-LIMIT-BATCH
               PERFORM 3480-SET-TRAN
               PERFORM 3490-FLAG-ITEM
               PERFORM 3500-LIMIT-CASH-IN
               PERFORM 3510-LIMIT-ITEM
               PERFORM 3520-SET-CYCLE
               PERFORM 3530-EDIT-CONTACT
               PERFORM 3540-SELECT-DISCOUNT
               PERFORM 3550-EDIT-NAME
               PERFORM 3560-LIMIT-BATCH
               PERFORM 3570-SPLIT-VAULT-SELL
               PERFORM 3580-SELECT-DISCOUNT
               PERFORM 3590-EDIT-MEMO
               PERFORM 3600-ASSESS-STOP
               PERFORM 3610-ACCRUE-OVERSHORT
               PERFORM 3620-ACCUM-DRAWER
               PERFORM 3630-ACCRUE-CASH-OUT
               PERFORM 3640-AGE-LAST-PAY
               PERFORM 3650-LIMIT-VAULT-BUY
               PERFORM 3660-PRORATE-OVERSHORT
               PERFORM 3670-AGE-NEXT-REVIEW
               PERFORM 3680-PRORATE-CASH-OUT
               PERFORM 3690-FLAG-VAULT-SELL
               PERFORM 3700-EDIT-MEMO
               PERFORM 3710-LIMIT-VAULT-SELL
               PERFORM 3720-FLAG-VAULT-SELL
               PERFORM 3730-ACCRUE-CASH-OUT
               PERFORM 3740-AGE-OPEN
               PERFORM 3750-ACCRUE-VAULT-SELL
               PERFORM 3760-FLAG-CASH-OUT
               PERFORM 3770-ACCUM-DRAWER
               PERFORM 3780-SPLIT-OVERSHORT
               PERFORM 3790-COUNT-SEQ
               PERFORM 3800-ACCRUE-VAULT-SELL
               PERFORM 3810-SET-REASON
               PERFORM 3820-ASSESS-REVIEW
               PERFORM 3830-LIMIT-VAULT-SELL
               PERFORM 3840-LIMIT-CASH-OUT
               PERFORM 3850-FLAG-OVERSHORT
               PERFORM 3860-AGE-EXPIRY
               PERFORM 3870-COUNT-ITEM
               PERFORM 3880-EDIT-CUST-ID
               PERFORM 3890-ASSESS-OVERRIDE
               PERFORM 3900-LIMIT-OVERSHORT
               PERFORM 3910-DESCRIBE-SOURCE
               PERFORM 3920-PRORATE-CASH-IN
               PERFORM 3930-PRORATE-OVERSHORT
               PERFORM 3940-SET-DRAWER
               PERFORM 3950-ASSESS-REVERSAL
               PERFORM 3960-SELECT-DISCOUNT
               PERFORM 3970-SET-CHANNEL
               PERFORM 3980-ASSESS-REVIEW
               PERFORM 3990-ACCRUE-CASH-OUT
               PERFORM 4000-SET-SOURCE
               PERFORM 4010-COUNT-EXCEPT
               PERFORM 4020-DESCRIBE-REASON
               PERFORM 4030-COUNT-NOTICE
               PERFORM 4040-FLAG-CASH-OUT
               PERFORM 4050-COUNT-SEQ
               PERFORM 4060-PRORATE-VAULT-SELL
               PERFORM 4070-DESCRIBE-CHANNEL
               PERFORM 4080-DESCRIBE-CYCLE
               PERFORM 4090-ACCUM-REASON
               PERFORM 4100-SET-CHANNEL
               PERFORM 4110-FLAG-VAULT-SELL
               PERFORM 4120-PRORATE-OVERSHORT
               PERFORM 4130-ACCUM-CHANNEL
               PERFORM 4140-ACCRUE-VAULT-SELL
               PERFORM 4150-SELECT-FEE-PCT
               PERFORM 4160-ASSESS-OVERRIDE
               PERFORM 4170-SELECT-FEE-PCT
               PERFORM 4180-EDIT-CUST-ID
               PERFORM 4190-SPLIT-OVERSHORT
               PERFORM 4200-COUNT-RETRY
               PERFORM 4210-SPLIT-VAULT-SELL
               PERFORM 4220-SET-REASON
               PERFORM 4230-DESCRIBE-SOURCE
               PERFORM 4240-FLAG-ITEM
               PERFORM 4250-SET-CYCLE
               PERFORM 4260-FLAG-VAULT-SELL
               PERFORM 4270-PRORATE-VAULT-BUY
               PERFORM 4280-SELECT-FEE-PCT
               PERFORM 4290-AGE-NOTICE
               PERFORM 4300-LIMIT-CASH-IN
               PERFORM 4310-SET-TRAN
               PERFORM 4320-EDIT-ADDR
               PERFORM 4330-AGE-NOTICE
               PERFORM 4340-SELECT-RATE
               PERFORM 4350-AGE-EXPIRY
               PERFORM 4360-ASSESS-STOP
               PERFORM 4370-ASSESS-STOP
               PERFORM 4380-FLAG-CASH-IN
               PERFORM 4390-ACCUM-TRAN
               PERFORM 4400-PRORATE-VAULT-SELL
               PERFORM 4410-LIMIT-VAULT-SELL
               PERFORM 4420-SELECT-DISCOUNT
               PERFORM 4430-DESCRIBE-DRAWER
               PERFORM 4440-PRORATE-OVERSHORT
               PERFORM 4450-AGE-LAST-PAY
               PERFORM 4460-AGE-NEXT-REVIEW
               PERFORM 4470-DESCRIBE-CHANNEL
               PERFORM 4480-SELECT-FEE-PCT
               PERFORM 4490-SELECT-RATE
               PERFORM 4500-PRORATE-CASH-OUT
               PERFORM 4510-SET-CHANNEL
               PERFORM 4520-CHECK-NOTICE
               PERFORM 4530-COUNT-NOTICE
               PERFORM 4540-EDIT-DESC
               PERFORM 4550-ACCUM-CHANNEL
               PERFORM 4560-SPLIT-CASH-IN
               PERFORM 4570-AGE-LAST-PAY
               PERFORM 4580-LIMIT-BATCH
               PERFORM 4590-AGE-NOTICE
               PERFORM 4600-SPLIT-OVERSHORT
               PERFORM 4610-COUNT-NOTICE
               PERFORM 4620-SELECT-FEE-PCT
               PERFORM 4630-SET-CHANNEL
               PERFORM 4640-ACCRUE-CASH-OUT
               PERFORM 4650-AGE-EFFECTIVE
               PERFORM 4660-EDIT-MEMO
               PERFORM 7000-CALL-TLFEE
               PERFORM 7030-CALL-TLELG
               PERFORM 7020-CALL-TLWAIV
               REWRITE TBL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'TBLMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM TBL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * SELECT THE VAULT BU SCHED TIER FOR THE CASH OUT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3000-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-BU-SCHED-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TBLM-CASH-OUT-YTD NOT < WS-VAULT-BU-SCHED-LOW(WS-TX)
                   MOVE WS-VAULT-BU-SCHED-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VAULT SELL YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3010-LIMIT-VAULT-SELL.
           IF TBLM-VAULT-SELL-YTD < 25.00
               MOVE 25.00 TO TBLM-VAULT-SELL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-SELL-YTD > 2500.00
               MOVE 2500.00 TO TBLM-VAULT-SELL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VAULT SELL AMT.
      *----------------------------------------------------------------*
       3020-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-VAULT-SELL-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLT-VAULT-SELL-AMT
           END-IF
           ADD TBLT-VAULT-SELL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BI -> BO WHEN EFF DATE IS PAST
      * BO -> VB WHEN EFF DATE IS PAST
      * VB -> VS WHEN EFF DATE IS PAST
      * VS -> OS WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3030-SET-TRAN.
           EVALUATE TBLT-TRAN-CD
               WHEN 'BI'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BO' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BO'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VB' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VB'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VS'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BATCH YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3040-ACCRUE-BATCH.
           IF TBLM-BATCH-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-BATCH-YTD * TBLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TBLT-CASH-OUT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH OUT FEE ON THE VAULT BUY YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3050-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TBLT-CASH-OUT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-HOLD-FLAG = 'Y'
                   COMPUTE TBLT-CASH-OUT-AMT ROUNDED =
                       TBLM-VAULT-BUY-YTD * TBLM-RATE / 200
               WHEN TBLT-TRAN-CD = 'BI'
                   MOVE 10.00 TO TBLT-CASH-OUT-AMT
               WHEN TBLT-TRAN-CD = 'BO'
                   MOVE 12.00 TO TBLT-CASH-OUT-AMT
               WHEN OTHER
                   COMPUTE TBLT-CASH-OUT-AMT ROUNDED =
                       TBLM-VAULT-BUY-YTD * TBLM-RATE / 100
           END-EVALUATE
           IF TBLT-CASH-OUT-AMT > 0 AND TBLT-CASH-OUT-AMT < 10.00
               MOVE 10.00 TO TBLT-CASH-OUT-AMT
           END-IF
           IF TBLT-CASH-OUT-AMT > 150.00
               MOVE 150.00 TO TBLT-CASH-OUT-AMT
           END-IF
           ADD TBLT-CASH-OUT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE VAULT BU SCHED TIER FOR THE CASH OUT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3060-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-BU-SCHED-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TBLT-CASH-OUT-AMT NOT < WS-VAULT-BU-SCHED-LOW(WS-TX)
                   MOVE WS-VAULT-BU-SCHED-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3070-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3080-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF TBLT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TBLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH OUT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3090-SPLIT-CASH-OUT.
           COMPUTE WS-WORK-AMT = TBLM-CASH-OUT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TBLM-CASH-OUT-YTD - (WS-WORK-AMT *
               12)
           COMPUTE TBLM-OVERSHORT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BATCH YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3100-LIMIT-BATCH.
           IF TBLM-BATCH-YTD < 100.00
               MOVE 100.00 TO TBLM-BATCH-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-BATCH-YTD > 2500.00
               MOVE 2500.00 TO TBLM-BATCH-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VAULT SELL AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3110-LIMIT-VAULT-SELL.
           IF TBLM-VAULT-SELL-AMT < 10.00
               MOVE 10.00 TO TBLM-VAULT-SELL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-SELL-AMT > 10000.00
               MOVE 10000.00 TO TBLM-VAULT-SELL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3120-COUNT-EXCEPT.
           IF TBLM-EXCEPT-CNT < 9999999
               ADD 1 TO TBLM-EXCEPT-CNT
           END-IF
           IF TBLM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT SELL AMT BY CHANNEL CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3130-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-CHANNEL-CD
                   ADD TBLT-VAULT-SELL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLT-VAULT-SELL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E014 REF NO REQUIRED
      * E889 OVERSHORT YTD NEGATIVE
      * E078 OVERSHORT YTD OVER 99999
      * E959 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3140-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-REF-NO = SPACES
                   MOVE 'E014' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-YTD < 0
                   MOVE 'E889' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-YTD > 99999.00
                   MOVE 'E078' TO WS-REJECT-CODE
               WHEN TBLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E959' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT BU TIER TIER FOR THE VAULT BUY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-BU-TIER-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TBLM-VAULT-BUY-YTD NOT < WS-VAULT-BU-TIER-LOW(WS-TX)
                   MOVE WS-VAULT-BU-TIER-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT BUY YTD FEE ON THE BATCH YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3160-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TBLM-VAULT-BUY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   COMPUTE TBLM-VAULT-BUY-YTD ROUNDED = TBLM-BATCH-YTD *
                       TBLM-DISCOUNT / 200
               WHEN TBLM-DRAWER-CD = 'D'
                   MOVE 3.00 TO TBLM-VAULT-BUY-YTD
               WHEN TBLM-DRAWER-CD = 'F'
                   MOVE 15.00 TO TBLM-VAULT-BUY-YTD
               WHEN OTHER
                   COMPUTE TBLM-VAULT-BUY-YTD ROUNDED = TBLM-BATCH-YTD *
                       TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-VAULT-BUY-YTD > 0 AND TBLM-VAULT-BUY-YTD < 5.00
               MOVE 5.00 TO TBLM-VAULT-BUY-YTD
           END-IF
           IF TBLM-VAULT-BUY-YTD > 50.00
               MOVE 50.00 TO TBLM-VAULT-BUY-YTD
           END-IF
           ADD TBLM-VAULT-BUY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT BATCH YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TBLM-BATCH-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TBLM-BATCH-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3180-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3190-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3200-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-CHANNEL-CD
                   ADD TBLM-OVERSHORT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-OVERSHORT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VAULT BUY YTD.
      *----------------------------------------------------------------*
       3210-PRORATE-VAULT-BUY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-VAULT-BUY-YTD ROUNDED =
                   TBLM-VAULT-SELL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-VAULT-BUY-YTD
           END-IF
           ADD TBLM-VAULT-BUY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH OUT AMT.
      *----------------------------------------------------------------*
       3220-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-CASH-OUT-AMT ROUNDED =
                   TBLM-VAULT-SELL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLT-CASH-OUT-AMT
           END-IF
           ADD TBLT-CASH-OUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT VAULT BUY AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3230-SPLIT-VAULT-BUY.
           COMPUTE WS-WORK-AMT = TBLM-VAULT-BUY-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TBLM-VAULT-BUY-AMT - (WS-WORK-AMT *
               2)
           COMPUTE TBLM-BATCH-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3240-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TBLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BATCH AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3250-FLAG-BATCH.
           IF TBLT-BATCH-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLT-BATCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E456 NAME REQUIRED
      * E377 VAULT SELL AMT NEGATIVE
      * E978 VAULT SELL AMT OVER 1000000
      * E984 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3260-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-NAME = SPACES
                   MOVE 'E456' TO WS-REJECT-CODE
               WHEN TBLT-VAULT-SELL-AMT < 0
                   MOVE 'E377' TO WS-REJECT-CODE
               WHEN TBLT-VAULT-SELL-AMT > 1000000.00
                   MOVE 'E978' TO WS-REJECT-CODE
               WHEN TBLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E984' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH OUT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3270-ACCRUE-CASH-OUT.
           IF TBLT-CASH-OUT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-CASH-OUT-AMT * TBLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TBLM-OVERSHORT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3280-COUNT-RETRY.
           IF TBLM-RETRY-CNT < 9999999
               ADD 1 TO TBLM-RETRY-CNT
           END-IF
           IF TBLM-RETRY-CNT NOT < 6
               MOVE 'Y' TO TBLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VAULT BUY YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3290-FLAG-VAULT-BUY.
           IF TBLM-VAULT-BUY-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-VAULT-BUY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3300-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-CYCLE-CD
                   ADD TBLT-OVERSHORT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLT-OVERSHORT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E699 OPERATOR REQUIRED
      * E351 BATCH AMT NEGATIVE
      * E436 BATCH AMT OVER 5000
      * E958 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3310-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-OPERATOR = SPACES
                   MOVE 'E699' TO WS-REJECT-CODE
               WHEN TBLT-BATCH-AMT < 0
                   MOVE 'E351' TO WS-REJECT-CODE
               WHEN TBLT-BATCH-AMT > 5000.00
                   MOVE 'E436' TO WS-REJECT-CODE
               WHEN TBLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E958' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH IN AMT.
      *----------------------------------------------------------------*
       3320-PRORATE-CASH-IN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-CASH-IN-AMT ROUNDED =
                   TBLM-VAULT-SELL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-CASH-IN-AMT
           END-IF
           ADD TBLM-CASH-IN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E222 REJECT CD REQUIRED
      * E906 BATCH AMT NEGATIVE
      * E343 BATCH AMT OVER 25000
      * E738 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3330-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-REJECT-CD = SPACES
                   MOVE 'E222' TO WS-REJECT-CODE
               WHEN TBLT-BATCH-AMT < 0
                   MOVE 'E906' TO WS-REJECT-CODE
               WHEN TBLT-BATCH-AMT > 25000.00
                   MOVE 'E343' TO WS-REJECT-CODE
               WHEN TBLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E738' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VAULT BUY YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3340-LIMIT-VAULT-BUY.
           IF TBLM-VAULT-BUY-YTD < 10.00
               MOVE 10.00 TO TBLM-VAULT-BUY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-BUY-YTD > 2500.00
               MOVE 2500.00 TO TBLM-VAULT-BUY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT FEE ON THE VAULT SELL AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3350-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TBLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TBLT-OVERSHORT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   COMPUTE TBLT-OVERSHORT-AMT ROUNDED =
                       TBLM-VAULT-SELL-AMT * TBLM-RATE / 200
               WHEN TBLM-PROOF-TYPE-CD = 'JG'
                   MOVE 15.00 TO TBLT-OVERSHORT-AMT
               WHEN TBLM-PROOF-TYPE-CD = 'RX'
                   MOVE 5.00 TO TBLT-OVERSHORT-AMT
               WHEN OTHER
                   COMPUTE TBLT-OVERSHORT-AMT ROUNDED =
                       TBLM-VAULT-SELL-AMT * TBLM-RATE / 100
           END-EVALUATE
           IF TBLT-OVERSHORT-AMT > 0 AND TBLT-OVERSHORT-AMT < 1.00
               MOVE 1.00 TO TBLT-OVERSHORT-AMT
           END-IF
           IF TBLT-OVERSHORT-AMT > 250.00
               MOVE 250.00 TO TBLT-OVERSHORT-AMT
           END-IF
           ADD TBLT-OVERSHORT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VAULT SELL AMT.
      *----------------------------------------------------------------*
       3360-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-VAULT-SELL-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-VAULT-SELL-AMT
           END-IF
           ADD TBLM-VAULT-SELL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE VAULT BU SCHED TIER FOR THE CASH OUT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3370-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-BU-SCHED-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TBLM-CASH-OUT-YTD NOT < WS-VAULT-BU-SCHED-LOW(WS-TX)
                   MOVE WS-VAULT-BU-SCHED-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH OUT AMT.
      *----------------------------------------------------------------*
       3380-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-CASH-OUT-AMT ROUNDED =
                   TBLM-OVERSHORT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLT-CASH-OUT-AMT
           END-IF
           ADD TBLT-CASH-OUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE VAULT BU SCHED TIER FOR THE CASH IN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3390-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-BU-SCHED-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TBLM-CASH-IN-YTD NOT < WS-VAULT-BU-SCHED-LOW(WS-TX)
                   MOVE WS-VAULT-BU-SCHED-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3400-FLAG-OVERSHORT.
           IF TBLM-OVERSHORT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLM-OVERSHORT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * BATCH YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3410-FLAG-BATCH.
           IF TBLM-BATCH-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLM-BATCH-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3420-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TBLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3430-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TBLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3440-ACCRUE-VAULT-SELL.
           IF TBLM-VAULT-SELL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-SELL-AMT * TBLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-OVERSHORT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3450-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EXPIRY-DATE > 0 AND TBLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3460-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLT-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * BATCH YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3470-LIMIT-BATCH.
           IF TBLM-BATCH-YTD < 50.00
               MOVE 50.00 TO TBLM-BATCH-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-BATCH-YTD > 2500.00
               MOVE 2500.00 TO TBLM-BATCH-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BI -> BO WHEN LAST MAINT DATE IS PAST
      * BO -> VB WHEN LAST MAINT DATE IS PAST
      * VB -> VS WHEN LAST MAINT DATE IS PAST
      * VS -> OS WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3480-SET-TRAN.
           EVALUATE TBLT-TRAN-CD
               WHEN 'BI'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BO' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BO'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VB' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VB'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VS'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'OS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3490-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CASH IN AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3500-LIMIT-CASH-IN.
           IF TBLM-CASH-IN-AMT < 50.00
               MOVE 50.00 TO TBLM-CASH-IN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-IN-AMT > 5000.00
               MOVE 5000.00 TO TBLM-CASH-IN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3510-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * KX -> TA WHEN LAST MAINT DATE IS PAST
      * TA -> TG WHEN LAST MAINT DATE IS PAST
      * TG -> WG WHEN LAST MAINT DATE IS PAST
      * WG -> KX WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3520-SET-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TA' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TG'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WG'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KX' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E118 CONTACT REQUIRED
      * E842 CASH IN YTD NEGATIVE
      * E294 CASH IN YTD OVER 1000000
      * E784 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3530-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-CONTACT = SPACES
                   MOVE 'E118' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-YTD < 0
                   MOVE 'E842' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-YTD > 1000000.00
                   MOVE 'E294' TO WS-REJECT-CODE
               WHEN TBLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E784' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH IN BAND TIER FOR THE CASH OUT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3540-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-IN-BAND-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TBLM-CASH-OUT-AMT NOT < WS-CASH-IN-BAND-LOW(WS-TX)
                   MOVE WS-CASH-IN-BAND-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E146 NAME REQUIRED
      * E119 VAULT SELL YTD NEGATIVE
      * E345 VAULT SELL YTD OVER 25000
      * E300 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3550-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-NAME = SPACES
                   MOVE 'E146' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-SELL-YTD < 0
                   MOVE 'E119' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-SELL-YTD > 25000.00
                   MOVE 'E345' TO WS-REJECT-CODE
               WHEN TBLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E300' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BATCH AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3560-LIMIT-BATCH.
           IF TBLM-BATCH-AMT < 0.00
               MOVE 0.00 TO TBLM-BATCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-BATCH-AMT > 50000.00
               MOVE 50000.00 TO TBLM-BATCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3570-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TBLT-VAULT-SELL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TBLT-VAULT-SELL-AMT - (WS-WORK-AMT *
               2)
           COMPUTE TBLM-OVERSHORT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH IN BAND TIER FOR THE BATCH YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3580-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-IN-BAND-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TBLM-BATCH-YTD NOT < WS-CASH-IN-BAND-LOW(WS-TX)
                   MOVE WS-CASH-IN-BAND-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E797 MEMO REQUIRED
      * E261 CASH IN AMT NEGATIVE
      * E117 CASH IN AMT OVER 50000
      * E891 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3590-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-MEMO = SPACES
                   MOVE 'E797' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-AMT < 0
                   MOVE 'E261' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-AMT > 50000.00
                   MOVE 'E117' TO WS-REJECT-CODE
               WHEN TBLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E891' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN YTD FEE ON THE OVERSHORT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3600-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TBLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-IN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TBLM-CASH-IN-YTD ROUNDED = TBLT-OVERSHORT-AMT
                       * TBLM-DISCOUNT / 200
               WHEN TBLT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO TBLM-CASH-IN-YTD
               WHEN TBLT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO TBLM-CASH-IN-YTD
               WHEN OTHER
                   COMPUTE TBLM-CASH-IN-YTD ROUNDED = TBLT-OVERSHORT-AMT
                       * TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-CASH-IN-YTD > 0 AND TBLM-CASH-IN-YTD < 10.00
               MOVE 10.00 TO TBLM-CASH-IN-YTD
           END-IF
           IF TBLM-CASH-IN-YTD > 250.00
               MOVE 250.00 TO TBLM-CASH-IN-YTD
           END-IF
           ADD TBLM-CASH-IN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OVERSHORT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3610-ACCRUE-OVERSHORT.
           IF TBLT-OVERSHORT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-OVERSHORT-AMT * TBLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TBLM-VAULT-BUY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BATCH AMT BY DRAWER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3620-ACCUM-DRAWER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-DRAWER-CD
                   ADD TBLM-BATCH-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-DRAWER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-BATCH-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH OUT AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3630-ACCRUE-CASH-OUT.
           IF TBLT-CASH-OUT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-CASH-OUT-AMT * TBLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3640-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT BUY YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3650-LIMIT-VAULT-BUY.
           IF TBLM-VAULT-BUY-YTD < 0.00
               MOVE 0.00 TO TBLM-VAULT-BUY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-BUY-YTD > 500.00
               MOVE 500.00 TO TBLM-VAULT-BUY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OVERSHORT AMT.
      *----------------------------------------------------------------*
       3660-PRORATE-OVERSHORT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-OVERSHORT-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLT-OVERSHORT-AMT
           END-IF
           ADD TBLT-OVERSHORT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3670-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NEXT-REVIEW-DATE > 0 AND TBLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH OUT AMT.
      *----------------------------------------------------------------*
       3680-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-CASH-OUT-AMT ROUNDED =
                   TBLT-OVERSHORT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-CASH-OUT-AMT
           END-IF
           ADD TBLM-CASH-OUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * VAULT SELL AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3690-FLAG-VAULT-SELL.
           IF TBLT-VAULT-SELL-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLT-VAULT-SELL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E417 MEMO REQUIRED
      * E794 BATCH YTD NEGATIVE
      * E580 BATCH YTD OVER 1000000
      * E878 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3700-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-MEMO = SPACES
                   MOVE 'E417' TO WS-REJECT-CODE
               WHEN TBLM-BATCH-YTD < 0
                   MOVE 'E794' TO WS-REJECT-CODE
               WHEN TBLM-BATCH-YTD > 1000000.00
                   MOVE 'E580' TO WS-REJECT-CODE
               WHEN TBLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E878' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VAULT SELL AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3710-LIMIT-VAULT-SELL.
           IF TBLM-VAULT-SELL-AMT < 10.00
               MOVE 10.00 TO TBLM-VAULT-SELL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-SELL-AMT > 1000.00
               MOVE 1000.00 TO TBLM-VAULT-SELL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VAULT SELL AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3720-FLAG-VAULT-SELL.
           IF TBLT-VAULT-SELL-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLT-VAULT-SELL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH OUT AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3730-ACCRUE-CASH-OUT.
           IF TBLM-CASH-OUT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-CASH-OUT-AMT * TBLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TBLT-VAULT-SELL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3740-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3750-ACCRUE-VAULT-SELL.
           IF TBLM-VAULT-SELL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-SELL-AMT * TBLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-BATCH-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3760-FLAG-CASH-OUT.
           IF TBLM-CASH-OUT-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-OUT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT AMT BY DRAWER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3770-ACCUM-DRAWER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-DRAWER-CD
                   ADD TBLM-CASH-OUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-DRAWER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-CASH-OUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OVERSHORT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3780-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = TBLM-OVERSHORT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TBLM-OVERSHORT-AMT - (WS-WORK-AMT *
               2)
           COMPUTE TBLM-CASH-OUT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3790-COUNT-SEQ.
           IF TBLT-SEQ < 9999999
               ADD 1 TO TBLT-SEQ
           END-IF
           IF TBLT-SEQ NOT < 3
               MOVE 'Y' TO TBLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3800-ACCRUE-VAULT-SELL.
           IF TBLM-VAULT-SELL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-SELL-YTD * TBLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-OVERSHORT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GS -> GV WHEN LAST MAINT DATE IS PAST
      * GV -> PG WHEN LAST MAINT DATE IS PAST
      * PG -> SX WHEN LAST MAINT DATE IS PAST
      * SX -> XN WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3810-SET-REASON.
           EVALUATE TBLM-REASON-CD
               WHEN 'GS'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GV' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PG' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PG'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'SX' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SX'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XN' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CASH OUT FEE ON THE VAULT BUY AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3820-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-OUT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   COMPUTE TBLM-CASH-OUT-AMT ROUNDED =
                       TBLM-VAULT-BUY-AMT * TBLM-FEE-PCT / 200
               WHEN TBLM-DRAWER-CD = 'D'
                   MOVE 7.00 TO TBLM-CASH-OUT-AMT
               WHEN TBLM-DRAWER-CD = 'F'
                   MOVE 7.00 TO TBLM-CASH-OUT-AMT
               WHEN OTHER
                   COMPUTE TBLM-CASH-OUT-AMT ROUNDED =
                       TBLM-VAULT-BUY-AMT * TBLM-FEE-PCT / 100
           END-EVALUATE
           IF TBLM-CASH-OUT-AMT > 0 AND TBLM-CASH-OUT-AMT < 10.00
               MOVE 10.00 TO TBLM-CASH-OUT-AMT
           END-IF
           IF TBLM-CASH-OUT-AMT > 100.00
               MOVE 100.00 TO TBLM-CASH-OUT-AMT
           END-IF
           ADD TBLM-CASH-OUT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * VAULT SELL YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3830-LIMIT-VAULT-SELL.
           IF TBLM-VAULT-SELL-YTD < 100.00
               MOVE 100.00 TO TBLM-VAULT-SELL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-SELL-YTD > 50000.00
               MOVE 50000.00 TO TBLM-VAULT-SELL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3840-LIMIT-CASH-OUT.
           IF TBLM-CASH-OUT-YTD < 10.00
               MOVE 10.00 TO TBLM-CASH-OUT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-OUT-YTD > 5000.00
               MOVE 5000.00 TO TBLM-CASH-OUT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3850-FLAG-OVERSHORT.
           IF TBLT-OVERSHORT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLT-OVERSHORT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3860-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EXPIRY-DATE > 0 AND TBLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3870-COUNT-ITEM.
           IF TBLM-ITEM-CNT < 9999999
               ADD 1 TO TBLM-ITEM-CNT
           END-IF
           IF TBLM-ITEM-CNT NOT < 10
               MOVE 'Y' TO TBLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E197 CUST ID REQUIRED
      * E229 CASH OUT AMT NEGATIVE
      * E869 CASH OUT AMT OVER 99999
      * E228 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3880-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-CUST-ID = SPACES
                   MOVE 'E197' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-AMT < 0
                   MOVE 'E229' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-AMT > 99999.00
                   MOVE 'E869' TO WS-REJECT-CODE
               WHEN TBLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E228' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT BUY FEE ON THE VAULT SELL AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3890-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TBLM-VAULT-BUY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLM-VAULT-BUY-AMT ROUNDED =
                       TBLT-VAULT-SELL-AMT * TBLM-FEE-PCT / 200
               WHEN TBLM-DRAWER-CD = 'D'
                   MOVE 15.00 TO TBLM-VAULT-BUY-AMT
               WHEN TBLM-DRAWER-CD = 'F'
                   MOVE 15.00 TO TBLM-VAULT-BUY-AMT
               WHEN OTHER
                   COMPUTE TBLM-VAULT-BUY-AMT ROUNDED =
                       TBLT-VAULT-SELL-AMT * TBLM-FEE-PCT / 100
           END-EVALUATE
           IF TBLM-VAULT-BUY-AMT > 0 AND TBLM-VAULT-BUY-AMT < 2.00
               MOVE 2.00 TO TBLM-VAULT-BUY-AMT
           END-IF
           IF TBLM-VAULT-BUY-AMT > 75.00
               MOVE 75.00 TO TBLM-VAULT-BUY-AMT
           END-IF
           ADD TBLM-VAULT-BUY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * OVERSHORT AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3900-LIMIT-OVERSHORT.
           IF TBLT-OVERSHORT-AMT < 50.00
               MOVE 50.00 TO TBLT-OVERSHORT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-OVERSHORT-AMT > 10000.00
               MOVE 10000.00 TO TBLT-OVERSHORT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3910-DESCRIBE-SOURCE.
           EVALUATE TBLM-SOURCE-CD
               WHEN 'AF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CASH IN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH IN AMT.
      *----------------------------------------------------------------*
       3920-PRORATE-CASH-IN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-CASH-IN-AMT ROUNDED =
                   TBLM-CASH-IN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-CASH-IN-AMT
           END-IF
           ADD TBLM-CASH-IN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OVERSHORT AMT.
      *----------------------------------------------------------------*
       3930-PRORATE-OVERSHORT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-OVERSHORT-AMT ROUNDED =
                   TBLM-VAULT-BUY-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLT-OVERSHORT-AMT
           END-IF
           ADD TBLT-OVERSHORT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DRAWER CD:
      * D -> F WHEN EFF DATE IS PAST
      * F -> R WHEN EFF DATE IS PAST
      * R -> D WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3940-SET-DRAWER.
           EVALUATE TBLM-DRAWER-CD
               WHEN 'D'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO TBLM-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO TBLM-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TBLM-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CASH OUT FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3950-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TBLT-CASH-OUT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   COMPUTE TBLT-CASH-OUT-AMT ROUNDED = WS-WORK-AMT-2 *
                       TBLM-FEE-PCT / 200
               WHEN TBLM-PROOF-TYPE-CD = 'JG'
                   MOVE 12.00 TO TBLT-CASH-OUT-AMT
               WHEN TBLM-PROOF-TYPE-CD = 'RX'
                   MOVE 15.00 TO TBLT-CASH-OUT-AMT
               WHEN OTHER
                   COMPUTE TBLT-CASH-OUT-AMT ROUNDED = WS-WORK-AMT-2 *
                       TBLM-FEE-PCT / 100
           END-EVALUATE
           IF TBLT-CASH-OUT-AMT > 0 AND TBLT-CASH-OUT-AMT < 10.00
               MOVE 10.00 TO TBLT-CASH-OUT-AMT
           END-IF
           IF TBLT-CASH-OUT-AMT > 100.00
               MOVE 100.00 TO TBLT-CASH-OUT-AMT
           END-IF
           ADD TBLT-CASH-OUT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CASH IN BAND TIER FOR THE CASH IN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3960-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-IN-BAND-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TBLM-CASH-IN-AMT NOT < WS-CASH-IN-BAND-LOW(WS-TX)
                   MOVE WS-CASH-IN-BAND-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * D -> F WHEN EXPIRY DATE IS PAST
      * F -> M WHEN EXPIRY DATE IS PAST
      * M -> N WHEN EXPIRY DATE IS PAST
      * N -> R WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3970-SET-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   IF TBLM-EXPIRY-DATE > 0 AND TBLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TBLM-EXPIRY-DATE > 0 AND TBLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TBLM-EXPIRY-DATE > 0 AND TBLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TBLM-EXPIRY-DATE > 0 AND TBLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BATCH FEE ON THE OVERSHORT YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3980-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TBLT-BATCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TBLT-BATCH-AMT ROUNDED = TBLM-OVERSHORT-YTD *
                       TBLM-RATE / 200
               WHEN TBLM-DRAWER-CD = 'D'
                   MOVE 10.00 TO TBLT-BATCH-AMT
               WHEN TBLM-DRAWER-CD = 'F'
                   MOVE 10.00 TO TBLT-BATCH-AMT
               WHEN OTHER
                   COMPUTE TBLT-BATCH-AMT ROUNDED = TBLM-OVERSHORT-YTD *
                       TBLM-RATE / 100
           END-EVALUATE
           IF TBLT-BATCH-AMT > 0 AND TBLT-BATCH-AMT < 10.00
               MOVE 10.00 TO TBLT-BATCH-AMT
           END-IF
           IF TBLT-BATCH-AMT > 100.00
               MOVE 100.00 TO TBLT-BATCH-AMT
           END-IF
           ADD TBLT-BATCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH OUT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3990-ACCRUE-CASH-OUT.
           IF TBLM-CASH-OUT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-CASH-OUT-AMT * TBLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-VAULT-BUY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AF -> PT WHEN EFF DATE IS PAST
      * PT -> SZ WHEN EFF DATE IS PAST
      * SZ -> VK WHEN EFF DATE IS PAST
      * VK -> VW WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4000-SET-SOURCE.
           EVALUATE TBLM-SOURCE-CD
               WHEN 'AF'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SZ' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SZ'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VK' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VW' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4010-COUNT-EXCEPT.
           IF TBLM-EXCEPT-CNT < 9999999
               ADD 1 TO TBLM-EXCEPT-CNT
           END-IF
           IF TBLM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO TBLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4020-DESCRIBE-REASON.
           EVALUATE TBLM-REASON-CD
               WHEN 'GS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4030-COUNT-NOTICE.
           IF TBLM-NOTICE-CNT < 9999999
               ADD 1 TO TBLM-NOTICE-CNT
           END-IF
           IF TBLM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO TBLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4040-FLAG-CASH-OUT.
           IF TBLM-CASH-OUT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-OUT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4050-COUNT-SEQ.
           IF TBLT-SEQ < 9999999
               ADD 1 TO TBLT-SEQ
           END-IF
           IF TBLT-SEQ NOT < 12
               MOVE 'Y' TO TBLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VAULT SELL YTD.
      *----------------------------------------------------------------*
       4060-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-VAULT-SELL-YTD ROUNDED =
                   TBLM-VAULT-BUY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-VAULT-SELL-YTD
           END-IF
           ADD TBLM-VAULT-SELL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4070-DESCRIBE-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'N'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4080-DESCRIBE-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH IN YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-REASON-CD
                   ADD TBLM-CASH-IN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-CASH-IN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT REVIEW DATE IS PAST
      * BAT -> MAL WHEN NEXT REVIEW DATE IS PAST
      * MAL -> TEL WHEN NEXT REVIEW DATE IS PAST
      * TEL -> ONL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4100-SET-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT SELL AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4110-FLAG-VAULT-SELL.
           IF TBLM-VAULT-SELL-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLM-VAULT-SELL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OVERSHORT YTD.
      *----------------------------------------------------------------*
       4120-PRORATE-OVERSHORT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-OVERSHORT-YTD ROUNDED =
                   TBLT-VAULT-SELL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-OVERSHORT-YTD
           END-IF
           ADD TBLM-OVERSHORT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4130-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLT-CHANNEL-CD
                   ADD TBLT-OVERSHORT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLT-OVERSHORT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4140-ACCRUE-VAULT-SELL.
           IF TBLM-VAULT-SELL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-SELL-YTD * TBLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TBLT-VAULT-SELL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT BU SCHED TIER FOR THE VAULT BUY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4150-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-BU-SCHED-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TBLM-VAULT-BUY-AMT NOT < WS-VAULT-BU-SCHED-LOW(WS-TX)
                   MOVE WS-VAULT-BU-SCHED-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BATCH FEE ON THE CASH IN AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4160-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TBLT-BATCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-STOP-FLAG = 'Y'
                   COMPUTE TBLT-BATCH-AMT ROUNDED = TBLM-CASH-IN-AMT *
                       TBLM-RATE / 200
               WHEN TBLT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO TBLT-BATCH-AMT
               WHEN TBLT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO TBLT-BATCH-AMT
               WHEN OTHER
                   COMPUTE TBLT-BATCH-AMT ROUNDED = TBLM-CASH-IN-AMT *
                       TBLM-RATE / 100
           END-EVALUATE
           IF TBLT-BATCH-AMT > 0 AND TBLT-BATCH-AMT < 1.00
               MOVE 1.00 TO TBLT-BATCH-AMT
           END-IF
           IF TBLT-BATCH-AMT > 100.00
               MOVE 100.00 TO TBLT-BATCH-AMT
           END-IF
           ADD TBLT-BATCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE VAULT BU SCHED TIER FOR THE CASH OUT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4170-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-BU-SCHED-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TBLM-CASH-OUT-YTD NOT < WS-VAULT-BU-SCHED-LOW(WS-TX)
                   MOVE WS-VAULT-BU-SCHED-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E351 CUST ID REQUIRED
      * E610 CASH IN AMT NEGATIVE
      * E411 CASH IN AMT OVER 250000
      * E909 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4180-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-CUST-ID = SPACES
                   MOVE 'E351' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-AMT < 0
                   MOVE 'E610' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-AMT > 250000.00
                   MOVE 'E411' TO WS-REJECT-CODE
               WHEN TBLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E909' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OVERSHORT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4190-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = TBLM-OVERSHORT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TBLM-OVERSHORT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE TBLM-CASH-OUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4200-COUNT-RETRY.
           IF TBLM-RETRY-CNT < 9999999
               ADD 1 TO TBLM-RETRY-CNT
           END-IF
           IF TBLM-RETRY-CNT NOT < 5
               MOVE 'Y' TO TBLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4210-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TBLM-VAULT-SELL-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TBLM-VAULT-SELL-YTD - (WS-WORK-AMT *
               2)
           COMPUTE TBLM-VAULT-SELL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GS -> GV WHEN EFFECTIVE DATE IS PAST
      * GV -> PG WHEN EFFECTIVE DATE IS PAST
      * PG -> SX WHEN EFFECTIVE DATE IS PAST
      * SX -> XN WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4220-SET-REASON.
           EVALUATE TBLM-REASON-CD
               WHEN 'GS'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PG' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PG'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SX' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SX'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XN' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4230-DESCRIBE-SOURCE.
           EVALUATE TBLM-SOURCE-CD
               WHEN 'AF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SZ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4240-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * KX -> TA WHEN OPEN DATE IS PAST
      * TA -> TG WHEN OPEN DATE IS PAST
      * TG -> WG WHEN OPEN DATE IS PAST
      * WG -> KX WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4250-SET-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TA' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TG'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WG'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT SELL AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4260-FLAG-VAULT-SELL.
           IF TBLT-VAULT-SELL-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLT-VAULT-SELL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4270-PRORATE-VAULT-BUY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TBLM-VAULT-BUY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE VAULT BU SCHED TIER FOR THE CASH OUT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4280-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-BU-SCHED-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TBLM-CASH-OUT-AMT NOT < WS-VAULT-BU-SCHED-LOW(WS-TX)
                   MOVE WS-VAULT-BU-SCHED-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       4290-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH IN YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4300-LIMIT-CASH-IN.
           IF TBLM-CASH-IN-YTD < 0.00
               MOVE 0.00 TO TBLM-CASH-IN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-IN-YTD > 1000.00
               MOVE 1000.00 TO TBLM-CASH-IN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BI -> BO WHEN OPEN DATE IS PAST
      * BO -> VB WHEN OPEN DATE IS PAST
      * VB -> VS WHEN OPEN DATE IS PAST
      * VS -> OS WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4310-SET-TRAN.
           EVALUATE TBLT-TRAN-CD
               WHEN 'BI'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BO' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BO'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VB' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VB'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VS'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E691 ADDR REQUIRED
      * E385 OVERSHORT YTD NEGATIVE
      * E397 OVERSHORT YTD OVER 99999
      * E600 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4320-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-ADDR = SPACES
                   MOVE 'E691' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-YTD < 0
                   MOVE 'E385' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-YTD > 99999.00
                   MOVE 'E397' TO WS-REJECT-CODE
               WHEN TBLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E600' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4330-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASH IN BAND TIER FOR THE OVERSHORT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4340-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-IN-BAND-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TBLM-OVERSHORT-YTD NOT < WS-CASH-IN-BAND-LOW(WS-TX)
                   MOVE WS-CASH-IN-BAND-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4350-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EXPIRY-DATE > 0 AND TBLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BATCH YTD FEE ON THE OVERSHORT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4360-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TBLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TBLM-BATCH-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   COMPUTE TBLM-BATCH-YTD ROUNDED = TBLM-OVERSHORT-AMT *
                       TBLM-FEE-PCT / 200
               WHEN TBLT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO TBLM-BATCH-YTD
               WHEN TBLT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO TBLM-BATCH-YTD
               WHEN OTHER
                   COMPUTE TBLM-BATCH-YTD ROUNDED = TBLM-OVERSHORT-AMT *
                       TBLM-FEE-PCT / 100
           END-EVALUATE
           IF TBLM-BATCH-YTD > 0 AND TBLM-BATCH-YTD < 5.00
               MOVE 5.00 TO TBLM-BATCH-YTD
           END-IF
           IF TBLM-BATCH-YTD > 50.00
               MOVE 50.00 TO TBLM-BATCH-YTD
           END-IF
           ADD TBLM-BATCH-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE BATCH FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4370-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TBLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TBLM-BATCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   COMPUTE TBLM-BATCH-AMT ROUNDED = WS-WORK-AMT *
                       TBLM-RATE / 200
               WHEN TBLM-REASON-CD = 'GS'
                   MOVE 15.00 TO TBLM-BATCH-AMT
               WHEN TBLM-REASON-CD = 'GV'
                   MOVE 5.00 TO TBLM-BATCH-AMT
               WHEN OTHER
                   COMPUTE TBLM-BATCH-AMT ROUNDED = WS-WORK-AMT *
                       TBLM-RATE / 100
           END-EVALUATE
           IF TBLM-BATCH-AMT > 0 AND TBLM-BATCH-AMT < 10.00
               MOVE 10.00 TO TBLM-BATCH-AMT
           END-IF
           IF TBLM-BATCH-AMT > 50.00
               MOVE 50.00 TO TBLM-BATCH-AMT
           END-IF
           ADD TBLM-BATCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CASH IN AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4380-FLAG-CASH-IN.
           IF TBLM-CASH-IN-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-IN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BATCH AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4390-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLT-TRAN-CD
                   ADD TBLT-BATCH-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLT-BATCH-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VAULT SELL AMT.
      *----------------------------------------------------------------*
       4400-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-VAULT-SELL-AMT ROUNDED =
                   TBLM-OVERSHORT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLT-VAULT-SELL-AMT
           END-IF
           ADD TBLT-VAULT-SELL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * VAULT SELL AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4410-LIMIT-VAULT-SELL.
           IF TBLM-VAULT-SELL-AMT < 25.00
               MOVE 25.00 TO TBLM-VAULT-SELL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-SELL-AMT > 500.00
               MOVE 500.00 TO TBLM-VAULT-SELL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH IN BAND TIER FOR THE BATCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4420-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-IN-BAND-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TBLM-BATCH-AMT NOT < WS-CASH-IN-BAND-LOW(WS-TX)
                   MOVE WS-CASH-IN-BAND-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4430-DESCRIBE-DRAWER.
           EVALUATE TBLM-DRAWER-CD
               WHEN 'D'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'F'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OVERSHORT AMT.
      *----------------------------------------------------------------*
       4440-PRORATE-OVERSHORT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-OVERSHORT-AMT ROUNDED =
                   TBLM-VAULT-BUY-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLT-OVERSHORT-AMT
           END-IF
           ADD TBLT-OVERSHORT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4450-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4460-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NEXT-REVIEW-DATE > 0 AND TBLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4470-DESCRIBE-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE VAULT BU SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4480-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-BU-SCHED-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-VAULT-BU-SCHED-LOW(WS-TX)
                   MOVE WS-VAULT-BU-SCHED-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT BU TIER TIER FOR THE OVERSHORT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4490-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-BU-TIER-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TBLM-OVERSHORT-AMT NOT < WS-VAULT-BU-TIER-LOW(WS-TX)
                   MOVE WS-VAULT-BU-TIER-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH OUT AMT.
      *----------------------------------------------------------------*
       4500-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-CASH-OUT-AMT ROUNDED =
                   TBLT-VAULT-SELL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLT-CASH-OUT-AMT
           END-IF
           ADD TBLT-CASH-OUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * D -> F WHEN EXPIRY DATE IS PAST
      * F -> M WHEN EXPIRY DATE IS PAST
      * M -> N WHEN EXPIRY DATE IS PAST
      * N -> R WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4510-SET-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   IF TBLM-EXPIRY-DATE > 0 AND TBLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TBLM-EXPIRY-DATE > 0 AND TBLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TBLM-EXPIRY-DATE > 0 AND TBLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TBLM-EXPIRY-DATE > 0 AND TBLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4520-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TBLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4530-COUNT-NOTICE.
           IF TBLM-NOTICE-CNT < 9999999
               ADD 1 TO TBLM-NOTICE-CNT
           END-IF
           IF TBLM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO TBLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E261 DESC REQUIRED
      * E463 CASH OUT AMT NEGATIVE
      * E202 CASH OUT AMT OVER 99999
      * E511 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4540-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-DESC = SPACES
                   MOVE 'E261' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-AMT < 0
                   MOVE 'E463' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-AMT > 99999.00
                   MOVE 'E202' TO WS-REJECT-CODE
               WHEN TBLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E511' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4550-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-CHANNEL-CD
                   ADD TBLM-OVERSHORT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-OVERSHORT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH IN AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4560-SPLIT-CASH-IN.
           COMPUTE WS-WORK-AMT = TBLM-CASH-IN-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TBLM-CASH-IN-AMT - (WS-WORK-AMT * 2)
           COMPUTE TBLM-VAULT-BUY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4570-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLT-OVERSHORT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TBLT-OVERSHORT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TBLT-OVERSHORT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLT-OVERSHORT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLT-OVERSHORT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BATCH YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4580-LIMIT-BATCH.
           IF TBLM-BATCH-YTD < 10.00
               MOVE 10.00 TO TBLM-BATCH-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-BATCH-YTD > 2500.00
               MOVE 2500.00 TO TBLM-BATCH-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4590-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT OVERSHORT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4600-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = TBLM-OVERSHORT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TBLM-OVERSHORT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE TBLM-BATCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4610-COUNT-NOTICE.
           IF TBLM-NOTICE-CNT < 9999999
               ADD 1 TO TBLM-NOTICE-CNT
           END-IF
           IF TBLM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT BU TIER TIER FOR THE CASH OUT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4620-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-BU-TIER-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TBLT-CASH-OUT-AMT NOT < WS-VAULT-BU-TIER-LOW(WS-TX)
                   MOVE WS-VAULT-BU-TIER-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4630-SET-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH OUT YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4640-ACCRUE-CASH-OUT.
           IF TBLM-CASH-OUT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-CASH-OUT-YTD * TBLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4650-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E753 MEMO REQUIRED
      * E328 CASH IN AMT NEGATIVE
      * E211 CASH IN AMT OVER 5000
      * E535 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4660-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-MEMO = SPACES
                   MOVE 'E753' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-AMT < 0
                   MOVE 'E328' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-AMT > 5000.00
                   MOVE 'E211' TO WS-REJECT-CODE
               WHEN TBLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E535' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE TBL FIELDS TO TLFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-TLFEE.
           INITIALIZE TLFEE-PARMS
           MOVE TBLM-VAULT-BUY-AMT
             TO TLFEE-VAULT-BUY-AMT
           MOVE TBLM-VAULT-SELL-YTD
             TO TLFEE-VAULT-SELL-YTD
           MOVE TBLM-CASH-IN-YTD
             TO TLFEE-CASH-IN-YTD
           MOVE TBLM-VAULT-BUY-YTD
             TO TLFEE-VAULT-BUY-YTD
           MOVE TBLM-VAULT-SELL-AMT
             TO TLFEE-VAULT-SELL-AMT
           MOVE TBLM-CASH-OUT-YTD
             TO TLFEE-CASH-OUT-YTD
           MOVE TBLM-OPEN-DATE
             TO TLFEE-OPEN-DATE
           MOVE TBLM-NEXT-REVIEW-DATE
             TO TLFEE-NEXT-REVIEW-DATE
           MOVE TBLM-LAST-PAY-DATE
             TO TLFEE-LAST-PAY-DATE
           MOVE TBLM-LAST-ACT-DATE
             TO TLFEE-LAST-ACT-DATE
           MOVE TBLM-CHANNEL-CD
             TO TLFEE-CHANNEL-CD
           MOVE TBLM-PROOF-TYPE-CD
             TO TLFEE-PROOF-TYPE-CD
           MOVE TBLM-DRAWER-CD
             TO TLFEE-DRAWER-CD
           MOVE TBLM-CYCLE-CD
             TO TLFEE-CYCLE-CD
           MOVE TBLM-EXEMPT-FLAG
             TO TLFEE-EXEMPT-FLAG
           MOVE TBLM-STOP-FLAG
             TO TLFEE-STOP-FLAG
           MOVE TBLM-OVERRIDE-FLAG
             TO TLFEE-OVERRIDE-FLAG
           MOVE TBLM-EXCEPT-CNT
             TO TLFEE-EXCEPT-CNT
           MOVE TBLM-NOTICE-CNT
             TO TLFEE-NOTICE-CNT
           MOVE TBLM-DISCOUNT
             TO TLFEE-DISCOUNT
           MOVE TBLM-RATE
             TO TLFEE-RATE
           MOVE TBLM-DESC
             TO TLFEE-DESC
           MOVE TBLM-NAME
             TO TLFEE-NAME
           CALL 'TLFEE' USING TLFEE-PARMS
           EVALUATE TLFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TLFEE-VAULT-BUY-AMT
                     TO TBLM-VAULT-BUY-AMT
                   MOVE TLFEE-CASH-IN-YTD
                     TO TBLM-CASH-IN-YTD
                   MOVE TLFEE-VAULT-BUY-YTD
                     TO TBLM-VAULT-BUY-YTD
                   MOVE TLFEE-EXEMPT-FLAG
                     TO TBLM-EXEMPT-FLAG
                   MOVE TLFEE-OVERRIDE-FLAG
                     TO TBLM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-TLFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TBL FIELDS TO TLELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-TLELG.
           INITIALIZE TLELG-PARMS
           MOVE TBLM-VAULT-SELL-YTD
             TO TLELG-VAULT-SELL-YTD
           MOVE TBLM-CASH-IN-AMT
             TO TLELG-CASH-IN-AMT
           MOVE TBLM-CASH-OUT-AMT
             TO TLELG-CASH-OUT-AMT
           MOVE TBLM-CASH-OUT-YTD
             TO TLELG-CASH-OUT-YTD
           MOVE TBLM-OVERSHORT-YTD
             TO TLELG-OVERSHORT-YTD
           MOVE TBLM-OVERSHORT-AMT
             TO TLELG-OVERSHORT-AMT
           MOVE TBLM-EXPIRY-DATE
             TO TLELG-EXPIRY-DATE
           MOVE TBLM-LAST-ACT-DATE
             TO TLELG-LAST-ACT-DATE
           MOVE TBLM-LAST-PAY-DATE
             TO TLELG-LAST-PAY-DATE
           MOVE TBLM-EFFECTIVE-DATE
             TO TLELG-EFFECTIVE-DATE
           MOVE TBLM-PROOF-TYPE-CD
             TO TLELG-PROOF-TYPE-CD
           MOVE TBLM-CYCLE-CD
             TO TLELG-CYCLE-CD
           MOVE TBLM-SOURCE-CD
             TO TLELG-SOURCE-CD
           MOVE TBLM-CHANNEL-CD
             TO TLELG-CHANNEL-CD
           MOVE TBLM-REVIEW-FLAG
             TO TLELG-REVIEW-FLAG
           MOVE TBLM-OVERRIDE-FLAG
             TO TLELG-OVERRIDE-FLAG
           MOVE TBLM-STOP-FLAG
             TO TLELG-STOP-FLAG
           MOVE TBLM-ITEM-CNT
             TO TLELG-ITEM-CNT
           MOVE TBLM-NOTICE-CNT
             TO TLELG-NOTICE-CNT
           MOVE TBLM-RATE
             TO TLELG-RATE
           MOVE TBLM-DISCOUNT
             TO TLELG-DISCOUNT
           MOVE TBLM-DESC
             TO TLELG-DESC
           MOVE TBLM-ADDR
             TO TLELG-ADDR
           CALL 'TLELG' USING TLELG-PARMS
           EVALUATE TLELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TLELG-CASH-IN-AMT
                     TO TBLM-CASH-IN-AMT
                   MOVE TLELG-CASH-OUT-AMT
                     TO TBLM-CASH-OUT-AMT
                   MOVE TLELG-OVERSHORT-YTD
                     TO TBLM-OVERSHORT-YTD
                   MOVE TLELG-OVERRIDE-FLAG
                     TO TBLM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-TLELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TBL FIELDS TO TLWAIV AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-TLWAIV.
           INITIALIZE TLWAIV-PARMS
           MOVE TBLM-VAULT-SELL-AMT
             TO TLWAIV-VAULT-SELL-AMT
           MOVE TBLM-VAULT-BUY-YTD
             TO TLWAIV-VAULT-BUY-YTD
           MOVE TBLM-BATCH-YTD
             TO TLWAIV-BATCH-YTD
           MOVE TBLM-CASH-IN-AMT
             TO TLWAIV-CASH-IN-AMT
           MOVE TBLM-CASH-IN-YTD
             TO TLWAIV-CASH-IN-YTD
           MOVE TBLM-CASH-OUT-YTD
             TO TLWAIV-CASH-OUT-YTD
           MOVE TBLM-EFFECTIVE-DATE
             TO TLWAIV-EFFECTIVE-DATE
           MOVE TBLM-LAST-STMT-DATE
             TO TLWAIV-LAST-STMT-DATE
           MOVE TBLM-LAST-PAY-DATE
             TO TLWAIV-LAST-PAY-DATE
           MOVE TBLM-OPEN-DATE
             TO TLWAIV-OPEN-DATE
           MOVE TBLM-SOURCE-CD
             TO TLWAIV-SOURCE-CD
           MOVE TBLM-DRAWER-CD
             TO TLWAIV-DRAWER-CD
           MOVE TBLM-CHANNEL-CD
             TO TLWAIV-CHANNEL-CD
           MOVE TBLM-REASON-CD
             TO TLWAIV-REASON-CD
           MOVE TBLM-STOP-FLAG
             TO TLWAIV-STOP-FLAG
           MOVE TBLM-SUSPEND-FLAG
             TO TLWAIV-SUSPEND-FLAG
           MOVE TBLM-EXEMPT-FLAG
             TO TLWAIV-EXEMPT-FLAG
           MOVE TBLM-ITEM-CNT
             TO TLWAIV-ITEM-CNT
           MOVE TBLM-EXCEPT-CNT
             TO TLWAIV-EXCEPT-CNT
           MOVE TBLM-FEE-PCT
             TO TLWAIV-FEE-PCT
           MOVE TBLM-RATE
             TO TLWAIV-RATE
           MOVE TBLM-DESC
             TO TLWAIV-DESC
           MOVE TBLM-REF-NO
             TO TLWAIV-REF-NO
           CALL 'TLWAIV' USING TLWAIV-PARMS
           EVALUATE TLWAIV-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TLWAIV-VAULT-SELL-AMT
                     TO TBLM-VAULT-SELL-AMT
                   MOVE TLWAIV-BATCH-YTD
                     TO TBLM-BATCH-YTD
                   MOVE TLWAIV-CASH-IN-YTD
                     TO TBLM-CASH-IN-YTD
                   MOVE TLWAIV-STOP-FLAG
                     TO TBLM-STOP-FLAG
                   MOVE TLWAIV-SUSPEND-FLAG
                     TO TBLM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RAIV' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-TLWAIV' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE TBLT-KEY TO RD-KEY
           MOVE TBLT-TRAN-CD TO RD-CODE
           MOVE TBLT-CASH-OUT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE TBLT-KEY TO RD-KEY
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
           DISPLAY 'TLLANAL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TLLANAL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
