      *================================================================*
      * PROGRAM  : SLLACCR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : STUDENT LOANS - DAILY ACCRUAL. READS THE STL        *
      *            TRANSACTIONS, UPDATES THE STL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. GUARANTEED STUDENT LOAN         *
      *            DISBURSEMENT, DEFERMENT AND REPAYMENT               *
      * INPUT    : STLMST (KSDS), STLTRN, BANKCTL                      *
      * OUTPUT   : STLOUT, SLLACRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, SLCAL, SLVAL            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1987-11-14 EWS  ORIGINAL                                       *
      * 1989-04-10 MKL  PERFORMANCE - PAYMENT READ AHEAD INC2791       *
      * 1989-11-19 RJT  PAYMENT RERUN PROTECTION PRB2077               *
      * 1990-05-28 SKN  SUBSIDY LIMIT REVISED CHG4579                  *
      * 1990-02-25 BHS  DEFERINT ROUNDING - HALF UP CHG1985            *
      * 1992-06-08 RJT  DISBURSE TABLE EXPANDED INC9328                *
      * 1999-10-20 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-06-22 DPS  PAYMENT EXCEPTIONS TO OPS QUEUE CHG2463        *
      * 2000-01-21 DPS  PAYMENT NOTICE TEXT PER LEGAL INC1181          *
      * 2002-09-26 DPS  DEFERINT STATUS CODES ADDED REQ1548            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SLLACCR.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "STLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS STLM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "STLTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "STLOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "SLLACRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY STLMST.
       FD  TRAN-FILE.
       COPY STLTRN.
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
      * WS-DEFERINT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEFERINT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000773000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000656500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000358500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000556500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000158500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011600000000092000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014100000000835500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019100000000824500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029100000000114500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030100000000369000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030150000000893000'.
       01  WS-DEFERINT-TIER-TBL REDEFINES WS-DEFERINT-TIER-VALUES.
           05  WS-DEFERINT-TIER-ROW OCCURS 11 TIMES.
               10  WS-DEFERINT-TIER-LOW    PIC 9(09)V99.
               10  WS-DEFERINT-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DISBURSE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISBURSE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000126500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000591000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000563500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008500000000168000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008750000000355000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009000000000579000'.
       01  WS-DISBURSE-BAND-TBL REDEFINES WS-DISBURSE-BAND-VALUES.
           05  WS-DISBURSE-BAND-ROW OCCURS 6 TIMES.
               10  WS-DISBURSE-BAND-LOW    PIC 9(09)V99.
               10  WS-DISBURSE-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CAPINT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CAPINT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000562000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000151500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000840000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000026500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013500000000733000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013550000000084000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013650000000599500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013900000000815500'.
       01  WS-CAPINT-SCHED-TBL REDEFINES WS-CAPINT-SCHED-VALUES.
           05  WS-CAPINT-SCHED-ROW OCCURS 8 TIMES.
               10  WS-CAPINT-SCHED-LOW     PIC 9(09)V99.
               10  WS-CAPINT-SCHED-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DEFERINT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEFERINT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000511500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000277000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000198000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015250000000626000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025250000000765500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030250000000523000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040250000000346500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040350000000347500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040450000000632500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00050450000000626500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00050700000000442000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00050750000000019500'.
       01  WS-DEFERINT-BAND-TBL REDEFINES WS-DEFERINT-BAND-VALUES.
           05  WS-DEFERINT-BAND-ROW OCCURS 12 TIMES.
               10  WS-DEFERINT-BAND-LOW    PIC 9(09)V99.
               10  WS-DEFERINT-BAND-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  SLLACCR  STUDENT LOANS'.
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
       COPY SLCALP.
       COPY SLVALP.

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
               MOVE 'STLMST' TO ABND-FILE-NAME
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
           MOVE STLT-KEY TO STLM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SELECT-DISCOUNT
               PERFORM 3010-COUNT-ITEM
               PERFORM 3020-ASSESS-PREFERRED
               PERFORM 3030-ASSESS-REVERSAL
               PERFORM 3040-SPLIT-CAPINT
               PERFORM 3050-CHECK-CLOSE
               PERFORM 3060-AGE-EFF
               PERFORM 3070-AGE-LAST-MAINT
               PERFORM 3080-SELECT-FEE-PCT
               PERFORM 3090-SPLIT-CAPINT
               PERFORM 3100-PRORATE-DISBURSE
               PERFORM 3110-ASSESS-NOTICE-SENT
               PERFORM 3120-ASSESS-PREFERRED
               PERFORM 3130-CHECK-CLOSE
               PERFORM 3140-ACCUM-CYCLE
               PERFORM 3150-COUNT-PAYMENT
               PERFORM 3160-ACCRUE-SUBSIDY
               PERFORM 3170-DESCRIBE-SOURCE
               PERFORM 3180-ACCUM-DEFER-CODE
               PERFORM 3190-COUNT-SEQ
               PERFORM 3200-ASSESS-SUSPEND
               PERFORM 3210-COUNT-PAYMENT
               PERFORM 3220-PRORATE-DEFERINT
               PERFORM 3230-CHECK-CLOSE
               PERFORM 3240-SET-REGION
               PERFORM 3250-DESCRIBE-CYCLE
               PERFORM 3260-SET-TRAN
               PERFORM 3270-SPLIT-PRINCIPAL
               PERFORM 3280-SPLIT-PRINCIPAL
               PERFORM 3290-EDIT-CONTACT
               PERFORM 3300-ACCRUE-PENALTY-PCT
               PERFORM 3310-CHECK-EXPIRY
               PERFORM 3320-PRORATE-DISBURSE
               PERFORM 3330-ACCUM-DEFER-CODE
               PERFORM 3340-SELECT-DISCOUNT
               PERFORM 3350-DESCRIBE-DEFER-CODE
               PERFORM 3360-PRORATE-DISBURSE
               PERFORM 3370-EDIT-CONTACT
               PERFORM 3380-AGE-POST
               PERFORM 3390-ASSESS-REVERSAL
               PERFORM 3400-LIMIT-DISBURSE
               PERFORM 3410-CHECK-MATURITY
               PERFORM 3420-ASSESS-REVERSAL
               PERFORM 3430-ASSESS-PREFERRED
               PERFORM 3440-AGE-LAST-ACT
               PERFORM 3450-ASSESS-OVERRIDE
               PERFORM 3460-ASSESS-NOTICE-SENT
               PERFORM 3470-LIMIT-PAYMENT
               PERFORM 3480-COUNT-CYCLE
               PERFORM 3490-LIMIT-CAPINT
               PERFORM 3500-DESCRIBE-CHANNEL
               PERFORM 3510-COUNT-EXCEPT
               PERFORM 3520-SPLIT-DISBURSE
               PERFORM 3530-AGE-LAST-MAINT
               PERFORM 3540-COUNT-EXCEPT
               PERFORM 3550-LIMIT-DEFERINT
               PERFORM 3560-COUNT-EXCEPT
               PERFORM 3570-DESCRIBE-CYCLE
               PERFORM 3580-DESCRIBE-TRAN
               PERFORM 3590-ASSESS-SUSPEND
               PERFORM 3600-PRORATE-CAPINT
               PERFORM 3610-DESCRIBE-TRAN
               PERFORM 3620-SPLIT-DEFERINT
               PERFORM 3630-LIMIT-SUBSIDY
               PERFORM 3640-LIMIT-CAPINT
               PERFORM 3650-SET-CHANNEL
               PERFORM 3660-ACCRUE-DISCOUNT
               PERFORM 3670-SPLIT-CAPINT
               PERFORM 3680-COUNT-ITEM
               PERFORM 3690-SELECT-FEE-PCT
               PERFORM 3700-PRORATE-DISBURSE
               PERFORM 3710-AGE-EXPIRY
               PERFORM 3720-AGE-OPEN
               PERFORM 3730-EDIT-REJECT
               PERFORM 3740-ACCUM-CHANNEL
               PERFORM 3750-ACCUM-CYCLE
               PERFORM 3760-EDIT-REJECT
               PERFORM 3770-AGE-LAST-MAINT
               PERFORM 3780-SET-REGION
               PERFORM 3790-FLAG-SUBSIDY
               PERFORM 3800-SPLIT-SUBSIDY
               PERFORM 3810-ACCRUE-DISBURSE
               PERFORM 3820-FLAG-ITEM
               PERFORM 3830-ACCRUE-PRINCIPAL
               PERFORM 3840-SPLIT-DISBURSE
               PERFORM 3850-CHECK-POST
               PERFORM 3860-ACCUM-DEFER-CODE
               PERFORM 3870-SELECT-PENALTY-PCT
               PERFORM 3880-SET-REGION
               PERFORM 3890-COUNT-ITEM
               PERFORM 3900-AGE-EXPIRY
               PERFORM 3910-PRORATE-PAYMENT
               PERFORM 3920-LIMIT-DEFERINT
               PERFORM 3930-SELECT-PENALTY-PCT
               PERFORM 3940-SELECT-PENALTY-PCT
               PERFORM 3950-AGE-LAST-PAY
               PERFORM 3960-SET-STATUS
               PERFORM 3970-ASSESS-PREFERRED
               PERFORM 3980-FLAG-PRINCIPAL
               PERFORM 3990-SPLIT-CAPINT
               PERFORM 4000-PRORATE-DEFERINT
               PERFORM 4010-AGE-MATURITY
               PERFORM 4020-FLAG-CAPINT
               PERFORM 4030-SPLIT-PRINCIPAL
               PERFORM 4040-CHECK-OPEN
               PERFORM 4050-COUNT-ITEM
               PERFORM 4060-ACCUM-TRAN
               PERFORM 4070-FLAG-PAYMENT
               PERFORM 4080-EDIT-REF-NO
               PERFORM 4090-EDIT-MEMO
               PERFORM 4100-LIMIT-PRINCIPAL
               PERFORM 4110-CHECK-CLOSE
               PERFORM 4120-DESCRIBE-STATUS
               PERFORM 4130-SELECT-PENALTY-PCT
               PERFORM 4140-ACCUM-TRAN
               PERFORM 4150-PRORATE-DEFERINT
               PERFORM 4160-CHECK-LAST-MAINT
               PERFORM 4170-EDIT-REJECT
               PERFORM 4180-SPLIT-PAYMENT
               PERFORM 4190-LIMIT-PRINCIPAL
               PERFORM 4200-CHECK-EXPIRY
               PERFORM 4210-PRORATE-CAPINT
               PERFORM 4220-ACCUM-STATUS
               PERFORM 4230-SPLIT-SUBSIDY
               PERFORM 4240-SELECT-FEE-PCT
               PERFORM 4250-CHECK-MATURITY
               PERFORM 4260-ACCUM-LOAN-TYPE
               PERFORM 4270-ASSESS-REVERSAL
               PERFORM 4280-PRORATE-DISBURSE
               PERFORM 4290-COUNT-ITEM
               PERFORM 4300-COUNT-CYCLE
               PERFORM 4310-SELECT-DISCOUNT
               PERFORM 4320-ACCRUE-DISBURSE
               PERFORM 4330-SET-SOURCE
               PERFORM 4340-ACCRUE-DISBURSE
               PERFORM 4350-COUNT-SEQ
               PERFORM 4360-PRORATE-SUBSIDY
               PERFORM 4370-COUNT-CYCLE
               PERFORM 4380-CHECK-EXPIRY
               PERFORM 4390-COUNT-CYCLE
               PERFORM 4400-ASSESS-OVERRIDE
               PERFORM 4410-SELECT-DISCOUNT
               PERFORM 4420-FLAG-DEFERINT
               PERFORM 4430-FLAG-CAPINT
               PERFORM 4440-AGE-NEXT-REVIEW
               PERFORM 4450-SELECT-PENALTY-PCT
               PERFORM 4460-PRORATE-CAPINT
               PERFORM 4470-FLAG-DISBURSE
               PERFORM 4480-SPLIT-DEFERINT
               PERFORM 4490-COUNT-EXCEPT
               PERFORM 4500-CHECK-OPEN
               PERFORM 4510-LIMIT-DEFERINT
               PERFORM 4520-ASSESS-OVERRIDE
               PERFORM 4530-FLAG-CAPINT
               PERFORM 4540-SET-CHANNEL
               PERFORM 4550-FLAG-DEFERINT
               PERFORM 4560-SPLIT-SUBSIDY
               PERFORM 4570-SPLIT-DISBURSE
               PERFORM 4580-SET-CHANNEL
               PERFORM 4590-EDIT-ADDR
               PERFORM 4600-DESCRIBE-LOAN-TYPE
               PERFORM 4610-DESCRIBE-REGION
               PERFORM 4620-CHECK-LAST-PAY
               PERFORM 4630-LIMIT-DISBURSE
               PERFORM 4640-SELECT-DISCOUNT
               PERFORM 4650-LIMIT-PRINCIPAL
               PERFORM 4660-CHECK-NEXT-REVIEW
               PERFORM 4670-SPLIT-PAYMENT
               PERFORM 4680-COUNT-PAYMENT
               PERFORM 4690-ACCRUE-DISBURSE
               PERFORM 4700-CHECK-POST
               PERFORM 4710-COUNT-EXCEPT
               PERFORM 4720-PRORATE-SUBSIDY
               PERFORM 4730-LIMIT-ITEM
               PERFORM 4740-SELECT-PENALTY-PCT
               PERFORM 4750-SET-LOAN-TYPE
               PERFORM 7010-CALL-SLCAL
               PERFORM 7020-CALL-SLVAL
               REWRITE STL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'STLMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM STL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT TIER TIER FOR THE CAPINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3000-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-TIER-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF STLM-CAPINT-AMT NOT < WS-DEFERINT-TIER-LOW(WS-TX)
                   MOVE WS-DEFERINT-TIER-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3010-COUNT-ITEM.
           IF STLM-ITEM-CNT < 9999999
               ADD 1 TO STLM-ITEM-CNT
           END-IF
           IF STLM-ITEM-CNT NOT < 3
               MOVE 'Y' TO STLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE CAPINT YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3020-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN STLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO STLM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE STLM-PRINCIPAL-YTD ROUNDED = STLM-CAPINT-YTD
                       * STLM-PENALTY-PCT / 200
               WHEN STLM-STATUS-CD = 'HY'
                   MOVE 10.00 TO STLM-PRINCIPAL-YTD
               WHEN STLM-STATUS-CD = 'JZ'
                   MOVE 7.00 TO STLM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE STLM-PRINCIPAL-YTD ROUNDED = STLM-CAPINT-YTD
                       * STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLM-PRINCIPAL-YTD > 0 AND STLM-PRINCIPAL-YTD < 2.00
               MOVE 2.00 TO STLM-PRINCIPAL-YTD
           END-IF
           IF STLM-PRINCIPAL-YTD > 75.00
               MOVE 75.00 TO STLM-PRINCIPAL-YTD
           END-IF
           ADD STLM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE DISBURSE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3030-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN STLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO STLM-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE STLM-PRINCIPAL-AMT ROUNDED =
                       STLT-DISBURSE-AMT * STLM-PENALTY-PCT / 200
               WHEN STLM-CYCLE-CD = 'CFJ'
                   MOVE 10.00 TO STLM-PRINCIPAL-AMT
               WHEN STLM-CYCLE-CD = 'KHC'
                   MOVE 5.00 TO STLM-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE STLM-PRINCIPAL-AMT ROUNDED =
                       STLT-DISBURSE-AMT * STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLM-PRINCIPAL-AMT > 0 AND STLM-PRINCIPAL-AMT < 1.00
               MOVE 1.00 TO STLM-PRINCIPAL-AMT
           END-IF
           IF STLM-PRINCIPAL-AMT > 50.00
               MOVE 50.00 TO STLM-PRINCIPAL-AMT
           END-IF
           ADD STLM-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT CAPINT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3040-SPLIT-CAPINT.
           COMPUTE WS-WORK-AMT = STLM-CAPINT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = STLM-CAPINT-YTD - (WS-WORK-AMT * 2)
           COMPUTE STLM-CAPINT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3050-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF STLM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO STLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3060-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-DEFERINT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-DEFERINT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD STLM-DEFERINT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD STLM-DEFERINT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-DEFERINT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3070-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-MAINT-DATE > 0 AND STLM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE BAND TIER FOR THE DEFERINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3080-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-BAND-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF STLM-DEFERINT-AMT NOT < WS-DISBURSE-BAND-LOW(WS-TX)
                   MOVE WS-DISBURSE-BAND-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CAPINT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3090-SPLIT-CAPINT.
           COMPUTE WS-WORK-AMT = STLT-CAPINT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = STLT-CAPINT-AMT - (WS-WORK-AMT * 2)
           COMPUTE STLT-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISBURSE YTD.
      *----------------------------------------------------------------*
       3100-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-DISBURSE-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-DISBURSE-YTD
           END-IF
           ADD STLM-DISBURSE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE DEFERINT YTD FEE ON THE SUBSIDY AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3110-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN STLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO STLM-DEFERINT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-REVIEW-FLAG = 'Y'
                   COMPUTE STLM-DEFERINT-YTD ROUNDED = STLM-SUBSIDY-AMT
                       * STLM-PENALTY-PCT / 200
               WHEN STLM-LOAN-TYPE-CD = 'CS'
                   MOVE 12.00 TO STLM-DEFERINT-YTD
               WHEN STLM-LOAN-TYPE-CD = 'DV'
                   MOVE 15.00 TO STLM-DEFERINT-YTD
               WHEN OTHER
                   COMPUTE STLM-DEFERINT-YTD ROUNDED = STLM-SUBSIDY-AMT
                       * STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLM-DEFERINT-YTD > 0 AND STLM-DEFERINT-YTD < 10.00
               MOVE 10.00 TO STLM-DEFERINT-YTD
           END-IF
           IF STLM-DEFERINT-YTD > 50.00
               MOVE 50.00 TO STLM-DEFERINT-YTD
           END-IF
           ADD STLM-DEFERINT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT YTD FEE ON THE DISBURSE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3120-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN STLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO STLM-CAPINT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-HOLD-FLAG = 'Y'
                   COMPUTE STLM-CAPINT-YTD ROUNDED = STLT-DISBURSE-AMT *
                       STLM-PENALTY-PCT / 200
               WHEN STLM-REGION-CD = 'C'
                   MOVE 10.00 TO STLM-CAPINT-YTD
               WHEN STLM-REGION-CD = 'D'
                   MOVE 5.00 TO STLM-CAPINT-YTD
               WHEN OTHER
                   COMPUTE STLM-CAPINT-YTD ROUNDED = STLT-DISBURSE-AMT *
                       STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLM-CAPINT-YTD > 0 AND STLM-CAPINT-YTD < 5.00
               MOVE 5.00 TO STLM-CAPINT-YTD
           END-IF
           IF STLM-CAPINT-YTD > 75.00
               MOVE 75.00 TO STLM-CAPINT-YTD
           END-IF
           ADD STLM-CAPINT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF STLM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO STLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3140-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-CYCLE-CD
                   ADD STLM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3150-COUNT-PAYMENT.
           IF STLM-PAYMENT-CNT < 9999999
               ADD 1 TO STLM-PAYMENT-CNT
           END-IF
           IF STLM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO STLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SUBSIDY YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-SUBSIDY.
           IF STLM-SUBSIDY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-SUBSIDY-YTD * STLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO STLT-CAPINT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3170-DESCRIBE-SOURCE.
           EVALUATE STLM-SOURCE-CD
               WHEN 'AST'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BCK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'CJG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DSJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JVY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VEF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY DEFER CODE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3180-ACCUM-DEFER-CODE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-DEFER-CODE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-DEFER-CODE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3190-COUNT-SEQ.
           IF STLT-SEQ < 9999999
               ADD 1 TO STLT-SEQ
           END-IF
           IF STLT-SEQ NOT < 3
               MOVE 'Y' TO STLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISBURSE FEE ON THE WORK AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3200-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN STLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO STLM-DISBURSE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-PREFERRED-FLAG = 'Y'
                   COMPUTE STLM-DISBURSE-AMT ROUNDED = WS-WORK-AMT *
                       STLM-FEE-PCT / 200
               WHEN STLM-CYCLE-CD = 'CFJ'
                   MOVE 7.00 TO STLM-DISBURSE-AMT
               WHEN STLM-CYCLE-CD = 'KHC'
                   MOVE 20.00 TO STLM-DISBURSE-AMT
               WHEN OTHER
                   COMPUTE STLM-DISBURSE-AMT ROUNDED = WS-WORK-AMT *
                       STLM-FEE-PCT / 100
           END-EVALUATE
           IF STLM-DISBURSE-AMT > 0 AND STLM-DISBURSE-AMT < 1.00
               MOVE 1.00 TO STLM-DISBURSE-AMT
           END-IF
           IF STLM-DISBURSE-AMT > 100.00
               MOVE 100.00 TO STLM-DISBURSE-AMT
           END-IF
           ADD STLM-DISBURSE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-PAYMENT.
           IF STLM-PAYMENT-CNT < 9999999
               ADD 1 TO STLM-PAYMENT-CNT
           END-IF
           IF STLM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO STLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEFERINT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3220-PRORATE-DEFERINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-DEFERINT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3230-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF STLM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO STLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> D WHEN LAST STMT DATE IS PAST
      * D -> T WHEN LAST STMT DATE IS PAST
      * T -> U WHEN LAST STMT DATE IS PAST
      * U -> W WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3240-SET-REGION.
           EVALUATE STLM-REGION-CD
               WHEN 'C'
                   IF STLM-LAST-STMT-DATE > 0 AND STLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF STLM-LAST-STMT-DATE > 0 AND STLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF STLM-LAST-STMT-DATE > 0 AND STLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF STLM-LAST-STMT-DATE > 0 AND STLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3250-DESCRIBE-CYCLE.
           EVALUATE STLM-CYCLE-CD
               WHEN 'CFJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KHC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SEU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XKA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DB -> PY WHEN LAST STMT DATE IS PAST
      * PY -> CP WHEN LAST STMT DATE IS PAST
      * CP -> DF WHEN LAST STMT DATE IS PAST
      * DF -> FB WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3260-SET-TRAN.
           EVALUATE STLT-TRAN-CD
               WHEN 'DB'
                   IF STLM-LAST-STMT-DATE > 0 AND STLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF STLM-LAST-STMT-DATE > 0 AND STLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CP'
                   IF STLM-LAST-STMT-DATE > 0 AND STLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DF'
                   IF STLM-LAST-STMT-DATE > 0 AND STLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3270-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = STLM-PRINCIPAL-YTD / 2
           COMPUTE WS-WORK-AMT-2 = STLM-PRINCIPAL-YTD - (WS-WORK-AMT *
               2)
           COMPUTE STLT-DISBURSE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3280-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE STLT-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E758 CONTACT REQUIRED
      * E971 PRINCIPAL AMT NEGATIVE
      * E416 PRINCIPAL AMT OVER 25000
      * E220 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3290-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-CONTACT = SPACES
                   MOVE 'E758' TO WS-REJECT-CODE
               WHEN STLT-PRINCIPAL-AMT < 0
                   MOVE 'E971' TO WS-REJECT-CODE
               WHEN STLT-PRINCIPAL-AMT > 25000.00
                   MOVE 'E416' TO WS-REJECT-CODE
               WHEN STLM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E220' TO WS-REJECT-CODE
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
       3300-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * STLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO STLM-CAPINT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3310-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF STLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO STLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CAPINT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISBURSE AMT.
      *----------------------------------------------------------------*
       3320-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLT-DISBURSE-AMT ROUNDED =
                   STLM-CAPINT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLT-DISBURSE-AMT
           END-IF
           ADD STLT-DISBURSE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE DEFERINT AMT BY DEFER CODE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3330-ACCUM-DEFER-CODE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-DEFER-CODE-CD
                   ADD STLM-DEFERINT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-DEFER-CODE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-DEFERINT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CAPINT SCHED TIER FOR THE DEFERINT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3340-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CAPINT-SCHED-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF STLM-DEFERINT-YTD NOT < WS-CAPINT-SCHED-LOW(WS-TX)
                   MOVE WS-CAPINT-SCHED-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DEFER CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3350-DESCRIBE-DEFER-CODE.
           EVALUATE STLM-DEFER-CODE-CD
               WHEN 'AH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'CZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ND'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SUBSIDY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISBURSE YTD.
      *----------------------------------------------------------------*
       3360-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-DISBURSE-YTD ROUNDED =
                   STLM-SUBSIDY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-DISBURSE-YTD
           END-IF
           ADD STLM-DISBURSE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E381 CONTACT REQUIRED
      * E307 CAPINT YTD NEGATIVE
      * E086 CAPINT YTD OVER 10000
      * E517 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3370-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-CONTACT = SPACES
                   MOVE 'E381' TO WS-REJECT-CODE
               WHEN STLM-CAPINT-YTD < 0
                   MOVE 'E307' TO WS-REJECT-CODE
               WHEN STLM-CAPINT-YTD > 10000.00
                   MOVE 'E086' TO WS-REJECT-CODE
               WHEN STLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E517' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3380-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF STLT-POST-DATE > 0 AND STLT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE DISBURSE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3390-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN STLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = STLM-DISBURSE-AMT *
                       STLM-FEE-PCT / 200
               WHEN STLM-REGION-CD = 'C'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN STLM-REGION-CD = 'D'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = STLM-DISBURSE-AMT *
                       STLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DISBURSE YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3400-LIMIT-DISBURSE.
           IF STLM-DISBURSE-YTD < 0.00
               MOVE 0.00 TO STLM-DISBURSE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-DISBURSE-YTD > 2500.00
               MOVE 2500.00 TO STLM-DISBURSE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3410-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF STLM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO STLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE CAPINT AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3420-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN STLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO STLT-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-HOLD-FLAG = 'Y'
                   COMPUTE STLT-PAYMENT-AMT ROUNDED = STLT-CAPINT-AMT *
                       STLM-FEE-PCT / 200
               WHEN STLT-TRAN-CD = 'DB'
                   MOVE 3.00 TO STLT-PAYMENT-AMT
               WHEN STLT-TRAN-CD = 'PY'
                   MOVE 15.00 TO STLT-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE STLT-PAYMENT-AMT ROUNDED = STLT-CAPINT-AMT *
                       STLM-FEE-PCT / 100
           END-EVALUATE
           IF STLT-PAYMENT-AMT > 0 AND STLT-PAYMENT-AMT < 2.00
               MOVE 2.00 TO STLT-PAYMENT-AMT
           END-IF
           IF STLT-PAYMENT-AMT > 100.00
               MOVE 100.00 TO STLT-PAYMENT-AMT
           END-IF
           ADD STLT-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE DISBURSE FEE ON THE SUBSIDY AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3430-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN STLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO STLM-DISBURSE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-HOLD-FLAG = 'Y'
                   COMPUTE STLM-DISBURSE-AMT ROUNDED = STLM-SUBSIDY-AMT
                       * STLM-PENALTY-PCT / 200
               WHEN STLM-LOAN-TYPE-CD = 'CS'
                   MOVE 7.00 TO STLM-DISBURSE-AMT
               WHEN STLM-LOAN-TYPE-CD = 'DV'
                   MOVE 12.00 TO STLM-DISBURSE-AMT
               WHEN OTHER
                   COMPUTE STLM-DISBURSE-AMT ROUNDED = STLM-SUBSIDY-AMT
                       * STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLM-DISBURSE-AMT > 0 AND STLM-DISBURSE-AMT < 1.00
               MOVE 1.00 TO STLM-DISBURSE-AMT
           END-IF
           IF STLM-DISBURSE-AMT > 35.00
               MOVE 35.00 TO STLM-DISBURSE-AMT
           END-IF
           ADD STLM-DISBURSE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3440-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DISBURSE FEE ON THE DISBURSE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3450-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN STLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO STLT-DISBURSE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-PREFERRED-FLAG = 'Y'
                   COMPUTE STLT-DISBURSE-AMT ROUNDED = STLM-DISBURSE-AMT
                       * STLM-PENALTY-PCT / 200
               WHEN STLT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO STLT-DISBURSE-AMT
               WHEN STLT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO STLT-DISBURSE-AMT
               WHEN OTHER
                   COMPUTE STLT-DISBURSE-AMT ROUNDED = STLM-DISBURSE-AMT
                       * STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLT-DISBURSE-AMT > 0 AND STLT-DISBURSE-AMT < 2.00
               MOVE 2.00 TO STLT-DISBURSE-AMT
           END-IF
           IF STLT-DISBURSE-AMT > 75.00
               MOVE 75.00 TO STLT-DISBURSE-AMT
           END-IF
           ADD STLT-DISBURSE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE CAPINT AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3460-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN STLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO STLT-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-REVIEW-FLAG = 'Y'
                   COMPUTE STLT-PAYMENT-AMT ROUNDED = STLT-CAPINT-AMT *
                       STLM-DISCOUNT / 200
               WHEN STLM-SOURCE-CD = 'AST'
                   MOVE 15.00 TO STLT-PAYMENT-AMT
               WHEN STLM-SOURCE-CD = 'BCK'
                   MOVE 12.00 TO STLT-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE STLT-PAYMENT-AMT ROUNDED = STLT-CAPINT-AMT *
                       STLM-DISCOUNT / 100
           END-EVALUATE
           IF STLT-PAYMENT-AMT > 0 AND STLT-PAYMENT-AMT < 1.00
               MOVE 1.00 TO STLT-PAYMENT-AMT
           END-IF
           IF STLT-PAYMENT-AMT > 35.00
               MOVE 35.00 TO STLT-PAYMENT-AMT
           END-IF
           ADD STLT-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3470-LIMIT-PAYMENT.
           IF STLM-PAYMENT-YTD < 0.00
               MOVE 0.00 TO STLM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-PAYMENT-YTD > 500.00
               MOVE 500.00 TO STLM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3480-COUNT-CYCLE.
           IF STLM-CYCLE-CNT < 9999999
               ADD 1 TO STLM-CYCLE-CNT
           END-IF
           IF STLM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO STLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CAPINT AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3490-LIMIT-CAPINT.
           IF STLM-CAPINT-AMT < 10.00
               MOVE 10.00 TO STLM-CAPINT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-CAPINT-AMT > 500.00
               MOVE 500.00 TO STLM-CAPINT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-EXCEPT.
           IF STLM-EXCEPT-CNT < 9999999
               ADD 1 TO STLM-EXCEPT-CNT
           END-IF
           IF STLM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO STLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3520-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = STLM-DISBURSE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = STLM-DISBURSE-YTD - (WS-WORK-AMT * 2)
           COMPUTE STLM-DISBURSE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3530-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-MAINT-DATE > 0 AND STLM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3540-COUNT-EXCEPT.
           IF STLM-EXCEPT-CNT < 9999999
               ADD 1 TO STLM-EXCEPT-CNT
           END-IF
           IF STLM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO STLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DEFERINT AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3550-LIMIT-DEFERINT.
           IF STLM-DEFERINT-AMT < 25.00
               MOVE 25.00 TO STLM-DEFERINT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-DEFERINT-AMT > 50000.00
               MOVE 50000.00 TO STLM-DEFERINT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3560-COUNT-EXCEPT.
           IF STLM-EXCEPT-CNT < 9999999
               ADD 1 TO STLM-EXCEPT-CNT
           END-IF
           IF STLM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO STLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3570-DESCRIBE-CYCLE.
           EVALUATE STLM-CYCLE-CD
               WHEN 'CFJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KHC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SEU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XKA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3580-DESCRIBE-TRAN.
           EVALUATE STLT-TRAN-CD
               WHEN 'DB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'CP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE CAPINT AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3590-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN STLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO STLT-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE STLT-PRINCIPAL-AMT ROUNDED = STLT-CAPINT-AMT
                       * STLM-FEE-PCT / 200
               WHEN STLM-DEFER-CODE-CD = 'AH'
                   MOVE 20.00 TO STLT-PRINCIPAL-AMT
               WHEN STLM-DEFER-CODE-CD = 'CZ'
                   MOVE 5.00 TO STLT-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE STLT-PRINCIPAL-AMT ROUNDED = STLT-CAPINT-AMT
                       * STLM-FEE-PCT / 100
           END-EVALUATE
           IF STLT-PRINCIPAL-AMT > 0 AND STLT-PRINCIPAL-AMT < 5.00
               MOVE 5.00 TO STLT-PRINCIPAL-AMT
           END-IF
           IF STLT-PRINCIPAL-AMT > 25.00
               MOVE 25.00 TO STLT-PRINCIPAL-AMT
           END-IF
           ADD STLT-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CAPINT AMT.
      *----------------------------------------------------------------*
       3600-PRORATE-CAPINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-CAPINT-AMT ROUNDED =
                   STLM-PRINCIPAL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-CAPINT-AMT
           END-IF
           ADD STLM-CAPINT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3610-DESCRIBE-TRAN.
           EVALUATE STLT-TRAN-CD
               WHEN 'DB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DEFERINT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3620-SPLIT-DEFERINT.
           COMPUTE WS-WORK-AMT = STLM-DEFERINT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = STLM-DEFERINT-AMT - (WS-WORK-AMT * 2)
           COMPUTE STLM-CAPINT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SUBSIDY AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3630-LIMIT-SUBSIDY.
           IF STLM-SUBSIDY-AMT < 10.00
               MOVE 10.00 TO STLM-SUBSIDY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-SUBSIDY-AMT > 1000.00
               MOVE 1000.00 TO STLM-SUBSIDY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CAPINT AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3640-LIMIT-CAPINT.
           IF STLM-CAPINT-AMT < 0.00
               MOVE 0.00 TO STLM-CAPINT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-CAPINT-AMT > 1000.00
               MOVE 1000.00 TO STLM-CAPINT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST ACT DATE IS PAST
      * BAT -> MAL WHEN LAST ACT DATE IS PAST
      * MAL -> TEL WHEN LAST ACT DATE IS PAST
      * TEL -> ONL WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3650-SET-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * STLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO STLM-DEFERINT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CAPINT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3670-SPLIT-CAPINT.
           COMPUTE WS-WORK-AMT = STLM-CAPINT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = STLM-CAPINT-YTD - (WS-WORK-AMT * 2)
           COMPUTE STLM-DISBURSE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3680-COUNT-ITEM.
           IF STLM-ITEM-CNT < 9999999
               ADD 1 TO STLM-ITEM-CNT
           END-IF
           IF STLM-ITEM-CNT NOT < 5
               MOVE 'Y' TO STLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT BAND TIER FOR THE DEFERINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3690-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-BAND-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF STLM-DEFERINT-AMT NOT < WS-DEFERINT-BAND-LOW(WS-TX)
                   MOVE WS-DEFERINT-BAND-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISBURSE YTD.
      *----------------------------------------------------------------*
       3700-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-DISBURSE-YTD ROUNDED =
                   STLM-PRINCIPAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-DISBURSE-YTD
           END-IF
           ADD STLM-DISBURSE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3710-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3720-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF STLM-OPEN-DATE > 0 AND STLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E670 REJECT CD REQUIRED
      * E203 PAYMENT AMT NEGATIVE
      * E718 PAYMENT AMT OVER 5000
      * E544 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3730-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-REJECT-CD = SPACES
                   MOVE 'E670' TO WS-REJECT-CODE
               WHEN STLT-PAYMENT-AMT < 0
                   MOVE 'E203' TO WS-REJECT-CODE
               WHEN STLT-PAYMENT-AMT > 5000.00
                   MOVE 'E718' TO WS-REJECT-CODE
               WHEN STLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E544' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3740-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLT-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3750-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-CYCLE-CD
                   ADD STLM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E540 REJECT CD REQUIRED
      * E960 CAPINT AMT NEGATIVE
      * E227 CAPINT AMT OVER 10000
      * E142 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3760-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-REJECT-CD = SPACES
                   MOVE 'E540' TO WS-REJECT-CODE
               WHEN STLM-CAPINT-AMT < 0
                   MOVE 'E960' TO WS-REJECT-CODE
               WHEN STLM-CAPINT-AMT > 10000.00
                   MOVE 'E227' TO WS-REJECT-CODE
               WHEN STLM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E142' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3770-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-MAINT-DATE > 0 AND STLM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> D WHEN NEXT REVIEW DATE IS PAST
      * D -> T WHEN NEXT REVIEW DATE IS PAST
      * T -> U WHEN NEXT REVIEW DATE IS PAST
      * U -> W WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3780-SET-REGION.
           EVALUATE STLM-REGION-CD
               WHEN 'C'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'D' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'T' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'U' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'W' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SUBSIDY AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3790-FLAG-SUBSIDY.
           IF STLM-SUBSIDY-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE STLM-SUBSIDY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SUBSIDY YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3800-SPLIT-SUBSIDY.
           COMPUTE WS-WORK-AMT = STLM-SUBSIDY-YTD / 3
           COMPUTE WS-WORK-AMT-2 = STLM-SUBSIDY-YTD - (WS-WORK-AMT * 3)
           COMPUTE STLT-DISBURSE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3810-ACCRUE-DISBURSE.
           IF STLM-DISBURSE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-DISBURSE-YTD * STLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3820-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3830-ACCRUE-PRINCIPAL.
           IF STLT-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLT-PRINCIPAL-AMT * STLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO STLM-DISBURSE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3840-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = STLT-DISBURSE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = STLT-DISBURSE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF STLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY DEFER CODE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3860-ACCUM-DEFER-CODE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-DEFER-CODE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-DEFER-CODE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT TIER TIER FOR THE SUBSIDY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3870-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-TIER-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF STLM-SUBSIDY-AMT NOT < WS-DEFERINT-TIER-LOW(WS-TX)
                   MOVE WS-DEFERINT-TIER-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> D WHEN LAST PAY DATE IS PAST
      * D -> T WHEN LAST PAY DATE IS PAST
      * T -> U WHEN LAST PAY DATE IS PAST
      * U -> W WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3880-SET-REGION.
           EVALUATE STLM-REGION-CD
               WHEN 'C'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3890-COUNT-ITEM.
           IF STLM-ITEM-CNT < 9999999
               ADD 1 TO STLM-ITEM-CNT
           END-IF
           IF STLM-ITEM-CNT NOT < 6
               MOVE 'Y' TO STLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3900-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-DISBURSE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD STLM-DISBURSE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD STLM-DISBURSE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD STLM-DISBURSE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-DISBURSE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3910-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLT-PAYMENT-AMT ROUNDED =
                   STLM-DISBURSE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLT-PAYMENT-AMT
           END-IF
           ADD STLT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DEFERINT AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3920-LIMIT-DEFERINT.
           IF STLM-DEFERINT-AMT < 10.00
               MOVE 10.00 TO STLM-DEFERINT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-DEFERINT-AMT > 500.00
               MOVE 500.00 TO STLM-DEFERINT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CAPINT SCHED TIER FOR THE PRINCIPAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CAPINT-SCHED-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF STLM-PRINCIPAL-YTD NOT < WS-CAPINT-SCHED-LOW(WS-TX)
                   MOVE WS-CAPINT-SCHED-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT TIER TIER FOR THE CAPINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3940-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-TIER-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF STLT-CAPINT-AMT NOT < WS-DEFERINT-TIER-LOW(WS-TX)
                   MOVE WS-DEFERINT-TIER-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3950-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * HY -> JZ WHEN EFF DATE IS PAST
      * JZ -> RK WHEN EFF DATE IS PAST
      * RK -> HY WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3960-SET-STATUS.
           EVALUATE STLM-STATUS-CD
               WHEN 'HY'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO STLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RK' TO STLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RK'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HY' TO STLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT FEE ON THE DISBURSE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3970-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN STLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO STLM-CAPINT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-HOLD-FLAG = 'Y'
                   COMPUTE STLM-CAPINT-AMT ROUNDED = STLM-DISBURSE-YTD *
                       STLM-PENALTY-PCT / 200
               WHEN STLT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO STLM-CAPINT-AMT
               WHEN STLT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO STLM-CAPINT-AMT
               WHEN OTHER
                   COMPUTE STLM-CAPINT-AMT ROUNDED = STLM-DISBURSE-YTD *
                       STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLM-CAPINT-AMT > 0 AND STLM-CAPINT-AMT < 10.00
               MOVE 10.00 TO STLM-CAPINT-AMT
           END-IF
           IF STLM-CAPINT-AMT > 25.00
               MOVE 25.00 TO STLM-CAPINT-AMT
           END-IF
           ADD STLM-CAPINT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3980-FLAG-PRINCIPAL.
           IF STLM-PRINCIPAL-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE STLM-PRINCIPAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CAPINT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3990-SPLIT-CAPINT.
           COMPUTE WS-WORK-AMT = STLT-CAPINT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = STLT-CAPINT-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CAPINT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEFERINT YTD.
      *----------------------------------------------------------------*
       4000-PRORATE-DEFERINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-DEFERINT-YTD ROUNDED =
                   STLT-CAPINT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-DEFERINT-YTD
           END-IF
           ADD STLM-DEFERINT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4010-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF STLM-MATURITY-DATE > 0 AND STLM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLT-CAPINT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD STLT-CAPINT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD STLT-CAPINT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD STLT-CAPINT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLT-CAPINT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CAPINT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4020-FLAG-CAPINT.
           IF STLM-CAPINT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE STLM-CAPINT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4030-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = STLM-PRINCIPAL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = STLM-PRINCIPAL-AMT - (WS-WORK-AMT *
               3)
           COMPUTE STLT-CAPINT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4040-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF STLM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4050-COUNT-ITEM.
           IF STLM-ITEM-CNT < 9999999
               ADD 1 TO STLM-ITEM-CNT
           END-IF
           IF STLM-ITEM-CNT NOT < 3
               MOVE 'Y' TO STLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4060-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLT-TRAN-CD
                   ADD STLM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4070-FLAG-PAYMENT.
           IF STLT-PAYMENT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE STLT-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E092 REF NO REQUIRED
      * E805 WORK AMT 2 NEGATIVE
      * E970 WORK AMT 2 OVER 10000
      * E368 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4080-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-REF-NO = SPACES
                   MOVE 'E092' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E805' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E970' TO WS-REJECT-CODE
               WHEN STLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E368' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E143 MEMO REQUIRED
      * E956 WORK AMT NEGATIVE
      * E708 WORK AMT OVER 25000
      * E961 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4090-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-MEMO = SPACES
                   MOVE 'E143' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E956' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E708' TO WS-REJECT-CODE
               WHEN STLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E961' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4100-LIMIT-PRINCIPAL.
           IF STLM-PRINCIPAL-AMT < 0.00
               MOVE 0.00 TO STLM-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-PRINCIPAL-AMT > 10000.00
               MOVE 10000.00 TO STLM-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4110-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF STLM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4120-DESCRIBE-STATUS.
           EVALUATE STLM-STATUS-CD
               WHEN 'HY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT TIER TIER FOR THE SUBSIDY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4130-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-TIER-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF STLM-SUBSIDY-AMT NOT < WS-DEFERINT-TIER-LOW(WS-TX)
                   MOVE WS-DEFERINT-TIER-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4140-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLT-TRAN-CD
                   ADD STLM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEFERINT YTD.
      *----------------------------------------------------------------*
       4150-PRORATE-DEFERINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-DEFERINT-YTD ROUNDED =
                   STLT-DISBURSE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-DEFERINT-YTD
           END-IF
           ADD STLM-DEFERINT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4160-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO STLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E320 REJECT CD REQUIRED
      * E673 CAPINT AMT NEGATIVE
      * E246 CAPINT AMT OVER 5000
      * E052 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4170-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-REJECT-CD = SPACES
                   MOVE 'E320' TO WS-REJECT-CODE
               WHEN STLM-CAPINT-AMT < 0
                   MOVE 'E673' TO WS-REJECT-CODE
               WHEN STLM-CAPINT-AMT > 5000.00
                   MOVE 'E246' TO WS-REJECT-CODE
               WHEN STLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E052' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4180-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = STLM-PAYMENT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = STLM-PAYMENT-YTD - (WS-WORK-AMT * 12)
           COMPUTE STLM-CAPINT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4190-LIMIT-PRINCIPAL.
           IF STLM-PRINCIPAL-YTD < 10.00
               MOVE 10.00 TO STLM-PRINCIPAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-PRINCIPAL-YTD > 500.00
               MOVE 500.00 TO STLM-PRINCIPAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4200-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF STLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO STLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CAPINT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4210-PRORATE-CAPINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   STLM-CAPINT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE CAPINT AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4220-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-STATUS-CD
                   ADD STLM-CAPINT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-CAPINT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SUBSIDY AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4230-SPLIT-SUBSIDY.
           COMPUTE WS-WORK-AMT = STLM-SUBSIDY-AMT / 12
           COMPUTE WS-WORK-AMT-2 = STLM-SUBSIDY-AMT - (WS-WORK-AMT * 12)
           COMPUTE STLM-DISBURSE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT TIER TIER FOR THE CAPINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4240-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-TIER-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF STLT-CAPINT-AMT NOT < WS-DEFERINT-TIER-LOW(WS-TX)
                   MOVE WS-DEFERINT-TIER-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4250-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF STLM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO STLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE AMT BY LOAN TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4260-ACCUM-LOAN-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-LOAN-TYPE-CD
                   ADD STLM-DISBURSE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-LOAN-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-DISBURSE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT FEE ON THE CAPINT YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4270-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN STLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO STLT-CAPINT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-SUSPEND-FLAG = 'Y'
                   COMPUTE STLT-CAPINT-AMT ROUNDED = STLM-CAPINT-YTD *
                       STLM-PENALTY-PCT / 200
               WHEN STLM-REGION-CD = 'C'
                   MOVE 3.00 TO STLT-CAPINT-AMT
               WHEN STLM-REGION-CD = 'D'
                   MOVE 20.00 TO STLT-CAPINT-AMT
               WHEN OTHER
                   COMPUTE STLT-CAPINT-AMT ROUNDED = STLM-CAPINT-YTD *
                       STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLT-CAPINT-AMT > 0 AND STLT-CAPINT-AMT < 2.00
               MOVE 2.00 TO STLT-CAPINT-AMT
           END-IF
           IF STLT-CAPINT-AMT > 250.00
               MOVE 250.00 TO STLT-CAPINT-AMT
           END-IF
           ADD STLT-CAPINT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISBURSE AMT.
      *----------------------------------------------------------------*
       4280-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLT-DISBURSE-AMT ROUNDED =
                   STLM-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLT-DISBURSE-AMT
           END-IF
           ADD STLT-DISBURSE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4290-COUNT-ITEM.
           IF STLM-ITEM-CNT < 9999999
               ADD 1 TO STLM-ITEM-CNT
           END-IF
           IF STLM-ITEM-CNT NOT < 10
               MOVE 'Y' TO STLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4300-COUNT-CYCLE.
           IF STLM-CYCLE-CNT < 9999999
               ADD 1 TO STLM-CYCLE-CNT
           END-IF
           IF STLM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT TIER TIER FOR THE CAPINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4310-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-TIER-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF STLT-CAPINT-AMT NOT < WS-DEFERINT-TIER-LOW(WS-TX)
                   MOVE WS-DEFERINT-TIER-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4320-ACCRUE-DISBURSE.
           IF STLM-DISBURSE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-DISBURSE-AMT * STLM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO STLM-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AST -> BCK WHEN LAST PAY DATE IS PAST
      * BCK -> CJG WHEN LAST PAY DATE IS PAST
      * CJG -> DSJ WHEN LAST PAY DATE IS PAST
      * DSJ -> JVY WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4330-SET-SOURCE.
           EVALUATE STLM-SOURCE-CD
               WHEN 'AST'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BCK' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BCK'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CJG' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJG'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DSJ' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DSJ'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JVY' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4340-ACCRUE-DISBURSE.
           IF STLM-DISBURSE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-DISBURSE-YTD * STLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO STLT-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4350-COUNT-SEQ.
           IF STLT-SEQ < 9999999
               ADD 1 TO STLT-SEQ
           END-IF
           IF STLT-SEQ NOT < 3
               MOVE 'Y' TO STLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEFERINT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SUBSIDY YTD.
      *----------------------------------------------------------------*
       4360-PRORATE-SUBSIDY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-SUBSIDY-YTD ROUNDED =
                   STLM-DEFERINT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-SUBSIDY-YTD
           END-IF
           ADD STLM-SUBSIDY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4370-COUNT-CYCLE.
           IF STLM-CYCLE-CNT < 9999999
               ADD 1 TO STLM-CYCLE-CNT
           END-IF
           IF STLM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO STLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4380-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF STLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO STLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4390-COUNT-CYCLE.
           IF STLM-CYCLE-CNT < 9999999
               ADD 1 TO STLM-CYCLE-CNT
           END-IF
           IF STLM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO STLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SUBSIDY FEE ON THE DEFERINT YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4400-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN STLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO STLM-SUBSIDY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLT-REVERSAL-FLAG = 'Y'
                   COMPUTE STLM-SUBSIDY-AMT ROUNDED = STLM-DEFERINT-YTD
                       * STLM-FEE-PCT / 200
               WHEN STLM-SOURCE-CD = 'AST'
                   MOVE 12.00 TO STLM-SUBSIDY-AMT
               WHEN STLM-SOURCE-CD = 'BCK'
                   MOVE 12.00 TO STLM-SUBSIDY-AMT
               WHEN OTHER
                   COMPUTE STLM-SUBSIDY-AMT ROUNDED = STLM-DEFERINT-YTD
                       * STLM-FEE-PCT / 100
           END-EVALUATE
           IF STLM-SUBSIDY-AMT > 0 AND STLM-SUBSIDY-AMT < 1.00
               MOVE 1.00 TO STLM-SUBSIDY-AMT
           END-IF
           IF STLM-SUBSIDY-AMT > 25.00
               MOVE 25.00 TO STLM-SUBSIDY-AMT
           END-IF
           ADD STLM-SUBSIDY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE BAND TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4410-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-BAND-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF STLM-PAYMENT-AMT NOT < WS-DISBURSE-BAND-LOW(WS-TX)
                   MOVE WS-DISBURSE-BAND-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEFERINT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4420-FLAG-DEFERINT.
           IF STLM-DEFERINT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE STLM-DEFERINT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CAPINT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4430-FLAG-CAPINT.
           IF STLM-CAPINT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE STLM-CAPINT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4440-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF STLM-NEXT-REVIEW-DATE > 0 AND STLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CAPINT SCHED TIER FOR THE DEFERINT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4450-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CAPINT-SCHED-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF STLM-DEFERINT-YTD NOT < WS-CAPINT-SCHED-LOW(WS-TX)
                   MOVE WS-CAPINT-SCHED-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CAPINT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4460-PRORATE-CAPINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   STLT-CAPINT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DISBURSE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4470-FLAG-DISBURSE.
           IF STLM-DISBURSE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE STLM-DISBURSE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEFERINT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4480-SPLIT-DEFERINT.
           COMPUTE WS-WORK-AMT = STLM-DEFERINT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = STLM-DEFERINT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE STLM-PRINCIPAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4490-COUNT-EXCEPT.
           IF STLM-EXCEPT-CNT < 9999999
               ADD 1 TO STLM-EXCEPT-CNT
           END-IF
           IF STLM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO STLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4500-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF STLM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO STLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEFERINT AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4510-LIMIT-DEFERINT.
           IF STLM-DEFERINT-AMT < 25.00
               MOVE 25.00 TO STLM-DEFERINT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-DEFERINT-AMT > 500.00
               MOVE 500.00 TO STLM-DEFERINT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4520-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN STLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = STLM-PRINCIPAL-AMT *
                       STLM-FEE-PCT / 200
               WHEN STLM-SOURCE-CD = 'AST'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN STLM-SOURCE-CD = 'BCK'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = STLM-PRINCIPAL-AMT *
                       STLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CAPINT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4530-FLAG-CAPINT.
           IF STLM-CAPINT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE STLM-CAPINT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST MAINT DATE IS PAST
      * BAT -> MAL WHEN LAST MAINT DATE IS PAST
      * MAL -> TEL WHEN LAST MAINT DATE IS PAST
      * TEL -> ONL WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4540-SET-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   IF STLM-LAST-MAINT-DATE > 0 AND STLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF STLM-LAST-MAINT-DATE > 0 AND STLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF STLM-LAST-MAINT-DATE > 0 AND STLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF STLM-LAST-MAINT-DATE > 0 AND STLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DEFERINT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4550-FLAG-DEFERINT.
           IF STLM-DEFERINT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE STLM-DEFERINT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SUBSIDY AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4560-SPLIT-SUBSIDY.
           COMPUTE WS-WORK-AMT = STLM-SUBSIDY-AMT / 2
           COMPUTE WS-WORK-AMT-2 = STLM-SUBSIDY-AMT - (WS-WORK-AMT * 2)
           COMPUTE STLT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4570-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = STLM-DISBURSE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = STLM-DISBURSE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE STLM-SUBSIDY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4580-SET-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   IF STLM-OPEN-DATE > 0 AND STLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF STLM-OPEN-DATE > 0 AND STLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF STLM-OPEN-DATE > 0 AND STLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF STLM-OPEN-DATE > 0 AND STLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E528 ADDR REQUIRED
      * E167 PAYMENT AMT NEGATIVE
      * E749 PAYMENT AMT OVER 250000
      * E465 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4590-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-ADDR = SPACES
                   MOVE 'E528' TO WS-REJECT-CODE
               WHEN STLT-PAYMENT-AMT < 0
                   MOVE 'E167' TO WS-REJECT-CODE
               WHEN STLT-PAYMENT-AMT > 250000.00
                   MOVE 'E749' TO WS-REJECT-CODE
               WHEN STLM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E465' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LOAN TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4600-DESCRIBE-LOAN-TYPE.
           EVALUATE STLM-LOAN-TYPE-CD
               WHEN 'CS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4610-DESCRIBE-REGION.
           EVALUATE STLM-REGION-CD
               WHEN 'C'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'D'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'W'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4620-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4630-LIMIT-DISBURSE.
           IF STLT-DISBURSE-AMT < 10.00
               MOVE 10.00 TO STLT-DISBURSE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLT-DISBURSE-AMT > 50000.00
               MOVE 50000.00 TO STLT-DISBURSE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT TIER TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4640-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-TIER-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF STLT-PAYMENT-AMT NOT < WS-DEFERINT-TIER-LOW(WS-TX)
                   MOVE WS-DEFERINT-TIER-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4650-LIMIT-PRINCIPAL.
           IF STLM-PRINCIPAL-AMT < 100.00
               MOVE 100.00 TO STLM-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-PRINCIPAL-AMT > 50000.00
               MOVE 50000.00 TO STLM-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4660-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF STLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO STLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4670-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = STLT-PAYMENT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = STLT-PAYMENT-AMT - (WS-WORK-AMT * 12)
           COMPUTE STLM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4680-COUNT-PAYMENT.
           IF STLM-PAYMENT-CNT < 9999999
               ADD 1 TO STLM-PAYMENT-CNT
           END-IF
           IF STLM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO STLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4690-ACCRUE-DISBURSE.
           IF STLM-DISBURSE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-DISBURSE-YTD * STLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO STLT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4700-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF STLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO STLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4710-COUNT-EXCEPT.
           IF STLM-EXCEPT-CNT < 9999999
               ADD 1 TO STLM-EXCEPT-CNT
           END-IF
           IF STLM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SUBSIDY AMT.
      *----------------------------------------------------------------*
       4720-PRORATE-SUBSIDY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-SUBSIDY-AMT ROUNDED =
                   STLT-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-SUBSIDY-AMT
           END-IF
           ADD STLM-SUBSIDY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4730-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CAPINT SCHED TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4740-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CAPINT-SCHED-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF STLT-PAYMENT-AMT NOT < WS-CAPINT-SCHED-LOW(WS-TX)
                   MOVE WS-CAPINT-SCHED-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOAN TYPE CD:
      * CS -> DV WHEN POST DATE IS PAST
      * DV -> JL WHEN POST DATE IS PAST
      * JL -> NE WHEN POST DATE IS PAST
      * NE -> SH WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4750-SET-LOAN-TYPE.
           EVALUATE STLM-LOAN-TYPE-CD
               WHEN 'CS'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DV' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DV'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JL'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NE' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NE'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE STL FIELDS TO SLCAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-SLCAL.
           INITIALIZE SLCAL-PARMS
           MOVE STLM-CAPINT-AMT
             TO SLCAL-CAPINT-AMT
           MOVE STLM-SUBSIDY-YTD
             TO SLCAL-SUBSIDY-YTD
           MOVE STLM-PAYMENT-AMT
             TO SLCAL-PAYMENT-AMT
           MOVE STLM-PRINCIPAL-AMT
             TO SLCAL-PRINCIPAL-AMT
           MOVE STLM-DEFERINT-YTD
             TO SLCAL-DEFERINT-YTD
           MOVE STLM-DISBURSE-YTD
             TO SLCAL-DISBURSE-YTD
           MOVE STLM-OPEN-DATE
             TO SLCAL-OPEN-DATE
           MOVE STLM-EXPIRY-DATE
             TO SLCAL-EXPIRY-DATE
           MOVE STLM-LAST-ACT-DATE
             TO SLCAL-LAST-ACT-DATE
           MOVE STLM-LAST-PAY-DATE
             TO SLCAL-LAST-PAY-DATE
           MOVE STLM-CYCLE-CD
             TO SLCAL-CYCLE-CD
           MOVE STLM-DEFER-CODE-CD
             TO SLCAL-DEFER-CODE-CD
           MOVE STLM-SOURCE-CD
             TO SLCAL-SOURCE-CD
           MOVE STLM-REGION-CD
             TO SLCAL-REGION-CD
           MOVE STLM-REVIEW-FLAG
             TO SLCAL-REVIEW-FLAG
           MOVE STLM-NOTICE-SENT-FLAG
             TO SLCAL-NOTICE-SENT-FLAG
           MOVE STLM-OVERRIDE-FLAG
             TO SLCAL-OVERRIDE-FLAG
           MOVE STLM-EXCEPT-CNT
             TO SLCAL-EXCEPT-CNT
           MOVE STLM-PAYMENT-CNT
             TO SLCAL-PAYMENT-CNT
           MOVE STLM-PENALTY-PCT
             TO SLCAL-PENALTY-PCT
           MOVE STLM-FEE-PCT
             TO SLCAL-FEE-PCT
           MOVE STLM-NAME
             TO SLCAL-NAME
           MOVE STLM-ADDR
             TO SLCAL-ADDR
           CALL 'SLCAL' USING SLCAL-PARMS
           EVALUATE SLCAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SLCAL-PRINCIPAL-AMT
                     TO STLM-PRINCIPAL-AMT
                   MOVE SLCAL-DEFERINT-YTD
                     TO STLM-DEFERINT-YTD
                   MOVE SLCAL-REVIEW-FLAG
                     TO STLM-REVIEW-FLAG
                   MOVE SLCAL-OVERRIDE-FLAG
                     TO STLM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-SLCAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE STL FIELDS TO SLVAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-SLVAL.
           INITIALIZE SLVAL-PARMS
           MOVE STLM-DEFERINT-YTD
             TO SLVAL-DEFERINT-YTD
           MOVE STLM-SUBSIDY-YTD
             TO SLVAL-SUBSIDY-YTD
           MOVE STLM-PAYMENT-YTD
             TO SLVAL-PAYMENT-YTD
           MOVE STLM-CAPINT-AMT
             TO SLVAL-CAPINT-AMT
           MOVE STLM-DISBURSE-YTD
             TO SLVAL-DISBURSE-YTD
           MOVE STLM-CAPINT-YTD
             TO SLVAL-CAPINT-YTD
           MOVE STLM-EXPIRY-DATE
             TO SLVAL-EXPIRY-DATE
           MOVE STLM-LAST-MAINT-DATE
             TO SLVAL-LAST-MAINT-DATE
           MOVE STLM-LAST-PAY-DATE
             TO SLVAL-LAST-PAY-DATE
           MOVE STLM-LAST-ACT-DATE
             TO SLVAL-LAST-ACT-DATE
           MOVE STLM-SOURCE-CD
             TO SLVAL-SOURCE-CD
           MOVE STLM-CYCLE-CD
             TO SLVAL-CYCLE-CD
           MOVE STLM-LOAN-TYPE-CD
             TO SLVAL-LOAN-TYPE-CD
           MOVE STLM-DEFER-CODE-CD
             TO SLVAL-DEFER-CODE-CD
           MOVE STLM-PREFERRED-FLAG
             TO SLVAL-PREFERRED-FLAG
           MOVE STLM-SUSPEND-FLAG
             TO SLVAL-SUSPEND-FLAG
           MOVE STLM-REVIEW-FLAG
             TO SLVAL-REVIEW-FLAG
           MOVE STLM-PAYMENT-CNT
             TO SLVAL-PAYMENT-CNT
           MOVE STLM-ITEM-CNT
             TO SLVAL-ITEM-CNT
           MOVE STLM-FEE-PCT
             TO SLVAL-FEE-PCT
           MOVE STLM-DISCOUNT
             TO SLVAL-DISCOUNT
           MOVE STLM-DESC
             TO SLVAL-DESC
           MOVE STLM-CUST-ID
             TO SLVAL-CUST-ID
           CALL 'SLVAL' USING SLVAL-PARMS
           EVALUATE SLVAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SLVAL-DEFERINT-YTD
                     TO STLM-DEFERINT-YTD
                   MOVE SLVAL-SUBSIDY-YTD
                     TO STLM-SUBSIDY-YTD
                   MOVE SLVAL-PAYMENT-YTD
                     TO STLM-PAYMENT-YTD
                   MOVE SLVAL-DISBURSE-YTD
                     TO STLM-DISBURSE-YTD
                   MOVE SLVAL-SUSPEND-FLAG
                     TO STLM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-SLVAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE STLT-KEY TO RD-KEY
           MOVE STLT-TRAN-CD TO RD-CODE
           MOVE STLT-DISBURSE-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE STLT-KEY TO RD-KEY
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
           DISPLAY 'SLLACCR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'SLLACCR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
