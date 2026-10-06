      *================================================================*
      * PROGRAM  : ESCCALC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : MORTGAGE ESCROW - PERIODIC CALCULATION. READS THE   *
      *            ESC TRANSACTIONS, UPDATES THE ESC MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. ESCROW ANALYSIS, TAX   *
      *            AND INSURANCE DISBURSEMENT AND SHORTAGE             *
      * INPUT    : ESCMST (KSDS), ESCTRN, BANKCTL                      *
      * OUTPUT   : ESCOUT, ESCCARPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ESRISK, ESFEE           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-06-14 SKN  ORIGINAL                                       *
      * 1991-07-13 MKL  SURPLUS NOTICE TEXT PER LEGAL CHG6161          *
      * 1991-09-27 TWH  INSURANCE FEE SCHEDULE UPDATE PRB2416          *
      * 1992-06-26 RJT  CUSHION LIMIT REVISED INC9081                  *
      * 1992-11-16 SKN  SURPLUS LIMIT REVISED INC8226                  *
      * 1994-06-08 TWH  TAX FEE SCHEDULE UPDATE REQ7607                *
      * 2003-06-13 CRP  SURPLUS LIMIT REVISED PRB4387                  *
      * 2004-11-21 JLM  AUDIT FINDING - TAX CONTROL TOTALS INC6821     *
      * 2008-08-14 TWH  CUSHION NOTICE TEXT PER LEGAL INC3546          *
      * 2010-12-14 PQR  AUDIT FINDING - TAX CONTROL TOTALS CHG1142     *
      * 2011-02-03 SKN  NEW SURPLUS REPORT COLUMNS CHG2546             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESCCALC.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "ESCMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ESCM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "ESCTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "ESCOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "ESCCARPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY ESCMST.
       FD  TRAN-FILE.
       COPY ESCTRN.
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
      * WS-SURPLUS-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SURPLUS-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000510000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000370000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000311500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000876500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022550000000168000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022800000000904500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023300000000573500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024300000000850000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025300000000689000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035300000000842000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036300000000683000'.
       01  WS-SURPLUS-TIER-TBL REDEFINES WS-SURPLUS-TIER-VALUES.
           05  WS-SURPLUS-TIER-ROW OCCURS 11 TIMES.
               10  WS-SURPLUS-TIER-LOW     PIC 9(09)V99.
               10  WS-SURPLUS-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYMENT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000328000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000553000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000728000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004000000000568500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014000000000708000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019000000000216000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029000000000467000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00034000000000312500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036500000000792500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036550000000021000'.
       01  WS-PAYMENT-GRID-TBL REDEFINES WS-PAYMENT-GRID-VALUES.
           05  WS-PAYMENT-GRID-ROW OCCURS 10 TIMES.
               10  WS-PAYMENT-GRID-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SURPLUS-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SURPLUS-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000051000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000015500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002000000000767500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002250000000767000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002300000000768000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012300000000205500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012350000000064000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014850000000302000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014900000000874000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000323000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000755500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016500000000763000'.
       01  WS-SURPLUS-BAND-TBL REDEFINES WS-SURPLUS-BAND-VALUES.
           05  WS-SURPLUS-BAND-ROW OCCURS 12 TIMES.
               10  WS-SURPLUS-BAND-LOW     PIC 9(09)V99.
               10  WS-SURPLUS-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYMENT-GRIDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-GRIDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000800000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000148000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000317500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010300000000425500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015300000000069500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015800000000241000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018300000000792000'.
       01  WS-PAYMENT-GRIDX-TBL REDEFINES WS-PAYMENT-GRIDX-VALUES.
           05  WS-PAYMENT-GRIDX-ROW OCCURS 7 TIMES.
               10  WS-PAYMENT-GRIDX-LOW    PIC 9(09)V99.
               10  WS-PAYMENT-GRIDX-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ESCCALC  MORTGAGE ESCROW'.
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
       COPY ESRISKP.
       COPY ESFEEP.

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
               MOVE 'ESCMST' TO ABND-FILE-NAME
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
           MOVE ESCT-KEY TO ESCM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SELECT-RATE
               PERFORM 3010-ACCUM-STATUS
               PERFORM 3020-ASSESS-OVERRIDE
               PERFORM 3030-SPLIT-SHORTAGE
               PERFORM 3040-CHECK-NOTICE
               PERFORM 3050-LIMIT-SURPLUS
               PERFORM 3060-LIMIT-DISBURSE
               PERFORM 3070-COUNT-EXCEPT
               PERFORM 3080-DESCRIBE-ANALYSIS
               PERFORM 3090-ASSESS-HOLD
               PERFORM 3100-DESCRIBE-REGION
               PERFORM 3110-SELECT-FEE-PCT
               PERFORM 3120-PRORATE-CUSHION
               PERFORM 3130-ACCRUE-SHORTAGE
               PERFORM 3140-AGE-LAST-STMT
               PERFORM 3150-LIMIT-SHORTAGE
               PERFORM 3160-AGE-LAST-PAY
               PERFORM 3170-ASSESS-STOP
               PERFORM 3180-ASSESS-OVERRIDE
               PERFORM 3190-PRORATE-PAYMENT
               PERFORM 3200-SELECT-DISCOUNT
               PERFORM 3210-ACCRUE-RATE
               PERFORM 3220-ASSESS-NOTICE-SENT
               PERFORM 3230-CHECK-LAST-PAY
               PERFORM 3240-ACCRUE-SHORTAGE
               PERFORM 3250-CHECK-NEXT-REVIEW
               PERFORM 3260-FLAG-PAYMENT
               PERFORM 3270-FLAG-SURPLUS
               PERFORM 3280-FLAG-SURPLUS
               PERFORM 3290-DESCRIBE-REASON
               PERFORM 3300-AGE-EXPIRY
               PERFORM 3310-CHECK-LAST-ACT
               PERFORM 3320-FLAG-CUSHION
               PERFORM 3330-ACCRUE-DISBURSE
               PERFORM 3340-PRORATE-CUSHION
               PERFORM 3350-SELECT-DISCOUNT
               PERFORM 3360-SET-STATUS
               PERFORM 3370-COUNT-SEQ
               PERFORM 3380-EDIT-ADDR
               PERFORM 3390-COUNT-CYCLE
               PERFORM 3400-SELECT-DISCOUNT
               PERFORM 3410-ACCRUE-INSURANCE
               PERFORM 3420-SET-REGION
               PERFORM 3430-ACCUM-STATUS
               PERFORM 3440-PRORATE-SHORTAGE
               PERFORM 3450-PRORATE-TAX
               PERFORM 3460-ACCUM-ANALYSIS
               PERFORM 3470-DESCRIBE-CHANNEL
               PERFORM 3480-EDIT-REF-NO
               PERFORM 3490-AGE-LAST-PAY
               PERFORM 3500-ACCUM-TYPE
               PERFORM 3510-PRORATE-INSURANCE
               PERFORM 3520-LIMIT-TAX
               PERFORM 3530-FLAG-CUSHION
               PERFORM 3540-EDIT-REJECT
               PERFORM 3550-EDIT-CUST-ID
               PERFORM 3560-COUNT-RETRY
               PERFORM 3570-SET-TRAN
               PERFORM 3580-LIMIT-ITEM
               PERFORM 3590-LIMIT-DISBURSE
               PERFORM 3600-ACCRUE-PAYMENT
               PERFORM 3610-SELECT-RATE
               PERFORM 3620-PRORATE-SHORTAGE
               PERFORM 3630-DESCRIBE-REASON
               PERFORM 3640-AGE-NOTICE
               PERFORM 3650-CHECK-NEXT-DUE
               PERFORM 3660-ACCRUE-DISBURSE
               PERFORM 3670-SELECT-RATE
               PERFORM 3680-SET-TRAN
               PERFORM 3690-AGE-NEXT-REVIEW
               PERFORM 3700-ACCUM-PAYEE-TYPE
               PERFORM 3710-DESCRIBE-STATUS
               PERFORM 3720-ACCUM-ANALYSIS
               PERFORM 3730-DESCRIBE-TYPE
               PERFORM 3740-ACCUM-STATUS
               PERFORM 3750-ASSESS-NOTICE-SENT
               PERFORM 3760-SPLIT-PAYMENT
               PERFORM 3770-FLAG-PAYMENT
               PERFORM 3780-SET-STATUS
               PERFORM 3790-PRORATE-INSURANCE
               PERFORM 3800-ASSESS-SUSPEND
               PERFORM 3810-FLAG-INSURANCE
               PERFORM 3820-ASSESS-NOTICE-SENT
               PERFORM 3830-AGE-LAST-STMT
               PERFORM 3840-SET-TRAN
               PERFORM 3850-COUNT-NOTICE
               PERFORM 3860-LIMIT-INSURANCE
               PERFORM 3870-AGE-NOTICE
               PERFORM 3880-SELECT-RATE
               PERFORM 3890-PRORATE-TAX
               PERFORM 3900-FLAG-TAX
               PERFORM 3910-DESCRIBE-REASON
               PERFORM 3920-ASSESS-PREFERRED
               PERFORM 3930-DESCRIBE-STATUS
               PERFORM 3940-DESCRIBE-TRAN
               PERFORM 3950-FLAG-SHORTAGE
               PERFORM 3960-FLAG-TAX
               PERFORM 3970-LIMIT-DISBURSE
               PERFORM 3980-LIMIT-CUSHION
               PERFORM 3990-ACCUM-TRAN
               PERFORM 4000-COUNT-RETRY
               PERFORM 4010-ACCUM-TYPE
               PERFORM 4020-ACCRUE-FEE-PCT
               PERFORM 4030-CHECK-CLOSE
               PERFORM 4040-ACCUM-PAYEE-TYPE
               PERFORM 4050-SELECT-FEE-PCT
               PERFORM 4060-CHECK-NEXT-DUE
               PERFORM 4070-COUNT-NOTICE
               PERFORM 4080-PRORATE-DISBURSE
               PERFORM 4090-ACCRUE-DISBURSE
               PERFORM 4100-COUNT-SEQ
               PERFORM 4110-ASSESS-SUSPEND
               PERFORM 4120-CHECK-NEXT-REVIEW
               PERFORM 4130-COUNT-CYCLE
               PERFORM 4140-SET-REASON
               PERFORM 4150-COUNT-CYCLE
               PERFORM 4160-SET-TYPE
               PERFORM 4170-FLAG-SHORTAGE
               PERFORM 4180-DESCRIBE-STATUS
               PERFORM 4190-SET-STATUS
               PERFORM 4200-SET-STATUS
               PERFORM 4210-SET-TRAN
               PERFORM 4220-DESCRIBE-TRAN
               PERFORM 4230-COUNT-SEQ
               PERFORM 4240-ACCUM-ANALYSIS
               PERFORM 4250-PRORATE-PAYMENT
               PERFORM 4260-ACCRUE-DISBURSE
               PERFORM 4270-FLAG-SHORTAGE
               PERFORM 4280-DESCRIBE-REGION
               PERFORM 4290-ASSESS-REVERSAL
               PERFORM 4300-AGE-NEXT-DUE
               PERFORM 4310-SPLIT-SHORTAGE
               PERFORM 4320-SPLIT-DISBURSE
               PERFORM 4330-CHECK-EFFECTIVE
               PERFORM 4340-ACCUM-TYPE
               PERFORM 4350-DESCRIBE-REASON
               PERFORM 4360-ACCUM-REASON
               PERFORM 4370-EDIT-CONTACT
               PERFORM 4380-ACCRUE-RATE
               PERFORM 4390-CHECK-NEXT-DUE
               PERFORM 4400-CHECK-LAST-STMT
               PERFORM 7030-CALL-ESRISK
               PERFORM 7000-CALL-ESFEE
               REWRITE ESC-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'ESCMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM ESC-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * SELECT THE SURPLUS BAND TIER FOR THE CUSHION AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3000-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SURPLUS-BAND-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ESCM-CUSHION-AMT NOT < WS-SURPLUS-BAND-LOW(WS-TX)
                   MOVE WS-SURPLUS-BAND-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3010-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-STATUS-CD
                   ADD ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE INSURANCE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3020-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ESCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ESCM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-SUSPEND-FLAG = 'Y'
                   COMPUTE ESCM-PAYMENT-YTD ROUNDED = ESCT-INSURANCE-AMT
                       * ESCM-DISCOUNT / 200
               WHEN ESCT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO ESCM-PAYMENT-YTD
               WHEN ESCT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO ESCM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE ESCM-PAYMENT-YTD ROUNDED = ESCT-INSURANCE-AMT
                       * ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-PAYMENT-YTD > 0 AND ESCM-PAYMENT-YTD < 1.00
               MOVE 1.00 TO ESCM-PAYMENT-YTD
           END-IF
           IF ESCM-PAYMENT-YTD > 150.00
               MOVE 150.00 TO ESCM-PAYMENT-YTD
           END-IF
           ADD ESCM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT SHORTAGE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3030-SPLIT-SHORTAGE.
           COMPUTE WS-WORK-AMT = ESCM-SHORTAGE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ESCM-SHORTAGE-YTD - (WS-WORK-AMT * 3)
           COMPUTE ESCM-CUSHION-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3040-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SURPLUS YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3050-LIMIT-SURPLUS.
           IF ESCM-SURPLUS-YTD < 25.00
               MOVE 25.00 TO ESCM-SURPLUS-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-SURPLUS-YTD > 500.00
               MOVE 500.00 TO ESCM-SURPLUS-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3060-LIMIT-DISBURSE.
           IF ESCM-DISBURSE-YTD < 0.00
               MOVE 0.00 TO ESCM-DISBURSE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-DISBURSE-YTD > 1000.00
               MOVE 1000.00 TO ESCM-DISBURSE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3070-COUNT-EXCEPT.
           IF ESCM-EXCEPT-CNT < 9999999
               ADD 1 TO ESCM-EXCEPT-CNT
           END-IF
           IF ESCM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ANALYSIS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3080-DESCRIBE-ANALYSIS.
           EVALUATE ESCM-ANALYSIS-CD
               WHEN 'GD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'LY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SHORTAGE FEE ON THE CUSHION YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3090-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ESCM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ESCT-SHORTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE ESCT-SHORTAGE-AMT ROUNDED = ESCM-CUSHION-YTD
                       * ESCM-RATE / 200
               WHEN ESCT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO ESCT-SHORTAGE-AMT
               WHEN ESCT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO ESCT-SHORTAGE-AMT
               WHEN OTHER
                   COMPUTE ESCT-SHORTAGE-AMT ROUNDED = ESCM-CUSHION-YTD
                       * ESCM-RATE / 100
           END-EVALUATE
           IF ESCT-SHORTAGE-AMT > 0 AND ESCT-SHORTAGE-AMT < 1.00
               MOVE 1.00 TO ESCT-SHORTAGE-AMT
           END-IF
           IF ESCT-SHORTAGE-AMT > 50.00
               MOVE 50.00 TO ESCT-SHORTAGE-AMT
           END-IF
           ADD ESCT-SHORTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3100-DESCRIBE-REGION.
           EVALUATE ESCM-REGION-CD
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'G'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE SURPLUS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3110-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ESCM-SURPLUS-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SURPLUS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CUSHION YTD.
      *----------------------------------------------------------------*
       3120-PRORATE-CUSHION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-CUSHION-YTD ROUNDED =
                   ESCM-SURPLUS-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-CUSHION-YTD
           END-IF
           ADD ESCM-CUSHION-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3130-ACCRUE-SHORTAGE.
           IF ESCM-SHORTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-SHORTAGE-AMT * ESCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ESCT-CUSHION-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3140-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHORTAGE AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3150-LIMIT-SHORTAGE.
           IF ESCM-SHORTAGE-AMT < 100.00
               MOVE 100.00 TO ESCM-SHORTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-SHORTAGE-AMT > 2500.00
               MOVE 2500.00 TO ESCM-SHORTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3160-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SHORTAGE YTD FEE ON THE CUSHION YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3170-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ESCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ESCM-SHORTAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ESCM-SHORTAGE-YTD ROUNDED = ESCM-CUSHION-YTD
                       * ESCM-DISCOUNT / 200
               WHEN ESCM-ANALYSIS-CD = 'GD'
                   MOVE 12.00 TO ESCM-SHORTAGE-YTD
               WHEN ESCM-ANALYSIS-CD = 'LY'
                   MOVE 7.00 TO ESCM-SHORTAGE-YTD
               WHEN OTHER
                   COMPUTE ESCM-SHORTAGE-YTD ROUNDED = ESCM-CUSHION-YTD
                       * ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-SHORTAGE-YTD > 0 AND ESCM-SHORTAGE-YTD < 1.00
               MOVE 1.00 TO ESCM-SHORTAGE-YTD
           END-IF
           IF ESCM-SHORTAGE-YTD > 150.00
               MOVE 150.00 TO ESCM-SHORTAGE-YTD
           END-IF
           ADD ESCM-SHORTAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE PAYMENT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3180-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ESCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ESCM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-HOLD-FLAG = 'Y'
                   COMPUTE ESCM-PAYMENT-YTD ROUNDED = ESCT-PAYMENT-AMT *
                       ESCM-FEE-PCT / 200
               WHEN ESCT-TRAN-CD = 'DS'
                   MOVE 10.00 TO ESCM-PAYMENT-YTD
               WHEN ESCT-TRAN-CD = 'PY'
                   MOVE 7.00 TO ESCM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE ESCM-PAYMENT-YTD ROUNDED = ESCT-PAYMENT-AMT *
                       ESCM-FEE-PCT / 100
           END-EVALUATE
           IF ESCM-PAYMENT-YTD > 0 AND ESCM-PAYMENT-YTD < 10.00
               MOVE 10.00 TO ESCM-PAYMENT-YTD
           END-IF
           IF ESCM-PAYMENT-YTD > 25.00
               MOVE 25.00 TO ESCM-PAYMENT-YTD
           END-IF
           ADD ESCM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE SHORTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3190-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-PAYMENT-AMT ROUNDED =
                   ESCT-SHORTAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-PAYMENT-AMT
           END-IF
           ADD ESCM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRIDX TIER FOR THE SHORTAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3200-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRIDX-RATE(1) TO ESCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ESCT-SHORTAGE-AMT NOT < WS-PAYMENT-GRIDX-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRIDX-RATE(WS-TX) TO ESCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3210-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ESCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ESCM-SHORTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CUSHION FEE ON THE CUSHION YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3220-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ESCM-CUSHION-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCT-REVERSAL-FLAG = 'Y'
                   COMPUTE ESCM-CUSHION-AMT ROUNDED = ESCM-CUSHION-YTD *
                       ESCM-FEE-PCT / 200
               WHEN ESCM-PAYEE-TYPE-CD = 'AM'
                   MOVE 7.00 TO ESCM-CUSHION-AMT
               WHEN ESCM-PAYEE-TYPE-CD = 'UE'
                   MOVE 5.00 TO ESCM-CUSHION-AMT
               WHEN OTHER
                   COMPUTE ESCM-CUSHION-AMT ROUNDED = ESCM-CUSHION-YTD *
                       ESCM-FEE-PCT / 100
           END-EVALUATE
           IF ESCM-CUSHION-AMT > 0 AND ESCM-CUSHION-AMT < 2.00
               MOVE 2.00 TO ESCM-CUSHION-AMT
           END-IF
           IF ESCM-CUSHION-AMT > 50.00
               MOVE 50.00 TO ESCM-CUSHION-AMT
           END-IF
           ADD ESCM-CUSHION-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3230-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ESCM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3240-ACCRUE-SHORTAGE.
           IF ESCM-SHORTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-SHORTAGE-AMT * ESCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ESCM-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3250-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ESCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3260-FLAG-PAYMENT.
           IF ESCT-PAYMENT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESCT-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SURPLUS YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3270-FLAG-SURPLUS.
           IF ESCM-SURPLUS-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCM-SURPLUS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SURPLUS YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3280-FLAG-SURPLUS.
           IF ESCM-SURPLUS-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ESCM-SURPLUS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3290-DESCRIBE-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'K'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3300-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ESCM-EXPIRY-DATE > 0 AND ESCM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3310-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ESCM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CUSHION YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3320-FLAG-CUSHION.
           IF ESCM-CUSHION-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCM-CUSHION-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3330-ACCRUE-DISBURSE.
           IF ESCM-DISBURSE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-DISBURSE-AMT * ESCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ESCM-SHORTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CUSHION AMT.
      *----------------------------------------------------------------*
       3340-PRORATE-CUSHION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-CUSHION-AMT ROUNDED =
                   ESCM-DISBURSE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-CUSHION-AMT
           END-IF
           ADD ESCM-CUSHION-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE INSURANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3350-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO ESCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ESCT-INSURANCE-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO ESCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * A -> E WHEN EFF DATE IS PAST
      * E -> R WHEN EFF DATE IS PAST
      * R -> U WHEN EFF DATE IS PAST
      * U -> Z WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3360-SET-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3370-COUNT-SEQ.
           IF ESCT-SEQ < 9999999
               ADD 1 TO ESCT-SEQ
           END-IF
           IF ESCT-SEQ NOT < 10
               MOVE 'Y' TO ESCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E868 ADDR REQUIRED
      * E788 INSURANCE AMT NEGATIVE
      * E453 INSURANCE AMT OVER 5000
      * E187 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3380-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-ADDR = SPACES
                   MOVE 'E868' TO WS-REJECT-CODE
               WHEN ESCM-INSURANCE-AMT < 0
                   MOVE 'E788' TO WS-REJECT-CODE
               WHEN ESCM-INSURANCE-AMT > 5000.00
                   MOVE 'E453' TO WS-REJECT-CODE
               WHEN ESCM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E187' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3390-COUNT-CYCLE.
           IF ESCM-CYCLE-CNT < 9999999
               ADD 1 TO ESCM-CYCLE-CNT
           END-IF
           IF ESCM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO ESCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURPLUS BAND TIER FOR THE CUSHION AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3400-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SURPLUS-BAND-RATE(1) TO ESCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ESCT-CUSHION-AMT NOT < WS-SURPLUS-BAND-LOW(WS-TX)
                   MOVE WS-SURPLUS-BAND-RATE(WS-TX) TO ESCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INSURANCE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3410-ACCRUE-INSURANCE.
           IF ESCM-INSURANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-INSURANCE-AMT * ESCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ESCM-TAX-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * B -> G WHEN CLOSE DATE IS PAST
      * G -> Y WHEN CLOSE DATE IS PAST
      * Y -> B WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3420-SET-REGION.
           EVALUATE ESCM-REGION-CD
               WHEN 'B'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO ESCM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO ESCM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO ESCM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CUSHION AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-STATUS-CD
                   ADD ESCM-CUSHION-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-CUSHION-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHORTAGE AMT.
      *----------------------------------------------------------------*
       3440-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-SHORTAGE-AMT ROUNDED =
                   ESCM-DISBURSE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-SHORTAGE-AMT
           END-IF
           ADD ESCM-SHORTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TAX AMT.
      *----------------------------------------------------------------*
       3450-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-TAX-AMT ROUNDED =
                   ESCM-TAX-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-TAX-AMT
           END-IF
           ADD ESCM-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX YTD BY ANALYSIS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3460-ACCUM-ANALYSIS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-ANALYSIS-CD
                   ADD ESCM-TAX-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-ANALYSIS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-TAX-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3470-DESCRIBE-CHANNEL.
           EVALUATE ESCT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E885 REF NO REQUIRED
      * E602 SHORTAGE AMT NEGATIVE
      * E523 SHORTAGE AMT OVER 5000
      * E903 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3480-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-REF-NO = SPACES
                   MOVE 'E885' TO WS-REJECT-CODE
               WHEN ESCT-SHORTAGE-AMT < 0
                   MOVE 'E602' TO WS-REJECT-CODE
               WHEN ESCT-SHORTAGE-AMT > 5000.00
                   MOVE 'E523' TO WS-REJECT-CODE
               WHEN ESCM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E903' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3490-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CUSHION YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3500-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-TYPE-CD
                   ADD ESCM-CUSHION-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-CUSHION-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INSURANCE YTD.
      *----------------------------------------------------------------*
       3510-PRORATE-INSURANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-INSURANCE-YTD ROUNDED =
                   ESCM-DISBURSE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-INSURANCE-YTD
           END-IF
           ADD ESCM-INSURANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TAX AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3520-LIMIT-TAX.
           IF ESCM-TAX-AMT < 100.00
               MOVE 100.00 TO ESCM-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-TAX-AMT > 10000.00
               MOVE 10000.00 TO ESCM-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CUSHION AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3530-FLAG-CUSHION.
           IF ESCM-CUSHION-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESCM-CUSHION-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E384 REJECT CD REQUIRED
      * E407 PAYMENT AMT NEGATIVE
      * E233 PAYMENT AMT OVER 250000
      * E419 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3540-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCT-REJECT-CD = SPACES
                   MOVE 'E384' TO WS-REJECT-CODE
               WHEN ESCT-PAYMENT-AMT < 0
                   MOVE 'E407' TO WS-REJECT-CODE
               WHEN ESCT-PAYMENT-AMT > 250000.00
                   MOVE 'E233' TO WS-REJECT-CODE
               WHEN ESCM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E419' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E834 CUST ID REQUIRED
      * E641 TAX YTD NEGATIVE
      * E348 TAX YTD OVER 99999
      * E559 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3550-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CUST-ID = SPACES
                   MOVE 'E834' TO WS-REJECT-CODE
               WHEN ESCM-TAX-YTD < 0
                   MOVE 'E641' TO WS-REJECT-CODE
               WHEN ESCM-TAX-YTD > 99999.00
                   MOVE 'E348' TO WS-REJECT-CODE
               WHEN ESCM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E559' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3560-COUNT-RETRY.
           IF ESCM-RETRY-CNT < 9999999
               ADD 1 TO ESCM-RETRY-CNT
           END-IF
           IF ESCM-RETRY-CNT NOT < 10
               MOVE 'Y' TO ESCM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DS -> PY WHEN NEXT REVIEW DATE IS PAST
      * PY -> AN WHEN NEXT REVIEW DATE IS PAST
      * AN -> RF WHEN NEXT REVIEW DATE IS PAST
      * RF -> SH WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3570-SET-TRAN.
           EVALUATE ESCT-TRAN-CD
               WHEN 'DS'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PY' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'AN' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AN'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'RF' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RF'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'SH' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3580-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3590-LIMIT-DISBURSE.
           IF ESCM-DISBURSE-AMT < 10.00
               MOVE 10.00 TO ESCM-DISBURSE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-DISBURSE-AMT > 10000.00
               MOVE 10000.00 TO ESCM-DISBURSE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3600-ACCRUE-PAYMENT.
           IF ESCM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-PAYMENT-YTD * ESCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURPLUS BAND TIER FOR THE DISBURSE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3610-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SURPLUS-BAND-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ESCM-DISBURSE-AMT NOT < WS-SURPLUS-BAND-LOW(WS-TX)
                   MOVE WS-SURPLUS-BAND-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHORTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHORTAGE YTD.
      *----------------------------------------------------------------*
       3620-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-SHORTAGE-YTD ROUNDED =
                   ESCT-SHORTAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-SHORTAGE-YTD
           END-IF
           ADD ESCM-SHORTAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'F'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'K'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'X'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3640-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3650-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ESCM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-DISBURSE.
           IF ESCM-DISBURSE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-DISBURSE-YTD * ESCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE CUSHION AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3670-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ESCT-CUSHION-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DS -> PY WHEN LAST STMT DATE IS PAST
      * PY -> AN WHEN LAST STMT DATE IS PAST
      * AN -> RF WHEN LAST STMT DATE IS PAST
      * RF -> SH WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3680-SET-TRAN.
           EVALUATE ESCT-TRAN-CD
               WHEN 'DS'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AN' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AN'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RF'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3690-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NEXT-REVIEW-DATE > 0 AND ESCM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE YTD BY PAYEE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3700-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-PAYEE-TYPE-CD
                   ADD ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3710-DESCRIBE-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'E'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'R'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX YTD BY ANALYSIS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3720-ACCUM-ANALYSIS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-ANALYSIS-CD
                   ADD ESCM-TAX-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-ANALYSIS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-TAX-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-TYPE.
           EVALUATE ESCM-TYPE-CD
               WHEN 'AKX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'REP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SMX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VJL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WKN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XKU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XTF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SURPLUS AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3740-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-STATUS-CD
                   ADD ESCM-SURPLUS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-SURPLUS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE DISBURSE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3750-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ESCM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   COMPUTE ESCM-PAYMENT-YTD ROUNDED = ESCM-DISBURSE-AMT
                       * ESCM-RATE / 200
               WHEN ESCM-REGION-CD = 'B'
                   MOVE 3.00 TO ESCM-PAYMENT-YTD
               WHEN ESCM-REGION-CD = 'G'
                   MOVE 7.00 TO ESCM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE ESCM-PAYMENT-YTD ROUNDED = ESCM-DISBURSE-AMT
                       * ESCM-RATE / 100
           END-EVALUATE
           IF ESCM-PAYMENT-YTD > 0 AND ESCM-PAYMENT-YTD < 5.00
               MOVE 5.00 TO ESCM-PAYMENT-YTD
           END-IF
           IF ESCM-PAYMENT-YTD > 250.00
               MOVE 250.00 TO ESCM-PAYMENT-YTD
           END-IF
           ADD ESCM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3760-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = ESCM-PAYMENT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ESCM-PAYMENT-YTD - (WS-WORK-AMT * 12)
           COMPUTE ESCM-SHORTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3770-FLAG-PAYMENT.
           IF ESCM-PAYMENT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ESCM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * A -> E WHEN POST DATE IS PAST
      * E -> R WHEN POST DATE IS PAST
      * R -> U WHEN POST DATE IS PAST
      * U -> Z WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3780-SET-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INSURANCE AMT.
      *----------------------------------------------------------------*
       3790-PRORATE-INSURANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCT-INSURANCE-AMT ROUNDED =
                   ESCM-TAX-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCT-INSURANCE-AMT
           END-IF
           ADD ESCT-INSURANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE INSURANCE FEE ON THE PAYMENT AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3800-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ESCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ESCM-INSURANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ESCM-INSURANCE-AMT ROUNDED = ESCT-PAYMENT-AMT
                       * ESCM-DISCOUNT / 200
               WHEN ESCT-TRAN-CD = 'DS'
                   MOVE 20.00 TO ESCM-INSURANCE-AMT
               WHEN ESCT-TRAN-CD = 'PY'
                   MOVE 7.00 TO ESCM-INSURANCE-AMT
               WHEN OTHER
                   COMPUTE ESCM-INSURANCE-AMT ROUNDED = ESCT-PAYMENT-AMT
                       * ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-INSURANCE-AMT > 0 AND ESCM-INSURANCE-AMT < 10.00
               MOVE 10.00 TO ESCM-INSURANCE-AMT
           END-IF
           IF ESCM-INSURANCE-AMT > 250.00
               MOVE 250.00 TO ESCM-INSURANCE-AMT
           END-IF
           ADD ESCM-INSURANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * INSURANCE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3810-FLAG-INSURANCE.
           IF ESCT-INSURANCE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCT-INSURANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE TAX AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3820-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = ESCM-TAX-AMT *
                       ESCM-DISCOUNT / 200
               WHEN ESCM-STATUS-CD = 'A'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN ESCM-STATUS-CD = 'E'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = ESCM-TAX-AMT *
                       ESCM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3830-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCM-CUSHION-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESCM-CUSHION-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ESCM-CUSHION-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESCM-CUSHION-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-CUSHION-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DS -> PY WHEN LAST PAY DATE IS PAST
      * PY -> AN WHEN LAST PAY DATE IS PAST
      * AN -> RF WHEN LAST PAY DATE IS PAST
      * RF -> SH WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3840-SET-TRAN.
           EVALUATE ESCT-TRAN-CD
               WHEN 'DS'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AN' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AN'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RF'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3850-COUNT-NOTICE.
           IF ESCM-NOTICE-CNT < 9999999
               ADD 1 TO ESCM-NOTICE-CNT
           END-IF
           IF ESCM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO ESCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3860-LIMIT-INSURANCE.
           IF ESCT-INSURANCE-AMT < 25.00
               MOVE 25.00 TO ESCT-INSURANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCT-INSURANCE-AMT > 10000.00
               MOVE 10000.00 TO ESCT-INSURANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3870-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NOTICE-DATE TO DTU-DATE-1
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
      * SELECT THE PAYMENT GRID TIER FOR THE CUSHION AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3880-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ESCT-CUSHION-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3890-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-TAX-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TAX AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3900-FLAG-TAX.
           IF ESCM-TAX-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCM-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3910-DESCRIBE-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'X'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SURPLUS YTD FEE ON THE PAYMENT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3920-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ESCM-SURPLUS-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ESCM-SURPLUS-YTD ROUNDED = ESCM-PAYMENT-AMT *
                       ESCM-DISCOUNT / 200
               WHEN ESCM-STATUS-CD = 'A'
                   MOVE 15.00 TO ESCM-SURPLUS-YTD
               WHEN ESCM-STATUS-CD = 'E'
                   MOVE 10.00 TO ESCM-SURPLUS-YTD
               WHEN OTHER
                   COMPUTE ESCM-SURPLUS-YTD ROUNDED = ESCM-PAYMENT-AMT *
                       ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-SURPLUS-YTD > 0 AND ESCM-SURPLUS-YTD < 5.00
               MOVE 5.00 TO ESCM-SURPLUS-YTD
           END-IF
           IF ESCM-SURPLUS-YTD > 150.00
               MOVE 150.00 TO ESCM-SURPLUS-YTD
           END-IF
           ADD ESCM-SURPLUS-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3930-DESCRIBE-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'E'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'R'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'U'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3940-DESCRIBE-TRAN.
           EVALUATE ESCT-TRAN-CD
               WHEN 'DS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'AN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHORTAGE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3950-FLAG-SHORTAGE.
           IF ESCM-SHORTAGE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ESCM-SHORTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TAX YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3960-FLAG-TAX.
           IF ESCM-TAX-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ESCM-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3970-LIMIT-DISBURSE.
           IF ESCM-DISBURSE-YTD < 0.00
               MOVE 0.00 TO ESCM-DISBURSE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-DISBURSE-YTD > 2500.00
               MOVE 2500.00 TO ESCM-DISBURSE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CUSHION YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3980-LIMIT-CUSHION.
           IF ESCM-CUSHION-YTD < 100.00
               MOVE 100.00 TO ESCM-CUSHION-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-CUSHION-YTD > 2500.00
               MOVE 2500.00 TO ESCM-CUSHION-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3990-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCT-TRAN-CD
                   ADD ESCM-INSURANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-INSURANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4000-COUNT-RETRY.
           IF ESCM-RETRY-CNT < 9999999
               ADD 1 TO ESCM-RETRY-CNT
           END-IF
           IF ESCM-RETRY-CNT NOT < 12
               MOVE 'Y' TO ESCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4010-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-TYPE-CD
                   ADD ESCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4020-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ESCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-INSURANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4030-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY PAYEE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4040-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-PAYEE-TYPE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURPLUS TIER TIER FOR THE SURPLUS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4050-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SURPLUS-TIER-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ESCM-SURPLUS-YTD NOT < WS-SURPLUS-TIER-LOW(WS-TX)
                   MOVE WS-SURPLUS-TIER-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4060-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ESCM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4070-COUNT-NOTICE.
           IF ESCM-NOTICE-CNT < 9999999
               ADD 1 TO ESCM-NOTICE-CNT
           END-IF
           IF ESCM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO ESCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CUSHION AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISBURSE AMT.
      *----------------------------------------------------------------*
       4080-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-DISBURSE-AMT ROUNDED =
                   ESCM-CUSHION-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-DISBURSE-AMT
           END-IF
           ADD ESCM-DISBURSE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4090-ACCRUE-DISBURSE.
           IF ESCM-DISBURSE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-DISBURSE-AMT * ESCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ESCM-SURPLUS-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4100-COUNT-SEQ.
           IF ESCT-SEQ < 9999999
               ADD 1 TO ESCT-SEQ
           END-IF
           IF ESCT-SEQ NOT < 10
               MOVE 'Y' TO ESCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE TAX YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4110-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ESCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ESCM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCT-REVERSAL-FLAG = 'Y'
                   COMPUTE ESCM-PAYMENT-YTD ROUNDED = ESCM-TAX-YTD *
                       ESCM-DISCOUNT / 200
               WHEN ESCT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO ESCM-PAYMENT-YTD
               WHEN ESCT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO ESCM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE ESCM-PAYMENT-YTD ROUNDED = ESCM-TAX-YTD *
                       ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-PAYMENT-YTD > 0 AND ESCM-PAYMENT-YTD < 2.00
               MOVE 2.00 TO ESCM-PAYMENT-YTD
           END-IF
           IF ESCM-PAYMENT-YTD > 50.00
               MOVE 50.00 TO ESCM-PAYMENT-YTD
           END-IF
           ADD ESCM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4120-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4130-COUNT-CYCLE.
           IF ESCM-CYCLE-CNT < 9999999
               ADD 1 TO ESCM-CYCLE-CNT
           END-IF
           IF ESCM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO ESCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * C -> F WHEN LAST PAY DATE IS PAST
      * F -> K WHEN LAST PAY DATE IS PAST
      * K -> L WHEN LAST PAY DATE IS PAST
      * L -> S WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4140-SET-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4150-COUNT-CYCLE.
           IF ESCM-CYCLE-CNT < 9999999
               ADD 1 TO ESCM-CYCLE-CNT
           END-IF
           IF ESCM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO ESCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AKX -> REP WHEN CLOSE DATE IS PAST
      * REP -> SMX WHEN CLOSE DATE IS PAST
      * SMX -> VJL WHEN CLOSE DATE IS PAST
      * VJL -> WKN WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4160-SET-TYPE.
           EVALUATE ESCM-TYPE-CD
               WHEN 'AKX'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'REP' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'REP'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SMX' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMX'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJL' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJL'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKN' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHORTAGE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4170-FLAG-SHORTAGE.
           IF ESCM-SHORTAGE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCM-SHORTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4180-DESCRIBE-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'E'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * A -> E WHEN EFF DATE IS PAST
      * E -> R WHEN EFF DATE IS PAST
      * R -> U WHEN EFF DATE IS PAST
      * U -> Z WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4190-SET-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * A -> E WHEN POST DATE IS PAST
      * E -> R WHEN POST DATE IS PAST
      * R -> U WHEN POST DATE IS PAST
      * U -> Z WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4200-SET-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DS -> PY WHEN LAST STMT DATE IS PAST
      * PY -> AN WHEN LAST STMT DATE IS PAST
      * AN -> RF WHEN LAST STMT DATE IS PAST
      * RF -> SH WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4210-SET-TRAN.
           EVALUATE ESCT-TRAN-CD
               WHEN 'DS'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AN' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AN'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RF'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4220-DESCRIBE-TRAN.
           EVALUATE ESCT-TRAN-CD
               WHEN 'DS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'AN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4230-COUNT-SEQ.
           IF ESCT-SEQ < 9999999
               ADD 1 TO ESCT-SEQ
           END-IF
           IF ESCT-SEQ NOT < 12
               MOVE 'Y' TO ESCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE AMT BY ANALYSIS CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4240-ACCUM-ANALYSIS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-ANALYSIS-CD
                   ADD ESCM-INSURANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-ANALYSIS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-INSURANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       4250-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCT-PAYMENT-AMT ROUNDED =
                   ESCM-DISBURSE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCT-PAYMENT-AMT
           END-IF
           ADD ESCT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4260-ACCRUE-DISBURSE.
           IF ESCM-DISBURSE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-DISBURSE-AMT * ESCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-SURPLUS-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4270-FLAG-SHORTAGE.
           IF ESCM-SHORTAGE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ESCM-SHORTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4280-DESCRIBE-REGION.
           EVALUATE ESCM-REGION-CD
               WHEN 'B'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'G'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Y'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CUSHION YTD FEE ON THE SHORTAGE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4290-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ESCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ESCM-CUSHION-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ESCM-CUSHION-YTD ROUNDED = ESCM-SHORTAGE-YTD
                       * ESCM-DISCOUNT / 200
               WHEN ESCM-ANALYSIS-CD = 'GD'
                   MOVE 10.00 TO ESCM-CUSHION-YTD
               WHEN ESCM-ANALYSIS-CD = 'LY'
                   MOVE 10.00 TO ESCM-CUSHION-YTD
               WHEN OTHER
                   COMPUTE ESCM-CUSHION-YTD ROUNDED = ESCM-SHORTAGE-YTD
                       * ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-CUSHION-YTD > 0 AND ESCM-CUSHION-YTD < 5.00
               MOVE 5.00 TO ESCM-CUSHION-YTD
           END-IF
           IF ESCM-CUSHION-YTD > 100.00
               MOVE 100.00 TO ESCM-CUSHION-YTD
           END-IF
           ADD ESCM-CUSHION-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4300-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCM-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ESCM-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ESCM-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4310-SPLIT-SHORTAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE ESCT-SHORTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4320-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = ESCM-DISBURSE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ESCM-DISBURSE-YTD - (WS-WORK-AMT * 2)
           COMPUTE ESCM-SHORTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4330-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ESCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4340-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-TYPE-CD
                   ADD ESCM-DISBURSE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-DISBURSE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4350-DESCRIBE-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'K'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'L'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'S'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SURPLUS YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4360-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-REASON-CD
                   ADD ESCM-SURPLUS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-SURPLUS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E652 CONTACT REQUIRED
      * E953 WORK AMT 2 NEGATIVE
      * E666 WORK AMT 2 OVER 250000
      * E920 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4370-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CONTACT = SPACES
                   MOVE 'E652' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E953' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E666' TO WS-REJECT-CODE
               WHEN ESCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E920' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4380-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ESCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ESCM-SURPLUS-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4390-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ESCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4400-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ESCM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ESCM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE ESC FIELDS TO ESRISK AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-ESRISK.
           INITIALIZE ESRISK-PARMS
           MOVE ESCM-DISBURSE-YTD
             TO ESRISK-DISBURSE-YTD
           MOVE ESCM-TAX-YTD
             TO ESRISK-TAX-YTD
           MOVE ESCM-SHORTAGE-AMT
             TO ESRISK-SHORTAGE-AMT
           MOVE ESCM-SHORTAGE-YTD
             TO ESRISK-SHORTAGE-YTD
           MOVE ESCM-INSURANCE-AMT
             TO ESRISK-INSURANCE-AMT
           MOVE ESCM-INSURANCE-YTD
             TO ESRISK-INSURANCE-YTD
           MOVE ESCM-LAST-PAY-DATE
             TO ESRISK-LAST-PAY-DATE
           MOVE ESCM-NEXT-REVIEW-DATE
             TO ESRISK-NEXT-REVIEW-DATE
           MOVE ESCM-NOTICE-DATE
             TO ESRISK-NOTICE-DATE
           MOVE ESCM-LAST-ACT-DATE
             TO ESRISK-LAST-ACT-DATE
           MOVE ESCM-REASON-CD
             TO ESRISK-REASON-CD
           MOVE ESCM-REGION-CD
             TO ESRISK-REGION-CD
           MOVE ESCM-TYPE-CD
             TO ESRISK-TYPE-CD
           MOVE ESCM-STATUS-CD
             TO ESRISK-STATUS-CD
           MOVE ESCM-HOLD-FLAG
             TO ESRISK-HOLD-FLAG
           MOVE ESCM-STOP-FLAG
             TO ESRISK-STOP-FLAG
           MOVE ESCM-PREFERRED-FLAG
             TO ESRISK-PREFERRED-FLAG
           MOVE ESCM-CYCLE-CNT
             TO ESRISK-CYCLE-CNT
           MOVE ESCM-EXCEPT-CNT
             TO ESRISK-EXCEPT-CNT
           MOVE ESCM-RATE
             TO ESRISK-RATE
           MOVE ESCM-FEE-PCT
             TO ESRISK-FEE-PCT
           MOVE ESCM-NAME
             TO ESRISK-NAME
           MOVE ESCM-DESC
             TO ESRISK-DESC
           CALL 'ESRISK' USING ESRISK-PARMS
           EVALUATE ESRISK-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ESRISK-DISBURSE-YTD
                     TO ESCM-DISBURSE-YTD
                   MOVE ESRISK-SHORTAGE-AMT
                     TO ESCM-SHORTAGE-AMT
                   MOVE ESRISK-SHORTAGE-YTD
                     TO ESCM-SHORTAGE-YTD
                   MOVE ESRISK-INSURANCE-YTD
                     TO ESCM-INSURANCE-YTD
                   MOVE ESRISK-STOP-FLAG
                     TO ESCM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-ESRISK' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE ESC FIELDS TO ESFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-ESFEE.
           INITIALIZE ESFEE-PARMS
           MOVE ESCM-INSURANCE-AMT
             TO ESFEE-INSURANCE-AMT
           MOVE ESCM-TAX-AMT
             TO ESFEE-TAX-AMT
           MOVE ESCM-DISBURSE-AMT
             TO ESFEE-DISBURSE-AMT
           MOVE ESCM-SURPLUS-YTD
             TO ESFEE-SURPLUS-YTD
           MOVE ESCM-CUSHION-YTD
             TO ESFEE-CUSHION-YTD
           MOVE ESCM-TAX-YTD
             TO ESFEE-TAX-YTD
           MOVE ESCM-LAST-STMT-DATE
             TO ESFEE-LAST-STMT-DATE
           MOVE ESCM-CLOSE-DATE
             TO ESFEE-CLOSE-DATE
           MOVE ESCM-EFFECTIVE-DATE
             TO ESFEE-EFFECTIVE-DATE
           MOVE ESCM-NEXT-REVIEW-DATE
             TO ESFEE-NEXT-REVIEW-DATE
           MOVE ESCM-REGION-CD
             TO ESFEE-REGION-CD
           MOVE ESCM-ANALYSIS-CD
             TO ESFEE-ANALYSIS-CD
           MOVE ESCM-REASON-CD
             TO ESFEE-REASON-CD
           MOVE ESCM-TYPE-CD
             TO ESFEE-TYPE-CD
           MOVE ESCM-PREFERRED-FLAG
             TO ESFEE-PREFERRED-FLAG
           MOVE ESCM-OVERRIDE-FLAG
             TO ESFEE-OVERRIDE-FLAG
           MOVE ESCM-SUSPEND-FLAG
             TO ESFEE-SUSPEND-FLAG
           MOVE ESCM-NOTICE-CNT
             TO ESFEE-NOTICE-CNT
           MOVE ESCM-EXCEPT-CNT
             TO ESFEE-EXCEPT-CNT
           MOVE ESCM-FEE-PCT
             TO ESFEE-FEE-PCT
           MOVE ESCM-DISCOUNT
             TO ESFEE-DISCOUNT
           MOVE ESCM-ADDR
             TO ESFEE-ADDR
           MOVE ESCM-CONTACT
             TO ESFEE-CONTACT
           CALL 'ESFEE' USING ESFEE-PARMS
           EVALUATE ESFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ESFEE-TAX-AMT
                     TO ESCM-TAX-AMT
                   MOVE ESFEE-DISBURSE-AMT
                     TO ESCM-DISBURSE-AMT
                   MOVE ESFEE-SURPLUS-YTD
                     TO ESCM-SURPLUS-YTD
                   MOVE ESFEE-TAX-YTD
                     TO ESCM-TAX-YTD
                   MOVE ESFEE-OVERRIDE-FLAG
                     TO ESCM-OVERRIDE-FLAG
                   MOVE ESFEE-SUSPEND-FLAG
                     TO ESCM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-ESFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE ESCT-KEY TO RD-KEY
           MOVE ESCT-TRAN-CD TO RD-CODE
           MOVE ESCT-CUSHION-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE ESCT-KEY TO RD-KEY
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
           DISPLAY 'ESCCALC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ESCCALC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
