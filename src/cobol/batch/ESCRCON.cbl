      *================================================================*
      * PROGRAM  : ESCRCON                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : MORTGAGE ESCROW - RECONCILIATION. READS THE ESC     *
      *            TRANSACTIONS, UPDATES THE ESC MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. ESCROW ANALYSIS, TAX AND        *
      *            INSURANCE DISBURSEMENT AND SHORTAGE                 *
      * INPUT    : ESCMST (KSDS), ESCTRN, BANKCTL                      *
      * OUTPUT   : ESCOUT, ESCRCRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ESLIM                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-01-03 GAF  ORIGINAL                                       *
      * 2005-08-27 JLM  DISBURSE TABLE EXPANDED CHG9688                *
      * 2007-06-19 CRP  SHORTAGE RERUN PROTECTION PRB3665              *
      * 2011-09-25 DPS  TAX TABLE EXPANDED CHG4657                     *
      * 2013-04-28 PQR  SHORTAGE FEE SCHEDULE UPDATE PRB7221           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESCRCON.
       AUTHOR. D P SHAW.

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
           SELECT REPORT-FILE ASSIGN TO "ESCRCRPT"
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
                                           VALUE 23.000000.
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
      * WS-PAYMENT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000337000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000237000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000152500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013500000000700500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014500000000686000'.
       01  WS-PAYMENT-TIER-TBL REDEFINES WS-PAYMENT-TIER-VALUES.
           05  WS-PAYMENT-TIER-ROW OCCURS 5 TIMES.
               10  WS-PAYMENT-TIER-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYMENT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000151500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000765500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000511500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004000000000721500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004100000000030000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006600000000858000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006700000000652500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009200000000720500'.
       01  WS-PAYMENT-BAND-TBL REDEFINES WS-PAYMENT-BAND-VALUES.
           05  WS-PAYMENT-BAND-ROW OCCURS 8 TIMES.
               10  WS-PAYMENT-BAND-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SURPLUS-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SURPLUS-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000536000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000290000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000181500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000335000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015550000000546000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015650000000266000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016150000000061000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017150000000646500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017200000000340500'.
       01  WS-SURPLUS-SCHED-TBL REDEFINES WS-SURPLUS-SCHED-VALUES.
           05  WS-SURPLUS-SCHED-ROW OCCURS 9 TIMES.
               10  WS-SURPLUS-SCHED-LOW    PIC 9(09)V99.
               10  WS-SURPLUS-SCHED-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ESCRCON  MORTGAGE ESCROW'.
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
       COPY ESLIMP.

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
               PERFORM 3000-PRORATE-CUSHION
               PERFORM 3010-EDIT-DESC
               PERFORM 3020-AGE-NEXT-DUE
               PERFORM 3030-DESCRIBE-ANALYSIS
               PERFORM 3040-DESCRIBE-PAYEE-TYPE
               PERFORM 3050-ACCRUE-INSURANCE
               PERFORM 3060-ASSESS-OVERRIDE
               PERFORM 3070-LIMIT-CUSHION
               PERFORM 3080-LIMIT-TAX
               PERFORM 3090-LIMIT-SURPLUS
               PERFORM 3100-EDIT-CUST-ID
               PERFORM 3110-AGE-NEXT-REVIEW
               PERFORM 3120-SELECT-RATE
               PERFORM 3130-SET-REASON
               PERFORM 3140-ACCRUE-DISCOUNT
               PERFORM 3150-SPLIT-CUSHION
               PERFORM 3160-SPLIT-DISBURSE
               PERFORM 3170-LIMIT-CUSHION
               PERFORM 3180-SELECT-DISCOUNT
               PERFORM 3190-COUNT-SEQ
               PERFORM 3200-CHECK-EXPIRY
               PERFORM 3210-PRORATE-TAX
               PERFORM 3220-SPLIT-TAX
               PERFORM 3230-FLAG-SURPLUS
               PERFORM 3240-ACCUM-TRAN
               PERFORM 3250-PRORATE-TAX
               PERFORM 3260-LIMIT-INSURANCE
               PERFORM 3270-SPLIT-TAX
               PERFORM 3280-ACCUM-REGION
               PERFORM 3290-ACCUM-PAYEE-TYPE
               PERFORM 3300-DESCRIBE-STATUS
               PERFORM 3310-EDIT-MEMO
               PERFORM 3320-ACCRUE-INSURANCE
               PERFORM 3330-SET-TYPE
               PERFORM 3340-CHECK-POST
               PERFORM 3350-COUNT-EXCEPT
               PERFORM 3360-EDIT-ADDR
               PERFORM 3370-SPLIT-SHORTAGE
               PERFORM 3380-SPLIT-INSURANCE
               PERFORM 3390-SET-ANALYSIS
               PERFORM 3400-ACCUM-TYPE
               PERFORM 3410-ACCRUE-TAX
               PERFORM 3420-COUNT-RETRY
               PERFORM 3430-SPLIT-DISBURSE
               PERFORM 3440-SELECT-RATE
               PERFORM 3450-AGE-NOTICE
               PERFORM 3460-CHECK-NEXT-REVIEW
               PERFORM 3470-ACCRUE-PAYMENT
               PERFORM 3480-ACCRUE-SURPLUS
               PERFORM 3490-EDIT-MEMO
               PERFORM 3500-SET-REASON
               PERFORM 3510-EDIT-CUST-ID
               PERFORM 3520-FLAG-SHORTAGE
               PERFORM 3530-COUNT-NOTICE
               PERFORM 3540-ACCRUE-INSURANCE
               PERFORM 3550-ACCUM-TYPE
               PERFORM 3560-ASSESS-OVERRIDE
               PERFORM 3570-ACCRUE-DISBURSE
               PERFORM 3580-SPLIT-INSURANCE
               PERFORM 3590-SELECT-FEE-PCT
               PERFORM 3600-LIMIT-PAYMENT
               PERFORM 3610-CHECK-NEXT-DUE
               PERFORM 3620-AGE-POST
               PERFORM 3630-SPLIT-SHORTAGE
               PERFORM 3640-SELECT-RATE
               PERFORM 3650-EDIT-NAME
               PERFORM 3660-PRORATE-TAX
               PERFORM 3670-DESCRIBE-TYPE
               PERFORM 3680-ACCUM-REGION
               PERFORM 3690-DESCRIBE-REGION
               PERFORM 3700-ACCUM-TYPE
               PERFORM 3710-LIMIT-SHORTAGE
               PERFORM 3720-LIMIT-PAYMENT
               PERFORM 3730-DESCRIBE-REASON
               PERFORM 3740-PRORATE-PAYMENT
               PERFORM 3750-FLAG-SHORTAGE
               PERFORM 3760-AGE-LAST-STMT
               PERFORM 3770-SELECT-RATE
               PERFORM 3780-SPLIT-CUSHION
               PERFORM 3790-AGE-CLOSE
               PERFORM 3800-LIMIT-TAX
               PERFORM 3810-ACCRUE-TAX
               PERFORM 3820-PRORATE-TAX
               PERFORM 3830-FLAG-ITEM
               PERFORM 3840-DESCRIBE-STATUS
               PERFORM 3850-PRORATE-INSURANCE
               PERFORM 3860-LIMIT-CUSHION
               PERFORM 3870-ASSESS-STOP
               PERFORM 3880-ASSESS-STOP
               PERFORM 3890-PRORATE-SURPLUS
               PERFORM 3900-CHECK-NEXT-DUE
               PERFORM 3910-LIMIT-INSURANCE
               PERFORM 3920-ASSESS-REVERSAL
               PERFORM 3930-PRORATE-SURPLUS
               PERFORM 3940-ASSESS-PREFERRED
               PERFORM 3950-CHECK-LAST-ACT
               PERFORM 3960-SPLIT-INSURANCE
               PERFORM 3970-CHECK-LAST-ACT
               PERFORM 3980-AGE-NOTICE
               PERFORM 3990-ACCRUE-CUSHION
               PERFORM 4000-LIMIT-SURPLUS
               PERFORM 4010-SPLIT-SURPLUS
               PERFORM 4020-AGE-LAST-PAY
               PERFORM 4030-COUNT-NOTICE
               PERFORM 4040-DESCRIBE-PAYEE-TYPE
               PERFORM 4050-ASSESS-HOLD
               PERFORM 4060-PRORATE-TAX
               PERFORM 4070-DESCRIBE-REASON
               PERFORM 4080-CHECK-NEXT-DUE
               PERFORM 4090-ACCRUE-RATE
               PERFORM 4100-SELECT-FEE-PCT
               PERFORM 4110-ASSESS-STOP
               PERFORM 4120-ACCRUE-INSURANCE
               PERFORM 4130-EDIT-CONTACT
               PERFORM 4140-SPLIT-TAX
               PERFORM 4150-CHECK-NEXT-REVIEW
               PERFORM 4160-PRORATE-INSURANCE
               PERFORM 4170-CHECK-EFFECTIVE
               PERFORM 4180-SPLIT-INSURANCE
               PERFORM 4190-EDIT-MEMO
               PERFORM 4200-DESCRIBE-REGION
               PERFORM 4210-PRORATE-DISBURSE
               PERFORM 4220-SET-TYPE
               PERFORM 7010-CALL-ESLIM
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
      * PRORATE INSURANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CUSHION YTD.
      *----------------------------------------------------------------*
       3000-PRORATE-CUSHION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-CUSHION-YTD ROUNDED =
                   ESCT-INSURANCE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-CUSHION-YTD
           END-IF
           ADD ESCM-CUSHION-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E800 DESC REQUIRED
      * E154 WORK AMT 2 NEGATIVE
      * E644 WORK AMT 2 OVER 1000000
      * E795 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3010-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-DESC = SPACES
                   MOVE 'E800' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E154' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E644' TO WS-REJECT-CODE
               WHEN ESCM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E795' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3020-AGE-NEXT-DUE.
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
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ANALYSIS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3030-DESCRIBE-ANALYSIS.
           EVALUATE ESCM-ANALYSIS-CD
               WHEN 'GD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3040-DESCRIBE-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INSURANCE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3050-ACCRUE-INSURANCE.
           IF ESCT-INSURANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCT-INSURANCE-AMT * ESCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ESCT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISBURSE FEE ON THE SHORTAGE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3060-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ESCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ESCM-DISBURSE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-HOLD-FLAG = 'Y'
                   COMPUTE ESCM-DISBURSE-AMT ROUNDED = ESCM-SHORTAGE-YTD
                       * ESCM-FEE-PCT / 200
               WHEN ESCM-PAYEE-TYPE-CD = 'AM'
                   MOVE 3.00 TO ESCM-DISBURSE-AMT
               WHEN ESCM-PAYEE-TYPE-CD = 'UE'
                   MOVE 10.00 TO ESCM-DISBURSE-AMT
               WHEN OTHER
                   COMPUTE ESCM-DISBURSE-AMT ROUNDED = ESCM-SHORTAGE-YTD
                       * ESCM-FEE-PCT / 100
           END-EVALUATE
           IF ESCM-DISBURSE-AMT > 0 AND ESCM-DISBURSE-AMT < 5.00
               MOVE 5.00 TO ESCM-DISBURSE-AMT
           END-IF
           IF ESCM-DISBURSE-AMT > 25.00
               MOVE 25.00 TO ESCM-DISBURSE-AMT
           END-IF
           ADD ESCM-DISBURSE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CUSHION AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3070-LIMIT-CUSHION.
           IF ESCT-CUSHION-AMT < 25.00
               MOVE 25.00 TO ESCT-CUSHION-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCT-CUSHION-AMT > 10000.00
               MOVE 10000.00 TO ESCT-CUSHION-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAX AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3080-LIMIT-TAX.
           IF ESCM-TAX-AMT < 0.00
               MOVE 0.00 TO ESCM-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-TAX-AMT > 2500.00
               MOVE 2500.00 TO ESCM-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SURPLUS AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3090-LIMIT-SURPLUS.
           IF ESCM-SURPLUS-AMT < 10.00
               MOVE 10.00 TO ESCM-SURPLUS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-SURPLUS-AMT > 10000.00
               MOVE 10000.00 TO ESCM-SURPLUS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E513 CUST ID REQUIRED
      * E960 PAYMENT AMT NEGATIVE
      * E553 PAYMENT AMT OVER 250000
      * E726 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3100-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CUST-ID = SPACES
                   MOVE 'E513' TO WS-REJECT-CODE
               WHEN ESCM-PAYMENT-AMT < 0
                   MOVE 'E960' TO WS-REJECT-CODE
               WHEN ESCM-PAYMENT-AMT > 250000.00
                   MOVE 'E553' TO WS-REJECT-CODE
               WHEN ESCM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E726' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3110-AGE-NEXT-REVIEW.
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
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE INSURANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ESCM-INSURANCE-AMT NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * C -> F WHEN LAST STMT DATE IS PAST
      * F -> K WHEN LAST STMT DATE IS PAST
      * K -> L WHEN LAST STMT DATE IS PAST
      * L -> S WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3130-SET-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3140-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ESCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-INSURANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CUSHION AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3150-SPLIT-CUSHION.
           COMPUTE WS-WORK-AMT = ESCM-CUSHION-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ESCM-CUSHION-AMT - (WS-WORK-AMT * 3)
           COMPUTE ESCM-INSURANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3160-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = ESCM-DISBURSE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ESCM-DISBURSE-AMT - (WS-WORK-AMT * 3)
           COMPUTE ESCM-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CUSHION AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3170-LIMIT-CUSHION.
           IF ESCM-CUSHION-AMT < 10.00
               MOVE 10.00 TO ESCM-CUSHION-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-CUSHION-AMT > 2500.00
               MOVE 2500.00 TO ESCM-CUSHION-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE INSURANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3180-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO ESCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ESCT-INSURANCE-AMT NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO ESCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3190-COUNT-SEQ.
           IF ESCT-SEQ < 9999999
               ADD 1 TO ESCT-SEQ
           END-IF
           IF ESCT-SEQ NOT < 10
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3200-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ESCM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ESCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INSURANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TAX AMT.
      *----------------------------------------------------------------*
       3210-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-TAX-AMT ROUNDED =
                   ESCM-INSURANCE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-TAX-AMT
           END-IF
           ADD ESCM-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT TAX AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3220-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = ESCM-TAX-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ESCM-TAX-AMT - (WS-WORK-AMT * 3)
           COMPUTE ESCM-SURPLUS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SURPLUS YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3230-FLAG-SURPLUS.
           IF ESCM-SURPLUS-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCM-SURPLUS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3240-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCT-TRAN-CD
                   ADD ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TAX AMT.
      *----------------------------------------------------------------*
       3250-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-TAX-AMT ROUNDED =
                   ESCM-TAX-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-TAX-AMT
           END-IF
           ADD ESCM-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * INSURANCE AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3260-LIMIT-INSURANCE.
           IF ESCM-INSURANCE-AMT < 50.00
               MOVE 50.00 TO ESCM-INSURANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-INSURANCE-AMT > 500.00
               MOVE 500.00 TO ESCM-INSURANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAX AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3270-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = ESCM-TAX-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ESCM-TAX-AMT - (WS-WORK-AMT * 4)
           COMPUTE ESCM-SHORTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CUSHION YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3280-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-REGION-CD
                   ADD ESCM-CUSHION-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-CUSHION-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY PAYEE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3290-ACCUM-PAYEE-TYPE.
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
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3300-DESCRIBE-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'E'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'R'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'U'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'Z'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E885 MEMO REQUIRED
      * E661 PAYMENT AMT NEGATIVE
      * E328 PAYMENT AMT OVER 10000
      * E662 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3310-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCT-MEMO = SPACES
                   MOVE 'E885' TO WS-REJECT-CODE
               WHEN ESCM-PAYMENT-AMT < 0
                   MOVE 'E661' TO WS-REJECT-CODE
               WHEN ESCM-PAYMENT-AMT > 10000.00
                   MOVE 'E328' TO WS-REJECT-CODE
               WHEN ESCM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E662' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INSURANCE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3320-ACCRUE-INSURANCE.
           IF ESCM-INSURANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-INSURANCE-AMT * ESCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ESCM-SHORTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AKX -> REP WHEN LAST STMT DATE IS PAST
      * REP -> SMX WHEN LAST STMT DATE IS PAST
      * SMX -> VJL WHEN LAST STMT DATE IS PAST
      * VJL -> WKN WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3330-SET-TYPE.
           EVALUATE ESCM-TYPE-CD
               WHEN 'AKX'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'REP' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'REP'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SMX' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMX'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJL' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJL'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKN' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF ESCT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ESCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3350-COUNT-EXCEPT.
           IF ESCM-EXCEPT-CNT < 9999999
               ADD 1 TO ESCM-EXCEPT-CNT
           END-IF
           IF ESCM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO ESCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E677 ADDR REQUIRED
      * E136 DISBURSE AMT NEGATIVE
      * E916 DISBURSE AMT OVER 25000
      * E928 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-ADDR = SPACES
                   MOVE 'E677' TO WS-REJECT-CODE
               WHEN ESCM-DISBURSE-AMT < 0
                   MOVE 'E136' TO WS-REJECT-CODE
               WHEN ESCM-DISBURSE-AMT > 25000.00
                   MOVE 'E916' TO WS-REJECT-CODE
               WHEN ESCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E928' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHORTAGE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3370-SPLIT-SHORTAGE.
           COMPUTE WS-WORK-AMT = ESCM-SHORTAGE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ESCM-SHORTAGE-YTD - (WS-WORK-AMT * 4)
           COMPUTE ESCT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INSURANCE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3380-SPLIT-INSURANCE.
           COMPUTE WS-WORK-AMT = ESCM-INSURANCE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ESCM-INSURANCE-YTD - (WS-WORK-AMT *
               3)
           COMPUTE ESCM-TAX-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ANALYSIS CD:
      * GD -> LY WHEN EFF DATE IS PAST
      * LY -> XJ WHEN EFF DATE IS PAST
      * XJ -> GD WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3390-SET-ANALYSIS.
           EVALUATE ESCM-ANALYSIS-CD
               WHEN 'GD'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LY' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LY'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XJ'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GD' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3400-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-TYPE-CD
                   ADD ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3410-ACCRUE-TAX.
           IF ESCM-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-TAX-YTD * ESCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ESCM-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3420-COUNT-RETRY.
           IF ESCM-RETRY-CNT < 9999999
               ADD 1 TO ESCM-RETRY-CNT
           END-IF
           IF ESCM-RETRY-CNT NOT < 10
               MOVE 'Y' TO ESCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3430-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = ESCM-DISBURSE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ESCM-DISBURSE-AMT - (WS-WORK-AMT * 4)
           COMPUTE ESCT-SHORTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE SHORTAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3440-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ESCT-SHORTAGE-AMT NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3450-AGE-NOTICE.
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
                   ADD ESCM-INSURANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-INSURANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-INSURANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ESCM-INSURANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-INSURANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3460-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO ESCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3470-ACCRUE-PAYMENT.
           IF ESCT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCT-PAYMENT-AMT * ESCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-DISBURSE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURPLUS AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3480-ACCRUE-SURPLUS.
           IF ESCM-SURPLUS-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-SURPLUS-AMT * ESCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ESCM-SURPLUS-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E680 MEMO REQUIRED
      * E962 CUSHION YTD NEGATIVE
      * E920 CUSHION YTD OVER 99999
      * E843 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3490-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCT-MEMO = SPACES
                   MOVE 'E680' TO WS-REJECT-CODE
               WHEN ESCM-CUSHION-YTD < 0
                   MOVE 'E962' TO WS-REJECT-CODE
               WHEN ESCM-CUSHION-YTD > 99999.00
                   MOVE 'E920' TO WS-REJECT-CODE
               WHEN ESCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E843' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * C -> F WHEN NEXT DUE DATE IS PAST
      * F -> K WHEN NEXT DUE DATE IS PAST
      * K -> L WHEN NEXT DUE DATE IS PAST
      * L -> S WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3500-SET-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E100 CUST ID REQUIRED
      * E293 CUSHION AMT NEGATIVE
      * E574 CUSHION AMT OVER 250000
      * E395 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3510-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CUST-ID = SPACES
                   MOVE 'E100' TO WS-REJECT-CODE
               WHEN ESCT-CUSHION-AMT < 0
                   MOVE 'E293' TO WS-REJECT-CODE
               WHEN ESCT-CUSHION-AMT > 250000.00
                   MOVE 'E574' TO WS-REJECT-CODE
               WHEN ESCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E395' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3520-FLAG-SHORTAGE.
           IF ESCM-SHORTAGE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCM-SHORTAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3530-COUNT-NOTICE.
           IF ESCM-NOTICE-CNT < 9999999
               ADD 1 TO ESCM-NOTICE-CNT
           END-IF
           IF ESCM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO ESCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INSURANCE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3540-ACCRUE-INSURANCE.
           IF ESCM-INSURANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-INSURANCE-YTD * ESCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ESCT-SHORTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHORTAGE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3550-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-TYPE-CD
                   ADD ESCM-SHORTAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-SHORTAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INSURANCE FEE ON THE SHORTAGE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3560-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ESCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ESCT-INSURANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-SUSPEND-FLAG = 'Y'
                   COMPUTE ESCT-INSURANCE-AMT ROUNDED =
                       ESCM-SHORTAGE-AMT * ESCM-FEE-PCT / 200
               WHEN ESCM-PAYEE-TYPE-CD = 'AM'
                   MOVE 20.00 TO ESCT-INSURANCE-AMT
               WHEN ESCM-PAYEE-TYPE-CD = 'UE'
                   MOVE 20.00 TO ESCT-INSURANCE-AMT
               WHEN OTHER
                   COMPUTE ESCT-INSURANCE-AMT ROUNDED =
                       ESCM-SHORTAGE-AMT * ESCM-FEE-PCT / 100
           END-EVALUATE
           IF ESCT-INSURANCE-AMT > 0 AND ESCT-INSURANCE-AMT < 10.00
               MOVE 10.00 TO ESCT-INSURANCE-AMT
           END-IF
           IF ESCT-INSURANCE-AMT > 75.00
               MOVE 75.00 TO ESCT-INSURANCE-AMT
           END-IF
           ADD ESCT-INSURANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3570-ACCRUE-DISBURSE.
           IF ESCM-DISBURSE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-DISBURSE-AMT * ESCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ESCM-SHORTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INSURANCE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3580-SPLIT-INSURANCE.
           COMPUTE WS-WORK-AMT = ESCM-INSURANCE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ESCM-INSURANCE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE ESCM-SHORTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE CUSHION AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3590-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ESCM-CUSHION-AMT NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3600-LIMIT-PAYMENT.
           IF ESCM-PAYMENT-AMT < 25.00
               MOVE 25.00 TO ESCM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-PAYMENT-AMT > 5000.00
               MOVE 5000.00 TO ESCM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3610-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO ESCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3620-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3630-SPLIT-SHORTAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE ESCM-SHORTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURPLUS SCHED TIER FOR THE SHORTAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3640-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SURPLUS-SCHED-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ESCM-SHORTAGE-AMT NOT < WS-SURPLUS-SCHED-LOW(WS-TX)
                   MOVE WS-SURPLUS-SCHED-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E053 NAME REQUIRED
      * E889 DISBURSE YTD NEGATIVE
      * E092 DISBURSE YTD OVER 99999
      * E235 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3650-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-NAME = SPACES
                   MOVE 'E053' TO WS-REJECT-CODE
               WHEN ESCM-DISBURSE-YTD < 0
                   MOVE 'E889' TO WS-REJECT-CODE
               WHEN ESCM-DISBURSE-YTD > 99999.00
                   MOVE 'E092' TO WS-REJECT-CODE
               WHEN ESCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E235' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CUSHION YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TAX AMT.
      *----------------------------------------------------------------*
       3660-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-TAX-AMT ROUNDED =
                   ESCM-CUSHION-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-TAX-AMT
           END-IF
           ADD ESCM-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3670-DESCRIBE-TYPE.
           EVALUATE ESCM-TYPE-CD
               WHEN 'AKX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'REP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SMX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VJL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WKN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XKU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XTF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3680-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-REGION-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3690-DESCRIBE-REGION.
           EVALUATE ESCM-REGION-CD
               WHEN 'B'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'G'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SHORTAGE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3700-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-TYPE-CD
                   ADD ESCM-SHORTAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-SHORTAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3710-LIMIT-SHORTAGE.
           IF ESCT-SHORTAGE-AMT < 0.00
               MOVE 0.00 TO ESCT-SHORTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCT-SHORTAGE-AMT > 2500.00
               MOVE 2500.00 TO ESCT-SHORTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3720-LIMIT-PAYMENT.
           IF ESCT-PAYMENT-AMT < 10.00
               MOVE 10.00 TO ESCT-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCT-PAYMENT-AMT > 10000.00
               MOVE 10000.00 TO ESCT-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'F'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'K'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'X'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3740-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-PAYMENT-AMT ROUNDED =
                   ESCM-PAYMENT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-PAYMENT-AMT
           END-IF
           ADD ESCM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SHORTAGE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3750-FLAG-SHORTAGE.
           IF ESCM-SHORTAGE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCM-SHORTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3760-AGE-LAST-STMT.
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
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT TIER TIER FOR THE SHORTAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3770-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-TIER-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ESCM-SHORTAGE-YTD NOT < WS-PAYMENT-TIER-LOW(WS-TX)
                   MOVE WS-PAYMENT-TIER-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CUSHION YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3780-SPLIT-CUSHION.
           COMPUTE WS-WORK-AMT = ESCM-CUSHION-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ESCM-CUSHION-YTD - (WS-WORK-AMT * 3)
           COMPUTE ESCM-SHORTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3790-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-CLOSE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TAX AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3800-LIMIT-TAX.
           IF ESCM-TAX-AMT < 10.00
               MOVE 10.00 TO ESCM-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-TAX-AMT > 50000.00
               MOVE 50000.00 TO ESCM-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3810-ACCRUE-TAX.
           IF ESCM-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-TAX-YTD * ESCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ESCM-SHORTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHORTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TAX YTD.
      *----------------------------------------------------------------*
       3820-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-TAX-YTD ROUNDED =
                   ESCT-SHORTAGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-TAX-YTD
           END-IF
           ADD ESCM-TAX-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3830-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3840-DESCRIBE-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'E'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'R'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Z'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INSURANCE AMT.
      *----------------------------------------------------------------*
       3850-PRORATE-INSURANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-INSURANCE-AMT ROUNDED =
                   ESCM-DISBURSE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-INSURANCE-AMT
           END-IF
           ADD ESCM-INSURANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CUSHION YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3860-LIMIT-CUSHION.
           IF ESCM-CUSHION-YTD < 50.00
               MOVE 50.00 TO ESCM-CUSHION-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-CUSHION-YTD > 50000.00
               MOVE 50000.00 TO ESCM-CUSHION-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE TAX YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3870-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ESCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = ESCM-TAX-YTD *
                       ESCM-DISCOUNT / 200
               WHEN ESCT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN ESCT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = ESCM-TAX-YTD *
                       ESCM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE INSURANCE FEE ON THE SURPLUS YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3880-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ESCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ESCM-INSURANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCT-REVERSAL-FLAG = 'Y'
                   COMPUTE ESCM-INSURANCE-AMT ROUNDED = ESCM-SURPLUS-YTD
                       * ESCM-DISCOUNT / 200
               WHEN ESCM-ANALYSIS-CD = 'GD'
                   MOVE 15.00 TO ESCM-INSURANCE-AMT
               WHEN ESCM-ANALYSIS-CD = 'LY'
                   MOVE 5.00 TO ESCM-INSURANCE-AMT
               WHEN OTHER
                   COMPUTE ESCM-INSURANCE-AMT ROUNDED = ESCM-SURPLUS-YTD
                       * ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-INSURANCE-AMT > 0 AND ESCM-INSURANCE-AMT < 1.00
               MOVE 1.00 TO ESCM-INSURANCE-AMT
           END-IF
           IF ESCM-INSURANCE-AMT > 100.00
               MOVE 100.00 TO ESCM-INSURANCE-AMT
           END-IF
           ADD ESCM-INSURANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE INSURANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SURPLUS YTD.
      *----------------------------------------------------------------*
       3890-PRORATE-SURPLUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-SURPLUS-YTD ROUNDED =
                   ESCM-INSURANCE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-SURPLUS-YTD
           END-IF
           ADD ESCM-SURPLUS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3900-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3910-LIMIT-INSURANCE.
           IF ESCM-INSURANCE-YTD < 10.00
               MOVE 10.00 TO ESCM-INSURANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-INSURANCE-YTD > 5000.00
               MOVE 5000.00 TO ESCM-INSURANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAX FEE ON THE SHORTAGE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3920-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ESCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ESCM-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-SUSPEND-FLAG = 'Y'
                   COMPUTE ESCM-TAX-AMT ROUNDED = ESCM-SHORTAGE-YTD *
                       ESCM-FEE-PCT / 200
               WHEN ESCM-ANALYSIS-CD = 'GD'
                   MOVE 5.00 TO ESCM-TAX-AMT
               WHEN ESCM-ANALYSIS-CD = 'LY'
                   MOVE 10.00 TO ESCM-TAX-AMT
               WHEN OTHER
                   COMPUTE ESCM-TAX-AMT ROUNDED = ESCM-SHORTAGE-YTD *
                       ESCM-FEE-PCT / 100
           END-EVALUATE
           IF ESCM-TAX-AMT > 0 AND ESCM-TAX-AMT < 5.00
               MOVE 5.00 TO ESCM-TAX-AMT
           END-IF
           IF ESCM-TAX-AMT > 150.00
               MOVE 150.00 TO ESCM-TAX-AMT
           END-IF
           ADD ESCM-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE INSURANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SURPLUS YTD.
      *----------------------------------------------------------------*
       3930-PRORATE-SURPLUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-SURPLUS-YTD ROUNDED =
                   ESCM-INSURANCE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-SURPLUS-YTD
           END-IF
           ADD ESCM-SURPLUS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE PAYMENT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3940-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ESCT-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-HOLD-FLAG = 'Y'
                   COMPUTE ESCT-PAYMENT-AMT ROUNDED = ESCM-PAYMENT-AMT *
                       ESCM-FEE-PCT / 200
               WHEN ESCM-STATUS-CD = 'A'
                   MOVE 12.00 TO ESCT-PAYMENT-AMT
               WHEN ESCM-STATUS-CD = 'E'
                   MOVE 5.00 TO ESCT-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE ESCT-PAYMENT-AMT ROUNDED = ESCM-PAYMENT-AMT *
                       ESCM-FEE-PCT / 100
           END-EVALUATE
           IF ESCT-PAYMENT-AMT > 0 AND ESCT-PAYMENT-AMT < 2.00
               MOVE 2.00 TO ESCT-PAYMENT-AMT
           END-IF
           IF ESCT-PAYMENT-AMT > 75.00
               MOVE 75.00 TO ESCT-PAYMENT-AMT
           END-IF
           ADD ESCT-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-LAST-ACT.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO ESCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INSURANCE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-INSURANCE.
           COMPUTE WS-WORK-AMT = ESCT-INSURANCE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ESCT-INSURANCE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE ESCM-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3970-CHECK-LAST-ACT.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3980-AGE-NOTICE.
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
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CUSHION AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3990-ACCRUE-CUSHION.
           IF ESCM-CUSHION-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-CUSHION-AMT * ESCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ESCM-SHORTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SURPLUS AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4000-LIMIT-SURPLUS.
           IF ESCM-SURPLUS-AMT < 10.00
               MOVE 10.00 TO ESCM-SURPLUS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-SURPLUS-AMT > 10000.00
               MOVE 10000.00 TO ESCM-SURPLUS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SURPLUS YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4010-SPLIT-SURPLUS.
           COMPUTE WS-WORK-AMT = ESCM-SURPLUS-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ESCM-SURPLUS-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4020-AGE-LAST-PAY.
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
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4030-COUNT-NOTICE.
           IF ESCM-NOTICE-CNT < 9999999
               ADD 1 TO ESCM-NOTICE-CNT
           END-IF
           IF ESCM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO ESCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4040-DESCRIBE-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SHORTAGE FEE ON THE CUSHION YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4050-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ESCM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ESCT-SHORTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   COMPUTE ESCT-SHORTAGE-AMT ROUNDED = ESCM-CUSHION-YTD
                       * ESCM-FEE-PCT / 200
               WHEN ESCT-TRAN-CD = 'DS'
                   MOVE 7.00 TO ESCT-SHORTAGE-AMT
               WHEN ESCT-TRAN-CD = 'PY'
                   MOVE 7.00 TO ESCT-SHORTAGE-AMT
               WHEN OTHER
                   COMPUTE ESCT-SHORTAGE-AMT ROUNDED = ESCM-CUSHION-YTD
                       * ESCM-FEE-PCT / 100
           END-EVALUATE
           IF ESCT-SHORTAGE-AMT > 0 AND ESCT-SHORTAGE-AMT < 1.00
               MOVE 1.00 TO ESCT-SHORTAGE-AMT
           END-IF
           IF ESCT-SHORTAGE-AMT > 100.00
               MOVE 100.00 TO ESCT-SHORTAGE-AMT
           END-IF
           ADD ESCT-SHORTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TAX AMT.
      *----------------------------------------------------------------*
       4060-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-TAX-AMT ROUNDED =
                   ESCM-PAYMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-TAX-AMT
           END-IF
           ADD ESCM-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4070-DESCRIBE-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'F'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'S'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'X'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4080-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4090-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ESCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ESCM-INSURANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT TIER TIER FOR THE DISBURSE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4100-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-TIER-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ESCM-DISBURSE-YTD NOT < WS-PAYMENT-TIER-LOW(WS-TX)
                   MOVE WS-PAYMENT-TIER-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISBURSE FEE ON THE WORK AMT 2.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4110-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ESCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ESCM-DISBURSE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCT-REVERSAL-FLAG = 'Y'
                   COMPUTE ESCM-DISBURSE-AMT ROUNDED = WS-WORK-AMT-2 *
                       ESCM-FEE-PCT / 200
               WHEN ESCM-STATUS-CD = 'A'
                   MOVE 20.00 TO ESCM-DISBURSE-AMT
               WHEN ESCM-STATUS-CD = 'E'
                   MOVE 20.00 TO ESCM-DISBURSE-AMT
               WHEN OTHER
                   COMPUTE ESCM-DISBURSE-AMT ROUNDED = WS-WORK-AMT-2 *
                       ESCM-FEE-PCT / 100
           END-EVALUATE
           IF ESCM-DISBURSE-AMT > 0 AND ESCM-DISBURSE-AMT < 1.00
               MOVE 1.00 TO ESCM-DISBURSE-AMT
           END-IF
           IF ESCM-DISBURSE-AMT > 150.00
               MOVE 150.00 TO ESCM-DISBURSE-AMT
           END-IF
           ADD ESCM-DISBURSE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INSURANCE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4120-ACCRUE-INSURANCE.
           IF ESCM-INSURANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-INSURANCE-AMT * ESCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ESCM-PAYMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E164 CONTACT REQUIRED
      * E776 WORK AMT NEGATIVE
      * E391 WORK AMT OVER 50000
      * E363 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4130-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CONTACT = SPACES
                   MOVE 'E164' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E776' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E391' TO WS-REJECT-CODE
               WHEN ESCM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E363' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAX YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4140-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = ESCM-TAX-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ESCM-TAX-YTD - (WS-WORK-AMT * 12)
           COMPUTE ESCT-SHORTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4150-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CUSHION YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INSURANCE YTD.
      *----------------------------------------------------------------*
       4160-PRORATE-INSURANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-INSURANCE-YTD ROUNDED =
                   ESCM-CUSHION-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-INSURANCE-YTD
           END-IF
           ADD ESCM-INSURANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4170-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INSURANCE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4180-SPLIT-INSURANCE.
           COMPUTE WS-WORK-AMT = ESCT-INSURANCE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ESCT-INSURANCE-AMT - (WS-WORK-AMT *
               3)
           COMPUTE ESCM-SURPLUS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E590 MEMO REQUIRED
      * E176 PAYMENT AMT NEGATIVE
      * E087 PAYMENT AMT OVER 50000
      * E286 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4190-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCT-MEMO = SPACES
                   MOVE 'E590' TO WS-REJECT-CODE
               WHEN ESCT-PAYMENT-AMT < 0
                   MOVE 'E176' TO WS-REJECT-CODE
               WHEN ESCT-PAYMENT-AMT > 50000.00
                   MOVE 'E087' TO WS-REJECT-CODE
               WHEN ESCM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E286' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4200-DESCRIBE-REGION.
           EVALUATE ESCM-REGION-CD
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'G'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CUSHION AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISBURSE AMT.
      *----------------------------------------------------------------*
       4210-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-DISBURSE-AMT ROUNDED =
                   ESCT-CUSHION-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-DISBURSE-AMT
           END-IF
           ADD ESCM-DISBURSE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AKX -> REP WHEN POST DATE IS PAST
      * REP -> SMX WHEN POST DATE IS PAST
      * SMX -> VJL WHEN POST DATE IS PAST
      * VJL -> WKN WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4220-SET-TYPE.
           EVALUATE ESCM-TYPE-CD
               WHEN 'AKX'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'REP' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'REP'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SMX' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMX'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJL' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJL'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKN' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE ESC FIELDS TO ESLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-ESLIM.
           INITIALIZE ESLIM-PARMS
           MOVE ESCM-TAX-YTD
             TO ESLIM-TAX-YTD
           MOVE ESCM-INSURANCE-AMT
             TO ESLIM-INSURANCE-AMT
           MOVE ESCM-SHORTAGE-YTD
             TO ESLIM-SHORTAGE-YTD
           MOVE ESCM-SURPLUS-AMT
             TO ESLIM-SURPLUS-AMT
           MOVE ESCM-PAYMENT-AMT
             TO ESLIM-PAYMENT-AMT
           MOVE ESCM-SHORTAGE-AMT
             TO ESLIM-SHORTAGE-AMT
           MOVE ESCM-EXPIRY-DATE
             TO ESLIM-EXPIRY-DATE
           MOVE ESCM-LAST-ACT-DATE
             TO ESLIM-LAST-ACT-DATE
           MOVE ESCM-LAST-PAY-DATE
             TO ESLIM-LAST-PAY-DATE
           MOVE ESCM-NOTICE-DATE
             TO ESLIM-NOTICE-DATE
           MOVE ESCM-REASON-CD
             TO ESLIM-REASON-CD
           MOVE ESCM-PAYEE-TYPE-CD
             TO ESLIM-PAYEE-TYPE-CD
           MOVE ESCM-TYPE-CD
             TO ESLIM-TYPE-CD
           MOVE ESCM-STATUS-CD
             TO ESLIM-STATUS-CD
           MOVE ESCM-NOTICE-SENT-FLAG
             TO ESLIM-NOTICE-SENT-FLAG
           MOVE ESCM-SUSPEND-FLAG
             TO ESLIM-SUSPEND-FLAG
           MOVE ESCM-OVERRIDE-FLAG
             TO ESLIM-OVERRIDE-FLAG
           MOVE ESCM-RETRY-CNT
             TO ESLIM-RETRY-CNT
           MOVE ESCM-EXCEPT-CNT
             TO ESLIM-EXCEPT-CNT
           MOVE ESCM-FEE-PCT
             TO ESLIM-FEE-PCT
           MOVE ESCM-RATE
             TO ESLIM-RATE
           MOVE ESCM-CUST-ID
             TO ESLIM-CUST-ID
           MOVE ESCM-DESC
             TO ESLIM-DESC
           CALL 'ESLIM' USING ESLIM-PARMS
           EVALUATE ESLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ESLIM-TAX-YTD
                     TO ESCM-TAX-YTD
                   MOVE ESLIM-SHORTAGE-YTD
                     TO ESCM-SHORTAGE-YTD
                   MOVE ESLIM-SURPLUS-AMT
                     TO ESCM-SURPLUS-AMT
                   MOVE ESLIM-PAYMENT-AMT
                     TO ESCM-PAYMENT-AMT
                   MOVE ESLIM-SHORTAGE-AMT
                     TO ESCM-SHORTAGE-AMT
                   MOVE ESLIM-SUSPEND-FLAG
                     TO ESCM-SUSPEND-FLAG
                   MOVE ESLIM-OVERRIDE-FLAG
                     TO ESCM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-ESLIM' TO ABND-PARAGRAPH
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
           DISPLAY 'ESCRCON READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ESCRCON' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
