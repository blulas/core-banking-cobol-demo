      *================================================================*
      * PROGRAM  : SLLMTHE                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : STUDENT LOANS - MONTH-END. READS THE STL            *
      *            TRANSACTIONS, UPDATES THE STL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. GUARANTEED STUDENT LOAN         *
      *            DISBURSEMENT, DEFERMENT AND REPAYMENT               *
      * INPUT    : STLMST (KSDS), STLTRN, BANKCTL                      *
      * OUTPUT   : STLOUT, SLLMTRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, SLCAL, SLVAL            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1987-09-03 SKN  ORIGINAL                                       *
      * 1988-11-03 PQR  CAPINT TABLE EXPANDED REQ2301                  *
      * 1988-07-05 CRP  SUBSIDY RERUN PROTECTION CHG2604               *
      * 1990-11-19 RJT  AUDIT FINDING - DISBURSE CONTROL TOTALS PRB0342*
      * 1993-07-16 PQR  DEFERINT NOTICE TEXT PER LEGAL INC8794         *
      * 1994-04-03 MKL  CAPINT ROUNDING - HALF UP REQ0859              *
      * 1994-07-23 LMB  CAPINT EXCEPTIONS TO OPS QUEUE INC8976         *
      * 2000-11-20 TWH  PAYMENT FEE SCHEDULE UPDATE PRB4732            *
      * 2003-12-06 TWH  NEW DEFERINT REPORT COLUMNS REQ5800            *
      * 2006-05-18 PQR  DISBURSE TABLE EXPANDED REQ1338                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SLLMTHE.
       AUTHOR. D P SHAW.

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
           SELECT REPORT-FILE ASSIGN TO "SLLMTRPT"
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
                                           VALUE 12.000000.
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
      * WS-DEFERINT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEFERINT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000718000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000171500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000217500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005350000000645500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007850000000475000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008350000000590000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018350000000041000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023350000000860500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023850000000339500'.
       01  WS-DEFERINT-SCHED-TBL REDEFINES WS-DEFERINT-SCHED-VALUES.
           05  WS-DEFERINT-SCHED-ROW OCCURS 9 TIMES.
               10  WS-DEFERINT-SCHED-LOW   PIC 9(09)V99.
               10  WS-DEFERINT-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DISBURSE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISBURSE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000487500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000138500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000553000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020250000000836000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021250000000602500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026250000000176000'.
       01  WS-DISBURSE-GRID-TBL REDEFINES WS-DISBURSE-GRID-VALUES.
           05  WS-DISBURSE-GRID-ROW OCCURS 6 TIMES.
               10  WS-DISBURSE-GRID-LOW    PIC 9(09)V99.
               10  WS-DISBURSE-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PRINCIPA-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINCIPA-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000388500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000404500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000503000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017500000000147000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027500000000075000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00037500000000593500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040000000000780500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040250000000195000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040750000000507000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040850000000503000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00041100000000019000'.
       01  WS-PRINCIPA-BAND-TBL REDEFINES WS-PRINCIPA-BAND-VALUES.
           05  WS-PRINCIPA-BAND-ROW OCCURS 11 TIMES.
               10  WS-PRINCIPA-BAND-LOW    PIC 9(09)V99.
               10  WS-PRINCIPA-BAND-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  SLLMTHE  STUDENT LOANS'.
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
               PERFORM 3000-COUNT-ITEM
               PERFORM 3010-AGE-MATURITY
               PERFORM 3020-COUNT-PAYMENT
               PERFORM 3030-FLAG-PRINCIPAL
               PERFORM 3040-EDIT-DESC
               PERFORM 3050-DESCRIBE-LOAN-TYPE
               PERFORM 3060-ACCRUE-PRINCIPAL
               PERFORM 3070-SELECT-FEE-PCT
               PERFORM 3080-ASSESS-OVERRIDE
               PERFORM 3090-EDIT-CUST-ID
               PERFORM 3100-COUNT-PAYMENT
               PERFORM 3110-CHECK-LAST-MAINT
               PERFORM 3120-EDIT-CUST-ID
               PERFORM 3130-CHECK-NEXT-REVIEW
               PERFORM 3140-ASSESS-REVERSAL
               PERFORM 3150-SELECT-PENALTY-PCT
               PERFORM 3160-FLAG-PRINCIPAL
               PERFORM 3170-ACCRUE-DEFERINT
               PERFORM 3180-SELECT-PENALTY-PCT
               PERFORM 3190-CHECK-LAST-ACT
               PERFORM 3200-AGE-POST
               PERFORM 3210-DESCRIBE-DEFER-CODE
               PERFORM 3220-CHECK-OPEN
               PERFORM 3230-AGE-POST
               PERFORM 3240-COUNT-PAYMENT
               PERFORM 3250-SELECT-DISCOUNT
               PERFORM 3260-AGE-LAST-MAINT
               PERFORM 3270-ACCRUE-PRINCIPAL
               PERFORM 3280-EDIT-OPERATOR
               PERFORM 3290-AGE-NEXT-REVIEW
               PERFORM 3300-PRORATE-DEFERINT
               PERFORM 3310-SPLIT-CAPINT
               PERFORM 3320-SELECT-FEE-PCT
               PERFORM 3330-FLAG-ITEM
               PERFORM 3340-ACCUM-SOURCE
               PERFORM 3350-PRORATE-CAPINT
               PERFORM 3360-ACCUM-CYCLE
               PERFORM 3370-SET-REGION
               PERFORM 3380-ACCUM-STATUS
               PERFORM 3390-COUNT-EXCEPT
               PERFORM 3400-AGE-POST
               PERFORM 3410-SPLIT-PAYMENT
               PERFORM 3420-PRORATE-DISBURSE
               PERFORM 3430-PRORATE-CAPINT
               PERFORM 3440-EDIT-OPERATOR
               PERFORM 3450-EDIT-CUST-ID
               PERFORM 3460-DESCRIBE-CYCLE
               PERFORM 3470-CHECK-CLOSE
               PERFORM 3480-EDIT-CONTACT
               PERFORM 3490-FLAG-CAPINT
               PERFORM 3500-ASSESS-HOLD
               PERFORM 3510-SELECT-FEE-PCT
               PERFORM 3520-PRORATE-DISBURSE
               PERFORM 3530-ACCRUE-DEFERINT
               PERFORM 3540-LIMIT-ITEM
               PERFORM 3550-PRORATE-DEFERINT
               PERFORM 3560-ACCRUE-PAYMENT
               PERFORM 3570-ACCUM-LOAN-TYPE
               PERFORM 3580-PRORATE-DISBURSE
               PERFORM 3590-EDIT-CONTACT
               PERFORM 3600-ACCUM-CHANNEL
               PERFORM 3610-SET-SOURCE
               PERFORM 3620-FLAG-PAYMENT
               PERFORM 3630-FLAG-DISBURSE
               PERFORM 3640-PRORATE-CAPINT
               PERFORM 3650-SET-CHANNEL
               PERFORM 3660-SPLIT-PAYMENT
               PERFORM 3670-SELECT-DISCOUNT
               PERFORM 3680-SET-REGION
               PERFORM 3690-SELECT-PENALTY-PCT
               PERFORM 3700-AGE-OPEN
               PERFORM 3710-DESCRIBE-CHANNEL
               PERFORM 3720-LIMIT-PRINCIPAL
               PERFORM 3730-ACCUM-REGION
               PERFORM 3740-COUNT-EXCEPT
               PERFORM 3750-COUNT-EXCEPT
               PERFORM 3760-AGE-LAST-STMT
               PERFORM 3770-SELECT-DISCOUNT
               PERFORM 3780-ACCUM-SOURCE
               PERFORM 3790-ACCUM-TRAN
               PERFORM 3800-SELECT-PENALTY-PCT
               PERFORM 3810-SELECT-DISCOUNT
               PERFORM 3820-CHECK-LAST-ACT
               PERFORM 3830-ASSESS-REVIEW
               PERFORM 3840-AGE-EFF
               PERFORM 3850-DESCRIBE-LOAN-TYPE
               PERFORM 3860-ASSESS-REVIEW
               PERFORM 3870-AGE-POST
               PERFORM 3880-SET-CHANNEL
               PERFORM 3890-PRORATE-DISBURSE
               PERFORM 3900-EDIT-REJECT
               PERFORM 3910-ACCUM-REGION
               PERFORM 3920-SPLIT-CAPINT
               PERFORM 3930-ACCRUE-DISCOUNT
               PERFORM 3940-COUNT-ITEM
               PERFORM 3950-ASSESS-REVIEW
               PERFORM 3960-CHECK-LAST-ACT
               PERFORM 3970-AGE-OPEN
               PERFORM 3980-COUNT-SEQ
               PERFORM 3990-SPLIT-DISBURSE
               PERFORM 4000-AGE-POST
               PERFORM 4010-CHECK-OPEN
               PERFORM 4020-LIMIT-PAYMENT
               PERFORM 4030-LIMIT-PAYMENT
               PERFORM 4040-ACCUM-CHANNEL
               PERFORM 4050-CHECK-MATURITY
               PERFORM 4060-AGE-LAST-ACT
               PERFORM 4070-FLAG-SUBSIDY
               PERFORM 4080-SPLIT-PRINCIPAL
               PERFORM 4090-DESCRIBE-CHANNEL
               PERFORM 4100-ACCRUE-DISBURSE
               PERFORM 4110-DESCRIBE-REGION
               PERFORM 4120-SET-SOURCE
               PERFORM 4130-ASSESS-PREFERRED
               PERFORM 4140-SPLIT-DEFERINT
               PERFORM 4150-ASSESS-OVERRIDE
               PERFORM 4160-ACCUM-TRAN
               PERFORM 4170-FLAG-DISBURSE
               PERFORM 4180-ASSESS-HOLD
               PERFORM 4190-ASSESS-NOTICE-SENT
               PERFORM 4200-PRORATE-CAPINT
               PERFORM 4210-ACCUM-SOURCE
               PERFORM 4220-DESCRIBE-SOURCE
               PERFORM 4230-SET-DEFER-CODE
               PERFORM 4240-CHECK-LAST-STMT
               PERFORM 4250-SPLIT-PAYMENT
               PERFORM 4260-ACCUM-CYCLE
               PERFORM 4270-PRORATE-DISBURSE
               PERFORM 4280-COUNT-SEQ
               PERFORM 4290-ACCRUE-PAYMENT
               PERFORM 4300-EDIT-CUST-ID
               PERFORM 4310-CHECK-LAST-PAY
               PERFORM 4320-DESCRIBE-REGION
               PERFORM 4330-CHECK-LAST-STMT
               PERFORM 4340-AGE-LAST-MAINT
               PERFORM 4350-ASSESS-NOTICE-SENT
               PERFORM 4360-CHECK-EFF
               PERFORM 4370-SELECT-PENALTY-PCT
               PERFORM 4380-SPLIT-PAYMENT
               PERFORM 4390-ACCRUE-PRINCIPAL
               PERFORM 4400-ASSESS-NOTICE-SENT
               PERFORM 4410-EDIT-REJECT
               PERFORM 4420-EDIT-REF-NO
               PERFORM 4430-ACCUM-CHANNEL
               PERFORM 4440-SELECT-PENALTY-PCT
               PERFORM 4450-AGE-EXPIRY
               PERFORM 4460-ACCUM-REGION
               PERFORM 4470-FLAG-PRINCIPAL
               PERFORM 4480-SPLIT-DEFERINT
               PERFORM 4490-ACCRUE-CAPINT
               PERFORM 4500-FLAG-DISBURSE
               PERFORM 4510-EDIT-REJECT
               PERFORM 4520-ACCUM-SOURCE
               PERFORM 4530-ASSESS-PREFERRED
               PERFORM 4540-FLAG-SUBSIDY
               PERFORM 4550-SELECT-FEE-PCT
               PERFORM 4560-COUNT-PAYMENT
               PERFORM 4570-SPLIT-PRINCIPAL
               PERFORM 4580-ACCUM-CHANNEL
               PERFORM 4590-ASSESS-REVIEW
               PERFORM 4600-FLAG-DISBURSE
               PERFORM 4610-ACCRUE-PAYMENT
               PERFORM 4620-DESCRIBE-DEFER-CODE
               PERFORM 4630-CHECK-EFF
               PERFORM 4640-LIMIT-ITEM
               PERFORM 4650-AGE-POST
               PERFORM 4660-DESCRIBE-SOURCE
               PERFORM 4670-DESCRIBE-TRAN
               PERFORM 4680-EDIT-CONTACT
               PERFORM 4690-AGE-CLOSE
               PERFORM 4700-COUNT-PAYMENT
               PERFORM 4710-SPLIT-DISBURSE
               PERFORM 4720-ACCRUE-PAYMENT
               PERFORM 4730-SET-STATUS
               PERFORM 4740-LIMIT-CAPINT
               PERFORM 4750-PRORATE-PAYMENT
               PERFORM 4760-DESCRIBE-DEFER-CODE
               PERFORM 4770-ACCUM-DEFER-CODE
               PERFORM 4780-SPLIT-DISBURSE
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
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3000-COUNT-ITEM.
           IF STLM-ITEM-CNT < 9999999
               ADD 1 TO STLM-ITEM-CNT
           END-IF
           IF STLM-ITEM-CNT NOT < 3
               MOVE 'Y' TO STLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3010-AGE-MATURITY.
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
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-PAYMENT.
           IF STLM-PAYMENT-CNT < 9999999
               ADD 1 TO STLM-PAYMENT-CNT
           END-IF
           IF STLM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO STLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3030-FLAG-PRINCIPAL.
           IF STLT-PRINCIPAL-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE STLT-PRINCIPAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E800 DESC REQUIRED
      * E499 PRINCIPAL YTD NEGATIVE
      * E215 PRINCIPAL YTD OVER 25000
      * E416 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3040-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-DESC = SPACES
                   MOVE 'E800' TO WS-REJECT-CODE
               WHEN STLM-PRINCIPAL-YTD < 0
                   MOVE 'E499' TO WS-REJECT-CODE
               WHEN STLM-PRINCIPAL-YTD > 25000.00
                   MOVE 'E215' TO WS-REJECT-CODE
               WHEN STLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E416' TO WS-REJECT-CODE
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
       3050-DESCRIBE-LOAN-TYPE.
           EVALUATE STLM-LOAN-TYPE-CD
               WHEN 'CS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'DV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3060-ACCRUE-PRINCIPAL.
           IF STLM-PRINCIPAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-PRINCIPAL-YTD * STLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE GRID TIER FOR THE SUBSIDY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3070-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-GRID-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF STLM-SUBSIDY-AMT NOT < WS-DISBURSE-GRID-LOW(WS-TX)
                   MOVE WS-DISBURSE-GRID-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE DEFERINT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3080-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN STLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO STLM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-HOLD-FLAG = 'Y'
                   COMPUTE STLM-PRINCIPAL-YTD ROUNDED =
                       STLM-DEFERINT-AMT * STLM-PENALTY-PCT / 200
               WHEN STLM-REGION-CD = 'C'
                   MOVE 7.00 TO STLM-PRINCIPAL-YTD
               WHEN STLM-REGION-CD = 'D'
                   MOVE 15.00 TO STLM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE STLM-PRINCIPAL-YTD ROUNDED =
                       STLM-DEFERINT-AMT * STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLM-PRINCIPAL-YTD > 0 AND STLM-PRINCIPAL-YTD < 2.00
               MOVE 2.00 TO STLM-PRINCIPAL-YTD
           END-IF
           IF STLM-PRINCIPAL-YTD > 25.00
               MOVE 25.00 TO STLM-PRINCIPAL-YTD
           END-IF
           ADD STLM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E616 CUST ID REQUIRED
      * E110 PAYMENT YTD NEGATIVE
      * E739 PAYMENT YTD OVER 99999
      * E962 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3090-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-CUST-ID = SPACES
                   MOVE 'E616' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-YTD < 0
                   MOVE 'E110' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-YTD > 99999.00
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN STLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E962' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3100-COUNT-PAYMENT.
           IF STLM-PAYMENT-CNT < 9999999
               ADD 1 TO STLM-PAYMENT-CNT
           END-IF
           IF STLM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3110-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO STLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E159 CUST ID REQUIRED
      * E739 WORK AMT NEGATIVE
      * E701 WORK AMT OVER 99999
      * E564 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3120-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-CUST-ID = SPACES
                   MOVE 'E159' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E701' TO WS-REJECT-CODE
               WHEN STLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E564' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO STLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT YTD FEE ON THE PRINCIPAL YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3140-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN STLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO STLM-CAPINT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-PREFERRED-FLAG = 'Y'
                   COMPUTE STLM-CAPINT-YTD ROUNDED = STLM-PRINCIPAL-YTD
                       * STLM-DISCOUNT / 200
               WHEN STLM-LOAN-TYPE-CD = 'CS'
                   MOVE 7.00 TO STLM-CAPINT-YTD
               WHEN STLM-LOAN-TYPE-CD = 'DV'
                   MOVE 3.00 TO STLM-CAPINT-YTD
               WHEN OTHER
                   COMPUTE STLM-CAPINT-YTD ROUNDED = STLM-PRINCIPAL-YTD
                       * STLM-DISCOUNT / 100
           END-EVALUATE
           IF STLM-CAPINT-YTD > 0 AND STLM-CAPINT-YTD < 5.00
               MOVE 5.00 TO STLM-CAPINT-YTD
           END-IF
           IF STLM-CAPINT-YTD > 250.00
               MOVE 250.00 TO STLM-CAPINT-YTD
           END-IF
           ADD STLM-CAPINT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT SCHED TIER FOR THE SUBSIDY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-SCHED-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF STLM-SUBSIDY-AMT NOT < WS-DEFERINT-SCHED-LOW(WS-TX)
                   MOVE WS-DEFERINT-SCHED-RATE(WS-TX) TO
                       STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3160-FLAG-PRINCIPAL.
           IF STLT-PRINCIPAL-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE STLT-PRINCIPAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEFERINT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3170-ACCRUE-DEFERINT.
           IF STLM-DEFERINT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-DEFERINT-AMT * STLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO STLM-PAYMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE GRID TIER FOR THE DEFERINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3180-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-GRID-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF STLM-DEFERINT-AMT NOT < WS-DISBURSE-GRID-LOW(WS-TX)
                   MOVE WS-DISBURSE-GRID-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO STLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3200-AGE-POST.
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
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE DEFER CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3210-DESCRIBE-DEFER-CODE.
           EVALUATE STLM-DEFER-CODE-CD
               WHEN 'AH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ND'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3220-CHECK-OPEN.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO STLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3230-AGE-POST.
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
               WHEN WS-DAYS NOT > 45
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3240-COUNT-PAYMENT.
           IF STLM-PAYMENT-CNT < 9999999
               ADD 1 TO STLM-PAYMENT-CNT
           END-IF
           IF STLM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE GRID TIER FOR THE DISBURSE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3250-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-GRID-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF STLT-DISBURSE-AMT NOT < WS-DISBURSE-GRID-LOW(WS-TX)
                   MOVE WS-DISBURSE-GRID-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3260-AGE-LAST-MAINT.
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
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3270-ACCRUE-PRINCIPAL.
           IF STLM-PRINCIPAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-PRINCIPAL-YTD * STLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO STLM-DEFERINT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E598 OPERATOR REQUIRED
      * E118 PAYMENT AMT NEGATIVE
      * E357 PAYMENT AMT OVER 25000
      * E471 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3280-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-OPERATOR = SPACES
                   MOVE 'E598' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-AMT < 0
                   MOVE 'E118' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-AMT > 25000.00
                   MOVE 'E357' TO WS-REJECT-CODE
               WHEN STLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E471' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3290-AGE-NEXT-REVIEW.
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
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DEFERINT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEFERINT YTD.
      *----------------------------------------------------------------*
       3300-PRORATE-DEFERINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-DEFERINT-YTD ROUNDED =
                   STLM-DEFERINT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-DEFERINT-YTD
           END-IF
           ADD STLM-DEFERINT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CAPINT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3310-SPLIT-CAPINT.
           COMPUTE WS-WORK-AMT = STLM-CAPINT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = STLM-CAPINT-AMT - (WS-WORK-AMT * 3)
           COMPUTE STLM-PAYMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT SCHED TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3320-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-SCHED-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF STLM-PRINCIPAL-AMT NOT < WS-DEFERINT-SCHED-LOW(WS-TX)
                   MOVE WS-DEFERINT-SCHED-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3330-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3340-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-SOURCE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CAPINT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3350-PRORATE-CAPINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   STLM-CAPINT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE SUBSIDY AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3360-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-CYCLE-CD
                   ADD STLM-SUBSIDY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-SUBSIDY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> D WHEN NEXT REVIEW DATE IS PAST
      * D -> T WHEN NEXT REVIEW DATE IS PAST
      * T -> U WHEN NEXT REVIEW DATE IS PAST
      * U -> W WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3370-SET-REGION.
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
      * ACCUMULATE DISBURSE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3380-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-STATUS-CD
                   ADD STLT-DISBURSE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLT-DISBURSE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3390-COUNT-EXCEPT.
           IF STLM-EXCEPT-CNT < 9999999
               ADD 1 TO STLM-EXCEPT-CNT
           END-IF
           IF STLM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO STLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3400-AGE-POST.
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
                   ADD STLM-DEFERINT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD STLM-DEFERINT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-DEFERINT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD STLM-DEFERINT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-DEFERINT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = STLM-PAYMENT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = STLM-PAYMENT-YTD - (WS-WORK-AMT * 2)
           COMPUTE STLM-DEFERINT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISBURSE YTD.
      *----------------------------------------------------------------*
       3420-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-DISBURSE-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-DISBURSE-YTD
           END-IF
           ADD STLM-DISBURSE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE CAPINT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CAPINT AMT.
      *----------------------------------------------------------------*
       3430-PRORATE-CAPINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-CAPINT-AMT ROUNDED =
                   STLM-CAPINT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-CAPINT-AMT
           END-IF
           ADD STLM-CAPINT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E244 OPERATOR REQUIRED
      * E450 CAPINT AMT NEGATIVE
      * E210 CAPINT AMT OVER 99999
      * E830 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3440-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-OPERATOR = SPACES
                   MOVE 'E244' TO WS-REJECT-CODE
               WHEN STLM-CAPINT-AMT < 0
                   MOVE 'E450' TO WS-REJECT-CODE
               WHEN STLM-CAPINT-AMT > 99999.00
                   MOVE 'E210' TO WS-REJECT-CODE
               WHEN STLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E830' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E354 CUST ID REQUIRED
      * E497 DISBURSE YTD NEGATIVE
      * E477 DISBURSE YTD OVER 250000
      * E368 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3450-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-CUST-ID = SPACES
                   MOVE 'E354' TO WS-REJECT-CODE
               WHEN STLM-DISBURSE-YTD < 0
                   MOVE 'E497' TO WS-REJECT-CODE
               WHEN STLM-DISBURSE-YTD > 250000.00
                   MOVE 'E477' TO WS-REJECT-CODE
               WHEN STLM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E368' TO WS-REJECT-CODE
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
       3460-DESCRIBE-CYCLE.
           EVALUATE STLM-CYCLE-CD
               WHEN 'CFJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KHC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SEU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XKA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3470-CHECK-CLOSE.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E681 CONTACT REQUIRED
      * E367 DEFERINT YTD NEGATIVE
      * E551 DEFERINT YTD OVER 25000
      * E560 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3480-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-CONTACT = SPACES
                   MOVE 'E681' TO WS-REJECT-CODE
               WHEN STLM-DEFERINT-YTD < 0
                   MOVE 'E367' TO WS-REJECT-CODE
               WHEN STLM-DEFERINT-YTD > 25000.00
                   MOVE 'E551' TO WS-REJECT-CODE
               WHEN STLM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E560' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CAPINT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3490-FLAG-CAPINT.
           IF STLT-CAPINT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE STLT-CAPINT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE CAPINT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3500-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN STLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO STLT-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLT-REVERSAL-FLAG = 'Y'
                   COMPUTE STLT-PRINCIPAL-AMT ROUNDED = STLT-CAPINT-AMT
                       * STLM-DISCOUNT / 200
               WHEN STLM-DEFER-CODE-CD = 'AH'
                   MOVE 15.00 TO STLT-PRINCIPAL-AMT
               WHEN STLM-DEFER-CODE-CD = 'CZ'
                   MOVE 12.00 TO STLT-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE STLT-PRINCIPAL-AMT ROUNDED = STLT-CAPINT-AMT
                       * STLM-DISCOUNT / 100
           END-EVALUATE
           IF STLT-PRINCIPAL-AMT > 0 AND STLT-PRINCIPAL-AMT < 10.00
               MOVE 10.00 TO STLT-PRINCIPAL-AMT
           END-IF
           IF STLT-PRINCIPAL-AMT > 25.00
               MOVE 25.00 TO STLT-PRINCIPAL-AMT
           END-IF
           ADD STLT-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT SCHED TIER FOR THE DISBURSE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3510-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-SCHED-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF STLM-DISBURSE-AMT NOT < WS-DEFERINT-SCHED-LOW(WS-TX)
                   MOVE WS-DEFERINT-SCHED-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISBURSE YTD.
      *----------------------------------------------------------------*
       3520-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-DISBURSE-YTD ROUNDED =
                   STLM-PRINCIPAL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-DISBURSE-YTD
           END-IF
           ADD STLM-DISBURSE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEFERINT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3530-ACCRUE-DEFERINT.
           IF STLM-DEFERINT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-DEFERINT-AMT * STLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO STLT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3540-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEFERINT AMT.
      *----------------------------------------------------------------*
       3550-PRORATE-DEFERINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-DEFERINT-AMT ROUNDED =
                   STLT-PRINCIPAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-DEFERINT-AMT
           END-IF
           ADD STLM-DEFERINT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3560-ACCRUE-PAYMENT.
           IF STLT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLT-PAYMENT-AMT * STLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO STLM-PRINCIPAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY LOAN TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3570-ACCUM-LOAN-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-LOAN-TYPE-CD
                   ADD STLT-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-LOAN-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLT-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEFERINT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISBURSE AMT.
      *----------------------------------------------------------------*
       3580-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLT-DISBURSE-AMT ROUNDED =
                   STLM-DEFERINT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLT-DISBURSE-AMT
           END-IF
           ADD STLT-DISBURSE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E324 CONTACT REQUIRED
      * E941 PRINCIPAL AMT NEGATIVE
      * E043 PRINCIPAL AMT OVER 5000
      * E166 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3590-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-CONTACT = SPACES
                   MOVE 'E324' TO WS-REJECT-CODE
               WHEN STLM-PRINCIPAL-AMT < 0
                   MOVE 'E941' TO WS-REJECT-CODE
               WHEN STLM-PRINCIPAL-AMT > 5000.00
                   MOVE 'E043' TO WS-REJECT-CODE
               WHEN STLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E166' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3600-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLT-CHANNEL-CD
                   ADD STLT-DISBURSE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLT-DISBURSE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AST -> BCK WHEN OPEN DATE IS PAST
      * BCK -> CJG WHEN OPEN DATE IS PAST
      * CJG -> DSJ WHEN OPEN DATE IS PAST
      * DSJ -> JVY WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3610-SET-SOURCE.
           EVALUATE STLM-SOURCE-CD
               WHEN 'AST'
                   IF STLM-OPEN-DATE > 0 AND STLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BCK' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BCK'
                   IF STLM-OPEN-DATE > 0 AND STLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CJG' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJG'
                   IF STLM-OPEN-DATE > 0 AND STLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DSJ' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DSJ'
                   IF STLM-OPEN-DATE > 0 AND STLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JVY' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3620-FLAG-PAYMENT.
           IF STLM-PAYMENT-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE STLM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3630-FLAG-DISBURSE.
           IF STLM-DISBURSE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE STLM-DISBURSE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEFERINT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CAPINT YTD.
      *----------------------------------------------------------------*
       3640-PRORATE-CAPINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-CAPINT-YTD ROUNDED =
                   STLM-DEFERINT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-CAPINT-YTD
           END-IF
           ADD STLM-CAPINT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST PAY DATE IS PAST
      * BAT -> MAL WHEN LAST PAY DATE IS PAST
      * MAL -> TEL WHEN LAST PAY DATE IS PAST
      * TEL -> ONL WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3650-SET-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3660-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = STLM-PAYMENT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = STLM-PAYMENT-AMT - (WS-WORK-AMT * 4)
           COMPUTE STLM-SUBSIDY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT SCHED TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3670-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-SCHED-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF STLM-PAYMENT-YTD NOT < WS-DEFERINT-SCHED-LOW(WS-TX)
                   MOVE WS-DEFERINT-SCHED-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> D WHEN EFF DATE IS PAST
      * D -> T WHEN EFF DATE IS PAST
      * T -> U WHEN EFF DATE IS PAST
      * U -> W WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3680-SET-REGION.
           EVALUATE STLM-REGION-CD
               WHEN 'C'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE GRID TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3690-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-GRID-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF STLM-PAYMENT-AMT NOT < WS-DISBURSE-GRID-LOW(WS-TX)
                   MOVE WS-DISBURSE-GRID-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-OPEN.
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
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3710-DESCRIBE-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3720-LIMIT-PRINCIPAL.
           IF STLT-PRINCIPAL-AMT < 25.00
               MOVE 25.00 TO STLT-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLT-PRINCIPAL-AMT > 1000.00
               MOVE 1000.00 TO STLT-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEFERINT YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3730-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-REGION-CD
                   ADD STLM-DEFERINT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-DEFERINT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3740-COUNT-EXCEPT.
           IF STLM-EXCEPT-CNT < 9999999
               ADD 1 TO STLM-EXCEPT-CNT
           END-IF
           IF STLM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO STLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3750-COUNT-EXCEPT.
           IF STLM-EXCEPT-CNT < 9999999
               ADD 1 TO STLM-EXCEPT-CNT
           END-IF
           IF STLM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO STLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3760-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-STMT-DATE > 0 AND STLM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA BAND TIER FOR THE DISBURSE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3770-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-BAND-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF STLT-DISBURSE-AMT NOT < WS-PRINCIPA-BAND-LOW(WS-TX)
                   MOVE WS-PRINCIPA-BAND-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3780-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-SOURCE-CD
                   ADD STLM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3790-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLT-TRAN-CD
                   ADD STLM-DISBURSE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-DISBURSE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE GRID TIER FOR THE DEFERINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3800-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-GRID-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF STLM-DEFERINT-AMT NOT < WS-DISBURSE-GRID-LOW(WS-TX)
                   MOVE WS-DISBURSE-GRID-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE GRID TIER FOR THE DEFERINT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3810-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-GRID-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF STLM-DEFERINT-YTD NOT < WS-DISBURSE-GRID-LOW(WS-TX)
                   MOVE WS-DISBURSE-GRID-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3820-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO STLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SUBSIDY FEE ON THE DISBURSE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3830-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN STLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO STLM-SUBSIDY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-PREFERRED-FLAG = 'Y'
                   COMPUTE STLM-SUBSIDY-AMT ROUNDED = STLM-DISBURSE-AMT
                       * STLM-PENALTY-PCT / 200
               WHEN STLM-REGION-CD = 'C'
                   MOVE 7.00 TO STLM-SUBSIDY-AMT
               WHEN STLM-REGION-CD = 'D'
                   MOVE 5.00 TO STLM-SUBSIDY-AMT
               WHEN OTHER
                   COMPUTE STLM-SUBSIDY-AMT ROUNDED = STLM-DISBURSE-AMT
                       * STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLM-SUBSIDY-AMT > 0 AND STLM-SUBSIDY-AMT < 1.00
               MOVE 1.00 TO STLM-SUBSIDY-AMT
           END-IF
           IF STLM-SUBSIDY-AMT > 25.00
               MOVE 25.00 TO STLM-SUBSIDY-AMT
           END-IF
           ADD STLM-SUBSIDY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3840-AGE-EFF.
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
                   ADD STLT-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD STLT-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD STLT-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD STLT-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLT-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE LOAN TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3850-DESCRIBE-LOAN-TYPE.
           EVALUATE STLM-LOAN-TYPE-CD
               WHEN 'CS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT YTD FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3860-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN STLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO STLM-CAPINT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-SUSPEND-FLAG = 'Y'
                   COMPUTE STLM-CAPINT-YTD ROUNDED = STLM-PRINCIPAL-AMT
                       * STLM-FEE-PCT / 200
               WHEN STLT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO STLM-CAPINT-YTD
               WHEN STLT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO STLM-CAPINT-YTD
               WHEN OTHER
                   COMPUTE STLM-CAPINT-YTD ROUNDED = STLM-PRINCIPAL-AMT
                       * STLM-FEE-PCT / 100
           END-EVALUATE
           IF STLM-CAPINT-YTD > 0 AND STLM-CAPINT-YTD < 2.00
               MOVE 2.00 TO STLM-CAPINT-YTD
           END-IF
           IF STLM-CAPINT-YTD > 25.00
               MOVE 25.00 TO STLM-CAPINT-YTD
           END-IF
           ADD STLM-CAPINT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3870-AGE-POST.
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
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EXPIRY DATE IS PAST
      * BAT -> MAL WHEN EXPIRY DATE IS PAST
      * MAL -> TEL WHEN EXPIRY DATE IS PAST
      * TEL -> ONL WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3880-SET-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISBURSE AMT.
      *----------------------------------------------------------------*
       3890-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLT-DISBURSE-AMT ROUNDED =
                   STLT-PAYMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLT-DISBURSE-AMT
           END-IF
           ADD STLT-DISBURSE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E086 REJECT CD REQUIRED
      * E881 CAPINT AMT NEGATIVE
      * E868 CAPINT AMT OVER 99999
      * E662 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3900-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-REJECT-CD = SPACES
                   MOVE 'E086' TO WS-REJECT-CODE
               WHEN STLT-CAPINT-AMT < 0
                   MOVE 'E881' TO WS-REJECT-CODE
               WHEN STLT-CAPINT-AMT > 99999.00
                   MOVE 'E868' TO WS-REJECT-CODE
               WHEN STLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E662' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3910-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-REGION-CD
                   ADD STLT-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLT-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CAPINT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3920-SPLIT-CAPINT.
           COMPUTE WS-WORK-AMT = STLM-CAPINT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = STLM-CAPINT-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3930-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * STLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO STLT-DISBURSE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3940-COUNT-ITEM.
           IF STLM-ITEM-CNT < 9999999
               ADD 1 TO STLM-ITEM-CNT
           END-IF
           IF STLM-ITEM-CNT NOT < 6
               MOVE 'Y' TO STLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE DEFERINT YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3950-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN STLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO STLM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-SUSPEND-FLAG = 'Y'
                   COMPUTE STLM-PRINCIPAL-YTD ROUNDED =
                       STLM-DEFERINT-YTD * STLM-PENALTY-PCT / 200
               WHEN STLM-STATUS-CD = 'HY'
                   MOVE 3.00 TO STLM-PRINCIPAL-YTD
               WHEN STLM-STATUS-CD = 'JZ'
                   MOVE 12.00 TO STLM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE STLM-PRINCIPAL-YTD ROUNDED =
                       STLM-DEFERINT-YTD * STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLM-PRINCIPAL-YTD > 0 AND STLM-PRINCIPAL-YTD < 2.00
               MOVE 2.00 TO STLM-PRINCIPAL-YTD
           END-IF
           IF STLM-PRINCIPAL-YTD > 150.00
               MOVE 150.00 TO STLM-PRINCIPAL-YTD
           END-IF
           ADD STLM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3960-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO STLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3970-AGE-OPEN.
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
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-SEQ.
           IF STLT-SEQ < 9999999
               ADD 1 TO STLT-SEQ
           END-IF
           IF STLT-SEQ NOT < 5
               MOVE 'Y' TO STLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3990-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = STLT-DISBURSE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = STLT-DISBURSE-AMT - (WS-WORK-AMT * 3)
           COMPUTE STLM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4000-AGE-POST.
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
                   ADD STLM-DISBURSE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-DISBURSE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD STLM-DISBURSE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLM-DISBURSE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-DISBURSE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4010-CHECK-OPEN.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4020-LIMIT-PAYMENT.
           IF STLM-PAYMENT-YTD < 10.00
               MOVE 10.00 TO STLM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-PAYMENT-YTD > 2500.00
               MOVE 2500.00 TO STLM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4030-LIMIT-PAYMENT.
           IF STLT-PAYMENT-AMT < 0.00
               MOVE 0.00 TO STLT-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLT-PAYMENT-AMT > 2500.00
               MOVE 2500.00 TO STLT-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEFERINT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4040-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLT-CHANNEL-CD
                   ADD STLM-DEFERINT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-DEFERINT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4050-CHECK-MATURITY.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO STLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4060-AGE-LAST-ACT.
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
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SUBSIDY YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4070-FLAG-SUBSIDY.
           IF STLM-SUBSIDY-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE STLM-SUBSIDY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4080-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = STLM-PRINCIPAL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = STLM-PRINCIPAL-AMT - (WS-WORK-AMT *
               3)
           COMPUTE STLM-DISBURSE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4090-DESCRIBE-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4100-ACCRUE-DISBURSE.
           IF STLM-DISBURSE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-DISBURSE-AMT * STLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO STLT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4110-DESCRIBE-REGION.
           EVALUATE STLM-REGION-CD
               WHEN 'C'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'D'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'T'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'U'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Y'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AST -> BCK WHEN EFF DATE IS PAST
      * BCK -> CJG WHEN EFF DATE IS PAST
      * CJG -> DSJ WHEN EFF DATE IS PAST
      * DSJ -> JVY WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4120-SET-SOURCE.
           EVALUATE STLM-SOURCE-CD
               WHEN 'AST'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BCK' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BCK'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CJG' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJG'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DSJ' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DSJ'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JVY' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE PAYMENT YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4130-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN STLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO STLT-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLT-REVERSAL-FLAG = 'Y'
                   COMPUTE STLT-PAYMENT-AMT ROUNDED = STLM-PAYMENT-YTD *
                       STLM-PENALTY-PCT / 200
               WHEN STLM-SOURCE-CD = 'AST'
                   MOVE 5.00 TO STLT-PAYMENT-AMT
               WHEN STLM-SOURCE-CD = 'BCK'
                   MOVE 12.00 TO STLT-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE STLT-PAYMENT-AMT ROUNDED = STLM-PAYMENT-YTD *
                       STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLT-PAYMENT-AMT > 0 AND STLT-PAYMENT-AMT < 2.00
               MOVE 2.00 TO STLT-PAYMENT-AMT
           END-IF
           IF STLT-PAYMENT-AMT > 100.00
               MOVE 100.00 TO STLT-PAYMENT-AMT
           END-IF
           ADD STLT-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT DEFERINT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4140-SPLIT-DEFERINT.
           COMPUTE WS-WORK-AMT = STLM-DEFERINT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = STLM-DEFERINT-AMT - (WS-WORK-AMT * 4)
           COMPUTE STLM-DEFERINT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT YTD FEE ON THE CAPINT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4150-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN STLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO STLM-CAPINT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-PREFERRED-FLAG = 'Y'
                   COMPUTE STLM-CAPINT-YTD ROUNDED = STLT-CAPINT-AMT *
                       STLM-PENALTY-PCT / 200
               WHEN STLM-SOURCE-CD = 'AST'
                   MOVE 3.00 TO STLM-CAPINT-YTD
               WHEN STLM-SOURCE-CD = 'BCK'
                   MOVE 15.00 TO STLM-CAPINT-YTD
               WHEN OTHER
                   COMPUTE STLM-CAPINT-YTD ROUNDED = STLT-CAPINT-AMT *
                       STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLM-CAPINT-YTD > 0 AND STLM-CAPINT-YTD < 10.00
               MOVE 10.00 TO STLM-CAPINT-YTD
           END-IF
           IF STLM-CAPINT-YTD > 35.00
               MOVE 35.00 TO STLM-CAPINT-YTD
           END-IF
           ADD STLM-CAPINT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE CAPINT YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4160-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLT-TRAN-CD
                   ADD STLM-CAPINT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-CAPINT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4170-FLAG-DISBURSE.
           IF STLM-DISBURSE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE STLM-DISBURSE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT FEE ON THE PRINCIPAL YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4180-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN STLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO STLT-CAPINT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-REVIEW-FLAG = 'Y'
                   COMPUTE STLT-CAPINT-AMT ROUNDED = STLM-PRINCIPAL-YTD
                       * STLM-PENALTY-PCT / 200
               WHEN STLM-STATUS-CD = 'HY'
                   MOVE 3.00 TO STLT-CAPINT-AMT
               WHEN STLM-STATUS-CD = 'JZ'
                   MOVE 10.00 TO STLT-CAPINT-AMT
               WHEN OTHER
                   COMPUTE STLT-CAPINT-AMT ROUNDED = STLM-PRINCIPAL-YTD
                       * STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLT-CAPINT-AMT > 0 AND STLT-CAPINT-AMT < 1.00
               MOVE 1.00 TO STLT-CAPINT-AMT
           END-IF
           IF STLT-CAPINT-AMT > 50.00
               MOVE 50.00 TO STLT-CAPINT-AMT
           END-IF
           ADD STLT-CAPINT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE PAYMENT AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4190-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN STLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO STLM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-REVIEW-FLAG = 'Y'
                   COMPUTE STLM-PAYMENT-YTD ROUNDED = STLM-PAYMENT-AMT *
                       STLM-FEE-PCT / 200
               WHEN STLT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO STLM-PAYMENT-YTD
               WHEN STLT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO STLM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE STLM-PAYMENT-YTD ROUNDED = STLM-PAYMENT-AMT *
                       STLM-FEE-PCT / 100
           END-EVALUATE
           IF STLM-PAYMENT-YTD > 0 AND STLM-PAYMENT-YTD < 10.00
               MOVE 10.00 TO STLM-PAYMENT-YTD
           END-IF
           IF STLM-PAYMENT-YTD > 75.00
               MOVE 75.00 TO STLM-PAYMENT-YTD
           END-IF
           ADD STLM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CAPINT AMT.
      *----------------------------------------------------------------*
       4200-PRORATE-CAPINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-CAPINT-AMT ROUNDED =
                   STLT-PRINCIPAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-CAPINT-AMT
           END-IF
           ADD STLM-CAPINT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE SUBSIDY AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4210-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-SOURCE-CD
                   ADD STLM-SUBSIDY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-SUBSIDY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4220-DESCRIBE-SOURCE.
           EVALUATE STLM-SOURCE-CD
               WHEN 'AST'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BCK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'CJG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'DSJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JVY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VEF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEFER CODE CD:
      * AH -> CZ WHEN MATURITY DATE IS PAST
      * CZ -> GM WHEN MATURITY DATE IS PAST
      * GM -> ND WHEN MATURITY DATE IS PAST
      * ND -> UC WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4230-SET-DEFER-CODE.
           EVALUATE STLM-DEFER-CODE-CD
               WHEN 'AH'
                   IF STLM-MATURITY-DATE > 0 AND STLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CZ' TO STLM-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CZ'
                   IF STLM-MATURITY-DATE > 0 AND STLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO STLM-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF STLM-MATURITY-DATE > 0 AND STLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ND' TO STLM-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF STLM-MATURITY-DATE > 0 AND STLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO STLM-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4240-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO STLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4250-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE STLT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4260-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEFERINT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISBURSE YTD.
      *----------------------------------------------------------------*
       4270-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-DISBURSE-YTD ROUNDED =
                   STLM-DEFERINT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-DISBURSE-YTD
           END-IF
           ADD STLM-DISBURSE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4280-COUNT-SEQ.
           IF STLT-SEQ < 9999999
               ADD 1 TO STLT-SEQ
           END-IF
           IF STLT-SEQ NOT < 12
               MOVE 'Y' TO STLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4290-ACCRUE-PAYMENT.
           IF STLT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLT-PAYMENT-AMT * STLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO STLT-DISBURSE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E199 CUST ID REQUIRED
      * E866 SUBSIDY AMT NEGATIVE
      * E486 SUBSIDY AMT OVER 25000
      * E596 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4300-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-CUST-ID = SPACES
                   MOVE 'E199' TO WS-REJECT-CODE
               WHEN STLM-SUBSIDY-AMT < 0
                   MOVE 'E866' TO WS-REJECT-CODE
               WHEN STLM-SUBSIDY-AMT > 25000.00
                   MOVE 'E486' TO WS-REJECT-CODE
               WHEN STLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E596' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4310-CHECK-LAST-PAY.
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
               MOVE 'Y' TO STLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4320-DESCRIBE-REGION.
           EVALUATE STLM-REGION-CD
               WHEN 'C'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4330-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO STLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4340-AGE-LAST-MAINT.
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
                   ADD STLT-DISBURSE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD STLT-DISBURSE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD STLT-DISBURSE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLT-DISBURSE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLT-DISBURSE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT YTD FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4350-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN STLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO STLM-CAPINT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-PREFERRED-FLAG = 'Y'
                   COMPUTE STLM-CAPINT-YTD ROUNDED = STLT-PRINCIPAL-AMT
                       * STLM-PENALTY-PCT / 200
               WHEN STLM-DEFER-CODE-CD = 'AH'
                   MOVE 15.00 TO STLM-CAPINT-YTD
               WHEN STLM-DEFER-CODE-CD = 'CZ'
                   MOVE 7.00 TO STLM-CAPINT-YTD
               WHEN OTHER
                   COMPUTE STLM-CAPINT-YTD ROUNDED = STLT-PRINCIPAL-AMT
                       * STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLM-CAPINT-YTD > 0 AND STLM-CAPINT-YTD < 1.00
               MOVE 1.00 TO STLM-CAPINT-YTD
           END-IF
           IF STLM-CAPINT-YTD > 250.00
               MOVE 250.00 TO STLM-CAPINT-YTD
           END-IF
           ADD STLM-CAPINT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4360-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF STLT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO STLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA BAND TIER FOR THE CAPINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4370-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-BAND-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF STLT-CAPINT-AMT NOT < WS-PRINCIPA-BAND-LOW(WS-TX)
                   MOVE WS-PRINCIPA-BAND-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4380-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = STLT-PAYMENT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = STLT-PAYMENT-AMT - (WS-WORK-AMT * 3)
           COMPUTE STLM-DISBURSE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4390-ACCRUE-PRINCIPAL.
           IF STLM-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-PRINCIPAL-AMT * STLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO STLM-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT FEE ON THE CAPINT YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4400-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN STLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO STLM-CAPINT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE STLM-CAPINT-AMT ROUNDED = STLM-CAPINT-YTD *
                       STLM-DISCOUNT / 200
               WHEN STLM-CYCLE-CD = 'CFJ'
                   MOVE 12.00 TO STLM-CAPINT-AMT
               WHEN STLM-CYCLE-CD = 'KHC'
                   MOVE 7.00 TO STLM-CAPINT-AMT
               WHEN OTHER
                   COMPUTE STLM-CAPINT-AMT ROUNDED = STLM-CAPINT-YTD *
                       STLM-DISCOUNT / 100
           END-EVALUATE
           IF STLM-CAPINT-AMT > 0 AND STLM-CAPINT-AMT < 2.00
               MOVE 2.00 TO STLM-CAPINT-AMT
           END-IF
           IF STLM-CAPINT-AMT > 250.00
               MOVE 250.00 TO STLM-CAPINT-AMT
           END-IF
           ADD STLM-CAPINT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E441 REJECT CD REQUIRED
      * E034 WORK AMT 2 NEGATIVE
      * E214 WORK AMT 2 OVER 5000
      * E164 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4410-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-REJECT-CD = SPACES
                   MOVE 'E441' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E034' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E214' TO WS-REJECT-CODE
               WHEN STLM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E164' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E530 REF NO REQUIRED
      * E847 DEFERINT AMT NEGATIVE
      * E731 DEFERINT AMT OVER 5000
      * E790 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4420-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-REF-NO = SPACES
                   MOVE 'E530' TO WS-REJECT-CODE
               WHEN STLM-DEFERINT-AMT < 0
                   MOVE 'E847' TO WS-REJECT-CODE
               WHEN STLM-DEFERINT-AMT > 5000.00
                   MOVE 'E731' TO WS-REJECT-CODE
               WHEN STLM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E790' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4430-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLT-CHANNEL-CD
                   ADD STLM-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE GRID TIER FOR THE CAPINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4440-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-GRID-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF STLM-CAPINT-AMT NOT < WS-DISBURSE-GRID-LOW(WS-TX)
                   MOVE WS-DISBURSE-GRID-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4450-AGE-EXPIRY.
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
               WHEN WS-DAYS NOT > 180
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4460-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-REGION-CD
                   ADD STLM-DISBURSE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-DISBURSE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4470-FLAG-PRINCIPAL.
           IF STLM-PRINCIPAL-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE STLM-PRINCIPAL-AMT TO WS-EXC-AMOUNT
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
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CAPINT AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4490-ACCRUE-CAPINT.
           IF STLM-CAPINT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-CAPINT-AMT * STLM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO STLT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4500-FLAG-DISBURSE.
           IF STLM-DISBURSE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE STLM-DISBURSE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E730 REJECT CD REQUIRED
      * E294 PRINCIPAL YTD NEGATIVE
      * E704 PRINCIPAL YTD OVER 99999
      * E151 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4510-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-REJECT-CD = SPACES
                   MOVE 'E730' TO WS-REJECT-CODE
               WHEN STLM-PRINCIPAL-YTD < 0
                   MOVE 'E294' TO WS-REJECT-CODE
               WHEN STLM-PRINCIPAL-YTD > 99999.00
                   MOVE 'E704' TO WS-REJECT-CODE
               WHEN STLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E151' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CAPINT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4520-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-SOURCE-CD
                   ADD STLM-CAPINT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-CAPINT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISBURSE FEE ON THE CAPINT YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4530-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN STLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO STLM-DISBURSE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-REVIEW-FLAG = 'Y'
                   COMPUTE STLM-DISBURSE-AMT ROUNDED = STLM-CAPINT-YTD *
                       STLM-FEE-PCT / 200
               WHEN STLM-LOAN-TYPE-CD = 'CS'
                   MOVE 12.00 TO STLM-DISBURSE-AMT
               WHEN STLM-LOAN-TYPE-CD = 'DV'
                   MOVE 5.00 TO STLM-DISBURSE-AMT
               WHEN OTHER
                   COMPUTE STLM-DISBURSE-AMT ROUNDED = STLM-CAPINT-YTD *
                       STLM-FEE-PCT / 100
           END-EVALUATE
           IF STLM-DISBURSE-AMT > 0 AND STLM-DISBURSE-AMT < 5.00
               MOVE 5.00 TO STLM-DISBURSE-AMT
           END-IF
           IF STLM-DISBURSE-AMT > 25.00
               MOVE 25.00 TO STLM-DISBURSE-AMT
           END-IF
           ADD STLM-DISBURSE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SUBSIDY YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4540-FLAG-SUBSIDY.
           IF STLM-SUBSIDY-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE STLM-SUBSIDY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT SCHED TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4550-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-SCHED-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF STLM-PRINCIPAL-AMT NOT < WS-DEFERINT-SCHED-LOW(WS-TX)
                   MOVE WS-DEFERINT-SCHED-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4560-COUNT-PAYMENT.
           IF STLM-PAYMENT-CNT < 9999999
               ADD 1 TO STLM-PAYMENT-CNT
           END-IF
           IF STLM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO STLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4570-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = STLM-PRINCIPAL-YTD / 3
           COMPUTE WS-WORK-AMT-2 = STLM-PRINCIPAL-YTD - (WS-WORK-AMT *
               3)
           COMPUTE STLM-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CAPINT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4580-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLT-CHANNEL-CD
                   ADD STLM-CAPINT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-CAPINT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE DISBURSE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4590-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN STLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO STLM-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE STLM-PRINCIPAL-AMT ROUNDED =
                       STLT-DISBURSE-AMT * STLM-DISCOUNT / 200
               WHEN STLM-LOAN-TYPE-CD = 'CS'
                   MOVE 10.00 TO STLM-PRINCIPAL-AMT
               WHEN STLM-LOAN-TYPE-CD = 'DV'
                   MOVE 15.00 TO STLM-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE STLM-PRINCIPAL-AMT ROUNDED =
                       STLT-DISBURSE-AMT * STLM-DISCOUNT / 100
           END-EVALUATE
           IF STLM-PRINCIPAL-AMT > 0 AND STLM-PRINCIPAL-AMT < 5.00
               MOVE 5.00 TO STLM-PRINCIPAL-AMT
           END-IF
           IF STLM-PRINCIPAL-AMT > 100.00
               MOVE 100.00 TO STLM-PRINCIPAL-AMT
           END-IF
           ADD STLM-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DISBURSE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4600-FLAG-DISBURSE.
           IF STLM-DISBURSE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE STLM-DISBURSE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4610-ACCRUE-PAYMENT.
           IF STLM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-PAYMENT-YTD * STLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO STLM-CAPINT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DEFER CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4620-DESCRIBE-DEFER-CODE.
           EVALUATE STLM-DEFER-CODE-CD
               WHEN 'AH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'CZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ND'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4630-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF STLT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO STLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4640-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4650-AGE-POST.
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
                   ADD STLT-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD STLT-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD STLT-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLT-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLT-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4660-DESCRIBE-SOURCE.
           EVALUATE STLM-SOURCE-CD
               WHEN 'AST'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BCK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'CJG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DSJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JVY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VEF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4670-DESCRIBE-TRAN.
           EVALUATE STLT-TRAN-CD
               WHEN 'DB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'CP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E046 CONTACT REQUIRED
      * E869 PAYMENT AMT NEGATIVE
      * E232 PAYMENT AMT OVER 50000
      * E963 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4680-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-CONTACT = SPACES
                   MOVE 'E046' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-AMT < 0
                   MOVE 'E869' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-AMT > 50000.00
                   MOVE 'E232' TO WS-REJECT-CODE
               WHEN STLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E963' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4690-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF STLM-CLOSE-DATE > 0 AND STLM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLT-CAPINT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD STLT-CAPINT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD STLT-CAPINT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD STLT-CAPINT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLT-CAPINT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4700-COUNT-PAYMENT.
           IF STLM-PAYMENT-CNT < 9999999
               ADD 1 TO STLM-PAYMENT-CNT
           END-IF
           IF STLM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO STLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4710-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = STLT-DISBURSE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = STLT-DISBURSE-AMT - (WS-WORK-AMT * 2)
           COMPUTE STLM-SUBSIDY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4720-ACCRUE-PAYMENT.
           IF STLM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-PAYMENT-YTD * STLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO STLM-CAPINT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * HY -> JZ WHEN LAST STMT DATE IS PAST
      * JZ -> RK WHEN LAST STMT DATE IS PAST
      * RK -> HY WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4730-SET-STATUS.
           EVALUATE STLM-STATUS-CD
               WHEN 'HY'
                   IF STLM-LAST-STMT-DATE > 0 AND STLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO STLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF STLM-LAST-STMT-DATE > 0 AND STLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RK' TO STLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RK'
                   IF STLM-LAST-STMT-DATE > 0 AND STLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HY' TO STLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CAPINT AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4740-LIMIT-CAPINT.
           IF STLT-CAPINT-AMT < 50.00
               MOVE 50.00 TO STLT-CAPINT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLT-CAPINT-AMT > 50000.00
               MOVE 50000.00 TO STLT-CAPINT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEFERINT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       4750-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLT-PAYMENT-AMT ROUNDED =
                   STLM-DEFERINT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLT-PAYMENT-AMT
           END-IF
           ADD STLT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE DEFER CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4760-DESCRIBE-DEFER-CODE.
           EVALUATE STLM-DEFER-CODE-CD
               WHEN 'AH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ND'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY DEFER CODE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4770-ACCUM-DEFER-CODE.
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
      * SPLIT DISBURSE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4780-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = STLM-DISBURSE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = STLM-DISBURSE-YTD - (WS-WORK-AMT * 4)
           COMPUTE STLM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

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
                   MOVE SLVAL-CAPINT-AMT
                     TO STLM-CAPINT-AMT
                   MOVE SLVAL-DISBURSE-YTD
                     TO STLM-DISBURSE-YTD
                   MOVE SLVAL-PREFERRED-FLAG
                     TO STLM-PREFERRED-FLAG
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
           DISPLAY 'SLLMTHE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'SLLMTHE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
