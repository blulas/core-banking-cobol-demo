      *================================================================*
      * PROGRAM  : DRPNOTC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : DIVIDEND REINVESTMENT - CUSTOMER NOTICES. READS THE *
      *            DRP TRANSACTIONS, UPDATES THE DRP MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. BANK HOLDING COMPANY   *
      *            STOCK DIVIDEND REINVESTMENT PLAN                    *
      * INPUT    : DRPMST (KSDS), DRPTRN, BANKCTL                      *
      * OUTPUT   : DRPOUT, DRPNORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, DRELG, DRFEE, DRSTAT    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-05-07 GAF  ORIGINAL                                       *
      * 1991-12-26 TWH  SHARES-AMT NOTICE TEXT PER LEGAL CHG6898       *
      * 1992-04-13 GAF  CASH-PAY ROUNDING - HALF UP REQ2473            *
      * 1996-11-18 CRP  DIVIDEND NOTICE TEXT PER LEGAL PRB6984         *
      * 1999-02-16 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-03-25 CRP  DIVIDEND FEE SCHEDULE UPDATE PRB6880           *
      * 2001-07-16 JLM  AUDIT FINDING - REINVEST CONTROL TOTALS REQ9456*
      * 2001-10-17 CRP  DIVIDEND CUTOFF MOVED REQ5315                  *
      * 2002-02-18 PQR  REINVEST STATUS CODES ADDED REQ5694            *
      * 2002-04-28 JLM  PERFORMANCE - FEE READ AHEAD PRB2329           *
      * 2005-01-23 TWH  FEE NOTICE TEXT PER LEGAL INC2819              *
      * 2006-08-28 BHS  WITHHOLD TABLE EXPANDED PRB5915                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DRPNOTC.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "DRPMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS DRPM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "DRPTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "DRPOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "DRPNORPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY DRPMST.
       FD  TRAN-FILE.
       COPY DRPTRN.
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
      * WS-SHARES-A-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHARES-A-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000425000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000621000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000316000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000015000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016100000000040000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026100000000356000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031100000000236000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033600000000756000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00034600000000744000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00044600000000578500'.
       01  WS-SHARES-A-SCHED-TBL REDEFINES WS-SHARES-A-SCHED-VALUES.
           05  WS-SHARES-A-SCHED-ROW OCCURS 10 TIMES.
               10  WS-SHARES-A-SCHED-LOW   PIC 9(09)V99.
               10  WS-SHARES-A-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DIVIDEND-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DIVIDEND-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000645000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000357000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000322000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013500000000008500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013550000000202500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016050000000246000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017050000000654000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017300000000582500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017800000000811500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017850000000575500'.
       01  WS-DIVIDEND-BAND-TBL REDEFINES WS-DIVIDEND-BAND-VALUES.
           05  WS-DIVIDEND-BAND-ROW OCCURS 10 TIMES.
               10  WS-DIVIDEND-BAND-LOW    PIC 9(09)V99.
               10  WS-DIVIDEND-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-REINVEST-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REINVEST-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000356000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000198000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002000000000108000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002100000000705000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012100000000248000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000422000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000784500'.
       01  WS-REINVEST-GRID-TBL REDEFINES WS-REINVEST-GRID-VALUES.
           05  WS-REINVEST-GRID-ROW OCCURS 7 TIMES.
               10  WS-REINVEST-GRID-LOW    PIC 9(09)V99.
               10  WS-REINVEST-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-WITHHOLD-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-WITHHOLD-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000639000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000690000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000509500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008500000000383000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009500000000373500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009600000000354500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000033500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020600000000789000'.
       01  WS-WITHHOLD-TIER-TBL REDEFINES WS-WITHHOLD-TIER-VALUES.
           05  WS-WITHHOLD-TIER-ROW OCCURS 8 TIMES.
               10  WS-WITHHOLD-TIER-LOW    PIC 9(09)V99.
               10  WS-WITHHOLD-TIER-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  DRPNOTC  DIVIDEND REINVESTMENT'.
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
       COPY DRELGP.
       COPY DRFEEP.
       COPY DRSTATP.

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
               MOVE 'DRPMST' TO ABND-FILE-NAME
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
           MOVE DRPT-KEY TO DRPM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ASSESS-OVERRIDE
               PERFORM 3010-ACCRUE-DISCOUNT
               PERFORM 3020-SPLIT-FEE
               PERFORM 3030-AGE-NOTICE
               PERFORM 3040-COUNT-ITEM
               PERFORM 3050-FLAG-WITHHOLD
               PERFORM 3060-CHECK-NEXT-REVIEW
               PERFORM 3070-ASSESS-OVERRIDE
               PERFORM 3080-SET-PLAN-OPTION
               PERFORM 3090-FLAG-FEE
               PERFORM 3100-ACCUM-PLAN-OPTION
               PERFORM 3110-ACCRUE-PENALTY-PCT
               PERFORM 3120-COUNT-PAYMENT
               PERFORM 3130-DESCRIBE-CHANNEL
               PERFORM 3140-AGE-NEXT-DUE
               PERFORM 3150-FLAG-REINVEST
               PERFORM 3160-PRORATE-DIVIDEND
               PERFORM 3170-ACCUM-HOLDER-TYPE
               PERFORM 3180-PRORATE-REINVEST
               PERFORM 3190-CHECK-LAST-ACT
               PERFORM 3200-CHECK-NEXT-DUE
               PERFORM 3210-ACCUM-CHANNEL
               PERFORM 3220-AGE-NEXT-DUE
               PERFORM 3230-SELECT-DISCOUNT
               PERFORM 3240-PRORATE-REINVEST
               PERFORM 3250-ASSESS-OVERRIDE
               PERFORM 3260-FLAG-FEE
               PERFORM 3270-COUNT-PAYMENT
               PERFORM 3280-SPLIT-DIVIDEND
               PERFORM 3290-AGE-OPEN
               PERFORM 3300-DESCRIBE-REASON
               PERFORM 3310-COUNT-ITEM
               PERFORM 3320-EDIT-MEMO
               PERFORM 3330-SET-TRAN
               PERFORM 3340-AGE-OPEN
               PERFORM 3350-ACCUM-SOURCE
               PERFORM 3360-ACCUM-REASON
               PERFORM 3370-SET-CYCLE
               PERFORM 3380-EDIT-CUST-ID
               PERFORM 3390-DESCRIBE-CHANNEL
               PERFORM 3400-CHECK-NOTICE
               PERFORM 3410-SPLIT-WITHHOLD
               PERFORM 3420-SPLIT-SHARES-AMT
               PERFORM 3430-ACCUM-PLAN-OPTION
               PERFORM 3440-ACCRUE-FEE
               PERFORM 3450-ACCRUE-DIVIDEND
               PERFORM 3460-COUNT-NOTICE
               PERFORM 3470-COUNT-EXCEPT
               PERFORM 3480-ACCUM-CHANNEL
               PERFORM 3490-ACCUM-REASON
               PERFORM 3500-PRORATE-REINVEST
               PERFORM 3510-ACCUM-SOURCE
               PERFORM 3520-SPLIT-FEE
               PERFORM 3530-LIMIT-ITEM
               PERFORM 3540-SELECT-PENALTY-PCT
               PERFORM 3550-FLAG-WITHHOLD
               PERFORM 3560-AGE-POST
               PERFORM 3570-SET-SOURCE
               PERFORM 3580-DESCRIBE-CYCLE
               PERFORM 3590-SPLIT-SHARES-AMT
               PERFORM 3600-SELECT-PENALTY-PCT
               PERFORM 3610-SPLIT-CASH-PAY
               PERFORM 3620-ACCUM-HOLDER-TYPE
               PERFORM 3630-ACCUM-TRAN
               PERFORM 3640-PRORATE-REINVEST
               PERFORM 3650-COUNT-ITEM
               PERFORM 3660-ASSESS-EXEMPT
               PERFORM 3670-PRORATE-FEE
               PERFORM 3680-DESCRIBE-CHANNEL
               PERFORM 3690-ACCRUE-FEE
               PERFORM 3700-AGE-MATURITY
               PERFORM 3710-ASSESS-OVERRIDE
               PERFORM 3720-LIMIT-CASH-PAY
               PERFORM 3730-PRORATE-DIVIDEND
               PERFORM 3740-CHECK-EFF
               PERFORM 3750-AGE-LAST-MAINT
               PERFORM 3760-ASSESS-OVERRIDE
               PERFORM 3770-PRORATE-DIVIDEND
               PERFORM 3780-SET-PLAN-OPTION
               PERFORM 3790-FLAG-SHARES-AMT
               PERFORM 3800-SET-CYCLE
               PERFORM 3810-LIMIT-REINVEST
               PERFORM 3820-DESCRIBE-HOLDER-TYPE
               PERFORM 3830-ACCUM-HOLDER-TYPE
               PERFORM 3840-DESCRIBE-TRAN
               PERFORM 3850-SET-REASON
               PERFORM 3860-CHECK-POST
               PERFORM 3870-ACCUM-CHANNEL
               PERFORM 3880-ASSESS-STOP
               PERFORM 3890-ASSESS-OVERRIDE
               PERFORM 3900-DESCRIBE-HOLDER-TYPE
               PERFORM 3910-DESCRIBE-SOURCE
               PERFORM 3920-ACCUM-TRAN
               PERFORM 3930-DESCRIBE-REASON
               PERFORM 3940-SET-CHANNEL
               PERFORM 3950-FLAG-DIVIDEND
               PERFORM 3960-SPLIT-SHARES-AMT
               PERFORM 3970-EDIT-OPERATOR
               PERFORM 3980-ASSESS-OVERRIDE
               PERFORM 3990-SPLIT-SHARES-AMT
               PERFORM 4000-SPLIT-DIVIDEND
               PERFORM 4010-SPLIT-SHARES-AMT
               PERFORM 4020-FLAG-SHARES-AMT
               PERFORM 4030-ACCRUE-CASH-PAY
               PERFORM 4040-SET-REASON
               PERFORM 4050-DESCRIBE-CHANNEL
               PERFORM 4060-FLAG-SHARES-AMT
               PERFORM 4070-EDIT-DESC
               PERFORM 4080-ACCUM-CYCLE
               PERFORM 4090-FLAG-WITHHOLD
               PERFORM 4100-SET-REASON
               PERFORM 4110-ACCRUE-SHARES-AMT
               PERFORM 4120-DESCRIBE-HOLDER-TYPE
               PERFORM 4130-CHECK-NEXT-REVIEW
               PERFORM 4140-ACCRUE-REINVEST
               PERFORM 4150-FLAG-SHARES-AMT
               PERFORM 4160-ASSESS-OVERRIDE
               PERFORM 4170-DESCRIBE-CHANNEL
               PERFORM 4180-PRORATE-SHARES-AMT
               PERFORM 4190-AGE-NOTICE
               PERFORM 4200-EDIT-DESC
               PERFORM 4210-PRORATE-SHARES-AMT
               PERFORM 4220-PRORATE-DIVIDEND
               PERFORM 4230-AGE-LAST-ACT
               PERFORM 4240-DESCRIBE-HOLDER-TYPE
               PERFORM 4250-SET-HOLDER-TYPE
               PERFORM 4260-COUNT-ITEM
               PERFORM 4270-EDIT-CONTACT
               PERFORM 4280-PRORATE-REINVEST
               PERFORM 4290-CHECK-LAST-ACT
               PERFORM 4300-COUNT-EXCEPT
               PERFORM 4310-LIMIT-REINVEST
               PERFORM 4320-PRORATE-CASH-PAY
               PERFORM 4330-SELECT-DISCOUNT
               PERFORM 4340-CHECK-LAST-MAINT
               PERFORM 4350-ACCRUE-FEE
               PERFORM 4360-LIMIT-REINVEST
               PERFORM 4370-CHECK-NOTICE
               PERFORM 4380-ACCUM-CYCLE
               PERFORM 4390-ASSESS-EXEMPT
               PERFORM 4400-ASSESS-SUSPEND
               PERFORM 4410-LIMIT-CASH-PAY
               PERFORM 4420-COUNT-PAYMENT
               PERFORM 4430-ACCRUE-DIVIDEND
               PERFORM 4440-ACCRUE-DIVIDEND
               PERFORM 4450-DESCRIBE-PLAN-OPTION
               PERFORM 4460-AGE-POST
               PERFORM 4470-DESCRIBE-CYCLE
               PERFORM 4480-LIMIT-CASH-PAY
               PERFORM 4490-ACCRUE-REINVEST
               PERFORM 4500-COUNT-PAYMENT
               PERFORM 4510-CHECK-NEXT-DUE
               PERFORM 4520-CHECK-POST
               PERFORM 4530-CHECK-MATURITY
               PERFORM 4540-CHECK-NEXT-REVIEW
               PERFORM 4550-SELECT-PENALTY-PCT
               PERFORM 4560-AGE-EFFECTIVE
               PERFORM 4570-SPLIT-CASH-PAY
               PERFORM 4580-AGE-MATURITY
               PERFORM 4590-DESCRIBE-HOLDER-TYPE
               PERFORM 4600-LIMIT-REINVEST
               PERFORM 4610-EDIT-MEMO
               PERFORM 4620-ACCUM-HOLDER-TYPE
               PERFORM 4630-ACCUM-CYCLE
               PERFORM 4640-COUNT-ITEM
               PERFORM 4650-ACCRUE-REINVEST
               PERFORM 4660-FLAG-WITHHOLD
               PERFORM 4670-EDIT-MEMO
               PERFORM 4680-SET-SOURCE
               PERFORM 4690-ACCUM-PLAN-OPTION
               PERFORM 4700-COUNT-ITEM
               PERFORM 4710-EDIT-ADDR
               PERFORM 4720-PRORATE-SHARES-AMT
               PERFORM 4730-ACCRUE-DIVIDEND
               PERFORM 4740-SET-PLAN-OPTION
               PERFORM 4750-LIMIT-FEE
               PERFORM 4760-SPLIT-SHARES-AMT
               PERFORM 4770-DESCRIBE-CYCLE
               PERFORM 4780-AGE-LAST-ACT
               PERFORM 4790-FLAG-DIVIDEND
               PERFORM 4800-CHECK-OPEN
               PERFORM 4810-CHECK-OPEN
               PERFORM 4820-SPLIT-SHARES-AMT
               PERFORM 4830-PRORATE-DIVIDEND
               PERFORM 4840-DESCRIBE-CYCLE
               PERFORM 4850-DESCRIBE-TRAN
               PERFORM 4860-ACCUM-CHANNEL
               PERFORM 4870-CHECK-EFFECTIVE
               PERFORM 4880-CHECK-EFFECTIVE
               PERFORM 7020-CALL-DRELG
               PERFORM 7030-CALL-DRFEE
               PERFORM 7000-CALL-DRSTAT
               REWRITE DRP-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'DRPMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM DRP-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND YTD FEE ON THE REINVEST AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3000-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRPM-DIVIDEND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-EXEMPT-FLAG = 'Y'
                   COMPUTE DRPM-DIVIDEND-YTD ROUNDED = DRPT-REINVEST-AMT
                       * DRPM-PENALTY-PCT / 200
               WHEN DRPM-PLAN-OPTION-CD = 'E'
                   MOVE 12.00 TO DRPM-DIVIDEND-YTD
               WHEN DRPM-PLAN-OPTION-CD = 'R'
                   MOVE 10.00 TO DRPM-DIVIDEND-YTD
               WHEN OTHER
                   COMPUTE DRPM-DIVIDEND-YTD ROUNDED = DRPT-REINVEST-AMT
                       * DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-DIVIDEND-YTD > 0 AND DRPM-DIVIDEND-YTD < 1.00
               MOVE 1.00 TO DRPM-DIVIDEND-YTD
           END-IF
           IF DRPM-DIVIDEND-YTD > 75.00
               MOVE 75.00 TO DRPM-DIVIDEND-YTD
           END-IF
           ADD DRPM-DIVIDEND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * DRPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-CASH-PAY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3020-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = DRPM-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-FEE-YTD - (WS-WORK-AMT * 12)
           COMPUTE DRPT-REINVEST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3030-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 12
               MOVE 'Y' TO DRPM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3050-FLAG-WITHHOLD.
           IF DRPM-WITHHOLD-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-WITHHOLD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3060-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND FEE ON THE REINVEST YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3070-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRPM-DIVIDEND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   COMPUTE DRPM-DIVIDEND-AMT ROUNDED = DRPM-REINVEST-YTD
                       * DRPM-PENALTY-PCT / 200
               WHEN DRPT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO DRPM-DIVIDEND-AMT
               WHEN DRPT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO DRPM-DIVIDEND-AMT
               WHEN OTHER
                   COMPUTE DRPM-DIVIDEND-AMT ROUNDED = DRPM-REINVEST-YTD
                       * DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-DIVIDEND-AMT > 0 AND DRPM-DIVIDEND-AMT < 1.00
               MOVE 1.00 TO DRPM-DIVIDEND-AMT
           END-IF
           IF DRPM-DIVIDEND-AMT > 25.00
               MOVE 25.00 TO DRPM-DIVIDEND-AMT
           END-IF
           ADD DRPM-DIVIDEND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN OPTION CD:
      * E -> R WHEN NEXT DUE DATE IS PAST
      * R -> T WHEN NEXT DUE DATE IS PAST
      * T -> Z WHEN NEXT DUE DATE IS PAST
      * Z -> E WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3080-SET-PLAN-OPTION.
           EVALUATE DRPM-PLAN-OPTION-CD
               WHEN 'E'
                   IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3090-FLAG-FEE.
           IF DRPM-FEE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH PAY AMT BY PLAN OPTION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-PLAN-OPTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-PLAN-OPTION-CD
                   ADD DRPM-CASH-PAY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-PLAN-OPTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-CASH-PAY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3110-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * DRPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-SHARES-AMT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3120-COUNT-PAYMENT.
           IF DRPM-PAYMENT-CNT < 9999999
               ADD 1 TO DRPM-PAYMENT-CNT
           END-IF
           IF DRPM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3130-DESCRIBE-CHANNEL.
           EVALUATE DRPM-CHANNEL-CD
               WHEN 'RM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3140-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REINVEST YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3150-FLAG-REINVEST.
           IF DRPM-REINVEST-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-REINVEST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DIVIDEND AMT.
      *----------------------------------------------------------------*
       3160-PRORATE-DIVIDEND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-DIVIDEND-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-DIVIDEND-AMT
           END-IF
           ADD DRPM-DIVIDEND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY HOLDER TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-HOLDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-HOLDER-TYPE-CD
                   ADD DRPT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-HOLDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DIVIDEND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REINVEST AMT.
      *----------------------------------------------------------------*
       3180-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPT-REINVEST-AMT ROUNDED =
                   DRPM-DIVIDEND-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPT-REINVEST-AMT
           END-IF
           ADD DRPT-REINVEST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3200-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3210-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-CHANNEL-CD
                   ADD DRPM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3220-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE WITHHOLD TIER TIER FOR THE SHARES AMT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3230-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-WITHHOLD-TIER-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF DRPM-SHARES-AMT-AMT NOT < WS-WITHHOLD-TIER-LOW(WS-TX)
                   MOVE WS-WITHHOLD-TIER-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REINVEST YTD.
      *----------------------------------------------------------------*
       3240-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-REINVEST-YTD ROUNDED =
                   DRPM-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-REINVEST-YTD
           END-IF
           ADD DRPM-REINVEST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WITHHOLD YTD FEE ON THE REINVEST AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3250-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRPM-WITHHOLD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   COMPUTE DRPM-WITHHOLD-YTD ROUNDED = DRPM-REINVEST-AMT
                       * DRPM-DISCOUNT / 200
               WHEN DRPT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO DRPM-WITHHOLD-YTD
               WHEN DRPT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO DRPM-WITHHOLD-YTD
               WHEN OTHER
                   COMPUTE DRPM-WITHHOLD-YTD ROUNDED = DRPM-REINVEST-AMT
                       * DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPM-WITHHOLD-YTD > 0 AND DRPM-WITHHOLD-YTD < 2.00
               MOVE 2.00 TO DRPM-WITHHOLD-YTD
           END-IF
           IF DRPM-WITHHOLD-YTD > 250.00
               MOVE 250.00 TO DRPM-WITHHOLD-YTD
           END-IF
           ADD DRPM-WITHHOLD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3260-FLAG-FEE.
           IF DRPT-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3270-COUNT-PAYMENT.
           IF DRPM-PAYMENT-CNT < 9999999
               ADD 1 TO DRPM-PAYMENT-CNT
           END-IF
           IF DRPM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO DRPM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DIVIDEND AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3280-SPLIT-DIVIDEND.
           COMPUTE WS-WORK-AMT = DRPT-DIVIDEND-AMT / 3
           COMPUTE WS-WORK-AMT-2 = DRPT-DIVIDEND-AMT - (WS-WORK-AMT * 3)
           COMPUTE DRPM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3290-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3300-DESCRIBE-REASON.
           EVALUATE DRPM-REASON-CD
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'G'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'J'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'K'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Y'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3310-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 10
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E028 MEMO REQUIRED
      * E604 FEE AMT NEGATIVE
      * E615 FEE AMT OVER 25000
      * E323 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3320-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-MEMO = SPACES
                   MOVE 'E028' TO WS-REJECT-CODE
               WHEN DRPM-FEE-AMT < 0
                   MOVE 'E604' TO WS-REJECT-CODE
               WHEN DRPM-FEE-AMT > 25000.00
                   MOVE 'E615' TO WS-REJECT-CODE
               WHEN DRPM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E323' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DV -> RI WHEN LAST ACT DATE IS PAST
      * RI -> CP WHEN LAST ACT DATE IS PAST
      * CP -> WH WHEN LAST ACT DATE IS PAST
      * WH -> SL WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3330-SET-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RI' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RI'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CP'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WH' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WH'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SL' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3340-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DIVIDEND AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3350-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-SOURCE-CD
                   ADD DRPM-DIVIDEND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-DIVIDEND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHARES AMT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3360-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-REASON-CD
                   ADD DRPM-SHARES-AMT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-SHARES-AMT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DE -> FB WHEN LAST MAINT DATE IS PAST
      * FB -> PC WHEN LAST MAINT DATE IS PAST
      * PC -> WY WHEN LAST MAINT DATE IS PAST
      * WY -> XD WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3370-SET-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FB' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PC' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WY' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WY'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XD' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E646 CUST ID REQUIRED
      * E149 CASH PAY AMT NEGATIVE
      * E377 CASH PAY AMT OVER 99999
      * E317 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3380-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CUST-ID = SPACES
                   MOVE 'E646' TO WS-REJECT-CODE
               WHEN DRPM-CASH-PAY-AMT < 0
                   MOVE 'E149' TO WS-REJECT-CODE
               WHEN DRPM-CASH-PAY-AMT > 99999.00
                   MOVE 'E377' TO WS-REJECT-CODE
               WHEN DRPM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E317' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3390-DESCRIBE-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3400-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WITHHOLD AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-WITHHOLD.
           COMPUTE WS-WORK-AMT = DRPM-WITHHOLD-AMT / 2
           COMPUTE WS-WORK-AMT-2 = DRPM-WITHHOLD-AMT - (WS-WORK-AMT * 2)
           COMPUTE DRPM-CASH-PAY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3420-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE DRPM-SHARES-AMT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DIVIDEND AMT BY PLAN OPTION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-PLAN-OPTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-PLAN-OPTION-CD
                   ADD DRPT-DIVIDEND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-PLAN-OPTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPT-DIVIDEND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3440-ACCRUE-FEE.
           IF DRPM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-FEE-YTD * DRPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO DRPM-WITHHOLD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DIVIDEND AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3450-ACCRUE-DIVIDEND.
           IF DRPM-DIVIDEND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-DIVIDEND-AMT * DRPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-WITHHOLD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3460-COUNT-NOTICE.
           IF DRPM-NOTICE-CNT < 9999999
               ADD 1 TO DRPM-NOTICE-CNT
           END-IF
           IF DRPM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3470-COUNT-EXCEPT.
           IF DRPM-EXCEPT-CNT < 9999999
               ADD 1 TO DRPM-EXCEPT-CNT
           END-IF
           IF DRPM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3480-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3490-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-REASON-CD
                   ADD DRPM-WITHHOLD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-WITHHOLD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DIVIDEND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REINVEST AMT.
      *----------------------------------------------------------------*
       3500-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-REINVEST-AMT ROUNDED =
                   DRPM-DIVIDEND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-REINVEST-AMT
           END-IF
           ADD DRPM-REINVEST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE DIVIDEND YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3510-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-SOURCE-CD
                   ADD DRPM-DIVIDEND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-DIVIDEND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3520-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = DRPM-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-FEE-YTD - (WS-WORK-AMT * 12)
           COMPUTE DRPM-REINVEST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3530-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REINVEST GRID TIER FOR THE DIVIDEND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3540-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REINVEST-GRID-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF DRPT-DIVIDEND-AMT NOT < WS-REINVEST-GRID-LOW(WS-TX)
                   MOVE WS-REINVEST-GRID-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3550-FLAG-WITHHOLD.
           IF DRPM-WITHHOLD-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-WITHHOLD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3560-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BT -> FL WHEN OPEN DATE IS PAST
      * FL -> NK WHEN OPEN DATE IS PAST
      * NK -> YC WHEN OPEN DATE IS PAST
      * YC -> BT WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3570-SET-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NK' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NK'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3580-DESCRIBE-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3590-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPM-SHARES-AMT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = DRPM-SHARES-AMT-AMT - (WS-WORK-AMT *
               3)
           COMPUTE DRPT-DIVIDEND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WITHHOLD TIER TIER FOR THE DIVIDEND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3600-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WITHHOLD-TIER-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF DRPT-DIVIDEND-AMT NOT < WS-WITHHOLD-TIER-LOW(WS-TX)
                   MOVE WS-WITHHOLD-TIER-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH PAY YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3610-SPLIT-CASH-PAY.
           COMPUTE WS-WORK-AMT = DRPM-CASH-PAY-YTD / 4
           COMPUTE WS-WORK-AMT-2 = DRPM-CASH-PAY-YTD - (WS-WORK-AMT * 4)
           COMPUTE DRPM-WITHHOLD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH PAY YTD BY HOLDER TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3620-ACCUM-HOLDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-HOLDER-TYPE-CD
                   ADD DRPM-CASH-PAY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-HOLDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-CASH-PAY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3630-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-TRAN-CD
                   ADD DRPM-REINVEST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-REINVEST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WITHHOLD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REINVEST AMT.
      *----------------------------------------------------------------*
       3640-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-REINVEST-AMT ROUNDED =
                   DRPM-WITHHOLD-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-REINVEST-AMT
           END-IF
           ADD DRPM-REINVEST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 6
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHARES YTD FEE ON THE SHARES AMT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3660-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN DRPM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO DRPM-SHARES-AMT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE DRPM-SHARES-AMT-YTD ROUNDED =
                       DRPT-SHARES-AMT-AMT * DRPM-RATE / 200
               WHEN DRPM-HOLDER-TYPE-CD = 'C'
                   MOVE 15.00 TO DRPM-SHARES-AMT-YTD
               WHEN DRPM-HOLDER-TYPE-CD = 'H'
                   MOVE 10.00 TO DRPM-SHARES-AMT-YTD
               WHEN OTHER
                   COMPUTE DRPM-SHARES-AMT-YTD ROUNDED =
                       DRPT-SHARES-AMT-AMT * DRPM-RATE / 100
           END-EVALUATE
           IF DRPM-SHARES-AMT-YTD > 0 AND DRPM-SHARES-AMT-YTD < 1.00
               MOVE 1.00 TO DRPM-SHARES-AMT-YTD
           END-IF
           IF DRPM-SHARES-AMT-YTD > 75.00
               MOVE 75.00 TO DRPM-SHARES-AMT-YTD
           END-IF
           ADD DRPM-SHARES-AMT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WITHHOLD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3670-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-FEE-YTD ROUNDED =
                   DRPM-WITHHOLD-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-FEE-YTD
           END-IF
           ADD DRPM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3680-DESCRIBE-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3690-ACCRUE-FEE.
           IF DRPT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPT-FEE-AMT * DRPM-RATE / 100 / 360
               ADD WS-WORK-AMT TO DRPM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND FEE ON THE WITHHOLD YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3710-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRPT-DIVIDEND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE DRPT-DIVIDEND-AMT ROUNDED = DRPM-WITHHOLD-YTD
                       * DRPM-PENALTY-PCT / 200
               WHEN DRPM-SOURCE-CD = 'BT'
                   MOVE 5.00 TO DRPT-DIVIDEND-AMT
               WHEN DRPM-SOURCE-CD = 'FL'
                   MOVE 15.00 TO DRPT-DIVIDEND-AMT
               WHEN OTHER
                   COMPUTE DRPT-DIVIDEND-AMT ROUNDED = DRPM-WITHHOLD-YTD
                       * DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPT-DIVIDEND-AMT > 0 AND DRPT-DIVIDEND-AMT < 1.00
               MOVE 1.00 TO DRPT-DIVIDEND-AMT
           END-IF
           IF DRPT-DIVIDEND-AMT > 50.00
               MOVE 50.00 TO DRPT-DIVIDEND-AMT
           END-IF
           ADD DRPT-DIVIDEND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CASH PAY AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3720-LIMIT-CASH-PAY.
           IF DRPM-CASH-PAY-AMT < 50.00
               MOVE 50.00 TO DRPM-CASH-PAY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-CASH-PAY-AMT > 2500.00
               MOVE 2500.00 TO DRPM-CASH-PAY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHARES AMT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DIVIDEND AMT.
      *----------------------------------------------------------------*
       3730-PRORATE-DIVIDEND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-DIVIDEND-AMT ROUNDED =
                   DRPM-SHARES-AMT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-DIVIDEND-AMT
           END-IF
           ADD DRPM-DIVIDEND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3740-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF DRPT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO DRPM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3750-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SHARES YTD FEE ON THE REINVEST AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3760-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRPM-SHARES-AMT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE DRPM-SHARES-AMT-YTD ROUNDED =
                       DRPT-REINVEST-AMT * DRPM-RATE / 200
               WHEN DRPT-TRAN-CD = 'DV'
                   MOVE 7.00 TO DRPM-SHARES-AMT-YTD
               WHEN DRPT-TRAN-CD = 'RI'
                   MOVE 5.00 TO DRPM-SHARES-AMT-YTD
               WHEN OTHER
                   COMPUTE DRPM-SHARES-AMT-YTD ROUNDED =
                       DRPT-REINVEST-AMT * DRPM-RATE / 100
           END-EVALUATE
           IF DRPM-SHARES-AMT-YTD > 0 AND DRPM-SHARES-AMT-YTD < 1.00
               MOVE 1.00 TO DRPM-SHARES-AMT-YTD
           END-IF
           IF DRPM-SHARES-AMT-YTD > 75.00
               MOVE 75.00 TO DRPM-SHARES-AMT-YTD
           END-IF
           ADD DRPM-SHARES-AMT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DIVIDEND YTD.
      *----------------------------------------------------------------*
       3770-PRORATE-DIVIDEND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-DIVIDEND-YTD ROUNDED =
                   DRPT-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-DIVIDEND-YTD
           END-IF
           ADD DRPM-DIVIDEND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN OPTION CD:
      * E -> R WHEN OPEN DATE IS PAST
      * R -> T WHEN OPEN DATE IS PAST
      * T -> Z WHEN OPEN DATE IS PAST
      * Z -> E WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3780-SET-PLAN-OPTION.
           EVALUATE DRPM-PLAN-OPTION-CD
               WHEN 'E'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHARES AMT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3790-FLAG-SHARES-AMT.
           IF DRPM-SHARES-AMT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-SHARES-AMT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DE -> FB WHEN LAST ACT DATE IS PAST
      * FB -> PC WHEN LAST ACT DATE IS PAST
      * PC -> WY WHEN LAST ACT DATE IS PAST
      * WY -> XD WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3800-SET-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WY'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REINVEST YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3810-LIMIT-REINVEST.
           IF DRPM-REINVEST-YTD < 10.00
               MOVE 10.00 TO DRPM-REINVEST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-REINVEST-YTD > 5000.00
               MOVE 5000.00 TO DRPM-REINVEST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE HOLDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3820-DESCRIBE-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'K'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'L'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST AMT BY HOLDER TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3830-ACCUM-HOLDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-HOLDER-TYPE-CD
                   ADD DRPM-REINVEST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-HOLDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-REINVEST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3840-DESCRIBE-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RI'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> G WHEN NEXT REVIEW DATE IS PAST
      * G -> J WHEN NEXT REVIEW DATE IS PAST
      * J -> K WHEN NEXT REVIEW DATE IS PAST
      * K -> L WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3850-SET-REASON.
           EVALUATE DRPM-REASON-CD
               WHEN 'B'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'G' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'J' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'K' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'L' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3860-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF DRPT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO DRPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3870-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CHANNEL-CD
                   ADD DRPM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHARES FEE ON THE SHARES AMT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3880-ASSESS-STOP.
           EVALUATE TRUE
               WHEN DRPM-STOP-FLAG = 'Y'
                   MOVE ZERO TO DRPT-SHARES-AMT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   COMPUTE DRPT-SHARES-AMT-AMT ROUNDED =
                       DRPM-SHARES-AMT-AMT * DRPM-PENALTY-PCT / 200
               WHEN DRPT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO DRPT-SHARES-AMT-AMT
               WHEN DRPT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO DRPT-SHARES-AMT-AMT
               WHEN OTHER
                   COMPUTE DRPT-SHARES-AMT-AMT ROUNDED =
                       DRPM-SHARES-AMT-AMT * DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPT-SHARES-AMT-AMT > 0 AND DRPT-SHARES-AMT-AMT < 10.00
               MOVE 10.00 TO DRPT-SHARES-AMT-AMT
           END-IF
           IF DRPT-SHARES-AMT-AMT > 25.00
               MOVE 25.00 TO DRPT-SHARES-AMT-AMT
           END-IF
           ADD DRPT-SHARES-AMT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND YTD FEE ON THE WITHHOLD AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3890-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRPM-DIVIDEND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   COMPUTE DRPM-DIVIDEND-YTD ROUNDED = DRPM-WITHHOLD-AMT
                       * DRPM-PENALTY-PCT / 200
               WHEN DRPM-CYCLE-CD = 'DE'
                   MOVE 10.00 TO DRPM-DIVIDEND-YTD
               WHEN DRPM-CYCLE-CD = 'FB'
                   MOVE 5.00 TO DRPM-DIVIDEND-YTD
               WHEN OTHER
                   COMPUTE DRPM-DIVIDEND-YTD ROUNDED = DRPM-WITHHOLD-AMT
                       * DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-DIVIDEND-YTD > 0 AND DRPM-DIVIDEND-YTD < 10.00
               MOVE 10.00 TO DRPM-DIVIDEND-YTD
           END-IF
           IF DRPM-DIVIDEND-YTD > 50.00
               MOVE 50.00 TO DRPM-DIVIDEND-YTD
           END-IF
           ADD DRPM-DIVIDEND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE HOLDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3900-DESCRIBE-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'K'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'L'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3910-DESCRIBE-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3920-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-TRAN-CD
                   ADD DRPT-REINVEST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPT-REINVEST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3930-DESCRIBE-REASON.
           EVALUATE DRPM-REASON-CD
               WHEN 'B'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'G'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'J'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'K'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST MAINT DATE IS PAST
      * BAT -> MAL WHEN LAST MAINT DATE IS PAST
      * MAL -> TEL WHEN LAST MAINT DATE IS PAST
      * TEL -> ONL WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3940-SET-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DIVIDEND AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3950-FLAG-DIVIDEND.
           IF DRPM-DIVIDEND-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-DIVIDEND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPM-SHARES-AMT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = DRPM-SHARES-AMT-YTD - (WS-WORK-AMT *
               2)
           COMPUTE DRPM-WITHHOLD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E304 OPERATOR REQUIRED
      * E537 WORK AMT 2 NEGATIVE
      * E215 WORK AMT 2 OVER 1000000
      * E290 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3970-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-OPERATOR = SPACES
                   MOVE 'E304' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E537' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E215' TO WS-REJECT-CODE
               WHEN DRPM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E290' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE REINVEST AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3980-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRPM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   COMPUTE DRPM-FEE-YTD ROUNDED = DRPT-REINVEST-AMT *
                       DRPM-PENALTY-PCT / 200
               WHEN DRPM-HOLDER-TYPE-CD = 'C'
                   MOVE 3.00 TO DRPM-FEE-YTD
               WHEN DRPM-HOLDER-TYPE-CD = 'H'
                   MOVE 7.00 TO DRPM-FEE-YTD
               WHEN OTHER
                   COMPUTE DRPM-FEE-YTD ROUNDED = DRPT-REINVEST-AMT *
                       DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-FEE-YTD > 0 AND DRPM-FEE-YTD < 5.00
               MOVE 5.00 TO DRPM-FEE-YTD
           END-IF
           IF DRPM-FEE-YTD > 50.00
               MOVE 50.00 TO DRPM-FEE-YTD
           END-IF
           ADD DRPM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3990-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPM-SHARES-AMT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = DRPM-SHARES-AMT-YTD - (WS-WORK-AMT *
               4)
           COMPUTE DRPM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DIVIDEND AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4000-SPLIT-DIVIDEND.
           COMPUTE WS-WORK-AMT = DRPM-DIVIDEND-AMT / 3
           COMPUTE WS-WORK-AMT-2 = DRPM-DIVIDEND-AMT - (WS-WORK-AMT * 3)
           COMPUTE DRPM-REINVEST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4010-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE DRPM-SHARES-AMT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHARES AMT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4020-FLAG-SHARES-AMT.
           IF DRPT-SHARES-AMT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE DRPT-SHARES-AMT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH PAY YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4030-ACCRUE-CASH-PAY.
           IF DRPM-CASH-PAY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-CASH-PAY-YTD * DRPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO DRPM-WITHHOLD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> G WHEN POST DATE IS PAST
      * G -> J WHEN POST DATE IS PAST
      * J -> K WHEN POST DATE IS PAST
      * K -> L WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4040-SET-REASON.
           EVALUATE DRPM-REASON-CD
               WHEN 'B'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4050-DESCRIBE-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHARES AMT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4060-FLAG-SHARES-AMT.
           IF DRPM-SHARES-AMT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-SHARES-AMT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E169 DESC REQUIRED
      * E571 WORK AMT NEGATIVE
      * E238 WORK AMT OVER 25000
      * E626 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4070-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-DESC = SPACES
                   MOVE 'E169' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E571' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E238' TO WS-REJECT-CODE
               WHEN DRPM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E626' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4080-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CYCLE-CD
                   ADD DRPM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4090-FLAG-WITHHOLD.
           IF DRPM-WITHHOLD-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-WITHHOLD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> G WHEN NEXT REVIEW DATE IS PAST
      * G -> J WHEN NEXT REVIEW DATE IS PAST
      * J -> K WHEN NEXT REVIEW DATE IS PAST
      * K -> L WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4100-SET-REASON.
           EVALUATE DRPM-REASON-CD
               WHEN 'B'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'G' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'J' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'K' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'L' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHARES AMT AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4110-ACCRUE-SHARES-AMT.
           IF DRPT-SHARES-AMT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPT-SHARES-AMT-AMT * DRPM-RATE / 100 / 365
               ADD WS-WORK-AMT TO DRPM-REINVEST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE HOLDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4120-DESCRIBE-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'M'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'X'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4130-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REINVEST AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4140-ACCRUE-REINVEST.
           IF DRPM-REINVEST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-REINVEST-AMT * DRPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRPT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHARES AMT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4150-FLAG-SHARES-AMT.
           IF DRPT-SHARES-AMT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPT-SHARES-AMT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND FEE ON THE REINVEST YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4160-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRPT-DIVIDEND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-STOP-FLAG = 'Y'
                   COMPUTE DRPT-DIVIDEND-AMT ROUNDED = DRPM-REINVEST-YTD
                       * DRPM-PENALTY-PCT / 200
               WHEN DRPM-HOLDER-TYPE-CD = 'C'
                   MOVE 7.00 TO DRPT-DIVIDEND-AMT
               WHEN DRPM-HOLDER-TYPE-CD = 'H'
                   MOVE 7.00 TO DRPT-DIVIDEND-AMT
               WHEN OTHER
                   COMPUTE DRPT-DIVIDEND-AMT ROUNDED = DRPM-REINVEST-YTD
                       * DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPT-DIVIDEND-AMT > 0 AND DRPT-DIVIDEND-AMT < 1.00
               MOVE 1.00 TO DRPT-DIVIDEND-AMT
           END-IF
           IF DRPT-DIVIDEND-AMT > 250.00
               MOVE 250.00 TO DRPT-DIVIDEND-AMT
           END-IF
           ADD DRPT-DIVIDEND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4170-DESCRIBE-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DIVIDEND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHARES AMT AMT.
      *----------------------------------------------------------------*
       4180-PRORATE-SHARES-AMT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-SHARES-AMT-AMT ROUNDED =
                   DRPM-DIVIDEND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-SHARES-AMT-AMT
           END-IF
           ADD DRPM-SHARES-AMT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4190-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E351 DESC REQUIRED
      * E718 CASH PAY YTD NEGATIVE
      * E989 CASH PAY YTD OVER 250000
      * E744 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4200-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-DESC = SPACES
                   MOVE 'E351' TO WS-REJECT-CODE
               WHEN DRPM-CASH-PAY-YTD < 0
                   MOVE 'E718' TO WS-REJECT-CODE
               WHEN DRPM-CASH-PAY-YTD > 250000.00
                   MOVE 'E989' TO WS-REJECT-CODE
               WHEN DRPT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E744' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WITHHOLD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHARES AMT AMT.
      *----------------------------------------------------------------*
       4210-PRORATE-SHARES-AMT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPT-SHARES-AMT-AMT ROUNDED =
                   DRPM-WITHHOLD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPT-SHARES-AMT-AMT
           END-IF
           ADD DRPT-SHARES-AMT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DIVIDEND AMT.
      *----------------------------------------------------------------*
       4220-PRORATE-DIVIDEND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPT-DIVIDEND-AMT ROUNDED =
                   DRPM-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPT-DIVIDEND-AMT
           END-IF
           ADD DRPT-DIVIDEND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4230-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE HOLDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4240-DESCRIBE-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'K'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR HOLDER TYPE CD:
      * C -> H WHEN EFFECTIVE DATE IS PAST
      * H -> K WHEN EFFECTIVE DATE IS PAST
      * K -> L WHEN EFFECTIVE DATE IS PAST
      * L -> M WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4250-SET-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4260-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 10
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E461 CONTACT REQUIRED
      * E305 CASH PAY YTD NEGATIVE
      * E001 CASH PAY YTD OVER 250000
      * E404 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4270-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CONTACT = SPACES
                   MOVE 'E461' TO WS-REJECT-CODE
               WHEN DRPM-CASH-PAY-YTD < 0
                   MOVE 'E305' TO WS-REJECT-CODE
               WHEN DRPM-CASH-PAY-YTD > 250000.00
                   MOVE 'E001' TO WS-REJECT-CODE
               WHEN DRPM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E404' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REINVEST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4280-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   DRPM-REINVEST-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4290-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4300-COUNT-EXCEPT.
           IF DRPM-EXCEPT-CNT < 9999999
               ADD 1 TO DRPM-EXCEPT-CNT
           END-IF
           IF DRPM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO DRPM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4310-LIMIT-REINVEST.
           IF DRPM-REINVEST-AMT < 0.00
               MOVE 0.00 TO DRPM-REINVEST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-REINVEST-AMT > 50000.00
               MOVE 50000.00 TO DRPM-REINVEST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REINVEST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH PAY YTD.
      *----------------------------------------------------------------*
       4320-PRORATE-CASH-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-CASH-PAY-YTD ROUNDED =
                   DRPM-REINVEST-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-CASH-PAY-YTD
           END-IF
           ADD DRPM-CASH-PAY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE WITHHOLD TIER TIER FOR THE WITHHOLD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4330-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-WITHHOLD-TIER-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF DRPM-WITHHOLD-YTD NOT < WS-WITHHOLD-TIER-LOW(WS-TX)
                   MOVE WS-WITHHOLD-TIER-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4340-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4350-ACCRUE-FEE.
           IF DRPM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-FEE-YTD * DRPM-RATE / 100 / 365
               ADD WS-WORK-AMT TO DRPM-CASH-PAY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4360-LIMIT-REINVEST.
           IF DRPT-REINVEST-AMT < 0.00
               MOVE 0.00 TO DRPT-REINVEST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPT-REINVEST-AMT > 10000.00
               MOVE 10000.00 TO DRPT-REINVEST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4370-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHARES AMT YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4380-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CYCLE-CD
                   ADD DRPM-SHARES-AMT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-SHARES-AMT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH PAY YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4390-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN DRPM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO DRPM-CASH-PAY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-STOP-FLAG = 'Y'
                   COMPUTE DRPM-CASH-PAY-YTD ROUNDED = WS-WORK-AMT-2 *
                       DRPM-PENALTY-PCT / 200
               WHEN DRPM-CHANNEL-CD = 'RM'
                   MOVE 10.00 TO DRPM-CASH-PAY-YTD
               WHEN DRPM-CHANNEL-CD = 'VP'
                   MOVE 7.00 TO DRPM-CASH-PAY-YTD
               WHEN OTHER
                   COMPUTE DRPM-CASH-PAY-YTD ROUNDED = WS-WORK-AMT-2 *
                       DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-CASH-PAY-YTD > 0 AND DRPM-CASH-PAY-YTD < 10.00
               MOVE 10.00 TO DRPM-CASH-PAY-YTD
           END-IF
           IF DRPM-CASH-PAY-YTD > 50.00
               MOVE 50.00 TO DRPM-CASH-PAY-YTD
           END-IF
           ADD DRPM-CASH-PAY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE SHARES FEE ON THE REINVEST AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4400-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRPT-SHARES-AMT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-EXEMPT-FLAG = 'Y'
                   COMPUTE DRPT-SHARES-AMT-AMT ROUNDED =
                       DRPT-REINVEST-AMT * DRPM-RATE / 200
               WHEN DRPM-SOURCE-CD = 'BT'
                   MOVE 5.00 TO DRPT-SHARES-AMT-AMT
               WHEN DRPM-SOURCE-CD = 'FL'
                   MOVE 12.00 TO DRPT-SHARES-AMT-AMT
               WHEN OTHER
                   COMPUTE DRPT-SHARES-AMT-AMT ROUNDED =
                       DRPT-REINVEST-AMT * DRPM-RATE / 100
           END-EVALUATE
           IF DRPT-SHARES-AMT-AMT > 0 AND DRPT-SHARES-AMT-AMT < 1.00
               MOVE 1.00 TO DRPT-SHARES-AMT-AMT
           END-IF
           IF DRPT-SHARES-AMT-AMT > 35.00
               MOVE 35.00 TO DRPT-SHARES-AMT-AMT
           END-IF
           ADD DRPT-SHARES-AMT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CASH PAY AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4410-LIMIT-CASH-PAY.
           IF DRPM-CASH-PAY-AMT < 25.00
               MOVE 25.00 TO DRPM-CASH-PAY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-CASH-PAY-AMT > 2500.00
               MOVE 2500.00 TO DRPM-CASH-PAY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4420-COUNT-PAYMENT.
           IF DRPM-PAYMENT-CNT < 9999999
               ADD 1 TO DRPM-PAYMENT-CNT
           END-IF
           IF DRPM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO DRPM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DIVIDEND AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4430-ACCRUE-DIVIDEND.
           IF DRPM-DIVIDEND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-DIVIDEND-AMT * DRPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRPT-SHARES-AMT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DIVIDEND YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4440-ACCRUE-DIVIDEND.
           IF DRPM-DIVIDEND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-DIVIDEND-YTD * DRPM-RATE / 100 / 365
               ADD WS-WORK-AMT TO DRPM-REINVEST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN OPTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4450-DESCRIBE-PLAN-OPTION.
           EVALUATE DRPM-PLAN-OPTION-CD
               WHEN 'E'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Z'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4460-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4470-DESCRIBE-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH PAY AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4480-LIMIT-CASH-PAY.
           IF DRPM-CASH-PAY-AMT < 0.00
               MOVE 0.00 TO DRPM-CASH-PAY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-CASH-PAY-AMT > 5000.00
               MOVE 5000.00 TO DRPM-CASH-PAY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REINVEST AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4490-ACCRUE-REINVEST.
           IF DRPT-REINVEST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPT-REINVEST-AMT * DRPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO DRPM-DIVIDEND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4500-COUNT-PAYMENT.
           IF DRPM-PAYMENT-CNT < 9999999
               ADD 1 TO DRPM-PAYMENT-CNT
           END-IF
           IF DRPM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4510-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4520-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF DRPT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4530-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4540-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHARES A SCHED TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4550-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHARES-A-SCHED-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF DRPM-FEE-AMT NOT < WS-SHARES-A-SCHED-LOW(WS-TX)
                   MOVE WS-SHARES-A-SCHED-RATE(WS-TX) TO
                       DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4560-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CASH PAY YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4570-SPLIT-CASH-PAY.
           COMPUTE WS-WORK-AMT = DRPM-CASH-PAY-YTD / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-CASH-PAY-YTD - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4580-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE HOLDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4590-DESCRIBE-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'H'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'K'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'L'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'X'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REINVEST AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4600-LIMIT-REINVEST.
           IF DRPM-REINVEST-AMT < 50.00
               MOVE 50.00 TO DRPM-REINVEST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-REINVEST-AMT > 500.00
               MOVE 500.00 TO DRPM-REINVEST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E925 MEMO REQUIRED
      * E652 DIVIDEND AMT NEGATIVE
      * E883 DIVIDEND AMT OVER 250000
      * E807 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4610-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-MEMO = SPACES
                   MOVE 'E925' TO WS-REJECT-CODE
               WHEN DRPM-DIVIDEND-AMT < 0
                   MOVE 'E652' TO WS-REJECT-CODE
               WHEN DRPM-DIVIDEND-AMT > 250000.00
                   MOVE 'E883' TO WS-REJECT-CODE
               WHEN DRPT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E807' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHARES AMT AMT BY HOLDER TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4620-ACCUM-HOLDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-HOLDER-TYPE-CD
                   ADD DRPT-SHARES-AMT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-HOLDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPT-SHARES-AMT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH PAY AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4630-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CYCLE-CD
                   ADD DRPM-CASH-PAY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-CASH-PAY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4640-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 10
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REINVEST YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4650-ACCRUE-REINVEST.
           IF DRPM-REINVEST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-REINVEST-YTD * DRPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-WITHHOLD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4660-FLAG-WITHHOLD.
           IF DRPM-WITHHOLD-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE DRPM-WITHHOLD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E199 MEMO REQUIRED
      * E765 DIVIDEND YTD NEGATIVE
      * E555 DIVIDEND YTD OVER 50000
      * E460 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4670-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-MEMO = SPACES
                   MOVE 'E199' TO WS-REJECT-CODE
               WHEN DRPM-DIVIDEND-YTD < 0
                   MOVE 'E765' TO WS-REJECT-CODE
               WHEN DRPM-DIVIDEND-YTD > 50000.00
                   MOVE 'E555' TO WS-REJECT-CODE
               WHEN DRPM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E460' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BT -> FL WHEN EFFECTIVE DATE IS PAST
      * FL -> NK WHEN EFFECTIVE DATE IS PAST
      * NK -> YC WHEN EFFECTIVE DATE IS PAST
      * YC -> BT WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4680-SET-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NK' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NK'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD AMT BY PLAN OPTION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4690-ACCUM-PLAN-OPTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-PLAN-OPTION-CD
                   ADD DRPM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-PLAN-OPTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4700-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 12
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E409 ADDR REQUIRED
      * E092 WORK AMT NEGATIVE
      * E089 WORK AMT OVER 1000000
      * E552 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4710-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-ADDR = SPACES
                   MOVE 'E409' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E092' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E089' TO WS-REJECT-CODE
               WHEN DRPM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E552' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REINVEST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHARES AMT AMT.
      *----------------------------------------------------------------*
       4720-PRORATE-SHARES-AMT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-SHARES-AMT-AMT ROUNDED =
                   DRPM-REINVEST-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-SHARES-AMT-AMT
           END-IF
           ADD DRPM-SHARES-AMT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DIVIDEND YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4730-ACCRUE-DIVIDEND.
           IF DRPM-DIVIDEND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-DIVIDEND-YTD * DRPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRPT-DIVIDEND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN OPTION CD:
      * E -> R WHEN LAST MAINT DATE IS PAST
      * R -> T WHEN LAST MAINT DATE IS PAST
      * T -> Z WHEN LAST MAINT DATE IS PAST
      * Z -> E WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4740-SET-PLAN-OPTION.
           EVALUATE DRPM-PLAN-OPTION-CD
               WHEN 'E'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'R' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'T' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'Z' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'E' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4750-LIMIT-FEE.
           IF DRPT-FEE-AMT < 50.00
               MOVE 50.00 TO DRPT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPT-FEE-AMT > 2500.00
               MOVE 2500.00 TO DRPT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4760-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE DRPM-SHARES-AMT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4770-DESCRIBE-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4780-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-WITHHOLD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-WITHHOLD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD DRPM-WITHHOLD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-WITHHOLD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-WITHHOLD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DIVIDEND YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4790-FLAG-DIVIDEND.
           IF DRPM-DIVIDEND-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-DIVIDEND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4800-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF DRPM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4810-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF DRPM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4820-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPM-SHARES-AMT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = DRPM-SHARES-AMT-AMT - (WS-WORK-AMT *
               3)
           COMPUTE DRPM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REINVEST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DIVIDEND AMT.
      *----------------------------------------------------------------*
       4830-PRORATE-DIVIDEND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPT-DIVIDEND-AMT ROUNDED =
                   DRPM-REINVEST-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPT-DIVIDEND-AMT
           END-IF
           ADD DRPT-DIVIDEND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4840-DESCRIBE-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4850-DESCRIBE-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RI'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4860-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CHANNEL-CD
                   ADD DRPM-WITHHOLD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-WITHHOLD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4870-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4880-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO DRPM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE DRP FIELDS TO DRELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-DRELG.
           INITIALIZE DRELG-PARMS
           MOVE DRPM-REINVEST-AMT
             TO DRELG-REINVEST-AMT
           MOVE DRPM-WITHHOLD-AMT
             TO DRELG-WITHHOLD-AMT
           MOVE DRPM-FEE-YTD
             TO DRELG-FEE-YTD
           MOVE DRPM-SHARES-AMT-AMT
             TO DRELG-SHARES-AMT-AMT
           MOVE DRPM-CASH-PAY-AMT
             TO DRELG-CASH-PAY-AMT
           MOVE DRPM-CASH-PAY-YTD
             TO DRELG-CASH-PAY-YTD
           MOVE DRPM-LAST-MAINT-DATE
             TO DRELG-LAST-MAINT-DATE
           MOVE DRPM-MATURITY-DATE
             TO DRELG-MATURITY-DATE
           MOVE DRPM-OPEN-DATE
             TO DRELG-OPEN-DATE
           MOVE DRPM-EFFECTIVE-DATE
             TO DRELG-EFFECTIVE-DATE
           MOVE DRPM-SOURCE-CD
             TO DRELG-SOURCE-CD
           MOVE DRPM-PLAN-OPTION-CD
             TO DRELG-PLAN-OPTION-CD
           MOVE DRPM-CYCLE-CD
             TO DRELG-CYCLE-CD
           MOVE DRPM-CHANNEL-CD
             TO DRELG-CHANNEL-CD
           MOVE DRPM-STOP-FLAG
             TO DRELG-STOP-FLAG
           MOVE DRPM-SUSPEND-FLAG
             TO DRELG-SUSPEND-FLAG
           MOVE DRPM-PREFERRED-FLAG
             TO DRELG-PREFERRED-FLAG
           MOVE DRPM-EXCEPT-CNT
             TO DRELG-EXCEPT-CNT
           MOVE DRPM-ITEM-CNT
             TO DRELG-ITEM-CNT
           MOVE DRPM-DISCOUNT
             TO DRELG-DISCOUNT
           MOVE DRPM-RATE
             TO DRELG-RATE
           MOVE DRPM-REF-NO
             TO DRELG-REF-NO
           MOVE DRPM-CUST-ID
             TO DRELG-CUST-ID
           CALL 'DRELG' USING DRELG-PARMS
           EVALUATE DRELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE DRELG-WITHHOLD-AMT
                     TO DRPM-WITHHOLD-AMT
                   MOVE DRELG-SHARES-AMT-AMT
                     TO DRPM-SHARES-AMT-AMT
                   MOVE DRELG-STOP-FLAG
                     TO DRPM-STOP-FLAG
                   MOVE DRELG-SUSPEND-FLAG
                     TO DRPM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-DRELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE DRP FIELDS TO DRFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-DRFEE.
           INITIALIZE DRFEE-PARMS
           MOVE DRPM-CASH-PAY-YTD
             TO DRFEE-CASH-PAY-YTD
           MOVE DRPM-WITHHOLD-YTD
             TO DRFEE-WITHHOLD-YTD
           MOVE DRPM-SHARES-AMT-AMT
             TO DRFEE-SHARES-AMT-AMT
           MOVE DRPM-FEE-AMT
             TO DRFEE-FEE-AMT
           MOVE DRPM-REINVEST-YTD
             TO DRFEE-REINVEST-YTD
           MOVE DRPM-SHARES-AMT-YTD
             TO DRFEE-SHARES-AMT-YTD
           MOVE DRPM-NOTICE-DATE
             TO DRFEE-NOTICE-DATE
           MOVE DRPM-LAST-PAY-DATE
             TO DRFEE-LAST-PAY-DATE
           MOVE DRPM-NEXT-REVIEW-DATE
             TO DRFEE-NEXT-REVIEW-DATE
           MOVE DRPM-OPEN-DATE
             TO DRFEE-OPEN-DATE
           MOVE DRPM-REASON-CD
             TO DRFEE-REASON-CD
           MOVE DRPM-CYCLE-CD
             TO DRFEE-CYCLE-CD
           MOVE DRPM-PLAN-OPTION-CD
             TO DRFEE-PLAN-OPTION-CD
           MOVE DRPM-HOLDER-TYPE-CD
             TO DRFEE-HOLDER-TYPE-CD
           MOVE DRPM-OVERRIDE-FLAG
             TO DRFEE-OVERRIDE-FLAG
           MOVE DRPM-SUSPEND-FLAG
             TO DRFEE-SUSPEND-FLAG
           MOVE DRPM-PREFERRED-FLAG
             TO DRFEE-PREFERRED-FLAG
           MOVE DRPM-PAYMENT-CNT
             TO DRFEE-PAYMENT-CNT
           MOVE DRPM-NOTICE-CNT
             TO DRFEE-NOTICE-CNT
           MOVE DRPM-DISCOUNT
             TO DRFEE-DISCOUNT
           MOVE DRPM-RATE
             TO DRFEE-RATE
           MOVE DRPM-DESC
             TO DRFEE-DESC
           MOVE DRPM-ADDR
             TO DRFEE-ADDR
           CALL 'DRFEE' USING DRFEE-PARMS
           EVALUATE DRFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE DRFEE-CASH-PAY-YTD
                     TO DRPM-CASH-PAY-YTD
                   MOVE DRFEE-WITHHOLD-YTD
                     TO DRPM-WITHHOLD-YTD
                   MOVE DRFEE-SHARES-AMT-AMT
                     TO DRPM-SHARES-AMT-AMT
                   MOVE DRFEE-SHARES-AMT-YTD
                     TO DRPM-SHARES-AMT-YTD
                   MOVE DRFEE-PREFERRED-FLAG
                     TO DRPM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-DRFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE DRP FIELDS TO DRSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-DRSTAT.
           INITIALIZE DRSTAT-PARMS
           MOVE DRPM-CASH-PAY-AMT
             TO DRSTAT-CASH-PAY-AMT
           MOVE DRPM-FEE-YTD
             TO DRSTAT-FEE-YTD
           MOVE DRPM-DIVIDEND-AMT
             TO DRSTAT-DIVIDEND-AMT
           MOVE DRPM-REINVEST-AMT
             TO DRSTAT-REINVEST-AMT
           MOVE DRPM-SHARES-AMT-AMT
             TO DRSTAT-SHARES-AMT-AMT
           MOVE DRPM-WITHHOLD-YTD
             TO DRSTAT-WITHHOLD-YTD
           MOVE DRPM-LAST-PAY-DATE
             TO DRSTAT-LAST-PAY-DATE
           MOVE DRPM-NEXT-REVIEW-DATE
             TO DRSTAT-NEXT-REVIEW-DATE
           MOVE DRPM-NEXT-DUE-DATE
             TO DRSTAT-NEXT-DUE-DATE
           MOVE DRPM-MATURITY-DATE
             TO DRSTAT-MATURITY-DATE
           MOVE DRPM-PLAN-OPTION-CD
             TO DRSTAT-PLAN-OPTION-CD
           MOVE DRPM-REASON-CD
             TO DRSTAT-REASON-CD
           MOVE DRPM-CHANNEL-CD
             TO DRSTAT-CHANNEL-CD
           MOVE DRPM-HOLDER-TYPE-CD
             TO DRSTAT-HOLDER-TYPE-CD
           MOVE DRPM-OVERRIDE-FLAG
             TO DRSTAT-OVERRIDE-FLAG
           MOVE DRPM-STOP-FLAG
             TO DRSTAT-STOP-FLAG
           MOVE DRPM-AUTO-RENEW-FLAG
             TO DRSTAT-AUTO-RENEW-FLAG
           MOVE DRPM-EXCEPT-CNT
             TO DRSTAT-EXCEPT-CNT
           MOVE DRPM-NOTICE-CNT
             TO DRSTAT-NOTICE-CNT
           MOVE DRPM-RATE
             TO DRSTAT-RATE
           MOVE DRPM-DISCOUNT
             TO DRSTAT-DISCOUNT
           MOVE DRPM-CUST-ID
             TO DRSTAT-CUST-ID
           MOVE DRPM-REF-NO
             TO DRSTAT-REF-NO
           CALL 'DRSTAT' USING DRSTAT-PARMS
           EVALUATE DRSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE DRSTAT-DIVIDEND-AMT
                     TO DRPM-DIVIDEND-AMT
                   MOVE DRSTAT-SHARES-AMT-AMT
                     TO DRPM-SHARES-AMT-AMT
                   MOVE DRSTAT-OVERRIDE-FLAG
                     TO DRPM-OVERRIDE-FLAG
                   MOVE DRSTAT-STOP-FLAG
                     TO DRPM-STOP-FLAG
                   MOVE DRSTAT-AUTO-RENEW-FLAG
                     TO DRPM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-DRSTAT' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE DRPT-KEY TO RD-KEY
           MOVE DRPT-TRAN-CD TO RD-CODE
           MOVE DRPT-FEE-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE DRPT-KEY TO RD-KEY
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
           DISPLAY 'DRPNOTC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'DRPNOTC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
