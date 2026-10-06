      *================================================================*
      * PROGRAM  : TRSAUDT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PERSONAL TRUST - AUDIT TRAIL. READS THE TRS         *
      *            TRANSACTIONS, UPDATES THE TRS MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. PERSONAL TRUST ACCOUNTING,      *
      *            INCOME DISTRIBUTION AND TRUSTEE FEES                *
      * INPUT    : TRSMST (KSDS), TRSTRN, BANKCTL                      *
      * OUTPUT   : TRSOUT, TRSAURPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TRPEN, TRLIM, TRVAL     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-03-06 CRP  ORIGINAL                                       *
      * 1988-03-19 SKN  AUDIT FINDING - PRINCIPAL CONTROL TOTALS PRB240*
      * 1990-03-14 TWH  NEW TRUSTEE-FEE REPORT COLUMNS CHG1899         *
      * 1992-08-07 SKN  AUDIT FINDING - MARKET CONTROL TOTALS PRB1408  *
      * 1999-03-11 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2007-09-03 LMB  TAX STATUS CODES ADDED REQ4204                 *
      * 2007-11-10 PQR  NEW MARKET REPORT COLUMNS INC9008              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRSAUDT.
       AUTHOR. D P SHAW.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "TRSMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TRSM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "TRSTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "TRSOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "TRSAURPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY TRSMST.
       FD  TRAN-FILE.
       COPY TRSTRN.
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
      * WS-TRUSTEE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TRUSTEE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000594000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000518000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000274000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002800000000858000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003050000000796500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005550000000325500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008050000000679500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009050000000773500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014050000000586000'.
       01  WS-TRUSTEE-BAND-TBL REDEFINES WS-TRUSTEE-BAND-VALUES.
           05  WS-TRUSTEE-BAND-ROW OCCURS 9 TIMES.
               10  WS-TRUSTEE-BAND-LOW     PIC 9(09)V99.
               10  WS-TRUSTEE-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-TAX-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TAX-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000018000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000122500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000031500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000358500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015150000000395000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015650000000295500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015700000000535000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016200000000368000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016250000000524500'.
       01  WS-TAX-TIER-TBL REDEFINES WS-TAX-TIER-VALUES.
           05  WS-TAX-TIER-ROW OCCURS 9 TIMES.
               10  WS-TAX-TIER-LOW         PIC 9(09)V99.
               10  WS-TAX-TIER-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INCOME-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INCOME-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000573000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000692000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000772500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000634000'.
       01  WS-INCOME-BAND-TBL REDEFINES WS-INCOME-BAND-VALUES.
           05  WS-INCOME-BAND-ROW OCCURS 4 TIMES.
               10  WS-INCOME-BAND-LOW      PIC 9(09)V99.
               10  WS-INCOME-BAND-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INCOME-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INCOME-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000293000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000701000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000446500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000575500'.
       01  WS-INCOME-SCHED-TBL REDEFINES WS-INCOME-SCHED-VALUES.
           05  WS-INCOME-SCHED-ROW OCCURS 4 TIMES.
               10  WS-INCOME-SCHED-LOW     PIC 9(09)V99.
               10  WS-INCOME-SCHED-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TRSAUDT  PERSONAL TRUST'.
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
       COPY TRPENP.
       COPY TRLIMP.
       COPY TRVALP.

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
               MOVE 'TRSMST' TO ABND-FILE-NAME
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
           MOVE TRST-KEY TO TRSM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ASSESS-REVERSAL
               PERFORM 3010-COUNT-SEQ
               PERFORM 3020-AGE-CLOSE
               PERFORM 3030-ACCRUE-TAX
               PERFORM 3040-ACCRUE-DISCOUNT
               PERFORM 3050-SPLIT-INCOME
               PERFORM 3060-PRORATE-PRINCIPAL
               PERFORM 3070-SELECT-DISCOUNT
               PERFORM 3080-SPLIT-INCOME
               PERFORM 3090-ACCUM-CYCLE
               PERFORM 3100-EDIT-DESC
               PERFORM 3110-ASSESS-REVERSAL
               PERFORM 3120-EDIT-NAME
               PERFORM 3130-SET-REGION
               PERFORM 3140-SET-TRUST-TYPE
               PERFORM 3150-LIMIT-INCOME
               PERFORM 3160-AGE-LAST-PAY
               PERFORM 3170-FLAG-INCOME
               PERFORM 3180-PRORATE-MARKET
               PERFORM 3190-COUNT-SEQ
               PERFORM 3200-COUNT-PAYMENT
               PERFORM 3210-ACCRUE-MARGIN
               PERFORM 3220-ACCUM-CYCLE
               PERFORM 3230-SPLIT-INCOME
               PERFORM 3240-EDIT-DESC
               PERFORM 3250-SPLIT-DISTRIB
               PERFORM 3260-PRORATE-TRUSTEE-FEE
               PERFORM 3270-COUNT-CYCLE
               PERFORM 3280-SELECT-MARGIN
               PERFORM 3290-PRORATE-TRUSTEE-FEE
               PERFORM 3300-COUNT-CYCLE
               PERFORM 3310-DESCRIBE-STATUS
               PERFORM 3320-ASSESS-STOP
               PERFORM 3330-EDIT-REF-NO
               PERFORM 3340-ASSESS-STOP
               PERFORM 3350-COUNT-RETRY
               PERFORM 3360-SET-STATUS
               PERFORM 3370-ACCUM-TRAN
               PERFORM 3380-DESCRIBE-DIST-FREQ
               PERFORM 3390-ASSESS-NOTICE-SENT
               PERFORM 3400-SPLIT-INCOME
               PERFORM 3410-AGE-NOTICE
               PERFORM 3420-PRORATE-TRUSTEE-FEE
               PERFORM 3430-SET-TRUST-TYPE
               PERFORM 3440-EDIT-CONTACT
               PERFORM 3450-LIMIT-DISTRIB
               PERFORM 3460-SELECT-MARGIN
               PERFORM 3470-SELECT-RATE
               PERFORM 3480-AGE-LAST-PAY
               PERFORM 3490-DESCRIBE-REASON
               PERFORM 3500-EDIT-MEMO
               PERFORM 3510-PRORATE-TAX
               PERFORM 3520-AGE-NOTICE
               PERFORM 3530-ACCRUE-MARGIN
               PERFORM 3540-SELECT-RATE
               PERFORM 3550-SPLIT-PRINCIPAL
               PERFORM 3560-FLAG-DISTRIB
               PERFORM 3570-ACCRUE-MARKET
               PERFORM 3580-SELECT-DISCOUNT
               PERFORM 3590-ACCUM-REASON
               PERFORM 3600-ACCRUE-MARKET
               PERFORM 3610-ACCRUE-TRUSTEE-FEE
               PERFORM 3620-LIMIT-INCOME
               PERFORM 3630-PRORATE-TRUSTEE-FEE
               PERFORM 3640-PRORATE-TRUSTEE-FEE
               PERFORM 3650-ACCRUE-PRINCIPAL
               PERFORM 3660-EDIT-OPERATOR
               PERFORM 3670-SELECT-MARGIN
               PERFORM 3680-ACCRUE-TAX
               PERFORM 3690-EDIT-ADDR
               PERFORM 3700-ASSESS-STOP
               PERFORM 3710-ASSESS-PREFERRED
               PERFORM 3720-EDIT-OPERATOR
               PERFORM 3730-LIMIT-PRINCIPAL
               PERFORM 3740-PRORATE-TAX
               PERFORM 3750-FLAG-TRUSTEE-FEE
               PERFORM 3760-ACCUM-REASON
               PERFORM 3770-COUNT-NOTICE
               PERFORM 3780-EDIT-CONTACT
               PERFORM 3790-CHECK-EXPIRY
               PERFORM 3800-AGE-NOTICE
               PERFORM 3810-CHECK-EFFECTIVE
               PERFORM 3820-LIMIT-INCOME
               PERFORM 3830-COUNT-RETRY
               PERFORM 3840-COUNT-NOTICE
               PERFORM 3850-ASSESS-STOP
               PERFORM 3860-ASSESS-PREFERRED
               PERFORM 3870-EDIT-CONTACT
               PERFORM 3880-SPLIT-PRINCIPAL
               PERFORM 3890-SPLIT-PRINCIPAL
               PERFORM 3900-SELECT-DISCOUNT
               PERFORM 3910-COUNT-NOTICE
               PERFORM 3920-AGE-EXPIRY
               PERFORM 3930-SELECT-MARGIN
               PERFORM 3940-SET-REASON
               PERFORM 3950-PRORATE-TRUSTEE-FEE
               PERFORM 3960-EDIT-NAME
               PERFORM 3970-SPLIT-DISTRIB
               PERFORM 3980-FLAG-TAX
               PERFORM 3990-DESCRIBE-DIST-FREQ
               PERFORM 4000-SELECT-MARGIN
               PERFORM 4010-CHECK-LAST-PAY
               PERFORM 4020-AGE-OPEN
               PERFORM 4030-AGE-LAST-STMT
               PERFORM 4040-ACCRUE-MARKET
               PERFORM 4050-PRORATE-DISTRIB
               PERFORM 4060-LIMIT-MARKET
               PERFORM 4070-FLAG-INCOME
               PERFORM 4080-LIMIT-INCOME
               PERFORM 4090-LIMIT-INCOME
               PERFORM 4100-CHECK-POST
               PERFORM 4110-AGE-MATURITY
               PERFORM 4120-SET-REGION
               PERFORM 4130-COUNT-PAYMENT
               PERFORM 4140-FLAG-INCOME
               PERFORM 4150-AGE-POST
               PERFORM 4160-ASSESS-PREFERRED
               PERFORM 4170-PRORATE-MARKET
               PERFORM 4180-EDIT-ADDR
               PERFORM 4190-PRORATE-TAX
               PERFORM 4200-AGE-LAST-PAY
               PERFORM 4210-DESCRIBE-CHANNEL
               PERFORM 4220-SELECT-MARGIN
               PERFORM 4230-SET-REASON
               PERFORM 4240-SELECT-MARGIN
               PERFORM 4250-AGE-LAST-MAINT
               PERFORM 4260-AGE-POST
               PERFORM 4270-CHECK-CLOSE
               PERFORM 4280-SPLIT-TRUSTEE-FEE
               PERFORM 4290-LIMIT-DISTRIB
               PERFORM 4300-AGE-LAST-STMT
               PERFORM 4310-PRORATE-TRUSTEE-FEE
               PERFORM 4320-PRORATE-TRUSTEE-FEE
               PERFORM 4330-SET-DIST-FREQ
               PERFORM 4340-ASSESS-STOP
               PERFORM 4350-FLAG-PRINCIPAL
               PERFORM 4360-EDIT-REF-NO
               PERFORM 4370-EDIT-DESC
               PERFORM 4380-SPLIT-INCOME
               PERFORM 4390-CHECK-POST
               PERFORM 4400-ACCRUE-TAX
               PERFORM 4410-CHECK-EFFECTIVE
               PERFORM 4420-FLAG-TAX
               PERFORM 4430-SELECT-MARGIN
               PERFORM 4440-SET-CYCLE
               PERFORM 4450-COUNT-SEQ
               PERFORM 4460-SET-CHANNEL
               PERFORM 7010-CALL-TRPEN
               PERFORM 7000-CALL-TRLIM
               PERFORM 7020-CALL-TRVAL
               REWRITE TRS-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'TRSMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM TRS-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB YTD FEE ON THE TAX AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3000-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TRSM-DISTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   COMPUTE TRSM-DISTRIB-YTD ROUNDED = TRSM-TAX-AMT *
                       TRSM-RATE / 200
               WHEN TRST-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO TRSM-DISTRIB-YTD
               WHEN TRST-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO TRSM-DISTRIB-YTD
               WHEN OTHER
                   COMPUTE TRSM-DISTRIB-YTD ROUNDED = TRSM-TAX-AMT *
                       TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-DISTRIB-YTD > 0 AND TRSM-DISTRIB-YTD < 2.00
               MOVE 2.00 TO TRSM-DISTRIB-YTD
           END-IF
           IF TRSM-DISTRIB-YTD > 50.00
               MOVE 50.00 TO TRSM-DISTRIB-YTD
           END-IF
           ADD TRSM-DISTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3010-COUNT-SEQ.
           IF TRST-SEQ < 9999999
               ADD 1 TO TRST-SEQ
           END-IF
           IF TRST-SEQ NOT < 6
               MOVE 'Y' TO TRSM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3020-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3030-ACCRUE-TAX.
           IF TRSM-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TAX-AMT * TRSM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRSM-TAX-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3040-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TRSM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TRSM-MARKET-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3050-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE TRSM-INCOME-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINCIPAL YTD.
      *----------------------------------------------------------------*
       3060-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-PRINCIPAL-YTD ROUNDED =
                   TRST-DISTRIB-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-PRINCIPAL-YTD
           END-IF
           ADD TRSM-PRINCIPAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE INCOME SCHED TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3070-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-SCHED-RATE(1) TO TRSM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TRST-PRINCIPAL-AMT NOT < WS-INCOME-SCHED-LOW(WS-TX)
                   MOVE WS-INCOME-SCHED-RATE(WS-TX) TO TRSM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INCOME AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3080-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = TRSM-INCOME-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TRSM-INCOME-AMT - (WS-WORK-AMT * 2)
           COMPUTE TRSM-DISTRIB-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INCOME AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3090-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-CYCLE-CD
                   ADD TRSM-INCOME-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-INCOME-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E593 DESC REQUIRED
      * E427 PRINCIPAL YTD NEGATIVE
      * E656 PRINCIPAL YTD OVER 1000000
      * E384 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3100-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-DESC = SPACES
                   MOVE 'E593' TO WS-REJECT-CODE
               WHEN TRSM-PRINCIPAL-YTD < 0
                   MOVE 'E427' TO WS-REJECT-CODE
               WHEN TRSM-PRINCIPAL-YTD > 1000000.00
                   MOVE 'E656' TO WS-REJECT-CODE
               WHEN TRSM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E384' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB FEE ON THE DISTRIB YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3110-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TRSM-DISTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TRSM-DISTRIB-AMT ROUNDED = TRSM-DISTRIB-YTD *
                       TRSM-MARGIN / 200
               WHEN TRSM-STATUS-CD = 'EH'
                   MOVE 10.00 TO TRSM-DISTRIB-AMT
               WHEN TRSM-STATUS-CD = 'GG'
                   MOVE 10.00 TO TRSM-DISTRIB-AMT
               WHEN OTHER
                   COMPUTE TRSM-DISTRIB-AMT ROUNDED = TRSM-DISTRIB-YTD *
                       TRSM-MARGIN / 100
           END-EVALUATE
           IF TRSM-DISTRIB-AMT > 0 AND TRSM-DISTRIB-AMT < 2.00
               MOVE 2.00 TO TRSM-DISTRIB-AMT
           END-IF
           IF TRSM-DISTRIB-AMT > 250.00
               MOVE 250.00 TO TRSM-DISTRIB-AMT
           END-IF
           ADD TRSM-DISTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E079 NAME REQUIRED
      * E046 INCOME YTD NEGATIVE
      * E984 INCOME YTD OVER 5000
      * E794 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3120-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-NAME = SPACES
                   MOVE 'E079' TO WS-REJECT-CODE
               WHEN TRSM-INCOME-YTD < 0
                   MOVE 'E046' TO WS-REJECT-CODE
               WHEN TRSM-INCOME-YTD > 5000.00
                   MOVE 'E984' TO WS-REJECT-CODE
               WHEN TRSM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E794' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> D WHEN NOTICE DATE IS PAST
      * D -> N WHEN NOTICE DATE IS PAST
      * N -> T WHEN NOTICE DATE IS PAST
      * T -> A WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3130-SET-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'A' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN CLOSE DATE IS PAST
      * MNG -> VFN WHEN CLOSE DATE IS PAST
      * VFN -> XPL WHEN CLOSE DATE IS PAST
      * XPL -> FLJ WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3140-SET-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INCOME AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3150-LIMIT-INCOME.
           IF TRSM-INCOME-AMT < 10.00
               MOVE 10.00 TO TRSM-INCOME-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-INCOME-AMT > 500.00
               MOVE 500.00 TO TRSM-INCOME-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3160-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INCOME AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3170-FLAG-INCOME.
           IF TRSM-INCOME-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-INCOME-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MARKET YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3180-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TRSM-MARKET-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3190-COUNT-SEQ.
           IF TRST-SEQ < 9999999
               ADD 1 TO TRST-SEQ
           END-IF
           IF TRST-SEQ NOT < 12
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3200-COUNT-PAYMENT.
           IF TRSM-PAYMENT-CNT < 9999999
               ADD 1 TO TRSM-PAYMENT-CNT
           END-IF
           IF TRSM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO TRST-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3210-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TRSM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRSM-INCOME-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3220-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-CYCLE-CD
                   ADD TRSM-TAX-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-TAX-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INCOME AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3230-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = TRSM-INCOME-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRSM-INCOME-AMT - (WS-WORK-AMT * 4)
           COMPUTE TRST-DISTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E472 DESC REQUIRED
      * E754 TRUSTEE FEE YTD NEGATIVE
      * E573 TRUSTEE FEE YTD OVER 99999
      * E239 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-DESC = SPACES
                   MOVE 'E472' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-YTD < 0
                   MOVE 'E754' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-YTD > 99999.00
                   MOVE 'E573' TO WS-REJECT-CODE
               WHEN TRSM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E239' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3250-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE TRST-DISTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TRUSTEE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TRUSTEE FEE AMT.
      *----------------------------------------------------------------*
       3260-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRST-TRUSTEE-FEE-AMT ROUNDED =
                   TRSM-TRUSTEE-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRST-TRUSTEE-FEE-AMT
           END-IF
           ADD TRST-TRUSTEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3270-COUNT-CYCLE.
           IF TRSM-CYCLE-CNT < 9999999
               ADD 1 TO TRSM-CYCLE-CNT
           END-IF
           IF TRSM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INCOME BAND TIER FOR THE TRUSTEE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3280-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-BAND-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TRSM-TRUSTEE-FEE-YTD NOT < WS-INCOME-BAND-LOW(WS-TX)
                   MOVE WS-INCOME-BAND-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TRUSTEE FEE YTD.
      *----------------------------------------------------------------*
       3290-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           ADD TRSM-TRUSTEE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-CYCLE.
           IF TRSM-CYCLE-CNT < 9999999
               ADD 1 TO TRSM-CYCLE-CNT
           END-IF
           IF TRSM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3310-DESCRIBE-STATUS.
           EVALUATE TRSM-STATUS-CD
               WHEN 'EH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE MARKET YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3320-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TRSM-MARKET-YTD *
                       TRSM-RATE / 200
               WHEN TRST-TRAN-CD = 'IN'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN TRST-TRAN-CD = 'DS'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TRSM-MARKET-YTD *
                       TRSM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E070 REF NO REQUIRED
      * E414 TRUSTEE FEE AMT NEGATIVE
      * E796 TRUSTEE FEE AMT OVER 250000
      * E640 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3330-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-REF-NO = SPACES
                   MOVE 'E070' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-AMT < 0
                   MOVE 'E414' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-AMT > 250000.00
                   MOVE 'E796' TO WS-REJECT-CODE
               WHEN TRST-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E640' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRUSTEE FEE FEE ON THE DISTRIB AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3340-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TRUSTEE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TRSM-TRUSTEE-FEE-AMT ROUNDED =
                       TRSM-DISTRIB-AMT * TRSM-DISCOUNT / 200
               WHEN TRSM-DIST-FREQ-CD = 'E'
                   MOVE 10.00 TO TRSM-TRUSTEE-FEE-AMT
               WHEN TRSM-DIST-FREQ-CD = 'K'
                   MOVE 20.00 TO TRSM-TRUSTEE-FEE-AMT
               WHEN OTHER
                   COMPUTE TRSM-TRUSTEE-FEE-AMT ROUNDED =
                       TRSM-DISTRIB-AMT * TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-TRUSTEE-FEE-AMT > 0 AND TRSM-TRUSTEE-FEE-AMT < 1.00
               MOVE 1.00 TO TRSM-TRUSTEE-FEE-AMT
           END-IF
           IF TRSM-TRUSTEE-FEE-AMT > 250.00
               MOVE 250.00 TO TRSM-TRUSTEE-FEE-AMT
           END-IF
           ADD TRSM-TRUSTEE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3350-COUNT-RETRY.
           IF TRSM-RETRY-CNT < 9999999
               ADD 1 TO TRSM-RETRY-CNT
           END-IF
           IF TRSM-RETRY-CNT NOT < 5
               MOVE 'Y' TO TRSM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EH -> GG WHEN LAST PAY DATE IS PAST
      * GG -> JG WHEN LAST PAY DATE IS PAST
      * JG -> EH WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3360-SET-STATUS.
           EVALUATE TRSM-STATUS-CD
               WHEN 'EH'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JG' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JG'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EH' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3370-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-TRAN-CD
                   ADD TRSM-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DIST FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3380-DESCRIBE-DIST-FREQ.
           EVALUATE TRSM-DIST-FREQ-CD
               WHEN 'E'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Y'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE TAX AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3390-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   COMPUTE TRSM-PRINCIPAL-AMT ROUNDED = TRSM-TAX-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRST-TRAN-CD = 'IN'
                   MOVE 7.00 TO TRSM-PRINCIPAL-AMT
               WHEN TRST-TRAN-CD = 'DS'
                   MOVE 15.00 TO TRSM-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE TRSM-PRINCIPAL-AMT ROUNDED = TRSM-TAX-AMT *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-PRINCIPAL-AMT > 0 AND TRSM-PRINCIPAL-AMT < 2.00
               MOVE 2.00 TO TRSM-PRINCIPAL-AMT
           END-IF
           IF TRSM-PRINCIPAL-AMT > 250.00
               MOVE 250.00 TO TRSM-PRINCIPAL-AMT
           END-IF
           ADD TRSM-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT INCOME YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3400-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = TRSM-INCOME-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TRSM-INCOME-YTD - (WS-WORK-AMT * 12)
           COMPUTE TRST-DISTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE TRUSTEE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TRUSTEE FEE AMT.
      *----------------------------------------------------------------*
       3420-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TRUSTEE-FEE-AMT ROUNDED =
                   TRST-TRUSTEE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-TRUSTEE-FEE-AMT
           END-IF
           ADD TRSM-TRUSTEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN LAST STMT DATE IS PAST
      * MNG -> VFN WHEN LAST STMT DATE IS PAST
      * VFN -> XPL WHEN LAST STMT DATE IS PAST
      * XPL -> FLJ WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3430-SET-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E834 CONTACT REQUIRED
      * E010 WORK AMT 2 NEGATIVE
      * E698 WORK AMT 2 OVER 250000
      * E182 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3440-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-CONTACT = SPACES
                   MOVE 'E834' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E010' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E698' TO WS-REJECT-CODE
               WHEN TRSM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E182' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3450-LIMIT-DISTRIB.
           IF TRSM-DISTRIB-AMT < 0.00
               MOVE 0.00 TO TRSM-DISTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-DISTRIB-AMT > 10000.00
               MOVE 10000.00 TO TRSM-DISTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INCOME SCHED TIER FOR THE DISTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3460-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-SCHED-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TRST-DISTRIB-AMT NOT < WS-INCOME-SCHED-LOW(WS-TX)
                   MOVE WS-INCOME-SCHED-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INCOME BAND TIER FOR THE MARKET AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3470-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-BAND-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TRSM-MARKET-AMT NOT < WS-INCOME-BAND-LOW(WS-TX)
                   MOVE WS-INCOME-BAND-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3480-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3490-DESCRIBE-REASON.
           EVALUATE TRSM-REASON-CD
               WHEN 'H'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E633 MEMO REQUIRED
      * E713 DISTRIB AMT NEGATIVE
      * E888 DISTRIB AMT OVER 250000
      * E137 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3500-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-MEMO = SPACES
                   MOVE 'E633' TO WS-REJECT-CODE
               WHEN TRST-DISTRIB-AMT < 0
                   MOVE 'E713' TO WS-REJECT-CODE
               WHEN TRST-DISTRIB-AMT > 250000.00
                   MOVE 'E888' TO WS-REJECT-CODE
               WHEN TRST-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E137' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TAX AMT.
      *----------------------------------------------------------------*
       3510-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TAX-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-TAX-AMT
           END-IF
           ADD TRSM-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3520-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-NOTICE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3530-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRSM-DISTRIB-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INCOME BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3540-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-BAND-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-INCOME-BAND-LOW(WS-TX)
                   MOVE WS-INCOME-BAND-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3550-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = TRST-PRINCIPAL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TRST-PRINCIPAL-AMT - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3560-FLAG-DISTRIB.
           IF TRST-DISTRIB-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRST-DISTRIB-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARKET YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3570-ACCRUE-MARKET.
           IF TRSM-MARKET-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-MARKET-YTD * TRSM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRST-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAX TIER TIER FOR THE INCOME YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3580-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-TIER-RATE(1) TO TRSM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TRSM-INCOME-YTD NOT < WS-TAX-TIER-LOW(WS-TX)
                   MOVE WS-TAX-TIER-RATE(WS-TX) TO TRSM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TRUSTEE FEE AMT BY REASON CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3590-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-REASON-CD
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-TRUSTEE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARKET AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3600-ACCRUE-MARKET.
           IF TRSM-MARKET-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-MARKET-AMT * TRSM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TRSM-TAX-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TRUSTEE FEE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3610-ACCRUE-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TRUSTEE-FEE-YTD * TRSM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TRSM-PRINCIPAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INCOME AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3620-LIMIT-INCOME.
           IF TRST-INCOME-AMT < 10.00
               MOVE 10.00 TO TRST-INCOME-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-INCOME-AMT > 5000.00
               MOVE 5000.00 TO TRST-INCOME-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TRUSTEE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3630-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TRUSTEE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TRUSTEE FEE YTD.
      *----------------------------------------------------------------*
       3640-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED =
                   TRST-DISTRIB-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           ADD TRSM-TRUSTEE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3650-ACCRUE-PRINCIPAL.
           IF TRSM-PRINCIPAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-PRINCIPAL-YTD * TRSM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRSM-INCOME-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E867 OPERATOR REQUIRED
      * E694 MARKET YTD NEGATIVE
      * E299 MARKET YTD OVER 10000
      * E553 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3660-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-OPERATOR = SPACES
                   MOVE 'E867' TO WS-REJECT-CODE
               WHEN TRSM-MARKET-YTD < 0
                   MOVE 'E694' TO WS-REJECT-CODE
               WHEN TRSM-MARKET-YTD > 10000.00
                   MOVE 'E299' TO WS-REJECT-CODE
               WHEN TRST-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E553' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE BAND TIER FOR THE DISTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3670-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-BAND-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TRSM-DISTRIB-AMT NOT < WS-TRUSTEE-BAND-LOW(WS-TX)
                   MOVE WS-TRUSTEE-BAND-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3680-ACCRUE-TAX.
           IF TRSM-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TAX-YTD * TRSM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRSM-TRUSTEE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E750 ADDR REQUIRED
      * E942 WORK AMT 2 NEGATIVE
      * E605 WORK AMT 2 OVER 1000000
      * E570 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3690-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-ADDR = SPACES
                   MOVE 'E750' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E942' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN TRSM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E570' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INCOME FEE ON THE DISTRIB AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3700-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRST-INCOME-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   COMPUTE TRST-INCOME-AMT ROUNDED = TRSM-DISTRIB-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRSM-REASON-CD = 'H'
                   MOVE 7.00 TO TRST-INCOME-AMT
               WHEN TRSM-REASON-CD = 'M'
                   MOVE 15.00 TO TRST-INCOME-AMT
               WHEN OTHER
                   COMPUTE TRST-INCOME-AMT ROUNDED = TRSM-DISTRIB-AMT *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRST-INCOME-AMT > 0 AND TRST-INCOME-AMT < 2.00
               MOVE 2.00 TO TRST-INCOME-AMT
           END-IF
           IF TRST-INCOME-AMT > 250.00
               MOVE 250.00 TO TRST-INCOME-AMT
           END-IF
           ADD TRST-INCOME-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE TAX FEE ON THE INCOME AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3710-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-STOP-FLAG = 'Y'
                   COMPUTE TRSM-TAX-AMT ROUNDED = TRSM-INCOME-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRSM-CYCLE-CD = 'EN'
                   MOVE 10.00 TO TRSM-TAX-AMT
               WHEN TRSM-CYCLE-CD = 'FY'
                   MOVE 5.00 TO TRSM-TAX-AMT
               WHEN OTHER
                   COMPUTE TRSM-TAX-AMT ROUNDED = TRSM-INCOME-AMT *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-TAX-AMT > 0 AND TRSM-TAX-AMT < 1.00
               MOVE 1.00 TO TRSM-TAX-AMT
           END-IF
           IF TRSM-TAX-AMT > 75.00
               MOVE 75.00 TO TRSM-TAX-AMT
           END-IF
           ADD TRSM-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E604 OPERATOR REQUIRED
      * E929 DISTRIB YTD NEGATIVE
      * E677 DISTRIB YTD OVER 25000
      * E928 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3720-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-OPERATOR = SPACES
                   MOVE 'E604' TO WS-REJECT-CODE
               WHEN TRSM-DISTRIB-YTD < 0
                   MOVE 'E929' TO WS-REJECT-CODE
               WHEN TRSM-DISTRIB-YTD > 25000.00
                   MOVE 'E677' TO WS-REJECT-CODE
               WHEN TRSM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E928' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3730-LIMIT-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT < 10.00
               MOVE 10.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-PRINCIPAL-AMT > 1000.00
               MOVE 1000.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TAX YTD.
      *----------------------------------------------------------------*
       3740-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TAX-YTD ROUNDED =
                   TRSM-TAX-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-TAX-YTD
           END-IF
           ADD TRSM-TAX-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRUSTEE FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3750-FLAG-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRSM-TRUSTEE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3760-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-REASON-CD
                   ADD TRSM-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3770-COUNT-NOTICE.
           IF TRSM-NOTICE-CNT < 9999999
               ADD 1 TO TRSM-NOTICE-CNT
           END-IF
           IF TRSM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO TRSM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E689 CONTACT REQUIRED
      * E045 DISTRIB YTD NEGATIVE
      * E473 DISTRIB YTD OVER 5000
      * E523 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3780-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-CONTACT = SPACES
                   MOVE 'E689' TO WS-REJECT-CODE
               WHEN TRSM-DISTRIB-YTD < 0
                   MOVE 'E045' TO WS-REJECT-CODE
               WHEN TRSM-DISTRIB-YTD > 5000.00
                   MOVE 'E473' TO WS-REJECT-CODE
               WHEN TRSM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E523' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3790-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3800-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3810-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TRSM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INCOME AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3820-LIMIT-INCOME.
           IF TRSM-INCOME-AMT < 0.00
               MOVE 0.00 TO TRSM-INCOME-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-INCOME-AMT > 50000.00
               MOVE 50000.00 TO TRSM-INCOME-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3830-COUNT-RETRY.
           IF TRSM-RETRY-CNT < 9999999
               ADD 1 TO TRSM-RETRY-CNT
           END-IF
           IF TRSM-RETRY-CNT NOT < 12
               MOVE 'Y' TO TRSM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3840-COUNT-NOTICE.
           IF TRSM-NOTICE-CNT < 9999999
               ADD 1 TO TRSM-NOTICE-CNT
           END-IF
           IF TRSM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3850-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TRSM-PRINCIPAL-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRSM-DIST-FREQ-CD = 'E'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN TRSM-DIST-FREQ-CD = 'K'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TRSM-PRINCIPAL-AMT *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE INCOME FEE ON THE INCOME YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3860-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TRST-INCOME-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   COMPUTE TRST-INCOME-AMT ROUNDED = TRSM-INCOME-YTD *
                       TRSM-DISCOUNT / 200
               WHEN TRST-TRAN-CD = 'IN'
                   MOVE 15.00 TO TRST-INCOME-AMT
               WHEN TRST-TRAN-CD = 'DS'
                   MOVE 15.00 TO TRST-INCOME-AMT
               WHEN OTHER
                   COMPUTE TRST-INCOME-AMT ROUNDED = TRSM-INCOME-YTD *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRST-INCOME-AMT > 0 AND TRST-INCOME-AMT < 2.00
               MOVE 2.00 TO TRST-INCOME-AMT
           END-IF
           IF TRST-INCOME-AMT > 25.00
               MOVE 25.00 TO TRST-INCOME-AMT
           END-IF
           ADD TRST-INCOME-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E704 CONTACT REQUIRED
      * E209 PRINCIPAL AMT NEGATIVE
      * E319 PRINCIPAL AMT OVER 10000
      * E702 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3870-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-CONTACT = SPACES
                   MOVE 'E704' TO WS-REJECT-CODE
               WHEN TRSM-PRINCIPAL-AMT < 0
                   MOVE 'E209' TO WS-REJECT-CODE
               WHEN TRSM-PRINCIPAL-AMT > 10000.00
                   MOVE 'E319' TO WS-REJECT-CODE
               WHEN TRSM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E702' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3880-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = TRSM-PRINCIPAL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRSM-PRINCIPAL-AMT - (WS-WORK-AMT *
               4)
           COMPUTE TRST-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3890-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = TRST-PRINCIPAL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRST-PRINCIPAL-AMT - (WS-WORK-AMT *
               4)
           COMPUTE TRSM-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INCOME SCHED TIER FOR THE TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3900-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-SCHED-RATE(1) TO TRSM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TRSM-TAX-YTD NOT < WS-INCOME-SCHED-LOW(WS-TX)
                   MOVE WS-INCOME-SCHED-RATE(WS-TX) TO TRSM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3910-COUNT-NOTICE.
           IF TRSM-NOTICE-CNT < 9999999
               ADD 1 TO TRSM-NOTICE-CNT
           END-IF
           IF TRSM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO TRSM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3920-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE TAX TIER TIER FOR THE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-TIER-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TRSM-TAX-AMT NOT < WS-TAX-TIER-LOW(WS-TX)
                   MOVE WS-TAX-TIER-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * H -> M WHEN OPEN DATE IS PAST
      * M -> T WHEN OPEN DATE IS PAST
      * T -> H WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3940-SET-REASON.
           EVALUATE TRSM-REASON-CD
               WHEN 'H'
                   IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE INCOME AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TRUSTEE FEE YTD.
      *----------------------------------------------------------------*
       3950-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED =
                   TRSM-INCOME-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           ADD TRSM-TRUSTEE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E034 NAME REQUIRED
      * E963 TAX YTD NEGATIVE
      * E810 TAX YTD OVER 99999
      * E219 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-NAME = SPACES
                   MOVE 'E034' TO WS-REJECT-CODE
               WHEN TRSM-TAX-YTD < 0
                   MOVE 'E963' TO WS-REJECT-CODE
               WHEN TRSM-TAX-YTD > 99999.00
                   MOVE 'E810' TO WS-REJECT-CODE
               WHEN TRSM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E219' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3970-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = TRSM-DISTRIB-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TRSM-DISTRIB-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAX YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3980-FLAG-TAX.
           IF TRSM-TAX-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRSM-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DIST FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-DIST-FREQ.
           EVALUATE TRSM-DIST-FREQ-CD
               WHEN 'E'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'K'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Z'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INCOME BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4000-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-BAND-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-INCOME-BAND-LOW(WS-TX)
                   MOVE WS-INCOME-BAND-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4010-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4020-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD TRSM-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4030-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARKET AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4040-ACCRUE-MARKET.
           IF TRSM-MARKET-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-MARKET-AMT * TRSM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TRSM-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISTRIB YTD.
      *----------------------------------------------------------------*
       4050-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-DISTRIB-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-DISTRIB-YTD
           END-IF
           ADD TRSM-DISTRIB-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * MARKET YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4060-LIMIT-MARKET.
           IF TRSM-MARKET-YTD < 50.00
               MOVE 50.00 TO TRSM-MARKET-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-MARKET-YTD > 1000.00
               MOVE 1000.00 TO TRSM-MARKET-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INCOME YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4070-FLAG-INCOME.
           IF TRSM-INCOME-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-INCOME-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * INCOME AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4080-LIMIT-INCOME.
           IF TRST-INCOME-AMT < 100.00
               MOVE 100.00 TO TRST-INCOME-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-INCOME-AMT > 10000.00
               MOVE 10000.00 TO TRST-INCOME-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INCOME AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4090-LIMIT-INCOME.
           IF TRSM-INCOME-AMT < 100.00
               MOVE 100.00 TO TRSM-INCOME-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-INCOME-AMT > 10000.00
               MOVE 10000.00 TO TRSM-INCOME-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4100-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF TRST-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRST-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TRST-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4110-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> D WHEN POST DATE IS PAST
      * D -> N WHEN POST DATE IS PAST
      * N -> T WHEN POST DATE IS PAST
      * T -> A WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4120-SET-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'A' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4130-COUNT-PAYMENT.
           IF TRSM-PAYMENT-CNT < 9999999
               ADD 1 TO TRSM-PAYMENT-CNT
           END-IF
           IF TRSM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO TRSM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INCOME AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4140-FLAG-INCOME.
           IF TRSM-INCOME-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRSM-INCOME-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4150-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF TRST-POST-DATE > 0 AND TRST-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRST-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE MARKET FEE ON THE DISTRIB YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4160-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TRSM-MARKET-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   COMPUTE TRSM-MARKET-AMT ROUNDED = TRSM-DISTRIB-YTD *
                       TRSM-RATE / 200
               WHEN TRSM-CYCLE-CD = 'EN'
                   MOVE 3.00 TO TRSM-MARKET-AMT
               WHEN TRSM-CYCLE-CD = 'FY'
                   MOVE 7.00 TO TRSM-MARKET-AMT
               WHEN OTHER
                   COMPUTE TRSM-MARKET-AMT ROUNDED = TRSM-DISTRIB-YTD *
                       TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-MARKET-AMT > 0 AND TRSM-MARKET-AMT < 1.00
               MOVE 1.00 TO TRSM-MARKET-AMT
           END-IF
           IF TRSM-MARKET-AMT > 35.00
               MOVE 35.00 TO TRSM-MARKET-AMT
           END-IF
           ADD TRSM-MARKET-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MARKET YTD.
      *----------------------------------------------------------------*
       4170-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-MARKET-YTD ROUNDED =
                   TRSM-TAX-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-MARKET-YTD
           END-IF
           ADD TRSM-MARKET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E020 ADDR REQUIRED
      * E020 DISTRIB AMT NEGATIVE
      * E360 DISTRIB AMT OVER 1000000
      * E480 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4180-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-ADDR = SPACES
                   MOVE 'E020' TO WS-REJECT-CODE
               WHEN TRST-DISTRIB-AMT < 0
                   MOVE 'E020' TO WS-REJECT-CODE
               WHEN TRST-DISTRIB-AMT > 1000000.00
                   MOVE 'E360' TO WS-REJECT-CODE
               WHEN TRST-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E480' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TAX AMT.
      *----------------------------------------------------------------*
       4190-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TAX-AMT ROUNDED =
                   TRSM-PRINCIPAL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-TAX-AMT
           END-IF
           ADD TRSM-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4200-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4210-DESCRIBE-CHANNEL.
           EVALUATE TRST-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INCOME SCHED TIER FOR THE MARKET YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4220-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-SCHED-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TRSM-MARKET-YTD NOT < WS-INCOME-SCHED-LOW(WS-TX)
                   MOVE WS-INCOME-SCHED-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * H -> M WHEN LAST PAY DATE IS PAST
      * M -> T WHEN LAST PAY DATE IS PAST
      * T -> H WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4230-SET-REASON.
           EVALUATE TRSM-REASON-CD
               WHEN 'H'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INCOME BAND TIER FOR THE INCOME YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4240-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-BAND-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TRSM-INCOME-YTD NOT < WS-INCOME-BAND-LOW(WS-TX)
                   MOVE WS-INCOME-BAND-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4250-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4260-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF TRST-POST-DATE > 0 AND TRST-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRST-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4270-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TRSM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TRUSTEE FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4280-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = TRSM-TRUSTEE-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TRSM-TRUSTEE-FEE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE TRSM-MARKET-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4290-LIMIT-DISTRIB.
           IF TRST-DISTRIB-AMT < 100.00
               MOVE 100.00 TO TRST-DISTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-DISTRIB-AMT > 500.00
               MOVE 500.00 TO TRST-DISTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4300-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TRUSTEE FEE YTD.
      *----------------------------------------------------------------*
       4310-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED =
                   TRST-DISTRIB-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           ADD TRSM-TRUSTEE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE MARKET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TRUSTEE FEE YTD.
      *----------------------------------------------------------------*
       4320-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED =
                   TRSM-MARKET-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           ADD TRSM-TRUSTEE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DIST FREQ CD:
      * E -> K WHEN EFFECTIVE DATE IS PAST
      * K -> P WHEN EFFECTIVE DATE IS PAST
      * P -> W WHEN EFFECTIVE DATE IS PAST
      * W -> Y WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4330-SET-DIST-FREQ.
           EVALUATE TRSM-DIST-FREQ-CD
               WHEN 'E'
                   IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INCOME YTD FEE ON THE TAX YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4340-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRSM-INCOME-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   COMPUTE TRSM-INCOME-YTD ROUNDED = TRSM-TAX-YTD *
                       TRSM-RATE / 200
               WHEN TRSM-CYCLE-CD = 'EN'
                   MOVE 12.00 TO TRSM-INCOME-YTD
               WHEN TRSM-CYCLE-CD = 'FY'
                   MOVE 3.00 TO TRSM-INCOME-YTD
               WHEN OTHER
                   COMPUTE TRSM-INCOME-YTD ROUNDED = TRSM-TAX-YTD *
                       TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-INCOME-YTD > 0 AND TRSM-INCOME-YTD < 2.00
               MOVE 2.00 TO TRSM-INCOME-YTD
           END-IF
           IF TRSM-INCOME-YTD > 35.00
               MOVE 35.00 TO TRSM-INCOME-YTD
           END-IF
           ADD TRSM-INCOME-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4350-FLAG-PRINCIPAL.
           IF TRSM-PRINCIPAL-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-PRINCIPAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E893 REF NO REQUIRED
      * E503 TRUSTEE FEE AMT NEGATIVE
      * E061 TRUSTEE FEE AMT OVER 1000000
      * E602 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4360-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-REF-NO = SPACES
                   MOVE 'E893' TO WS-REJECT-CODE
               WHEN TRST-TRUSTEE-FEE-AMT < 0
                   MOVE 'E503' TO WS-REJECT-CODE
               WHEN TRST-TRUSTEE-FEE-AMT > 1000000.00
                   MOVE 'E061' TO WS-REJECT-CODE
               WHEN TRSM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E602' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E745 DESC REQUIRED
      * E995 TRUSTEE FEE AMT NEGATIVE
      * E511 TRUSTEE FEE AMT OVER 99999
      * E821 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4370-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-DESC = SPACES
                   MOVE 'E745' TO WS-REJECT-CODE
               WHEN TRST-TRUSTEE-FEE-AMT < 0
                   MOVE 'E995' TO WS-REJECT-CODE
               WHEN TRST-TRUSTEE-FEE-AMT > 99999.00
                   MOVE 'E511' TO WS-REJECT-CODE
               WHEN TRSM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E821' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INCOME AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4380-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = TRSM-INCOME-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRSM-INCOME-AMT - (WS-WORK-AMT * 4)
           COMPUTE TRST-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4390-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF TRST-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRST-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TRSM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4400-ACCRUE-TAX.
           IF TRSM-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TAX-AMT * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRST-INCOME-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4410-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TRSM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAX YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4420-FLAG-TAX.
           IF TRSM-TAX-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRUSTEE BAND TIER FOR THE DISTRIB YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4430-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-TRUSTEE-BAND-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TRSM-DISTRIB-YTD NOT < WS-TRUSTEE-BAND-LOW(WS-TX)
                   MOVE WS-TRUSTEE-BAND-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EN -> FY WHEN CLOSE DATE IS PAST
      * FY -> HP WHEN CLOSE DATE IS PAST
      * HP -> JB WHEN CLOSE DATE IS PAST
      * JB -> KZ WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4440-SET-CYCLE.
           EVALUATE TRSM-CYCLE-CD
               WHEN 'EN'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FY' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FY'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HP' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HP'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JB' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JB'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KZ' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4450-COUNT-SEQ.
           IF TRST-SEQ < 9999999
               ADD 1 TO TRST-SEQ
           END-IF
           IF TRST-SEQ NOT < 3
               MOVE 'Y' TO TRSM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4460-SET-CHANNEL.
           EVALUATE TRST-CHANNEL-CD
               WHEN 'BRN'
                   IF TRST-EFF-DATE > 0 AND TRST-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TRST-EFF-DATE > 0 AND TRST-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TRST-EFF-DATE > 0 AND TRST-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TRST-EFF-DATE > 0 AND TRST-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TRS FIELDS TO TRPEN AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-TRPEN.
           INITIALIZE TRPEN-PARMS
           MOVE TRSM-DISTRIB-YTD
             TO TRPEN-DISTRIB-YTD
           MOVE TRSM-TRUSTEE-FEE-AMT
             TO TRPEN-TRUSTEE-FEE-AMT
           MOVE TRSM-INCOME-AMT
             TO TRPEN-INCOME-AMT
           MOVE TRSM-INCOME-YTD
             TO TRPEN-INCOME-YTD
           MOVE TRSM-TAX-AMT
             TO TRPEN-TAX-AMT
           MOVE TRSM-MARKET-YTD
             TO TRPEN-MARKET-YTD
           MOVE TRSM-CLOSE-DATE
             TO TRPEN-CLOSE-DATE
           MOVE TRSM-NOTICE-DATE
             TO TRPEN-NOTICE-DATE
           MOVE TRSM-LAST-STMT-DATE
             TO TRPEN-LAST-STMT-DATE
           MOVE TRSM-OPEN-DATE
             TO TRPEN-OPEN-DATE
           MOVE TRSM-REGION-CD
             TO TRPEN-REGION-CD
           MOVE TRSM-CYCLE-CD
             TO TRPEN-CYCLE-CD
           MOVE TRSM-STATUS-CD
             TO TRPEN-STATUS-CD
           MOVE TRSM-TRUST-TYPE-CD
             TO TRPEN-TRUST-TYPE-CD
           MOVE TRSM-SUSPEND-FLAG
             TO TRPEN-SUSPEND-FLAG
           MOVE TRSM-EXEMPT-FLAG
             TO TRPEN-EXEMPT-FLAG
           MOVE TRSM-PREFERRED-FLAG
             TO TRPEN-PREFERRED-FLAG
           MOVE TRSM-RETRY-CNT
             TO TRPEN-RETRY-CNT
           MOVE TRSM-PAYMENT-CNT
             TO TRPEN-PAYMENT-CNT
           MOVE TRSM-DISCOUNT
             TO TRPEN-DISCOUNT
           MOVE TRSM-RATE
             TO TRPEN-RATE
           MOVE TRSM-ADDR
             TO TRPEN-ADDR
           MOVE TRSM-REF-NO
             TO TRPEN-REF-NO
           CALL 'TRPEN' USING TRPEN-PARMS
           EVALUATE TRPEN-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TRPEN-DISTRIB-YTD
                     TO TRSM-DISTRIB-YTD
                   MOVE TRPEN-TRUSTEE-FEE-AMT
                     TO TRSM-TRUSTEE-FEE-AMT
                   MOVE TRPEN-TAX-AMT
                     TO TRSM-TAX-AMT
                   MOVE TRPEN-MARKET-YTD
                     TO TRSM-MARKET-YTD
                   MOVE TRPEN-EXEMPT-FLAG
                     TO TRSM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-TRPEN' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TRS FIELDS TO TRLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-TRLIM.
           INITIALIZE TRLIM-PARMS
           MOVE TRSM-TRUSTEE-FEE-AMT
             TO TRLIM-TRUSTEE-FEE-AMT
           MOVE TRSM-DISTRIB-YTD
             TO TRLIM-DISTRIB-YTD
           MOVE TRSM-DISTRIB-AMT
             TO TRLIM-DISTRIB-AMT
           MOVE TRSM-INCOME-YTD
             TO TRLIM-INCOME-YTD
           MOVE TRSM-INCOME-AMT
             TO TRLIM-INCOME-AMT
           MOVE TRSM-PRINCIPAL-YTD
             TO TRLIM-PRINCIPAL-YTD
           MOVE TRSM-CLOSE-DATE
             TO TRLIM-CLOSE-DATE
           MOVE TRSM-LAST-MAINT-DATE
             TO TRLIM-LAST-MAINT-DATE
           MOVE TRSM-OPEN-DATE
             TO TRLIM-OPEN-DATE
           MOVE TRSM-NOTICE-DATE
             TO TRLIM-NOTICE-DATE
           MOVE TRSM-CYCLE-CD
             TO TRLIM-CYCLE-CD
           MOVE TRSM-TRUST-TYPE-CD
             TO TRLIM-TRUST-TYPE-CD
           MOVE TRSM-REASON-CD
             TO TRLIM-REASON-CD
           MOVE TRSM-STATUS-CD
             TO TRLIM-STATUS-CD
           MOVE TRSM-STOP-FLAG
             TO TRLIM-STOP-FLAG
           MOVE TRSM-EXEMPT-FLAG
             TO TRLIM-EXEMPT-FLAG
           MOVE TRSM-SUSPEND-FLAG
             TO TRLIM-SUSPEND-FLAG
           MOVE TRSM-PAYMENT-CNT
             TO TRLIM-PAYMENT-CNT
           MOVE TRSM-RETRY-CNT
             TO TRLIM-RETRY-CNT
           MOVE TRSM-MARGIN
             TO TRLIM-MARGIN
           MOVE TRSM-RATE
             TO TRLIM-RATE
           MOVE TRSM-NAME
             TO TRLIM-NAME
           MOVE TRSM-CUST-ID
             TO TRLIM-CUST-ID
           CALL 'TRLIM' USING TRLIM-PARMS
           EVALUATE TRLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TRLIM-DISTRIB-YTD
                     TO TRSM-DISTRIB-YTD
                   MOVE TRLIM-INCOME-YTD
                     TO TRSM-INCOME-YTD
                   MOVE TRLIM-PRINCIPAL-YTD
                     TO TRSM-PRINCIPAL-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-TRLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TRS FIELDS TO TRVAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-TRVAL.
           INITIALIZE TRVAL-PARMS
           MOVE TRSM-MARKET-AMT
             TO TRVAL-MARKET-AMT
           MOVE TRSM-TAX-AMT
             TO TRVAL-TAX-AMT
           MOVE TRSM-PRINCIPAL-YTD
             TO TRVAL-PRINCIPAL-YTD
           MOVE TRSM-DISTRIB-YTD
             TO TRVAL-DISTRIB-YTD
           MOVE TRSM-MARKET-YTD
             TO TRVAL-MARKET-YTD
           MOVE TRSM-PRINCIPAL-AMT
             TO TRVAL-PRINCIPAL-AMT
           MOVE TRSM-NOTICE-DATE
             TO TRVAL-NOTICE-DATE
           MOVE TRSM-LAST-MAINT-DATE
             TO TRVAL-LAST-MAINT-DATE
           MOVE TRSM-MATURITY-DATE
             TO TRVAL-MATURITY-DATE
           MOVE TRSM-CLOSE-DATE
             TO TRVAL-CLOSE-DATE
           MOVE TRSM-REASON-CD
             TO TRVAL-REASON-CD
           MOVE TRSM-REGION-CD
             TO TRVAL-REGION-CD
           MOVE TRSM-STATUS-CD
             TO TRVAL-STATUS-CD
           MOVE TRSM-TRUST-TYPE-CD
             TO TRVAL-TRUST-TYPE-CD
           MOVE TRSM-NOTICE-SENT-FLAG
             TO TRVAL-NOTICE-SENT-FLAG
           MOVE TRSM-REVIEW-FLAG
             TO TRVAL-REVIEW-FLAG
           MOVE TRSM-EXEMPT-FLAG
             TO TRVAL-EXEMPT-FLAG
           MOVE TRSM-CYCLE-CNT
             TO TRVAL-CYCLE-CNT
           MOVE TRSM-NOTICE-CNT
             TO TRVAL-NOTICE-CNT
           MOVE TRSM-RATE
             TO TRVAL-RATE
           MOVE TRSM-MARGIN
             TO TRVAL-MARGIN
           MOVE TRSM-ADDR
             TO TRVAL-ADDR
           MOVE TRSM-CONTACT
             TO TRVAL-CONTACT
           CALL 'TRVAL' USING TRVAL-PARMS
           EVALUATE TRVAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TRVAL-MARKET-AMT
                     TO TRSM-MARKET-AMT
                   MOVE TRVAL-PRINCIPAL-YTD
                     TO TRSM-PRINCIPAL-YTD
                   MOVE TRVAL-MARKET-YTD
                     TO TRSM-MARKET-YTD
                   MOVE TRVAL-PRINCIPAL-AMT
                     TO TRSM-PRINCIPAL-AMT
                   MOVE TRVAL-EXEMPT-FLAG
                     TO TRSM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-TRVAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE TRST-KEY TO RD-KEY
           MOVE TRST-TRAN-CD TO RD-CODE
           MOVE TRST-INCOME-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE TRST-KEY TO RD-KEY
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
           DISPLAY 'TRSAUDT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TRSAUDT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
