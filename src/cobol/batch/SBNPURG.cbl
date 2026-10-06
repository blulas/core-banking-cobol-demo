      *================================================================*
      * PROGRAM  : SBNPURG                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : SAVINGS BOND SERVICES - PURGE AND ARCHIVE. READS THE*
      *            SBN TRANSACTIONS, UPDATES THE SBN MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. US SAVINGS BOND SALES, *
      *            REDEMPTION AND TAX REPORTING FOR CUSTOMERS          *
      * INPUT    : SBNMST (KSDS), SBNTRN, BANKCTL                      *
      * OUTPUT   : SBNOUT, SBNPURPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, SBLIM, SBPEN, SBFEE     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-07-03 LMB  ORIGINAL                                       *
      * 1992-05-10 TWH  AUDIT FINDING - REDEEM CONTROL TOTALS PRB2654  *
      * 1993-01-15 CRP  AUDIT FINDING - PURCHASE CONTROL TOTALS REQ8109*
      * 1994-12-02 GAF  ISSUE-PRICE EXCEPTIONS TO OPS QUEUE INC6324    *
      * 1996-10-10 EWS  ISSUE-PRICE NOTICE TEXT PER LEGAL REQ1950      *
      * 1998-10-14 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-11-09 DPS  ACCRUAL STATUS CODES ADDED INC9200             *
      * 2003-11-19 LMB  REDEEM ROUNDING - HALF UP CHG5799              *
      * 2004-11-20 BHS  TAXABLE RERUN PROTECTION INC3787               *
      * 2005-08-02 LMB  PURCHASE EXCEPTIONS TO OPS QUEUE INC9784       *
      * 2006-06-02 TWH  PURCHASE CUTOFF MOVED PRB1121                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SBNPURG.
       AUTHOR. B H SANDERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "SBNMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS SBNM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "SBNTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "SBNOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "SBNPURPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY SBNMST.
       FD  TRAN-FILE.
       COPY SBNTRN.
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
      * WS-TAXABLE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TAXABLE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000326000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000121500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000638500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000271000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001350000000526000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003850000000386500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004350000000365000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004600000000458000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007100000000810000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007600000000725000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008600000000227500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008700000000119000'.
       01  WS-TAXABLE-BAND-TBL REDEFINES WS-TAXABLE-BAND-VALUES.
           05  WS-TAXABLE-BAND-ROW OCCURS 12 TIMES.
               10  WS-TAXABLE-BAND-LOW     PIC 9(09)V99.
               10  WS-TAXABLE-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FACE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FACE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000469000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000259000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000393000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000650000000301000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005650000000258000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006650000000584500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016650000000332000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021650000000198000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031650000000816500'.
       01  WS-FACE-GRID-TBL REDEFINES WS-FACE-GRID-VALUES.
           05  WS-FACE-GRID-ROW OCCURS 9 TIMES.
               10  WS-FACE-GRID-LOW        PIC 9(09)V99.
               10  WS-FACE-GRID-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-TAXABLE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TAXABLE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000050000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000267000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000593500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006250000000054000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008750000000284500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018750000000135000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018850000000195500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018900000000391500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018950000000287000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019950000000424000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024950000000632000'.
       01  WS-TAXABLE-TIER-TBL REDEFINES WS-TAXABLE-TIER-VALUES.
           05  WS-TAXABLE-TIER-ROW OCCURS 11 TIMES.
               10  WS-TAXABLE-TIER-LOW     PIC 9(09)V99.
               10  WS-TAXABLE-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INTEREST-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000184000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000200500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000628500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025000000000162000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025250000000186500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025300000000190000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027800000000703000'.
       01  WS-INTEREST-SCHED-TBL REDEFINES WS-INTEREST-SCHED-VALUES.
           05  WS-INTEREST-SCHED-ROW OCCURS 7 TIMES.
               10  WS-INTEREST-SCHED-LOW   PIC 9(09)V99.
               10  WS-INTEREST-SCHED-RATE  PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  SBNPURG  SAVINGS BOND SERVICES'.
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
       COPY SBLIMP.
       COPY SBPENP.
       COPY SBFEEP.

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
               MOVE 'SBNMST' TO ABND-FILE-NAME
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
           MOVE SBNT-KEY TO SBNM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ACCRUE-ACCRUAL
               PERFORM 3010-COUNT-EXCEPT
               PERFORM 3020-AGE-MATURITY
               PERFORM 3030-ASSESS-NOTICE-SENT
               PERFORM 3040-DESCRIBE-REASON
               PERFORM 3050-DESCRIBE-BRANCH
               PERFORM 3060-FLAG-TAXABLE
               PERFORM 3070-SPLIT-FACE
               PERFORM 3080-SET-CHANNEL
               PERFORM 3090-SELECT-PENALTY-PCT
               PERFORM 3100-SPLIT-FACE
               PERFORM 3110-PRORATE-ACCRUAL
               PERFORM 3120-SELECT-FEE-PCT
               PERFORM 3130-ACCUM-TRAN
               PERFORM 3140-LIMIT-FACE
               PERFORM 3150-LIMIT-TAXABLE
               PERFORM 3160-ASSESS-HOLD
               PERFORM 3170-SELECT-PENALTY-PCT
               PERFORM 3180-EDIT-REJECT
               PERFORM 3190-COUNT-CYCLE
               PERFORM 3200-LIMIT-TAXABLE
               PERFORM 3210-CHECK-EFFECTIVE
               PERFORM 3220-AGE-POST
               PERFORM 3230-FLAG-REDEEM
               PERFORM 3240-ACCUM-REASON
               PERFORM 3250-EDIT-REF-NO
               PERFORM 3260-EDIT-REF-NO
               PERFORM 3270-FLAG-PURCHASE
               PERFORM 3280-DESCRIBE-SERIES
               PERFORM 3290-FLAG-ISSUE-PRICE
               PERFORM 3300-EDIT-ADDR
               PERFORM 3310-CHECK-POST
               PERFORM 3320-ACCUM-TRAN
               PERFORM 3330-CHECK-POST
               PERFORM 3340-EDIT-REF-NO
               PERFORM 3350-ACCUM-SERIES
               PERFORM 3360-LIMIT-INTEREST
               PERFORM 3370-SET-BRANCH
               PERFORM 3380-EDIT-DESC
               PERFORM 3390-CHECK-LAST-MAINT
               PERFORM 3400-FLAG-ACCRUAL
               PERFORM 3410-LIMIT-TAXABLE
               PERFORM 3420-CHECK-EFF
               PERFORM 3430-DESCRIBE-REASON
               PERFORM 3440-SELECT-PENALTY-PCT
               PERFORM 3450-EDIT-CUST-ID
               PERFORM 3460-EDIT-CONTACT
               PERFORM 3470-PRORATE-TAXABLE
               PERFORM 3480-ACCRUE-FEE-PCT
               PERFORM 3490-DESCRIBE-BRANCH
               PERFORM 3500-DESCRIBE-BRANCH
               PERFORM 3510-COUNT-EXCEPT
               PERFORM 3520-SELECT-MARGIN
               PERFORM 3530-FLAG-PURCHASE
               PERFORM 3540-AGE-EFF
               PERFORM 3550-PRORATE-ACCRUAL
               PERFORM 3560-ACCUM-SERIES
               PERFORM 3570-SELECT-MARGIN
               PERFORM 3580-FLAG-PURCHASE
               PERFORM 3590-LIMIT-ACCRUAL
               PERFORM 3600-LIMIT-PURCHASE
               PERFORM 3610-PRORATE-ACCRUAL
               PERFORM 3620-ACCRUE-REDEEM
               PERFORM 3630-COUNT-RETRY
               PERFORM 3640-EDIT-DESC
               PERFORM 3650-ACCRUE-INTEREST
               PERFORM 3660-SPLIT-ACCRUAL
               PERFORM 3670-COUNT-CYCLE
               PERFORM 3680-PRORATE-FACE
               PERFORM 3690-PRORATE-PURCHASE
               PERFORM 3700-PRORATE-ACCRUAL
               PERFORM 3710-SPLIT-REDEEM
               PERFORM 3720-ACCUM-SERIES
               PERFORM 3730-ACCUM-ISSUE-TYPE
               PERFORM 3740-SPLIT-ISSUE-PRICE
               PERFORM 3750-ASSESS-HOLD
               PERFORM 3760-CHECK-NEXT-REVIEW
               PERFORM 3770-PRORATE-REDEEM
               PERFORM 3780-ACCRUE-PURCHASE
               PERFORM 3790-CHECK-LAST-PAY
               PERFORM 3800-CHECK-NEXT-DUE
               PERFORM 3810-AGE-LAST-PAY
               PERFORM 3820-ASSESS-PREFERRED
               PERFORM 3830-ACCUM-SOURCE
               PERFORM 3840-SET-REASON
               PERFORM 3850-DESCRIBE-SOURCE
               PERFORM 3860-FLAG-REDEEM
               PERFORM 3870-SET-REASON
               PERFORM 3880-SPLIT-PURCHASE
               PERFORM 3890-AGE-EFFECTIVE
               PERFORM 3900-ACCRUE-MARGIN
               PERFORM 3910-SELECT-PENALTY-PCT
               PERFORM 3920-LIMIT-ITEM
               PERFORM 3930-COUNT-PAYMENT
               PERFORM 3940-SELECT-FEE-PCT
               PERFORM 3950-DESCRIBE-REASON
               PERFORM 3960-SET-ISSUE-TYPE
               PERFORM 3970-LIMIT-ACCRUAL
               PERFORM 3980-EDIT-CUST-ID
               PERFORM 3990-COUNT-CYCLE
               PERFORM 4000-DESCRIBE-BRANCH
               PERFORM 4010-CHECK-MATURITY
               PERFORM 4020-SET-TRAN
               PERFORM 4030-SELECT-PENALTY-PCT
               PERFORM 4040-SPLIT-TAXABLE
               PERFORM 4050-EDIT-NAME
               PERFORM 4060-PRORATE-ISSUE-PRICE
               PERFORM 4070-SPLIT-REDEEM
               PERFORM 4080-SELECT-MARGIN
               PERFORM 4090-PRORATE-FACE
               PERFORM 4100-ACCUM-SERIES
               PERFORM 4110-ACCRUE-MARGIN
               PERFORM 4120-ACCRUE-REDEEM
               PERFORM 4130-LIMIT-REDEEM
               PERFORM 4140-DESCRIBE-STATUS
               PERFORM 4150-AGE-MATURITY
               PERFORM 4160-FLAG-ITEM
               PERFORM 4170-ACCRUE-PURCHASE
               PERFORM 4180-FLAG-FACE
               PERFORM 4190-ASSESS-HOLD
               PERFORM 4200-SELECT-PENALTY-PCT
               PERFORM 4210-PRORATE-REDEEM
               PERFORM 4220-ACCUM-TRAN
               PERFORM 4230-AGE-POST
               PERFORM 4240-DESCRIBE-TRAN
               PERFORM 4250-CHECK-EFF
               PERFORM 4260-AGE-LAST-MAINT
               PERFORM 4270-DESCRIBE-BRANCH
               PERFORM 4280-PRORATE-TAXABLE
               PERFORM 4290-CHECK-EFF
               PERFORM 4300-ASSESS-OVERRIDE
               PERFORM 4310-PRORATE-PURCHASE
               PERFORM 4320-CHECK-POST
               PERFORM 4330-CHECK-NEXT-REVIEW
               PERFORM 4340-ASSESS-REVERSAL
               PERFORM 4350-ASSESS-OVERRIDE
               PERFORM 4360-COUNT-SEQ
               PERFORM 4370-COUNT-CYCLE
               PERFORM 4380-LIMIT-FACE
               PERFORM 4390-FLAG-ACCRUAL
               PERFORM 4400-DESCRIBE-REASON
               PERFORM 4410-EDIT-OPERATOR
               PERFORM 4420-ACCUM-BRANCH
               PERFORM 4430-DESCRIBE-REASON
               PERFORM 7020-CALL-SBLIM
               PERFORM 7000-CALL-SBPEN
               PERFORM 7030-CALL-SBFEE
               REWRITE SBN-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'SBNMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM SBN-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ACCRUAL AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3000-ACCRUE-ACCRUAL.
           IF SBNT-ACCRUAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNT-ACCRUAL-AMT * SBNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBNT-REDEEM-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3010-COUNT-EXCEPT.
           IF SBNM-EXCEPT-CNT < 9999999
               ADD 1 TO SBNM-EXCEPT-CNT
           END-IF
           IF SBNM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO SBNM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3020-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-ISSUE-PRICE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNM-ISSUE-PRICE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD SBNM-ISSUE-PRICE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SBNM-ISSUE-PRICE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-ISSUE-PRICE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUE PRICE FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3030-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SBNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO SBNM-ISSUE-PRICE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNT-REVERSAL-FLAG = 'Y'
                   COMPUTE SBNM-ISSUE-PRICE-AMT ROUNDED = WS-WORK-AMT *
                       SBNM-MARGIN / 200
               WHEN SBNM-SERIES-CD = 'DD'
                   MOVE 3.00 TO SBNM-ISSUE-PRICE-AMT
               WHEN SBNM-SERIES-CD = 'DF'
                   MOVE 12.00 TO SBNM-ISSUE-PRICE-AMT
               WHEN OTHER
                   COMPUTE SBNM-ISSUE-PRICE-AMT ROUNDED = WS-WORK-AMT *
                       SBNM-MARGIN / 100
           END-EVALUATE
           IF SBNM-ISSUE-PRICE-AMT > 0 AND SBNM-ISSUE-PRICE-AMT < 1.00
               MOVE 1.00 TO SBNM-ISSUE-PRICE-AMT
           END-IF
           IF SBNM-ISSUE-PRICE-AMT > 100.00
               MOVE 100.00 TO SBNM-ISSUE-PRICE-AMT
           END-IF
           ADD SBNM-ISSUE-PRICE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3040-DESCRIBE-REASON.
           EVALUATE SBNM-REASON-CD
               WHEN 'DG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3050-DESCRIBE-BRANCH.
           EVALUATE SBNM-BRANCH-CD
               WHEN 'BD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TAXABLE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3060-FLAG-TAXABLE.
           IF SBNM-TAXABLE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBNM-TAXABLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FACE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3070-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = SBNM-FACE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = SBNM-FACE-YTD - (WS-WORK-AMT * 3)
           COMPUTE SBNM-PURCHASE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3080-SET-CHANNEL.
           EVALUATE SBNT-CHANNEL-CD
               WHEN 'BRN'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FACE GRID TIER FOR THE TAXABLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3090-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-GRID-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SBNM-TAXABLE-AMT NOT < WS-FACE-GRID-LOW(WS-TX)
                   MOVE WS-FACE-GRID-RATE(WS-TX) TO SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FACE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3100-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = SBNM-FACE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = SBNM-FACE-AMT - (WS-WORK-AMT * 12)
           COMPUTE SBNM-FACE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FACE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ACCRUAL AMT.
      *----------------------------------------------------------------*
       3110-PRORATE-ACCRUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-ACCRUAL-AMT ROUNDED =
                   SBNM-FACE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNM-ACCRUAL-AMT
           END-IF
           ADD SBNM-ACCRUAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE PURCHASE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO SBNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF SBNM-PURCHASE-YTD NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO SBNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REDEEM YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3130-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNT-TRAN-CD
                   ADD SBNM-REDEEM-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-REDEEM-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FACE YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3140-LIMIT-FACE.
           IF SBNM-FACE-YTD < 50.00
               MOVE 50.00 TO SBNM-FACE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-FACE-YTD > 2500.00
               MOVE 2500.00 TO SBNM-FACE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAXABLE AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3150-LIMIT-TAXABLE.
           IF SBNT-TAXABLE-AMT < 0.00
               MOVE 0.00 TO SBNT-TAXABLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNT-TAXABLE-AMT > 5000.00
               MOVE 5000.00 TO SBNT-TAXABLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE FEE ON THE PURCHASE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3160-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN SBNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO SBNM-PURCHASE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE SBNM-PURCHASE-AMT ROUNDED = SBNM-PURCHASE-YTD
                       * SBNM-FEE-PCT / 200
               WHEN SBNM-REASON-CD = 'DG'
                   MOVE 12.00 TO SBNM-PURCHASE-AMT
               WHEN SBNM-REASON-CD = 'UK'
                   MOVE 3.00 TO SBNM-PURCHASE-AMT
               WHEN OTHER
                   COMPUTE SBNM-PURCHASE-AMT ROUNDED = SBNM-PURCHASE-YTD
                       * SBNM-FEE-PCT / 100
           END-EVALUATE
           IF SBNM-PURCHASE-AMT > 0 AND SBNM-PURCHASE-AMT < 2.00
               MOVE 2.00 TO SBNM-PURCHASE-AMT
           END-IF
           IF SBNM-PURCHASE-AMT > 25.00
               MOVE 25.00 TO SBNM-PURCHASE-AMT
           END-IF
           ADD SBNM-PURCHASE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE TAXABLE TIER TIER FOR THE ACCRUAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3170-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TAXABLE-TIER-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF SBNT-ACCRUAL-AMT NOT < WS-TAXABLE-TIER-LOW(WS-TX)
                   MOVE WS-TAXABLE-TIER-RATE(WS-TX) TO SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E754 REJECT CD REQUIRED
      * E728 ISSUE PRICE AMT NEGATIVE
      * E937 ISSUE PRICE AMT OVER 10000
      * E608 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3180-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-REJECT-CD = SPACES
                   MOVE 'E754' TO WS-REJECT-CODE
               WHEN SBNM-ISSUE-PRICE-AMT < 0
                   MOVE 'E728' TO WS-REJECT-CODE
               WHEN SBNM-ISSUE-PRICE-AMT > 10000.00
                   MOVE 'E937' TO WS-REJECT-CODE
               WHEN SBNM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E608' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3190-COUNT-CYCLE.
           IF SBNM-CYCLE-CNT < 9999999
               ADD 1 TO SBNM-CYCLE-CNT
           END-IF
           IF SBNM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO SBNM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TAXABLE YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3200-LIMIT-TAXABLE.
           IF SBNM-TAXABLE-YTD < 10.00
               MOVE 10.00 TO SBNM-TAXABLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-TAXABLE-YTD > 50000.00
               MOVE 50000.00 TO SBNM-TAXABLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3210-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF SBNM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO SBNM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3220-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REDEEM AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3230-FLAG-REDEEM.
           IF SBNT-REDEEM-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBNT-REDEEM-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REDEEM AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3240-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-REASON-CD
                   ADD SBNT-REDEEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNT-REDEEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E543 REF NO REQUIRED
      * E922 TAXABLE AMT NEGATIVE
      * E004 TAXABLE AMT OVER 25000
      * E750 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3250-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-REF-NO = SPACES
                   MOVE 'E543' TO WS-REJECT-CODE
               WHEN SBNM-TAXABLE-AMT < 0
                   MOVE 'E922' TO WS-REJECT-CODE
               WHEN SBNM-TAXABLE-AMT > 25000.00
                   MOVE 'E004' TO WS-REJECT-CODE
               WHEN SBNT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E750' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E189 REF NO REQUIRED
      * E686 ACCRUAL YTD NEGATIVE
      * E957 ACCRUAL YTD OVER 99999
      * E116 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3260-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-REF-NO = SPACES
                   MOVE 'E189' TO WS-REJECT-CODE
               WHEN SBNM-ACCRUAL-YTD < 0
                   MOVE 'E686' TO WS-REJECT-CODE
               WHEN SBNM-ACCRUAL-YTD > 99999.00
                   MOVE 'E957' TO WS-REJECT-CODE
               WHEN SBNM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E116' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3270-FLAG-PURCHASE.
           IF SBNM-PURCHASE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBNM-PURCHASE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SERIES CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3280-DESCRIBE-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'SS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ISSUE PRICE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3290-FLAG-ISSUE-PRICE.
           IF SBNM-ISSUE-PRICE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBNM-ISSUE-PRICE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E556 ADDR REQUIRED
      * E234 WORK AMT 2 NEGATIVE
      * E787 WORK AMT 2 OVER 5000
      * E385 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3300-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-ADDR = SPACES
                   MOVE 'E556' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E234' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E787' TO WS-REJECT-CODE
               WHEN SBNM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E385' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3310-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF SBNT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO SBNM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3320-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNT-TRAN-CD
                   ADD SBNT-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNT-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3330-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF SBNT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO SBNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E048 REF NO REQUIRED
      * E013 INTEREST YTD NEGATIVE
      * E447 INTEREST YTD OVER 10000
      * E468 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3340-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-REF-NO = SPACES
                   MOVE 'E048' TO WS-REJECT-CODE
               WHEN SBNM-INTEREST-YTD < 0
                   MOVE 'E013' TO WS-REJECT-CODE
               WHEN SBNM-INTEREST-YTD > 10000.00
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN SBNM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E468' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REDEEM AMT BY SERIES CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3350-ACCUM-SERIES.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-SERIES-CD
                   ADD SBNT-REDEEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-SERIES-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNT-REDEEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3360-LIMIT-INTEREST.
           IF SBNT-INTEREST-AMT < 25.00
               MOVE 25.00 TO SBNT-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNT-INTEREST-AMT > 2500.00
               MOVE 2500.00 TO SBNT-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * BD -> FC WHEN NEXT DUE DATE IS PAST
      * FC -> KR WHEN NEXT DUE DATE IS PAST
      * KR -> MP WHEN NEXT DUE DATE IS PAST
      * MP -> VJ WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3370-SET-BRANCH.
           EVALUATE SBNM-BRANCH-CD
               WHEN 'BD'
                   IF SBNM-NEXT-DUE-DATE > 0 AND SBNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FC' TO SBNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FC'
                   IF SBNM-NEXT-DUE-DATE > 0 AND SBNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO SBNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF SBNM-NEXT-DUE-DATE > 0 AND SBNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MP' TO SBNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MP'
                   IF SBNM-NEXT-DUE-DATE > 0 AND SBNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E233 DESC REQUIRED
      * E234 REDEEM AMT NEGATIVE
      * E361 REDEEM AMT OVER 25000
      * E061 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3380-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-DESC = SPACES
                   MOVE 'E233' TO WS-REJECT-CODE
               WHEN SBNT-REDEEM-AMT < 0
                   MOVE 'E234' TO WS-REJECT-CODE
               WHEN SBNT-REDEEM-AMT > 25000.00
                   MOVE 'E361' TO WS-REJECT-CODE
               WHEN SBNT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E061' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3390-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SBNM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SBNM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUAL YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3400-FLAG-ACCRUAL.
           IF SBNM-ACCRUAL-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBNM-ACCRUAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TAXABLE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3410-LIMIT-TAXABLE.
           IF SBNT-TAXABLE-AMT < 50.00
               MOVE 50.00 TO SBNT-TAXABLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNT-TAXABLE-AMT > 10000.00
               MOVE 10000.00 TO SBNT-TAXABLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3420-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF SBNT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3430-DESCRIBE-REASON.
           EVALUATE SBNM-REASON-CD
               WHEN 'DG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE TAXABLE BAND TIER FOR THE PURCHASE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3440-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TAXABLE-BAND-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SBNM-PURCHASE-AMT NOT < WS-TAXABLE-BAND-LOW(WS-TX)
                   MOVE WS-TAXABLE-BAND-RATE(WS-TX) TO SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E138 CUST ID REQUIRED
      * E757 INTEREST AMT NEGATIVE
      * E232 INTEREST AMT OVER 5000
      * E115 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3450-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-CUST-ID = SPACES
                   MOVE 'E138' TO WS-REJECT-CODE
               WHEN SBNT-INTEREST-AMT < 0
                   MOVE 'E757' TO WS-REJECT-CODE
               WHEN SBNT-INTEREST-AMT > 5000.00
                   MOVE 'E232' TO WS-REJECT-CODE
               WHEN SBNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E115' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E207 CONTACT REQUIRED
      * E356 ACCRUAL AMT NEGATIVE
      * E473 ACCRUAL AMT OVER 10000
      * E123 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3460-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-CONTACT = SPACES
                   MOVE 'E207' TO WS-REJECT-CODE
               WHEN SBNT-ACCRUAL-AMT < 0
                   MOVE 'E356' TO WS-REJECT-CODE
               WHEN SBNT-ACCRUAL-AMT > 10000.00
                   MOVE 'E473' TO WS-REJECT-CODE
               WHEN SBNM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E123' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ACCRUAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TAXABLE AMT.
      *----------------------------------------------------------------*
       3470-PRORATE-TAXABLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNT-TAXABLE-AMT ROUNDED =
                   SBNM-ACCRUAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNT-TAXABLE-AMT
           END-IF
           ADD SBNT-TAXABLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3480-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * SBNM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBNM-ACCRUAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3490-DESCRIBE-BRANCH.
           EVALUATE SBNM-BRANCH-CD
               WHEN 'BD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-BRANCH.
           EVALUATE SBNM-BRANCH-CD
               WHEN 'BD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-EXCEPT.
           IF SBNM-EXCEPT-CNT < 9999999
               ADD 1 TO SBNM-EXCEPT-CNT
           END-IF
           IF SBNM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO SBNM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAXABLE BAND TIER FOR THE REDEEM YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3520-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-TAXABLE-BAND-RATE(1) TO SBNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SBNM-REDEEM-YTD NOT < WS-TAXABLE-BAND-LOW(WS-TX)
                   MOVE WS-TAXABLE-BAND-RATE(WS-TX) TO SBNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3530-FLAG-PURCHASE.
           IF SBNM-PURCHASE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBNM-PURCHASE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3540-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF SBNT-EFF-DATE > 0 AND SBNT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-TAXABLE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD SBNM-TAXABLE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SBNM-TAXABLE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-TAXABLE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-TAXABLE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REDEEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ACCRUAL YTD.
      *----------------------------------------------------------------*
       3550-PRORATE-ACCRUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-ACCRUAL-YTD ROUNDED =
                   SBNT-REDEEM-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-ACCRUAL-YTD
           END-IF
           ADD SBNM-ACCRUAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE ACCRUAL AMT BY SERIES CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3560-ACCUM-SERIES.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-SERIES-CD
                   ADD SBNT-ACCRUAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-SERIES-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNT-ACCRUAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAXABLE TIER TIER FOR THE TAXABLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3570-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-TAXABLE-TIER-RATE(1) TO SBNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF SBNM-TAXABLE-YTD NOT < WS-TAXABLE-TIER-LOW(WS-TX)
                   MOVE WS-TAXABLE-TIER-RATE(WS-TX) TO SBNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3580-FLAG-PURCHASE.
           IF SBNM-PURCHASE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBNM-PURCHASE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUAL AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3590-LIMIT-ACCRUAL.
           IF SBNM-ACCRUAL-AMT < 0.00
               MOVE 0.00 TO SBNM-ACCRUAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-ACCRUAL-AMT > 2500.00
               MOVE 2500.00 TO SBNM-ACCRUAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3600-LIMIT-PURCHASE.
           IF SBNM-PURCHASE-AMT < 50.00
               MOVE 50.00 TO SBNM-PURCHASE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-PURCHASE-AMT > 1000.00
               MOVE 1000.00 TO SBNM-PURCHASE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REDEEM YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ACCRUAL AMT.
      *----------------------------------------------------------------*
       3610-PRORATE-ACCRUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNT-ACCRUAL-AMT ROUNDED =
                   SBNM-REDEEM-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNT-ACCRUAL-AMT
           END-IF
           ADD SBNT-ACCRUAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REDEEM YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3620-ACCRUE-REDEEM.
           IF SBNM-REDEEM-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-REDEEM-YTD * SBNM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBNM-TAXABLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3630-COUNT-RETRY.
           IF SBNM-RETRY-CNT < 9999999
               ADD 1 TO SBNM-RETRY-CNT
           END-IF
           IF SBNM-RETRY-CNT NOT < 5
               MOVE 'Y' TO SBNM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E630 DESC REQUIRED
      * E641 WORK AMT NEGATIVE
      * E896 WORK AMT OVER 99999
      * E581 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-DESC = SPACES
                   MOVE 'E630' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E641' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E896' TO WS-REJECT-CODE
               WHEN SBNM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E581' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3650-ACCRUE-INTEREST.
           IF SBNM-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-INTEREST-AMT * SBNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO SBNM-TAXABLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ACCRUAL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3660-SPLIT-ACCRUAL.
           COMPUTE WS-WORK-AMT = SBNT-ACCRUAL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SBNT-ACCRUAL-AMT - (WS-WORK-AMT * 2)
           COMPUTE SBNM-TAXABLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-CYCLE.
           IF SBNM-CYCLE-CNT < 9999999
               ADD 1 TO SBNM-CYCLE-CNT
           END-IF
           IF SBNM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO SBNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAXABLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FACE AMT.
      *----------------------------------------------------------------*
       3680-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-FACE-AMT ROUNDED =
                   SBNT-TAXABLE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNM-FACE-AMT
           END-IF
           ADD SBNM-FACE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE REDEEM YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PURCHASE YTD.
      *----------------------------------------------------------------*
       3690-PRORATE-PURCHASE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-PURCHASE-YTD ROUNDED =
                   SBNM-REDEEM-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-PURCHASE-YTD
           END-IF
           ADD SBNM-PURCHASE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE ISSUE PRICE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ACCRUAL AMT.
      *----------------------------------------------------------------*
       3700-PRORATE-ACCRUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNT-ACCRUAL-AMT ROUNDED =
                   SBNM-ISSUE-PRICE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNT-ACCRUAL-AMT
           END-IF
           ADD SBNT-ACCRUAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3710-SPLIT-REDEEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE SBNM-REDEEM-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY SERIES CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3720-ACCUM-SERIES.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-SERIES-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-SERIES-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REDEEM AMT BY ISSUE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3730-ACCUM-ISSUE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-ISSUE-TYPE-CD
                   ADD SBNT-REDEEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-ISSUE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNT-REDEEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUE PRICE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3740-SPLIT-ISSUE-PRICE.
           COMPUTE WS-WORK-AMT = SBNM-ISSUE-PRICE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SBNM-ISSUE-PRICE-AMT - (WS-WORK-AMT *
               2)
           COMPUTE SBNT-TAXABLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE FEE ON THE REDEEM YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3750-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN SBNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO SBNM-PURCHASE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-PREFERRED-FLAG = 'Y'
                   COMPUTE SBNM-PURCHASE-AMT ROUNDED = SBNM-REDEEM-YTD *
                       SBNM-MARGIN / 200
               WHEN SBNT-TRAN-CD = 'BY'
                   MOVE 3.00 TO SBNM-PURCHASE-AMT
               WHEN SBNT-TRAN-CD = 'RD'
                   MOVE 3.00 TO SBNM-PURCHASE-AMT
               WHEN OTHER
                   COMPUTE SBNM-PURCHASE-AMT ROUNDED = SBNM-REDEEM-YTD *
                       SBNM-MARGIN / 100
           END-EVALUATE
           IF SBNM-PURCHASE-AMT > 0 AND SBNM-PURCHASE-AMT < 10.00
               MOVE 10.00 TO SBNM-PURCHASE-AMT
           END-IF
           IF SBNM-PURCHASE-AMT > 25.00
               MOVE 25.00 TO SBNM-PURCHASE-AMT
           END-IF
           ADD SBNM-PURCHASE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3760-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF SBNM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REDEEM AMT.
      *----------------------------------------------------------------*
       3770-PRORATE-REDEEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-REDEEM-AMT ROUNDED =
                   SBNM-INTEREST-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-REDEEM-AMT
           END-IF
           ADD SBNM-REDEEM-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PURCHASE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3780-ACCRUE-PURCHASE.
           IF SBNM-PURCHASE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-PURCHASE-YTD * SBNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBNM-ISSUE-PRICE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3790-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF SBNM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO SBNM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SBNM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO SBNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3810-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SBNM-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SBNM-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FACE YTD FEE ON THE INTEREST AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3820-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SBNM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SBNM-FACE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-HOLD-FLAG = 'Y'
                   COMPUTE SBNM-FACE-YTD ROUNDED = SBNT-INTEREST-AMT *
                       SBNM-FEE-PCT / 200
               WHEN SBNM-ISSUE-TYPE-CD = 'CW'
                   MOVE 7.00 TO SBNM-FACE-YTD
               WHEN SBNM-ISSUE-TYPE-CD = 'DY'
                   MOVE 5.00 TO SBNM-FACE-YTD
               WHEN OTHER
                   COMPUTE SBNM-FACE-YTD ROUNDED = SBNT-INTEREST-AMT *
                       SBNM-FEE-PCT / 100
           END-EVALUATE
           IF SBNM-FACE-YTD > 0 AND SBNM-FACE-YTD < 10.00
               MOVE 10.00 TO SBNM-FACE-YTD
           END-IF
           IF SBNM-FACE-YTD > 75.00
               MOVE 75.00 TO SBNM-FACE-YTD
           END-IF
           ADD SBNM-FACE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3830-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-SOURCE-CD
                   ADD SBNM-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DG -> UK WHEN EFFECTIVE DATE IS PAST
      * UK -> VH WHEN EFFECTIVE DATE IS PAST
      * VH -> XD WHEN EFFECTIVE DATE IS PAST
      * XD -> DG WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3840-SET-REASON.
           EVALUATE SBNM-REASON-CD
               WHEN 'DG'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UK' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VH' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VH'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DG' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3850-DESCRIBE-SOURCE.
           EVALUATE SBNM-SOURCE-CD
               WHEN 'E'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'J'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'L'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'U'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REDEEM YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3860-FLAG-REDEEM.
           IF SBNM-REDEEM-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBNM-REDEEM-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DG -> UK WHEN LAST PAY DATE IS PAST
      * UK -> VH WHEN LAST PAY DATE IS PAST
      * VH -> XD WHEN LAST PAY DATE IS PAST
      * XD -> DG WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3870-SET-REASON.
           EVALUATE SBNM-REASON-CD
               WHEN 'DG'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UK' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VH' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VH'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DG' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PURCHASE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3880-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = SBNM-PURCHASE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SBNM-PURCHASE-AMT - (WS-WORK-AMT * 3)
           COMPUTE SBNM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3890-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * SBNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO SBNM-TAXABLE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAXABLE TIER TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3910-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TAXABLE-TIER-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF SBNM-INTEREST-YTD NOT < WS-TAXABLE-TIER-LOW(WS-TX)
                   MOVE WS-TAXABLE-TIER-RATE(WS-TX) TO SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3920-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3930-COUNT-PAYMENT.
           IF SBNM-PAYMENT-CNT < 9999999
               ADD 1 TO SBNM-PAYMENT-CNT
           END-IF
           IF SBNM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO SBNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAXABLE BAND TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3940-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TAXABLE-BAND-RATE(1) TO SBNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SBNM-INTEREST-YTD NOT < WS-TAXABLE-BAND-LOW(WS-TX)
                   MOVE WS-TAXABLE-BAND-RATE(WS-TX) TO SBNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3950-DESCRIBE-REASON.
           EVALUATE SBNM-REASON-CD
               WHEN 'DG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ISSUE TYPE CD:
      * CW -> DY WHEN LAST PAY DATE IS PAST
      * DY -> JA WHEN LAST PAY DATE IS PAST
      * JA -> KT WHEN LAST PAY DATE IS PAST
      * KT -> MF WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3960-SET-ISSUE-TYPE.
           EVALUATE SBNM-ISSUE-TYPE-CD
               WHEN 'CW'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DY' TO SBNM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DY'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JA' TO SBNM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KT' TO SBNM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KT'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MF' TO SBNM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCRUAL YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3970-LIMIT-ACCRUAL.
           IF SBNM-ACCRUAL-YTD < 10.00
               MOVE 10.00 TO SBNM-ACCRUAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-ACCRUAL-YTD > 5000.00
               MOVE 5000.00 TO SBNM-ACCRUAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E914 CUST ID REQUIRED
      * E327 ISSUE PRICE AMT NEGATIVE
      * E590 ISSUE PRICE AMT OVER 99999
      * E421 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3980-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-CUST-ID = SPACES
                   MOVE 'E914' TO WS-REJECT-CODE
               WHEN SBNM-ISSUE-PRICE-AMT < 0
                   MOVE 'E327' TO WS-REJECT-CODE
               WHEN SBNM-ISSUE-PRICE-AMT > 99999.00
                   MOVE 'E590' TO WS-REJECT-CODE
               WHEN SBNM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E421' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3990-COUNT-CYCLE.
           IF SBNM-CYCLE-CNT < 9999999
               ADD 1 TO SBNM-CYCLE-CNT
           END-IF
           IF SBNM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO SBNM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4000-DESCRIBE-BRANCH.
           EVALUATE SBNM-BRANCH-CD
               WHEN 'BD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4010-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SBNM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO SBNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BY -> RD WHEN OPEN DATE IS PAST
      * RD -> IR WHEN OPEN DATE IS PAST
      * IR -> TX WHEN OPEN DATE IS PAST
      * TX -> AD WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4020-SET-TRAN.
           EVALUATE SBNT-TRAN-CD
               WHEN 'BY'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'IR' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'IR'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TX'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE ACCRUAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4030-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF SBNM-ACCRUAL-YTD NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO
                       SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAXABLE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4040-SPLIT-TAXABLE.
           COMPUTE WS-WORK-AMT = SBNM-TAXABLE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = SBNM-TAXABLE-YTD - (WS-WORK-AMT * 2)
           COMPUTE SBNT-ACCRUAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E900 NAME REQUIRED
      * E417 ISSUE PRICE YTD NEGATIVE
      * E817 ISSUE PRICE YTD OVER 250000
      * E536 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4050-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-NAME = SPACES
                   MOVE 'E900' TO WS-REJECT-CODE
               WHEN SBNM-ISSUE-PRICE-YTD < 0
                   MOVE 'E417' TO WS-REJECT-CODE
               WHEN SBNM-ISSUE-PRICE-YTD > 250000.00
                   MOVE 'E817' TO WS-REJECT-CODE
               WHEN SBNM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E536' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ISSUE PRICE YTD.
      *----------------------------------------------------------------*
       4060-PRORATE-ISSUE-PRICE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-ISSUE-PRICE-YTD ROUNDED =
                   SBNT-INTEREST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNM-ISSUE-PRICE-YTD
           END-IF
           ADD SBNM-ISSUE-PRICE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT REDEEM YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4070-SPLIT-REDEEM.
           COMPUTE WS-WORK-AMT = SBNM-REDEEM-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SBNM-REDEEM-YTD - (WS-WORK-AMT * 4)
           COMPUTE SBNM-PURCHASE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4080-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO SBNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO SBNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FACE YTD.
      *----------------------------------------------------------------*
       4090-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-FACE-YTD ROUNDED =
                   SBNM-PURCHASE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-FACE-YTD
           END-IF
           ADD SBNM-FACE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY SERIES CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4100-ACCUM-SERIES.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-SERIES-CD
                   ADD SBNM-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-SERIES-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4110-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * SBNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO SBNM-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REDEEM YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4120-ACCRUE-REDEEM.
           IF SBNM-REDEEM-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-REDEEM-YTD * SBNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REDEEM YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4130-LIMIT-REDEEM.
           IF SBNM-REDEEM-YTD < 100.00
               MOVE 100.00 TO SBNM-REDEEM-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-REDEEM-YTD > 10000.00
               MOVE 10000.00 TO SBNM-REDEEM-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4140-DESCRIBE-STATUS.
           EVALUATE SBNM-STATUS-CD
               WHEN 'CS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4150-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNT-ACCRUAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SBNT-ACCRUAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SBNT-ACCRUAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD SBNT-ACCRUAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNT-ACCRUAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4160-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PURCHASE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4170-ACCRUE-PURCHASE.
           IF SBNM-PURCHASE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-PURCHASE-YTD * SBNM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBNT-ACCRUAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4180-FLAG-FACE.
           IF SBNM-FACE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBNM-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE YTD FEE ON THE ISSUE PRICE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4190-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN SBNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO SBNM-PURCHASE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SBNM-PURCHASE-YTD ROUNDED =
                       SBNM-ISSUE-PRICE-YTD * SBNM-MARGIN / 200
               WHEN SBNM-SOURCE-CD = 'E'
                   MOVE 15.00 TO SBNM-PURCHASE-YTD
               WHEN SBNM-SOURCE-CD = 'J'
                   MOVE 5.00 TO SBNM-PURCHASE-YTD
               WHEN OTHER
                   COMPUTE SBNM-PURCHASE-YTD ROUNDED =
                       SBNM-ISSUE-PRICE-YTD * SBNM-MARGIN / 100
           END-EVALUATE
           IF SBNM-PURCHASE-YTD > 0 AND SBNM-PURCHASE-YTD < 10.00
               MOVE 10.00 TO SBNM-PURCHASE-YTD
           END-IF
           IF SBNM-PURCHASE-YTD > 35.00
               MOVE 35.00 TO SBNM-PURCHASE-YTD
           END-IF
           ADD SBNM-PURCHASE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4200-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF SBNM-INTEREST-AMT NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO
                       SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAXABLE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REDEEM AMT.
      *----------------------------------------------------------------*
       4210-PRORATE-REDEEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNT-REDEEM-AMT ROUNDED =
                   SBNM-TAXABLE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNT-REDEEM-AMT
           END-IF
           ADD SBNT-REDEEM-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE PURCHASE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4220-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNT-TRAN-CD
                   ADD SBNM-PURCHASE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-PURCHASE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       4230-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4240-DESCRIBE-TRAN.
           EVALUATE SBNT-TRAN-CD
               WHEN 'BY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'IR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'AD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4250-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF SBNT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO SBNM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4260-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SBNM-LAST-MAINT-DATE > 0 AND SBNM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4270-DESCRIBE-BRANCH.
           EVALUATE SBNM-BRANCH-CD
               WHEN 'BD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REDEEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TAXABLE YTD.
      *----------------------------------------------------------------*
       4280-PRORATE-TAXABLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-TAXABLE-YTD ROUNDED =
                   SBNM-REDEEM-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-TAXABLE-YTD
           END-IF
           ADD SBNM-TAXABLE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4290-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF SBNT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SBNM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE TAXABLE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4300-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SBNM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SBNM-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-PREFERRED-FLAG = 'Y'
                   COMPUTE SBNM-INTEREST-AMT ROUNDED = SBNT-TAXABLE-AMT
                       * SBNM-FEE-PCT / 200
               WHEN SBNM-ISSUE-TYPE-CD = 'CW'
                   MOVE 15.00 TO SBNM-INTEREST-AMT
               WHEN SBNM-ISSUE-TYPE-CD = 'DY'
                   MOVE 10.00 TO SBNM-INTEREST-AMT
               WHEN OTHER
                   COMPUTE SBNM-INTEREST-AMT ROUNDED = SBNT-TAXABLE-AMT
                       * SBNM-FEE-PCT / 100
           END-EVALUATE
           IF SBNM-INTEREST-AMT > 0 AND SBNM-INTEREST-AMT < 1.00
               MOVE 1.00 TO SBNM-INTEREST-AMT
           END-IF
           IF SBNM-INTEREST-AMT > 100.00
               MOVE 100.00 TO SBNM-INTEREST-AMT
           END-IF
           ADD SBNM-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE REDEEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PURCHASE AMT.
      *----------------------------------------------------------------*
       4310-PRORATE-PURCHASE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-PURCHASE-AMT ROUNDED =
                   SBNM-REDEEM-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-PURCHASE-AMT
           END-IF
           ADD SBNM-PURCHASE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4320-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF SBNT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO SBNM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4330-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF SBNM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO SBNM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE YTD FEE ON THE FACE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4340-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN SBNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO SBNM-PURCHASE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-HOLD-FLAG = 'Y'
                   COMPUTE SBNM-PURCHASE-YTD ROUNDED = SBNM-FACE-YTD *
                       SBNM-FEE-PCT / 200
               WHEN SBNM-BRANCH-CD = 'BD'
                   MOVE 7.00 TO SBNM-PURCHASE-YTD
               WHEN SBNM-BRANCH-CD = 'FC'
                   MOVE 15.00 TO SBNM-PURCHASE-YTD
               WHEN OTHER
                   COMPUTE SBNM-PURCHASE-YTD ROUNDED = SBNM-FACE-YTD *
                       SBNM-FEE-PCT / 100
           END-EVALUATE
           IF SBNM-PURCHASE-YTD > 0 AND SBNM-PURCHASE-YTD < 10.00
               MOVE 10.00 TO SBNM-PURCHASE-YTD
           END-IF
           IF SBNM-PURCHASE-YTD > 75.00
               MOVE 75.00 TO SBNM-PURCHASE-YTD
           END-IF
           ADD SBNM-PURCHASE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE REDEEM YTD FEE ON THE PURCHASE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4350-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SBNM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SBNM-REDEEM-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE SBNM-REDEEM-YTD ROUNDED = SBNM-PURCHASE-YTD *
                       SBNM-FEE-PCT / 200
               WHEN SBNM-BRANCH-CD = 'BD'
                   MOVE 10.00 TO SBNM-REDEEM-YTD
               WHEN SBNM-BRANCH-CD = 'FC'
                   MOVE 5.00 TO SBNM-REDEEM-YTD
               WHEN OTHER
                   COMPUTE SBNM-REDEEM-YTD ROUNDED = SBNM-PURCHASE-YTD *
                       SBNM-FEE-PCT / 100
           END-EVALUATE
           IF SBNM-REDEEM-YTD > 0 AND SBNM-REDEEM-YTD < 5.00
               MOVE 5.00 TO SBNM-REDEEM-YTD
           END-IF
           IF SBNM-REDEEM-YTD > 35.00
               MOVE 35.00 TO SBNM-REDEEM-YTD
           END-IF
           ADD SBNM-REDEEM-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4360-COUNT-SEQ.
           IF SBNT-SEQ < 9999999
               ADD 1 TO SBNT-SEQ
           END-IF
           IF SBNT-SEQ NOT < 12
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4370-COUNT-CYCLE.
           IF SBNM-CYCLE-CNT < 9999999
               ADD 1 TO SBNM-CYCLE-CNT
           END-IF
           IF SBNM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO SBNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4380-LIMIT-FACE.
           IF SBNM-FACE-AMT < 10.00
               MOVE 10.00 TO SBNM-FACE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-FACE-AMT > 10000.00
               MOVE 10000.00 TO SBNM-FACE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUAL YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4390-FLAG-ACCRUAL.
           IF SBNM-ACCRUAL-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBNM-ACCRUAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4400-DESCRIBE-REASON.
           EVALUATE SBNM-REASON-CD
               WHEN 'DG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E814 OPERATOR REQUIRED
      * E319 PURCHASE AMT NEGATIVE
      * E679 PURCHASE AMT OVER 250000
      * E278 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4410-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-OPERATOR = SPACES
                   MOVE 'E814' TO WS-REJECT-CODE
               WHEN SBNM-PURCHASE-AMT < 0
                   MOVE 'E319' TO WS-REJECT-CODE
               WHEN SBNM-PURCHASE-AMT > 250000.00
                   MOVE 'E679' TO WS-REJECT-CODE
               WHEN SBNM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E278' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4420-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-BRANCH-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4430-DESCRIBE-REASON.
           EVALUATE SBNM-REASON-CD
               WHEN 'DG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE SBN FIELDS TO SBLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-SBLIM.
           INITIALIZE SBLIM-PARMS
           MOVE SBNM-ISSUE-PRICE-YTD
             TO SBLIM-ISSUE-PRICE-YTD
           MOVE SBNM-FACE-YTD
             TO SBLIM-FACE-YTD
           MOVE SBNM-PURCHASE-YTD
             TO SBLIM-PURCHASE-YTD
           MOVE SBNM-TAXABLE-AMT
             TO SBLIM-TAXABLE-AMT
           MOVE SBNM-PURCHASE-AMT
             TO SBLIM-PURCHASE-AMT
           MOVE SBNM-TAXABLE-YTD
             TO SBLIM-TAXABLE-YTD
           MOVE SBNM-LAST-MAINT-DATE
             TO SBLIM-LAST-MAINT-DATE
           MOVE SBNM-NOTICE-DATE
             TO SBLIM-NOTICE-DATE
           MOVE SBNM-MATURITY-DATE
             TO SBLIM-MATURITY-DATE
           MOVE SBNM-NEXT-REVIEW-DATE
             TO SBLIM-NEXT-REVIEW-DATE
           MOVE SBNM-SOURCE-CD
             TO SBLIM-SOURCE-CD
           MOVE SBNM-REASON-CD
             TO SBLIM-REASON-CD
           MOVE SBNM-ISSUE-TYPE-CD
             TO SBLIM-ISSUE-TYPE-CD
           MOVE SBNM-SERIES-CD
             TO SBLIM-SERIES-CD
           MOVE SBNM-OVERRIDE-FLAG
             TO SBLIM-OVERRIDE-FLAG
           MOVE SBNM-PREFERRED-FLAG
             TO SBLIM-PREFERRED-FLAG
           MOVE SBNM-HOLD-FLAG
             TO SBLIM-HOLD-FLAG
           MOVE SBNM-PAYMENT-CNT
             TO SBLIM-PAYMENT-CNT
           MOVE SBNM-EXCEPT-CNT
             TO SBLIM-EXCEPT-CNT
           MOVE SBNM-PENALTY-PCT
             TO SBLIM-PENALTY-PCT
           MOVE SBNM-FEE-PCT
             TO SBLIM-FEE-PCT
           MOVE SBNM-REF-NO
             TO SBLIM-REF-NO
           MOVE SBNM-ADDR
             TO SBLIM-ADDR
           CALL 'SBLIM' USING SBLIM-PARMS
           EVALUATE SBLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SBLIM-PURCHASE-YTD
                     TO SBNM-PURCHASE-YTD
                   MOVE SBLIM-TAXABLE-YTD
                     TO SBNM-TAXABLE-YTD
                   MOVE SBLIM-PREFERRED-FLAG
                     TO SBNM-PREFERRED-FLAG
                   MOVE SBLIM-HOLD-FLAG
                     TO SBNM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-SBLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE SBN FIELDS TO SBPEN AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-SBPEN.
           INITIALIZE SBPEN-PARMS
           MOVE SBNM-ACCRUAL-YTD
             TO SBPEN-ACCRUAL-YTD
           MOVE SBNM-PURCHASE-YTD
             TO SBPEN-PURCHASE-YTD
           MOVE SBNM-PURCHASE-AMT
             TO SBPEN-PURCHASE-AMT
           MOVE SBNM-REDEEM-AMT
             TO SBPEN-REDEEM-AMT
           MOVE SBNM-ACCRUAL-AMT
             TO SBPEN-ACCRUAL-AMT
           MOVE SBNM-INTEREST-YTD
             TO SBPEN-INTEREST-YTD
           MOVE SBNM-NOTICE-DATE
             TO SBPEN-NOTICE-DATE
           MOVE SBNM-LAST-MAINT-DATE
             TO SBPEN-LAST-MAINT-DATE
           MOVE SBNM-NEXT-REVIEW-DATE
             TO SBPEN-NEXT-REVIEW-DATE
           MOVE SBNM-NEXT-DUE-DATE
             TO SBPEN-NEXT-DUE-DATE
           MOVE SBNM-STATUS-CD
             TO SBPEN-STATUS-CD
           MOVE SBNM-SERIES-CD
             TO SBPEN-SERIES-CD
           MOVE SBNM-BRANCH-CD
             TO SBPEN-BRANCH-CD
           MOVE SBNM-SOURCE-CD
             TO SBPEN-SOURCE-CD
           MOVE SBNM-PREFERRED-FLAG
             TO SBPEN-PREFERRED-FLAG
           MOVE SBNM-HOLD-FLAG
             TO SBPEN-HOLD-FLAG
           MOVE SBNM-OVERRIDE-FLAG
             TO SBPEN-OVERRIDE-FLAG
           MOVE SBNM-PAYMENT-CNT
             TO SBPEN-PAYMENT-CNT
           MOVE SBNM-CYCLE-CNT
             TO SBPEN-CYCLE-CNT
           MOVE SBNM-MARGIN
             TO SBPEN-MARGIN
           MOVE SBNM-FEE-PCT
             TO SBPEN-FEE-PCT
           MOVE SBNM-REF-NO
             TO SBPEN-REF-NO
           MOVE SBNM-ADDR
             TO SBPEN-ADDR
           CALL 'SBPEN' USING SBPEN-PARMS
           EVALUATE SBPEN-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SBPEN-PURCHASE-YTD
                     TO SBNM-PURCHASE-YTD
                   MOVE SBPEN-ACCRUAL-AMT
                     TO SBNM-ACCRUAL-AMT
                   MOVE SBPEN-INTEREST-YTD
                     TO SBNM-INTEREST-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-SBPEN' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE SBN FIELDS TO SBFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-SBFEE.
           INITIALIZE SBFEE-PARMS
           MOVE SBNM-PURCHASE-AMT
             TO SBFEE-PURCHASE-AMT
           MOVE SBNM-FACE-YTD
             TO SBFEE-FACE-YTD
           MOVE SBNM-REDEEM-YTD
             TO SBFEE-REDEEM-YTD
           MOVE SBNM-ACCRUAL-AMT
             TO SBFEE-ACCRUAL-AMT
           MOVE SBNM-INTEREST-YTD
             TO SBFEE-INTEREST-YTD
           MOVE SBNM-TAXABLE-AMT
             TO SBFEE-TAXABLE-AMT
           MOVE SBNM-NOTICE-DATE
             TO SBFEE-NOTICE-DATE
           MOVE SBNM-NEXT-DUE-DATE
             TO SBFEE-NEXT-DUE-DATE
           MOVE SBNM-OPEN-DATE
             TO SBFEE-OPEN-DATE
           MOVE SBNM-LAST-MAINT-DATE
             TO SBFEE-LAST-MAINT-DATE
           MOVE SBNM-REASON-CD
             TO SBFEE-REASON-CD
           MOVE SBNM-STATUS-CD
             TO SBFEE-STATUS-CD
           MOVE SBNM-BRANCH-CD
             TO SBFEE-BRANCH-CD
           MOVE SBNM-ISSUE-TYPE-CD
             TO SBFEE-ISSUE-TYPE-CD
           MOVE SBNM-AUTO-RENEW-FLAG
             TO SBFEE-AUTO-RENEW-FLAG
           MOVE SBNM-NOTICE-SENT-FLAG
             TO SBFEE-NOTICE-SENT-FLAG
           MOVE SBNM-REVIEW-FLAG
             TO SBFEE-REVIEW-FLAG
           MOVE SBNM-RETRY-CNT
             TO SBFEE-RETRY-CNT
           MOVE SBNM-EXCEPT-CNT
             TO SBFEE-EXCEPT-CNT
           MOVE SBNM-MARGIN
             TO SBFEE-MARGIN
           MOVE SBNM-PENALTY-PCT
             TO SBFEE-PENALTY-PCT
           MOVE SBNM-CONTACT
             TO SBFEE-CONTACT
           MOVE SBNM-DESC
             TO SBFEE-DESC
           CALL 'SBFEE' USING SBFEE-PARMS
           EVALUATE SBFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SBFEE-PURCHASE-AMT
                     TO SBNM-PURCHASE-AMT
                   MOVE SBFEE-FACE-YTD
                     TO SBNM-FACE-YTD
                   MOVE SBFEE-REDEEM-YTD
                     TO SBNM-REDEEM-YTD
                   MOVE SBFEE-INTEREST-YTD
                     TO SBNM-INTEREST-YTD
                   MOVE SBFEE-TAXABLE-AMT
                     TO SBNM-TAXABLE-AMT
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-SBFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE SBNT-KEY TO RD-KEY
           MOVE SBNT-TRAN-CD TO RD-CODE
           MOVE SBNT-REDEEM-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE SBNT-KEY TO RD-KEY
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
           DISPLAY 'SBNPURG READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'SBNPURG' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
