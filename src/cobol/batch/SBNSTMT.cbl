      *================================================================*
      * PROGRAM  : SBNSTMT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : SAVINGS BOND SERVICES - STATEMENT PREPARATION. READS*
      *            THE SBN TRANSACTIONS, UPDATES THE SBN MASTER AND    *
      *            PRODUCES THE CONTROL REPORT. US SAVINGS BOND SALES, *
      *            REDEMPTION AND TAX REPORTING FOR CUSTOMERS          *
      * INPUT    : SBNMST (KSDS), SBNTRN, BANKCTL                      *
      * OUTPUT   : SBNOUT, SBNSTRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, SBFEE                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-12-18 EWS  ORIGINAL                                       *
      * 1993-05-08 PQR  PERFORMANCE - TAXABLE READ AHEAD REQ4007       *
      * 1998-02-20 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2005-11-23 JLM  FACE CUTOFF MOVED CHG4571                      *
      * 2007-09-24 RJT  REDEEM LIMIT REVISED CHG7061                   *
      * 2009-06-14 TWH  NEW TAXABLE REPORT COLUMNS REQ5975             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SBNSTMT.
       AUTHOR. C R PATEL.

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
           SELECT REPORT-FILE ASSIGN TO "SBNSTRPT"
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
                                           VALUE 23.000000.
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
      * WS-FACE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FACE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000270000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000710500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000167000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000400000000828500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005400000000072000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005450000000768500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005550000000564000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015550000000055000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025550000000457000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030550000000293500'.
       01  WS-FACE-SCHED-TBL REDEFINES WS-FACE-SCHED-VALUES.
           05  WS-FACE-SCHED-ROW OCCURS 10 TIMES.
               10  WS-FACE-SCHED-LOW       PIC 9(09)V99.
               10  WS-FACE-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-REDEEM-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REDEEM-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000567000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000152000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000254000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015050000000357500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016050000000024000'.
       01  WS-REDEEM-SCHED-TBL REDEFINES WS-REDEEM-SCHED-VALUES.
           05  WS-REDEEM-SCHED-ROW OCCURS 5 TIMES.
               10  WS-REDEEM-SCHED-LOW     PIC 9(09)V99.
               10  WS-REDEEM-SCHED-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ACCRUAL-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ACCRUAL-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000471000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000230000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000604000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000680500'.
       01  WS-ACCRUAL-TIER-TBL REDEFINES WS-ACCRUAL-TIER-VALUES.
           05  WS-ACCRUAL-TIER-ROW OCCURS 4 TIMES.
               10  WS-ACCRUAL-TIER-LOW     PIC 9(09)V99.
               10  WS-ACCRUAL-TIER-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  SBNSTMT  SAVINGS BOND SERVICES'.
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
               PERFORM 3000-DESCRIBE-STATUS
               PERFORM 3010-ACCUM-STATUS
               PERFORM 3020-SPLIT-REDEEM
               PERFORM 3030-ACCUM-STATUS
               PERFORM 3040-ACCUM-REASON
               PERFORM 3050-AGE-EXPIRY
               PERFORM 3060-LIMIT-PURCHASE
               PERFORM 3070-FLAG-ACCRUAL
               PERFORM 3080-FLAG-PURCHASE
               PERFORM 3090-SET-REASON
               PERFORM 3100-PRORATE-FACE
               PERFORM 3110-CHECK-LAST-MAINT
               PERFORM 3120-SPLIT-FACE
               PERFORM 3130-CHECK-EXPIRY
               PERFORM 3140-SET-TRAN
               PERFORM 3150-FLAG-ACCRUAL
               PERFORM 3160-SPLIT-ISSUE-PRICE
               PERFORM 3170-ACCUM-TRAN
               PERFORM 3180-SPLIT-TAXABLE
               PERFORM 3190-PRORATE-REDEEM
               PERFORM 3200-ACCRUE-FACE
               PERFORM 3210-COUNT-CYCLE
               PERFORM 3220-DESCRIBE-CHANNEL
               PERFORM 3230-COUNT-RETRY
               PERFORM 3240-ASSESS-REVERSAL
               PERFORM 3250-AGE-MATURITY
               PERFORM 3260-LIMIT-PURCHASE
               PERFORM 3270-CHECK-NOTICE
               PERFORM 3280-AGE-MATURITY
               PERFORM 3290-AGE-NOTICE
               PERFORM 3300-SELECT-PENALTY-PCT
               PERFORM 3310-FLAG-ISSUE-PRICE
               PERFORM 3320-CHECK-LAST-PAY
               PERFORM 3330-ASSESS-REVERSAL
               PERFORM 3340-FLAG-ISSUE-PRICE
               PERFORM 3350-SET-REASON
               PERFORM 3360-ACCRUE-INTEREST
               PERFORM 3370-SPLIT-REDEEM
               PERFORM 3380-AGE-OPEN
               PERFORM 3390-CHECK-LAST-MAINT
               PERFORM 3400-DESCRIBE-REASON
               PERFORM 3410-ACCRUE-ACCRUAL
               PERFORM 3420-EDIT-CUST-ID
               PERFORM 3430-FLAG-PURCHASE
               PERFORM 3440-ASSESS-PREFERRED
               PERFORM 3450-EDIT-REF-NO
               PERFORM 3460-COUNT-SEQ
               PERFORM 3470-PRORATE-REDEEM
               PERFORM 3480-COUNT-PAYMENT
               PERFORM 3490-DESCRIBE-REASON
               PERFORM 3500-PRORATE-TAXABLE
               PERFORM 3510-AGE-EFF
               PERFORM 3520-LIMIT-FACE
               PERFORM 3530-EDIT-REJECT
               PERFORM 3540-COUNT-RETRY
               PERFORM 3550-AGE-EXPIRY
               PERFORM 3560-SPLIT-REDEEM
               PERFORM 3570-FLAG-INTEREST
               PERFORM 3580-ASSESS-REVERSAL
               PERFORM 3590-ASSESS-PREFERRED
               PERFORM 3600-FLAG-REDEEM
               PERFORM 3610-COUNT-CYCLE
               PERFORM 3620-PRORATE-ISSUE-PRICE
               PERFORM 3630-EDIT-ADDR
               PERFORM 3640-SELECT-PENALTY-PCT
               PERFORM 3650-EDIT-ADDR
               PERFORM 3660-COUNT-SEQ
               PERFORM 3670-LIMIT-TAXABLE
               PERFORM 3680-FLAG-FACE
               PERFORM 3690-SET-TRAN
               PERFORM 3700-AGE-OPEN
               PERFORM 3710-COUNT-EXCEPT
               PERFORM 3720-ASSESS-REVERSAL
               PERFORM 3730-LIMIT-ACCRUAL
               PERFORM 3740-DESCRIBE-REASON
               PERFORM 3750-AGE-EFF
               PERFORM 3760-AGE-EFF
               PERFORM 3770-ASSESS-OVERRIDE
               PERFORM 3780-SPLIT-INTEREST
               PERFORM 3790-ASSESS-REVERSAL
               PERFORM 3800-EDIT-MEMO
               PERFORM 3810-SET-ISSUE-TYPE
               PERFORM 3820-CHECK-EFFECTIVE
               PERFORM 3830-ASSESS-HOLD
               PERFORM 3840-FLAG-PURCHASE
               PERFORM 3850-COUNT-RETRY
               PERFORM 3860-ACCRUE-TAXABLE
               PERFORM 3870-ACCUM-ISSUE-TYPE
               PERFORM 3880-LIMIT-TAXABLE
               PERFORM 3890-EDIT-REF-NO
               PERFORM 3900-AGE-LAST-MAINT
               PERFORM 3910-ACCRUE-TAXABLE
               PERFORM 3920-LIMIT-ITEM
               PERFORM 3930-ACCUM-SOURCE
               PERFORM 3940-ACCUM-TRAN
               PERFORM 3950-SPLIT-FACE
               PERFORM 3960-SET-SOURCE
               PERFORM 3970-PRORATE-PURCHASE
               PERFORM 3980-AGE-POST
               PERFORM 3990-SELECT-FEE-PCT
               PERFORM 4000-PRORATE-PURCHASE
               PERFORM 4010-ASSESS-OVERRIDE
               PERFORM 4020-ACCUM-ISSUE-TYPE
               PERFORM 4030-SPLIT-ACCRUAL
               PERFORM 4040-AGE-EFFECTIVE
               PERFORM 4050-COUNT-CYCLE
               PERFORM 4060-SET-SOURCE
               PERFORM 4070-COUNT-RETRY
               PERFORM 4080-PRORATE-ACCRUAL
               PERFORM 4090-FLAG-ACCRUAL
               PERFORM 4100-EDIT-REJECT
               PERFORM 4110-FLAG-REDEEM
               PERFORM 4120-COUNT-CYCLE
               PERFORM 4130-EDIT-OPERATOR
               PERFORM 4140-EDIT-MEMO
               PERFORM 4150-PRORATE-INTEREST
               PERFORM 4160-SET-BRANCH
               PERFORM 4170-ASSESS-NOTICE-SENT
               PERFORM 4180-SET-CHANNEL
               PERFORM 4190-SPLIT-ACCRUAL
               PERFORM 4200-EDIT-OPERATOR
               PERFORM 4210-DESCRIBE-ISSUE-TYPE
               PERFORM 4220-DESCRIBE-SERIES
               PERFORM 4230-SET-STATUS
               PERFORM 4240-ACCUM-SOURCE
               PERFORM 4250-ACCRUE-FACE
               PERFORM 4260-COUNT-CYCLE
               PERFORM 4270-ACCRUE-ACCRUAL
               PERFORM 4280-SPLIT-TAXABLE
               PERFORM 4290-SELECT-FEE-PCT
               PERFORM 4300-SELECT-PENALTY-PCT
               PERFORM 4310-ACCUM-STATUS
               PERFORM 4320-AGE-POST
               PERFORM 4330-AGE-EFF
               PERFORM 4340-PRORATE-PURCHASE
               PERFORM 4350-SELECT-FEE-PCT
               PERFORM 4360-ACCRUE-ISSUE-PRICE
               PERFORM 4370-PRORATE-REDEEM
               PERFORM 4380-SPLIT-REDEEM
               PERFORM 4390-LIMIT-INTEREST
               PERFORM 4400-LIMIT-INTEREST
               PERFORM 4410-PRORATE-TAXABLE
               PERFORM 4420-DESCRIBE-STATUS
               PERFORM 4430-LIMIT-ISSUE-PRICE
               PERFORM 4440-SET-SERIES
               PERFORM 4450-FLAG-FACE
               PERFORM 4460-SPLIT-TAXABLE
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
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3000-DESCRIBE-STATUS.
           EVALUATE SBNM-STATUS-CD
               WHEN 'CS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3010-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-STATUS-CD
                   ADD SBNM-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REDEEM YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3020-SPLIT-REDEEM.
           COMPUTE WS-WORK-AMT = SBNM-REDEEM-YTD / 2
           COMPUTE WS-WORK-AMT-2 = SBNM-REDEEM-YTD - (WS-WORK-AMT * 2)
           COMPUTE SBNM-PURCHASE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3030-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-STATUS-CD
                   ADD SBNM-FACE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-FACE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAXABLE YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3040-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-REASON-CD
                   ADD SBNM-TAXABLE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-TAXABLE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3050-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF SBNM-EXPIRY-DATE > 0 AND SBNM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PURCHASE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3060-LIMIT-PURCHASE.
           IF SBNM-PURCHASE-AMT < 50.00
               MOVE 50.00 TO SBNM-PURCHASE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-PURCHASE-AMT > 10000.00
               MOVE 10000.00 TO SBNM-PURCHASE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUAL YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3070-FLAG-ACCRUAL.
           IF SBNM-ACCRUAL-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBNM-ACCRUAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3080-FLAG-PURCHASE.
           IF SBNM-PURCHASE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBNM-PURCHASE-AMT TO WS-EXC-AMOUNT
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
       3090-SET-REASON.
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
      * PRORATE FACE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FACE AMT.
      *----------------------------------------------------------------*
       3100-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-FACE-AMT ROUNDED =
                   SBNM-FACE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNM-FACE-AMT
           END-IF
           ADD SBNM-FACE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3110-CHECK-LAST-MAINT.
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
               MOVE 'Y' TO SBNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FACE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3120-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = SBNM-FACE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = SBNM-FACE-YTD - (WS-WORK-AMT * 12)
           COMPUTE SBNM-FACE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF SBNM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO SBNM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BY -> RD WHEN LAST PAY DATE IS PAST
      * RD -> IR WHEN LAST PAY DATE IS PAST
      * IR -> TX WHEN LAST PAY DATE IS PAST
      * TX -> AD WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3140-SET-TRAN.
           EVALUATE SBNT-TRAN-CD
               WHEN 'BY'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'IR' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'IR'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TX'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCRUAL AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3150-FLAG-ACCRUAL.
           IF SBNT-ACCRUAL-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBNT-ACCRUAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUE PRICE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3160-SPLIT-ISSUE-PRICE.
           COMPUTE WS-WORK-AMT = SBNM-ISSUE-PRICE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SBNM-ISSUE-PRICE-YTD - (WS-WORK-AMT *
               4)
           COMPUTE SBNM-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUE PRICE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNT-TRAN-CD
                   ADD SBNM-ISSUE-PRICE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-ISSUE-PRICE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAXABLE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3180-SPLIT-TAXABLE.
           COMPUTE WS-WORK-AMT = SBNM-TAXABLE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = SBNM-TAXABLE-YTD - (WS-WORK-AMT * 2)
           COMPUTE SBNM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FACE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REDEEM AMT.
      *----------------------------------------------------------------*
       3190-PRORATE-REDEEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNT-REDEEM-AMT ROUNDED =
                   SBNM-FACE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNT-REDEEM-AMT
           END-IF
           ADD SBNT-REDEEM-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3200-ACCRUE-FACE.
           IF SBNM-FACE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-FACE-AMT * SBNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO SBNM-ISSUE-PRICE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-CYCLE.
           IF SBNM-CYCLE-CNT < 9999999
               ADD 1 TO SBNM-CYCLE-CNT
           END-IF
           IF SBNM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3220-DESCRIBE-CHANNEL.
           EVALUATE SBNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3230-COUNT-RETRY.
           IF SBNM-RETRY-CNT < 9999999
               ADD 1 TO SBNM-RETRY-CNT
           END-IF
           IF SBNM-RETRY-CNT NOT < 10
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REDEEM YTD FEE ON THE INTEREST YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3240-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN SBNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO SBNM-REDEEM-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE SBNM-REDEEM-YTD ROUNDED = SBNM-INTEREST-YTD *
                       SBNM-FEE-PCT / 200
               WHEN SBNM-STATUS-CD = 'CS'
                   MOVE 3.00 TO SBNM-REDEEM-YTD
               WHEN SBNM-STATUS-CD = 'HF'
                   MOVE 5.00 TO SBNM-REDEEM-YTD
               WHEN OTHER
                   COMPUTE SBNM-REDEEM-YTD ROUNDED = SBNM-INTEREST-YTD *
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
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3250-AGE-MATURITY.
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
                   ADD SBNM-ISSUE-PRICE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNM-ISSUE-PRICE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SBNM-ISSUE-PRICE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-ISSUE-PRICE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-ISSUE-PRICE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PURCHASE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3260-LIMIT-PURCHASE.
           IF SBNM-PURCHASE-AMT < 10.00
               MOVE 10.00 TO SBNM-PURCHASE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-PURCHASE-AMT > 50000.00
               MOVE 50000.00 TO SBNM-PURCHASE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3270-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBNM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3280-AGE-MATURITY.
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
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3290-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBNM-NOTICE-DATE > 0 AND SBNM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-TAXABLE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNM-TAXABLE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SBNM-TAXABLE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD SBNM-TAXABLE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-TAXABLE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FACE SCHED TIER FOR THE REDEEM AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3300-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-SCHED-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SBNT-REDEEM-AMT NOT < WS-FACE-SCHED-LOW(WS-TX)
                   MOVE WS-FACE-SCHED-RATE(WS-TX) TO SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ISSUE PRICE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3310-FLAG-ISSUE-PRICE.
           IF SBNM-ISSUE-PRICE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBNM-ISSUE-PRICE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3320-CHECK-LAST-PAY.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO SBNM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUAL FEE ON THE INTEREST AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3330-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN SBNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO SBNT-ACCRUAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-HOLD-FLAG = 'Y'
                   COMPUTE SBNT-ACCRUAL-AMT ROUNDED = SBNM-INTEREST-AMT
                       * SBNM-MARGIN / 200
               WHEN SBNM-ISSUE-TYPE-CD = 'CW'
                   MOVE 12.00 TO SBNT-ACCRUAL-AMT
               WHEN SBNM-ISSUE-TYPE-CD = 'DY'
                   MOVE 12.00 TO SBNT-ACCRUAL-AMT
               WHEN OTHER
                   COMPUTE SBNT-ACCRUAL-AMT ROUNDED = SBNM-INTEREST-AMT
                       * SBNM-MARGIN / 100
           END-EVALUATE
           IF SBNT-ACCRUAL-AMT > 0 AND SBNT-ACCRUAL-AMT < 10.00
               MOVE 10.00 TO SBNT-ACCRUAL-AMT
           END-IF
           IF SBNT-ACCRUAL-AMT > 150.00
               MOVE 150.00 TO SBNT-ACCRUAL-AMT
           END-IF
           ADD SBNT-ACCRUAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ISSUE PRICE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3340-FLAG-ISSUE-PRICE.
           IF SBNM-ISSUE-PRICE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBNM-ISSUE-PRICE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DG -> UK WHEN EXPIRY DATE IS PAST
      * UK -> VH WHEN EXPIRY DATE IS PAST
      * VH -> XD WHEN EXPIRY DATE IS PAST
      * XD -> DG WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3350-SET-REASON.
           EVALUATE SBNM-REASON-CD
               WHEN 'DG'
                   IF SBNM-EXPIRY-DATE > 0 AND SBNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UK' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF SBNM-EXPIRY-DATE > 0 AND SBNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VH' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VH'
                   IF SBNM-EXPIRY-DATE > 0 AND SBNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF SBNM-EXPIRY-DATE > 0 AND SBNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DG' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3360-ACCRUE-INTEREST.
           IF SBNT-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNT-INTEREST-AMT * SBNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO SBNM-REDEEM-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REDEEM AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3370-SPLIT-REDEEM.
           COMPUTE WS-WORK-AMT = SBNT-REDEEM-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SBNT-REDEEM-AMT - (WS-WORK-AMT * 2)
           COMPUTE SBNM-TAXABLE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3380-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-ACCRUAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNM-ACCRUAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SBNM-ACCRUAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-ACCRUAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-ACCRUAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO SBNM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3400-DESCRIBE-REASON.
           EVALUATE SBNM-REASON-CD
               WHEN 'DG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ACCRUAL YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3410-ACCRUE-ACCRUAL.
           IF SBNM-ACCRUAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-ACCRUAL-YTD * SBNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E657 CUST ID REQUIRED
      * E421 INTEREST AMT NEGATIVE
      * E865 INTEREST AMT OVER 50000
      * E795 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3420-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-CUST-ID = SPACES
                   MOVE 'E657' TO WS-REJECT-CODE
               WHEN SBNT-INTEREST-AMT < 0
                   MOVE 'E421' TO WS-REJECT-CODE
               WHEN SBNT-INTEREST-AMT > 50000.00
                   MOVE 'E865' TO WS-REJECT-CODE
               WHEN SBNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E795' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3430-FLAG-PURCHASE.
           IF SBNM-PURCHASE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBNM-PURCHASE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE FACE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3440-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SBNM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SBNM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNT-REVERSAL-FLAG = 'Y'
                   COMPUTE SBNM-INTEREST-YTD ROUNDED = SBNM-FACE-AMT *
                       SBNM-MARGIN / 200
               WHEN SBNM-STATUS-CD = 'CS'
                   MOVE 3.00 TO SBNM-INTEREST-YTD
               WHEN SBNM-STATUS-CD = 'HF'
                   MOVE 12.00 TO SBNM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE SBNM-INTEREST-YTD ROUNDED = SBNM-FACE-AMT *
                       SBNM-MARGIN / 100
           END-EVALUATE
           IF SBNM-INTEREST-YTD > 0 AND SBNM-INTEREST-YTD < 5.00
               MOVE 5.00 TO SBNM-INTEREST-YTD
           END-IF
           IF SBNM-INTEREST-YTD > 50.00
               MOVE 50.00 TO SBNM-INTEREST-YTD
           END-IF
           ADD SBNM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E681 REF NO REQUIRED
      * E287 ACCRUAL YTD NEGATIVE
      * E553 ACCRUAL YTD OVER 50000
      * E498 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3450-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-REF-NO = SPACES
                   MOVE 'E681' TO WS-REJECT-CODE
               WHEN SBNM-ACCRUAL-YTD < 0
                   MOVE 'E287' TO WS-REJECT-CODE
               WHEN SBNM-ACCRUAL-YTD > 50000.00
                   MOVE 'E553' TO WS-REJECT-CODE
               WHEN SBNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E498' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3460-COUNT-SEQ.
           IF SBNT-SEQ < 9999999
               ADD 1 TO SBNT-SEQ
           END-IF
           IF SBNT-SEQ NOT < 5
               MOVE 'Y' TO SBNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUE PRICE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REDEEM AMT.
      *----------------------------------------------------------------*
       3470-PRORATE-REDEEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNT-REDEEM-AMT ROUNDED =
                   SBNM-ISSUE-PRICE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNT-REDEEM-AMT
           END-IF
           ADD SBNT-REDEEM-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3480-COUNT-PAYMENT.
           IF SBNM-PAYMENT-CNT < 9999999
               ADD 1 TO SBNM-PAYMENT-CNT
           END-IF
           IF SBNM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO SBNM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3490-DESCRIBE-REASON.
           EVALUATE SBNM-REASON-CD
               WHEN 'DG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TAXABLE AMT.
      *----------------------------------------------------------------*
       3500-PRORATE-TAXABLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-TAXABLE-AMT ROUNDED =
                   SBNM-PURCHASE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-TAXABLE-AMT
           END-IF
           ADD SBNM-TAXABLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3510-AGE-EFF.
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
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FACE AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3520-LIMIT-FACE.
           IF SBNM-FACE-AMT < 25.00
               MOVE 25.00 TO SBNM-FACE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-FACE-AMT > 500.00
               MOVE 500.00 TO SBNM-FACE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E730 REJECT CD REQUIRED
      * E207 REDEEM YTD NEGATIVE
      * E324 REDEEM YTD OVER 25000
      * E311 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3530-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-REJECT-CD = SPACES
                   MOVE 'E730' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-YTD < 0
                   MOVE 'E207' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-YTD > 25000.00
                   MOVE 'E324' TO WS-REJECT-CODE
               WHEN SBNM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E311' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3540-COUNT-RETRY.
           IF SBNM-RETRY-CNT < 9999999
               ADD 1 TO SBNM-RETRY-CNT
           END-IF
           IF SBNM-RETRY-CNT NOT < 3
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3550-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF SBNM-EXPIRY-DATE > 0 AND SBNM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNT-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SBNT-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SBNT-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SBNT-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNT-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT REDEEM AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3560-SPLIT-REDEEM.
           COMPUTE WS-WORK-AMT = SBNM-REDEEM-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SBNM-REDEEM-AMT - (WS-WORK-AMT * 2)
           COMPUTE SBNM-PURCHASE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3570-FLAG-INTEREST.
           IF SBNT-INTEREST-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBNT-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUAL FEE ON THE REDEEM YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3580-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN SBNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO SBNT-ACCRUAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-PREFERRED-FLAG = 'Y'
                   COMPUTE SBNT-ACCRUAL-AMT ROUNDED = SBNM-REDEEM-YTD *
                       SBNM-PENALTY-PCT / 200
               WHEN SBNT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO SBNT-ACCRUAL-AMT
               WHEN SBNT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO SBNT-ACCRUAL-AMT
               WHEN OTHER
                   COMPUTE SBNT-ACCRUAL-AMT ROUNDED = SBNM-REDEEM-YTD *
                       SBNM-PENALTY-PCT / 100
           END-EVALUATE
           IF SBNT-ACCRUAL-AMT > 0 AND SBNT-ACCRUAL-AMT < 5.00
               MOVE 5.00 TO SBNT-ACCRUAL-AMT
           END-IF
           IF SBNT-ACCRUAL-AMT > 250.00
               MOVE 250.00 TO SBNT-ACCRUAL-AMT
           END-IF
           ADD SBNT-ACCRUAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUAL YTD FEE ON THE FACE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3590-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SBNM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SBNM-ACCRUAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE SBNM-ACCRUAL-YTD ROUNDED = SBNM-FACE-YTD *
                       SBNM-PENALTY-PCT / 200
               WHEN SBNT-TRAN-CD = 'BY'
                   MOVE 20.00 TO SBNM-ACCRUAL-YTD
               WHEN SBNT-TRAN-CD = 'RD'
                   MOVE 15.00 TO SBNM-ACCRUAL-YTD
               WHEN OTHER
                   COMPUTE SBNM-ACCRUAL-YTD ROUNDED = SBNM-FACE-YTD *
                       SBNM-PENALTY-PCT / 100
           END-EVALUATE
           IF SBNM-ACCRUAL-YTD > 0 AND SBNM-ACCRUAL-YTD < 1.00
               MOVE 1.00 TO SBNM-ACCRUAL-YTD
           END-IF
           IF SBNM-ACCRUAL-YTD > 250.00
               MOVE 250.00 TO SBNM-ACCRUAL-YTD
           END-IF
           ADD SBNM-ACCRUAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * REDEEM AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3600-FLAG-REDEEM.
           IF SBNM-REDEEM-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBNM-REDEEM-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3610-COUNT-CYCLE.
           IF SBNM-CYCLE-CNT < 9999999
               ADD 1 TO SBNM-CYCLE-CNT
           END-IF
           IF SBNM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO SBNM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUE PRICE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3620-PRORATE-ISSUE-PRICE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   SBNM-ISSUE-PRICE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E246 ADDR REQUIRED
      * E874 REDEEM YTD NEGATIVE
      * E430 REDEEM YTD OVER 99999
      * E230 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3630-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-ADDR = SPACES
                   MOVE 'E246' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-YTD < 0
                   MOVE 'E874' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-YTD > 99999.00
                   MOVE 'E430' TO WS-REJECT-CODE
               WHEN SBNM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E230' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REDEEM SCHED TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3640-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REDEEM-SCHED-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SBNM-INTEREST-YTD NOT < WS-REDEEM-SCHED-LOW(WS-TX)
                   MOVE WS-REDEEM-SCHED-RATE(WS-TX) TO SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E815 ADDR REQUIRED
      * E426 TAXABLE AMT NEGATIVE
      * E439 TAXABLE AMT OVER 1000000
      * E191 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3650-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-ADDR = SPACES
                   MOVE 'E815' TO WS-REJECT-CODE
               WHEN SBNM-TAXABLE-AMT < 0
                   MOVE 'E426' TO WS-REJECT-CODE
               WHEN SBNM-TAXABLE-AMT > 1000000.00
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN SBNM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E191' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3660-COUNT-SEQ.
           IF SBNT-SEQ < 9999999
               ADD 1 TO SBNT-SEQ
           END-IF
           IF SBNT-SEQ NOT < 12
               MOVE 'Y' TO SBNM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TAXABLE YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3670-LIMIT-TAXABLE.
           IF SBNM-TAXABLE-YTD < 25.00
               MOVE 25.00 TO SBNM-TAXABLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-TAXABLE-YTD > 10000.00
               MOVE 10000.00 TO SBNM-TAXABLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3680-FLAG-FACE.
           IF SBNM-FACE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBNM-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BY -> RD WHEN MATURITY DATE IS PAST
      * RD -> IR WHEN MATURITY DATE IS PAST
      * IR -> TX WHEN MATURITY DATE IS PAST
      * TX -> AD WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3690-SET-TRAN.
           EVALUATE SBNT-TRAN-CD
               WHEN 'BY'
                   IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'IR' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'IR'
                   IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TX'
                   IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNT-TAXABLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNT-TAXABLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SBNT-TAXABLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD SBNT-TAXABLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNT-TAXABLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3710-COUNT-EXCEPT.
           IF SBNM-EXCEPT-CNT < 9999999
               ADD 1 TO SBNM-EXCEPT-CNT
           END-IF
           IF SBNM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO SBNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE INTEREST AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3720-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN SBNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = SBNM-INTEREST-AMT *
                       SBNM-FEE-PCT / 200
               WHEN SBNM-SOURCE-CD = 'E'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN SBNM-SOURCE-CD = 'J'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = SBNM-INTEREST-AMT *
                       SBNM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCRUAL YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3730-LIMIT-ACCRUAL.
           IF SBNM-ACCRUAL-YTD < 0.00
               MOVE 0.00 TO SBNM-ACCRUAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-ACCRUAL-YTD > 50000.00
               MOVE 50000.00 TO SBNM-ACCRUAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3740-DESCRIBE-REASON.
           EVALUATE SBNM-REASON-CD
               WHEN 'DG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3750-AGE-EFF.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3760-AGE-EFF.
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
                   ADD SBNM-PURCHASE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD SBNM-PURCHASE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SBNM-PURCHASE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-PURCHASE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-PURCHASE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE REDEEM YTD FEE ON THE ISSUE PRICE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3770-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SBNM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SBNM-REDEEM-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-REVIEW-FLAG = 'Y'
                   COMPUTE SBNM-REDEEM-YTD ROUNDED =
                       SBNM-ISSUE-PRICE-YTD * SBNM-PENALTY-PCT / 200
               WHEN SBNM-SOURCE-CD = 'E'
                   MOVE 7.00 TO SBNM-REDEEM-YTD
               WHEN SBNM-SOURCE-CD = 'J'
                   MOVE 5.00 TO SBNM-REDEEM-YTD
               WHEN OTHER
                   COMPUTE SBNM-REDEEM-YTD ROUNDED =
                       SBNM-ISSUE-PRICE-YTD * SBNM-PENALTY-PCT / 100
           END-EVALUATE
           IF SBNM-REDEEM-YTD > 0 AND SBNM-REDEEM-YTD < 1.00
               MOVE 1.00 TO SBNM-REDEEM-YTD
           END-IF
           IF SBNM-REDEEM-YTD > 150.00
               MOVE 150.00 TO SBNM-REDEEM-YTD
           END-IF
           ADD SBNM-REDEEM-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3780-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = SBNM-INTEREST-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SBNM-INTEREST-AMT - (WS-WORK-AMT * 4)
           COMPUTE SBNM-PURCHASE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE ISSUE PRICE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3790-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN SBNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO SBNM-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SBNM-INTEREST-AMT ROUNDED =
                       SBNM-ISSUE-PRICE-AMT * SBNM-PENALTY-PCT / 200
               WHEN SBNM-BRANCH-CD = 'BD'
                   MOVE 15.00 TO SBNM-INTEREST-AMT
               WHEN SBNM-BRANCH-CD = 'FC'
                   MOVE 15.00 TO SBNM-INTEREST-AMT
               WHEN OTHER
                   COMPUTE SBNM-INTEREST-AMT ROUNDED =
                       SBNM-ISSUE-PRICE-AMT * SBNM-PENALTY-PCT / 100
           END-EVALUATE
           IF SBNM-INTEREST-AMT > 0 AND SBNM-INTEREST-AMT < 1.00
               MOVE 1.00 TO SBNM-INTEREST-AMT
           END-IF
           IF SBNM-INTEREST-AMT > 100.00
               MOVE 100.00 TO SBNM-INTEREST-AMT
           END-IF
           ADD SBNM-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E256 MEMO REQUIRED
      * E820 TAXABLE AMT NEGATIVE
      * E225 TAXABLE AMT OVER 250000
      * E221 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3800-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-MEMO = SPACES
                   MOVE 'E256' TO WS-REJECT-CODE
               WHEN SBNT-TAXABLE-AMT < 0
                   MOVE 'E820' TO WS-REJECT-CODE
               WHEN SBNT-TAXABLE-AMT > 250000.00
                   MOVE 'E225' TO WS-REJECT-CODE
               WHEN SBNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E221' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ISSUE TYPE CD:
      * CW -> DY WHEN EFF DATE IS PAST
      * DY -> JA WHEN EFF DATE IS PAST
      * JA -> KT WHEN EFF DATE IS PAST
      * KT -> MF WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3810-SET-ISSUE-TYPE.
           EVALUATE SBNM-ISSUE-TYPE-CD
               WHEN 'CW'
                   IF SBNT-EFF-DATE > 0 AND SBNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DY' TO SBNM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DY'
                   IF SBNT-EFF-DATE > 0 AND SBNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JA' TO SBNM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF SBNT-EFF-DATE > 0 AND SBNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KT' TO SBNM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KT'
                   IF SBNT-EFF-DATE > 0 AND SBNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MF' TO SBNM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3820-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE REDEEM AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3830-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN SBNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = SBNM-REDEEM-AMT *
                       SBNM-PENALTY-PCT / 200
               WHEN SBNM-REASON-CD = 'DG'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN SBNM-REASON-CD = 'UK'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = SBNM-REDEEM-AMT *
                       SBNM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PURCHASE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3840-FLAG-PURCHASE.
           IF SBNM-PURCHASE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBNM-PURCHASE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3850-COUNT-RETRY.
           IF SBNM-RETRY-CNT < 9999999
               ADD 1 TO SBNM-RETRY-CNT
           END-IF
           IF SBNM-RETRY-CNT NOT < 12
               MOVE 'Y' TO SBNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAXABLE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3860-ACCRUE-TAXABLE.
           IF SBNT-TAXABLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNT-TAXABLE-AMT * SBNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO SBNT-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PURCHASE YTD BY ISSUE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3870-ACCUM-ISSUE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-ISSUE-TYPE-CD
                   ADD SBNM-PURCHASE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-ISSUE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-PURCHASE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TAXABLE YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3880-LIMIT-TAXABLE.
           IF SBNM-TAXABLE-YTD < 25.00
               MOVE 25.00 TO SBNM-TAXABLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-TAXABLE-YTD > 50000.00
               MOVE 50000.00 TO SBNM-TAXABLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E985 REF NO REQUIRED
      * E043 REDEEM AMT NEGATIVE
      * E897 REDEEM AMT OVER 25000
      * E655 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3890-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-REF-NO = SPACES
                   MOVE 'E985' TO WS-REJECT-CODE
               WHEN SBNT-REDEEM-AMT < 0
                   MOVE 'E043' TO WS-REJECT-CODE
               WHEN SBNT-REDEEM-AMT > 25000.00
                   MOVE 'E897' TO WS-REJECT-CODE
               WHEN SBNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E655' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3900-AGE-LAST-MAINT.
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
                   ADD SBNM-ISSUE-PRICE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SBNM-ISSUE-PRICE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SBNM-ISSUE-PRICE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-ISSUE-PRICE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-ISSUE-PRICE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAXABLE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3910-ACCRUE-TAXABLE.
           IF SBNM-TAXABLE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-TAXABLE-YTD * SBNM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBNM-PURCHASE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3920-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PURCHASE AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3930-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-SOURCE-CD
                   ADD SBNM-PURCHASE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-PURCHASE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REDEEM AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3940-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNT-TRAN-CD
                   ADD SBNM-REDEEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-REDEEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FACE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3950-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = SBNM-FACE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SBNM-FACE-AMT - (WS-WORK-AMT * 3)
           COMPUTE SBNT-TAXABLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * E -> J WHEN EFFECTIVE DATE IS PAST
      * J -> L WHEN EFFECTIVE DATE IS PAST
      * L -> U WHEN EFFECTIVE DATE IS PAST
      * U -> E WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3960-SET-SOURCE.
           EVALUATE SBNM-SOURCE-CD
               WHEN 'E'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE TAXABLE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PURCHASE AMT.
      *----------------------------------------------------------------*
       3970-PRORATE-PURCHASE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-PURCHASE-AMT ROUNDED =
                   SBNM-TAXABLE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNM-PURCHASE-AMT
           END-IF
           ADD SBNM-PURCHASE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3980-AGE-POST.
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
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FACE SCHED TIER FOR THE FACE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3990-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-SCHED-RATE(1) TO SBNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SBNM-FACE-AMT NOT < WS-FACE-SCHED-LOW(WS-TX)
                   MOVE WS-FACE-SCHED-RATE(WS-TX) TO SBNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PURCHASE AMT.
      *----------------------------------------------------------------*
       4000-PRORATE-PURCHASE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-PURCHASE-AMT ROUNDED =
                   SBNM-INTEREST-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNM-PURCHASE-AMT
           END-IF
           ADD SBNM-PURCHASE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE TAXABLE FEE ON THE ACCRUAL YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4010-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SBNM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SBNM-TAXABLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-REVIEW-FLAG = 'Y'
                   COMPUTE SBNM-TAXABLE-AMT ROUNDED = SBNM-ACCRUAL-YTD *
                       SBNM-PENALTY-PCT / 200
               WHEN SBNM-SOURCE-CD = 'E'
                   MOVE 5.00 TO SBNM-TAXABLE-AMT
               WHEN SBNM-SOURCE-CD = 'J'
                   MOVE 5.00 TO SBNM-TAXABLE-AMT
               WHEN OTHER
                   COMPUTE SBNM-TAXABLE-AMT ROUNDED = SBNM-ACCRUAL-YTD *
                       SBNM-PENALTY-PCT / 100
           END-EVALUATE
           IF SBNM-TAXABLE-AMT > 0 AND SBNM-TAXABLE-AMT < 10.00
               MOVE 10.00 TO SBNM-TAXABLE-AMT
           END-IF
           IF SBNM-TAXABLE-AMT > 75.00
               MOVE 75.00 TO SBNM-TAXABLE-AMT
           END-IF
           ADD SBNM-TAXABLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE AMT BY ISSUE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4020-ACCUM-ISSUE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-ISSUE-TYPE-CD
                   ADD SBNM-FACE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-ISSUE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-FACE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ACCRUAL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4030-SPLIT-ACCRUAL.
           COMPUTE WS-WORK-AMT = SBNM-ACCRUAL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SBNM-ACCRUAL-AMT - (WS-WORK-AMT * 2)
           COMPUTE SBNT-TAXABLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4040-AGE-EFFECTIVE.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4050-COUNT-CYCLE.
           IF SBNM-CYCLE-CNT < 9999999
               ADD 1 TO SBNM-CYCLE-CNT
           END-IF
           IF SBNM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO SBNM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * E -> J WHEN POST DATE IS PAST
      * J -> L WHEN POST DATE IS PAST
      * L -> U WHEN POST DATE IS PAST
      * U -> E WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4060-SET-SOURCE.
           EVALUATE SBNM-SOURCE-CD
               WHEN 'E'
                   IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4070-COUNT-RETRY.
           IF SBNM-RETRY-CNT < 9999999
               ADD 1 TO SBNM-RETRY-CNT
           END-IF
           IF SBNM-RETRY-CNT NOT < 10
               MOVE 'Y' TO SBNM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REDEEM YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ACCRUAL AMT.
      *----------------------------------------------------------------*
       4080-PRORATE-ACCRUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-ACCRUAL-AMT ROUNDED =
                   SBNM-REDEEM-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-ACCRUAL-AMT
           END-IF
           ADD SBNM-ACCRUAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCRUAL AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4090-FLAG-ACCRUAL.
           IF SBNM-ACCRUAL-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBNM-ACCRUAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E162 REJECT CD REQUIRED
      * E279 WORK AMT 2 NEGATIVE
      * E477 WORK AMT 2 OVER 250000
      * E447 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4100-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-REJECT-CD = SPACES
                   MOVE 'E162' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E279' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E477' TO WS-REJECT-CODE
               WHEN SBNT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REDEEM AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4110-FLAG-REDEEM.
           IF SBNM-REDEEM-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBNM-REDEEM-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4120-COUNT-CYCLE.
           IF SBNM-CYCLE-CNT < 9999999
               ADD 1 TO SBNM-CYCLE-CNT
           END-IF
           IF SBNM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO SBNM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E797 OPERATOR REQUIRED
      * E715 WORK AMT NEGATIVE
      * E607 WORK AMT OVER 5000
      * E746 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4130-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-OPERATOR = SPACES
                   MOVE 'E797' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E715' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E607' TO WS-REJECT-CODE
               WHEN SBNM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E746' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E340 MEMO REQUIRED
      * E611 INTEREST YTD NEGATIVE
      * E250 INTEREST YTD OVER 1000000
      * E842 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4140-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-MEMO = SPACES
                   MOVE 'E340' TO WS-REJECT-CODE
               WHEN SBNM-INTEREST-YTD < 0
                   MOVE 'E611' TO WS-REJECT-CODE
               WHEN SBNM-INTEREST-YTD > 1000000.00
                   MOVE 'E250' TO WS-REJECT-CODE
               WHEN SBNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E842' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUE PRICE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       4150-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-INTEREST-AMT ROUNDED =
                   SBNM-ISSUE-PRICE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-INTEREST-AMT
           END-IF
           ADD SBNM-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * BD -> FC WHEN POST DATE IS PAST
      * FC -> KR WHEN POST DATE IS PAST
      * KR -> MP WHEN POST DATE IS PAST
      * MP -> VJ WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4160-SET-BRANCH.
           EVALUATE SBNM-BRANCH-CD
               WHEN 'BD'
                   IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FC' TO SBNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FC'
                   IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO SBNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MP' TO SBNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MP'
                   IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUAL FEE ON THE PURCHASE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4170-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SBNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO SBNM-ACCRUAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SBNM-ACCRUAL-AMT ROUNDED = SBNM-PURCHASE-YTD
                       * SBNM-MARGIN / 200
               WHEN SBNT-TRAN-CD = 'BY'
                   MOVE 7.00 TO SBNM-ACCRUAL-AMT
               WHEN SBNT-TRAN-CD = 'RD'
                   MOVE 7.00 TO SBNM-ACCRUAL-AMT
               WHEN OTHER
                   COMPUTE SBNM-ACCRUAL-AMT ROUNDED = SBNM-PURCHASE-YTD
                       * SBNM-MARGIN / 100
           END-EVALUATE
           IF SBNM-ACCRUAL-AMT > 0 AND SBNM-ACCRUAL-AMT < 2.00
               MOVE 2.00 TO SBNM-ACCRUAL-AMT
           END-IF
           IF SBNM-ACCRUAL-AMT > 100.00
               MOVE 100.00 TO SBNM-ACCRUAL-AMT
           END-IF
           ADD SBNM-ACCRUAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4180-SET-CHANNEL.
           EVALUATE SBNT-CHANNEL-CD
               WHEN 'BRN'
                   IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ACCRUAL AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4190-SPLIT-ACCRUAL.
           COMPUTE WS-WORK-AMT = SBNM-ACCRUAL-AMT / 12
           COMPUTE WS-WORK-AMT-2 = SBNM-ACCRUAL-AMT - (WS-WORK-AMT * 12)
           COMPUTE SBNM-FACE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E840 OPERATOR REQUIRED
      * E140 PURCHASE AMT NEGATIVE
      * E654 PURCHASE AMT OVER 99999
      * E243 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4200-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-OPERATOR = SPACES
                   MOVE 'E840' TO WS-REJECT-CODE
               WHEN SBNM-PURCHASE-AMT < 0
                   MOVE 'E140' TO WS-REJECT-CODE
               WHEN SBNM-PURCHASE-AMT > 99999.00
                   MOVE 'E654' TO WS-REJECT-CODE
               WHEN SBNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E243' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4210-DESCRIBE-ISSUE-TYPE.
           EVALUATE SBNM-ISSUE-TYPE-CD
               WHEN 'CW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SERIES CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4220-DESCRIBE-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CS -> HF WHEN EFFECTIVE DATE IS PAST
      * HF -> JZ WHEN EFFECTIVE DATE IS PAST
      * JZ -> CS WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4230-SET-STATUS.
           EVALUATE SBNM-STATUS-CD
               WHEN 'CS'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HF' TO SBNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO SBNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CS' TO SBNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PURCHASE YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4240-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-SOURCE-CD
                   ADD SBNM-PURCHASE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-PURCHASE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4250-ACCRUE-FACE.
           IF SBNM-FACE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-FACE-YTD * SBNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO SBNM-TAXABLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4260-COUNT-CYCLE.
           IF SBNM-CYCLE-CNT < 9999999
               ADD 1 TO SBNM-CYCLE-CNT
           END-IF
           IF SBNM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO SBNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ACCRUAL AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4270-ACCRUE-ACCRUAL.
           IF SBNM-ACCRUAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-ACCRUAL-AMT * SBNM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBNT-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAXABLE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4280-SPLIT-TAXABLE.
           COMPUTE WS-WORK-AMT = SBNM-TAXABLE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SBNM-TAXABLE-AMT - (WS-WORK-AMT * 3)
           COMPUTE SBNM-REDEEM-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REDEEM SCHED TIER FOR THE TAXABLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4290-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REDEEM-SCHED-RATE(1) TO SBNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SBNM-TAXABLE-YTD NOT < WS-REDEEM-SCHED-LOW(WS-TX)
                   MOVE WS-REDEEM-SCHED-RATE(WS-TX) TO SBNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUAL TIER TIER FOR THE ISSUE PRICE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4300-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUAL-TIER-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF SBNM-ISSUE-PRICE-YTD NOT < WS-ACCRUAL-TIER-LOW(WS-TX)
                   MOVE WS-ACCRUAL-TIER-RATE(WS-TX) TO SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAXABLE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4310-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-STATUS-CD
                   ADD SBNM-TAXABLE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-TAXABLE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4320-AGE-POST.
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
               WHEN WS-DAYS NOT > 45
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4330-AGE-EFF.
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
                   ADD SBNT-ACCRUAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNT-ACCRUAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD SBNT-ACCRUAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBNT-ACCRUAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNT-ACCRUAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE TAXABLE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PURCHASE YTD.
      *----------------------------------------------------------------*
       4340-PRORATE-PURCHASE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-PURCHASE-YTD ROUNDED =
                   SBNM-TAXABLE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNM-PURCHASE-YTD
           END-IF
           ADD SBNM-PURCHASE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FACE SCHED TIER FOR THE ISSUE PRICE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4350-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-SCHED-RATE(1) TO SBNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SBNM-ISSUE-PRICE-AMT NOT < WS-FACE-SCHED-LOW(WS-TX)
                   MOVE WS-FACE-SCHED-RATE(WS-TX) TO SBNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUE PRICE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4360-ACCRUE-ISSUE-PRICE.
           IF SBNM-ISSUE-PRICE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-ISSUE-PRICE-AMT * SBNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO SBNT-ACCRUAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REDEEM YTD.
      *----------------------------------------------------------------*
       4370-PRORATE-REDEEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-REDEEM-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNM-REDEEM-YTD
           END-IF
           ADD SBNM-REDEEM-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT REDEEM YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4380-SPLIT-REDEEM.
           COMPUTE WS-WORK-AMT = SBNM-REDEEM-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SBNM-REDEEM-YTD - (WS-WORK-AMT * 4)
           COMPUTE SBNM-TAXABLE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4390-LIMIT-INTEREST.
           IF SBNT-INTEREST-AMT < 0.00
               MOVE 0.00 TO SBNT-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNT-INTEREST-AMT > 2500.00
               MOVE 2500.00 TO SBNT-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4400-LIMIT-INTEREST.
           IF SBNM-INTEREST-YTD < 100.00
               MOVE 100.00 TO SBNM-INTEREST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-INTEREST-YTD > 10000.00
               MOVE 10000.00 TO SBNM-INTEREST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TAXABLE AMT.
      *----------------------------------------------------------------*
       4410-PRORATE-TAXABLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-TAXABLE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNM-TAXABLE-AMT
           END-IF
           ADD SBNM-TAXABLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4420-DESCRIBE-STATUS.
           EVALUATE SBNM-STATUS-CD
               WHEN 'CS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ISSUE PRICE YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4430-LIMIT-ISSUE-PRICE.
           IF SBNM-ISSUE-PRICE-YTD < 25.00
               MOVE 25.00 TO SBNM-ISSUE-PRICE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-ISSUE-PRICE-YTD > 1000.00
               MOVE 1000.00 TO SBNM-ISSUE-PRICE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SERIES CD:
      * DD -> DF WHEN LAST PAY DATE IS PAST
      * DF -> SS WHEN LAST PAY DATE IS PAST
      * SS -> VJ WHEN LAST PAY DATE IS PAST
      * VJ -> VX WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4440-SET-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DF'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SS' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SS'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJ'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FACE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4450-FLAG-FACE.
           IF SBNM-FACE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBNM-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAXABLE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4460-SPLIT-TAXABLE.
           COMPUTE WS-WORK-AMT = SBNT-TAXABLE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = SBNT-TAXABLE-AMT - (WS-WORK-AMT * 12)
           COMPUTE SBNM-REDEEM-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

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
                   MOVE SBFEE-ACCRUAL-AMT
                     TO SBNM-ACCRUAL-AMT
                   MOVE SBFEE-INTEREST-YTD
                     TO SBNM-INTEREST-YTD
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
           DISPLAY 'SBNSTMT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'SBNSTMT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
