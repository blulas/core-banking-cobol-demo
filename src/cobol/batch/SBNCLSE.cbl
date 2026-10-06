      *================================================================*
      * PROGRAM  : SBNCLSE                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : SAVINGS BOND SERVICES - CLOSURE PROCESSING. READS   *
      *            THE SBN TRANSACTIONS, UPDATES THE SBN MASTER AND    *
      *            PRODUCES THE CONTROL REPORT. US SAVINGS BOND SALES, *
      *            REDEMPTION AND TAX REPORTING FOR CUSTOMERS          *
      * INPUT    : SBNMST (KSDS), SBNTRN, BANKCTL                      *
      * OUTPUT   : SBNOUT, SBNCLRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, SBLIM, SBSTAT           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-06-19 TWH  ORIGINAL                                       *
      * 1993-11-06 DPS  INTEREST FEE SCHEDULE UPDATE REQ9498           *
      * 1994-12-18 JLM  ACCRUAL ROUNDING - HALF UP REQ4149             *
      * 1994-12-16 TWH  REDEEM EXCEPTIONS TO OPS QUEUE CHG3440         *
      * 1996-08-25 DPS  INTEREST EXCEPTIONS TO OPS QUEUE REQ6489       *
      * 1999-12-06 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-09-27 CRP  REDEEM NOTICE TEXT PER LEGAL PRB1952           *
      * 2005-06-23 LMB  PURCHASE FEE SCHEDULE UPDATE PRB9331           *
      * 2007-01-10 JLM  FACE EXCEPTIONS TO OPS QUEUE INC4706           *
      * 2009-12-28 GAF  FACE NOTICE TEXT PER LEGAL CHG6635             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SBNCLSE.
       AUTHOR. D P SHAW.

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
           SELECT REPORT-FILE ASSIGN TO "SBNCLRPT"
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
                                           VALUE 15.000000.
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
      * WS-FACE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FACE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000104000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000039500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000091000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000538500'.
       01  WS-FACE-TIER-TBL REDEFINES WS-FACE-TIER-VALUES.
           05  WS-FACE-TIER-ROW OCCURS 4 TIMES.
               10  WS-FACE-TIER-LOW        PIC 9(09)V99.
               10  WS-FACE-TIER-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PURCHASE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PURCHASE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000466000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000671500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000011000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015250000000357500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020250000000389000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020750000000533500'.
       01  WS-PURCHASE-TIER-TBL REDEFINES WS-PURCHASE-TIER-VALUES.
           05  WS-PURCHASE-TIER-ROW OCCURS 6 TIMES.
               10  WS-PURCHASE-TIER-LOW    PIC 9(09)V99.
               10  WS-PURCHASE-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ACCRUAL-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ACCRUAL-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000325000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000037000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000893500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000393500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012000000000315000'.
       01  WS-ACCRUAL-SCHED-TBL REDEFINES WS-ACCRUAL-SCHED-VALUES.
           05  WS-ACCRUAL-SCHED-ROW OCCURS 5 TIMES.
               10  WS-ACCRUAL-SCHED-LOW    PIC 9(09)V99.
               10  WS-ACCRUAL-SCHED-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  SBNCLSE  SAVINGS BOND SERVICES'.
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
       COPY SBSTATP.

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
               PERFORM 3000-DESCRIBE-BRANCH
               PERFORM 3010-AGE-POST
               PERFORM 3020-SELECT-MARGIN
               PERFORM 3030-EDIT-MEMO
               PERFORM 3040-DESCRIBE-SOURCE
               PERFORM 3050-SPLIT-ACCRUAL
               PERFORM 3060-ACCUM-STATUS
               PERFORM 3070-PRORATE-TAXABLE
               PERFORM 3080-PRORATE-ISSUE-PRICE
               PERFORM 3090-SET-ISSUE-TYPE
               PERFORM 3100-DESCRIBE-CHANNEL
               PERFORM 3110-PRORATE-ISSUE-PRICE
               PERFORM 3120-PRORATE-FACE
               PERFORM 3130-ASSESS-REVIEW
               PERFORM 3140-LIMIT-ITEM
               PERFORM 3150-CHECK-POST
               PERFORM 3160-SPLIT-ISSUE-PRICE
               PERFORM 3170-ACCRUE-ACCRUAL
               PERFORM 3180-COUNT-EXCEPT
               PERFORM 3190-ACCUM-STATUS
               PERFORM 3200-PRORATE-INTEREST
               PERFORM 3210-ACCRUE-ISSUE-PRICE
               PERFORM 3220-PRORATE-FACE
               PERFORM 3230-EDIT-MEMO
               PERFORM 3240-SET-CHANNEL
               PERFORM 3250-PRORATE-REDEEM
               PERFORM 3260-FLAG-FACE
               PERFORM 3270-SELECT-FEE-PCT
               PERFORM 3280-DESCRIBE-CHANNEL
               PERFORM 3290-CHECK-EXPIRY
               PERFORM 3300-SET-REASON
               PERFORM 3310-SET-TRAN
               PERFORM 3320-ASSESS-AUTO-RENEW
               PERFORM 3330-ACCUM-SOURCE
               PERFORM 3340-SPLIT-ISSUE-PRICE
               PERFORM 3350-COUNT-RETRY
               PERFORM 3360-EDIT-ADDR
               PERFORM 3370-SET-BRANCH
               PERFORM 3380-DESCRIBE-STATUS
               PERFORM 3390-CHECK-LAST-PAY
               PERFORM 3400-DESCRIBE-TRAN
               PERFORM 3410-SELECT-MARGIN
               PERFORM 3420-SET-STATUS
               PERFORM 3430-ASSESS-AUTO-RENEW
               PERFORM 3440-LIMIT-REDEEM
               PERFORM 3450-COUNT-EXCEPT
               PERFORM 3460-PRORATE-TAXABLE
               PERFORM 3470-ASSESS-REVERSAL
               PERFORM 3480-SPLIT-INTEREST
               PERFORM 3490-ASSESS-REVIEW
               PERFORM 3500-ACCRUE-REDEEM
               PERFORM 3510-CHECK-OPEN
               PERFORM 3520-AGE-EFFECTIVE
               PERFORM 3530-PRORATE-ISSUE-PRICE
               PERFORM 3540-FLAG-ACCRUAL
               PERFORM 3550-COUNT-CYCLE
               PERFORM 3560-LIMIT-FACE
               PERFORM 3570-CHECK-OPEN
               PERFORM 3580-FLAG-FACE
               PERFORM 3590-ACCRUE-PURCHASE
               PERFORM 3600-EDIT-NAME
               PERFORM 3610-SET-SERIES
               PERFORM 3620-PRORATE-INTEREST
               PERFORM 3630-PRORATE-REDEEM
               PERFORM 3640-PRORATE-PURCHASE
               PERFORM 3650-COUNT-EXCEPT
               PERFORM 3660-LIMIT-TAXABLE
               PERFORM 3670-PRORATE-ISSUE-PRICE
               PERFORM 3680-ASSESS-HOLD
               PERFORM 3690-COUNT-CYCLE
               PERFORM 3700-EDIT-CUST-ID
               PERFORM 3710-CHECK-EFF
               PERFORM 3720-COUNT-RETRY
               PERFORM 3730-ASSESS-AUTO-RENEW
               PERFORM 3740-AGE-NEXT-REVIEW
               PERFORM 3750-PRORATE-TAXABLE
               PERFORM 3760-CHECK-OPEN
               PERFORM 3770-DESCRIBE-SERIES
               PERFORM 3780-PRORATE-REDEEM
               PERFORM 3790-ASSESS-NOTICE-SENT
               PERFORM 3800-SET-SOURCE
               PERFORM 3810-LIMIT-PURCHASE
               PERFORM 3820-CHECK-LAST-PAY
               PERFORM 3830-SPLIT-REDEEM
               PERFORM 3840-ACCRUE-TAXABLE
               PERFORM 3850-EDIT-REJECT
               PERFORM 3860-ACCRUE-REDEEM
               PERFORM 3870-SET-TRAN
               PERFORM 3880-ACCRUE-ACCRUAL
               PERFORM 3890-ACCRUE-ISSUE-PRICE
               PERFORM 3900-LIMIT-INTEREST
               PERFORM 3910-SET-SERIES
               PERFORM 3920-SPLIT-ACCRUAL
               PERFORM 3930-SELECT-PENALTY-PCT
               PERFORM 3940-DESCRIBE-SOURCE
               PERFORM 3950-COUNT-CYCLE
               PERFORM 3960-SPLIT-TAXABLE
               PERFORM 3970-SELECT-FEE-PCT
               PERFORM 3980-EDIT-NAME
               PERFORM 3990-LIMIT-ITEM
               PERFORM 4000-SPLIT-ACCRUAL
               PERFORM 4010-SPLIT-INTEREST
               PERFORM 4020-ACCUM-BRANCH
               PERFORM 4030-EDIT-DESC
               PERFORM 4040-DESCRIBE-SERIES
               PERFORM 4050-AGE-LAST-PAY
               PERFORM 4060-CHECK-LAST-MAINT
               PERFORM 4070-ACCUM-SOURCE
               PERFORM 4080-SELECT-FEE-PCT
               PERFORM 4090-DESCRIBE-STATUS
               PERFORM 4100-ASSESS-HOLD
               PERFORM 4110-ACCUM-STATUS
               PERFORM 4120-CHECK-NOTICE
               PERFORM 4130-SPLIT-PURCHASE
               PERFORM 4140-SET-TRAN
               PERFORM 4150-PRORATE-TAXABLE
               PERFORM 4160-ACCUM-STATUS
               PERFORM 4170-PRORATE-REDEEM
               PERFORM 4180-SELECT-MARGIN
               PERFORM 4190-DESCRIBE-SERIES
               PERFORM 4200-CHECK-MATURITY
               PERFORM 4210-SELECT-FEE-PCT
               PERFORM 4220-FLAG-REDEEM
               PERFORM 4230-SPLIT-ACCRUAL
               PERFORM 4240-COUNT-PAYMENT
               PERFORM 4250-ACCRUE-TAXABLE
               PERFORM 4260-LIMIT-PURCHASE
               PERFORM 4270-ASSESS-REVIEW
               PERFORM 4280-LIMIT-ACCRUAL
               PERFORM 4290-ASSESS-AUTO-RENEW
               PERFORM 4300-SPLIT-TAXABLE
               PERFORM 7020-CALL-SBLIM
               PERFORM 7010-CALL-SBSTAT
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
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3000-DESCRIBE-BRANCH.
           EVALUATE SBNM-BRANCH-CD
               WHEN 'BD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3010-AGE-POST.
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
                   ADD SBNM-ISSUE-PRICE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNM-ISSUE-PRICE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD SBNM-ISSUE-PRICE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD SBNM-ISSUE-PRICE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-ISSUE-PRICE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUAL SCHED TIER FOR THE ISSUE PRICE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3020-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUAL-SCHED-RATE(1) TO SBNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SBNM-ISSUE-PRICE-AMT NOT < WS-ACCRUAL-SCHED-LOW(WS-TX)
                   MOVE WS-ACCRUAL-SCHED-RATE(WS-TX) TO SBNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E313 MEMO REQUIRED
      * E850 ACCRUAL AMT NEGATIVE
      * E945 ACCRUAL AMT OVER 50000
      * E348 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3030-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-MEMO = SPACES
                   MOVE 'E313' TO WS-REJECT-CODE
               WHEN SBNT-ACCRUAL-AMT < 0
                   MOVE 'E850' TO WS-REJECT-CODE
               WHEN SBNT-ACCRUAL-AMT > 50000.00
                   MOVE 'E945' TO WS-REJECT-CODE
               WHEN SBNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E348' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3040-DESCRIBE-SOURCE.
           EVALUATE SBNM-SOURCE-CD
               WHEN 'E'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'J'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'L'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'U'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ACCRUAL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3050-SPLIT-ACCRUAL.
           COMPUTE WS-WORK-AMT = SBNM-ACCRUAL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SBNM-ACCRUAL-AMT - (WS-WORK-AMT * 2)
           COMPUTE SBNM-ISSUE-PRICE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PURCHASE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3060-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-STATUS-CD
                   ADD SBNM-PURCHASE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-PURCHASE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAXABLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TAXABLE YTD.
      *----------------------------------------------------------------*
       3070-PRORATE-TAXABLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-TAXABLE-YTD ROUNDED =
                   SBNM-TAXABLE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-TAXABLE-YTD
           END-IF
           ADD SBNM-TAXABLE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE ACCRUAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUE PRICE AMT.
      *----------------------------------------------------------------*
       3080-PRORATE-ISSUE-PRICE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-ISSUE-PRICE-AMT ROUNDED =
                   SBNT-ACCRUAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-ISSUE-PRICE-AMT
           END-IF
           ADD SBNM-ISSUE-PRICE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ISSUE TYPE CD:
      * CW -> DY WHEN EFFECTIVE DATE IS PAST
      * DY -> JA WHEN EFFECTIVE DATE IS PAST
      * JA -> KT WHEN EFFECTIVE DATE IS PAST
      * KT -> MF WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3090-SET-ISSUE-TYPE.
           EVALUATE SBNM-ISSUE-TYPE-CD
               WHEN 'CW'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DY' TO SBNM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DY'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JA' TO SBNM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KT' TO SBNM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KT'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MF' TO SBNM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3100-DESCRIBE-CHANNEL.
           EVALUATE SBNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REDEEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUE PRICE YTD.
      *----------------------------------------------------------------*
       3110-PRORATE-ISSUE-PRICE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-ISSUE-PRICE-YTD ROUNDED =
                   SBNT-REDEEM-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-ISSUE-PRICE-YTD
           END-IF
           ADD SBNM-ISSUE-PRICE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FACE AMT.
      *----------------------------------------------------------------*
       3120-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-FACE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNM-FACE-AMT
           END-IF
           ADD SBNM-FACE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUAL YTD FEE ON THE ACCRUAL AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3130-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SBNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SBNM-ACCRUAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-HOLD-FLAG = 'Y'
                   COMPUTE SBNM-ACCRUAL-YTD ROUNDED = SBNT-ACCRUAL-AMT *
                       SBNM-MARGIN / 200
               WHEN SBNM-ISSUE-TYPE-CD = 'CW'
                   MOVE 12.00 TO SBNM-ACCRUAL-YTD
               WHEN SBNM-ISSUE-TYPE-CD = 'DY'
                   MOVE 10.00 TO SBNM-ACCRUAL-YTD
               WHEN OTHER
                   COMPUTE SBNM-ACCRUAL-YTD ROUNDED = SBNT-ACCRUAL-AMT *
                       SBNM-MARGIN / 100
           END-EVALUATE
           IF SBNM-ACCRUAL-YTD > 0 AND SBNM-ACCRUAL-YTD < 10.00
               MOVE 10.00 TO SBNM-ACCRUAL-YTD
           END-IF
           IF SBNM-ACCRUAL-YTD > 150.00
               MOVE 150.00 TO SBNM-ACCRUAL-YTD
           END-IF
           ADD SBNM-ACCRUAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3140-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-POST.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO SBNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUE PRICE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3160-SPLIT-ISSUE-PRICE.
           COMPUTE WS-WORK-AMT = SBNM-ISSUE-PRICE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = SBNM-ISSUE-PRICE-YTD - (WS-WORK-AMT *
               3)
           COMPUTE SBNT-ACCRUAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ACCRUAL AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3170-ACCRUE-ACCRUAL.
           IF SBNT-ACCRUAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNT-ACCRUAL-AMT * SBNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBNT-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3180-COUNT-EXCEPT.
           IF SBNM-EXCEPT-CNT < 9999999
               ADD 1 TO SBNM-EXCEPT-CNT
           END-IF
           IF SBNM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO SBNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REDEEM YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3190-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-STATUS-CD
                   ADD SBNM-REDEEM-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-REDEEM-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUE PRICE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       3200-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-INTEREST-YTD ROUNDED =
                   SBNM-ISSUE-PRICE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-INTEREST-YTD
           END-IF
           ADD SBNM-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUE PRICE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3210-ACCRUE-ISSUE-PRICE.
           IF SBNM-ISSUE-PRICE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-ISSUE-PRICE-AMT * SBNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO SBNM-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REDEEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FACE YTD.
      *----------------------------------------------------------------*
       3220-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-FACE-YTD ROUNDED =
                   SBNT-REDEEM-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-FACE-YTD
           END-IF
           ADD SBNM-FACE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E845 MEMO REQUIRED
      * E654 FACE AMT NEGATIVE
      * E044 FACE AMT OVER 1000000
      * E461 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3230-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-MEMO = SPACES
                   MOVE 'E845' TO WS-REJECT-CODE
               WHEN SBNM-FACE-AMT < 0
                   MOVE 'E654' TO WS-REJECT-CODE
               WHEN SBNM-FACE-AMT > 1000000.00
                   MOVE 'E044' TO WS-REJECT-CODE
               WHEN SBNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E461' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST PAY DATE IS PAST
      * BAT -> MAL WHEN LAST PAY DATE IS PAST
      * MAL -> TEL WHEN LAST PAY DATE IS PAST
      * TEL -> ONL WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3240-SET-CHANNEL.
           EVALUATE SBNT-CHANNEL-CD
               WHEN 'BRN'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REDEEM YTD.
      *----------------------------------------------------------------*
       3250-PRORATE-REDEEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-REDEEM-YTD ROUNDED =
                   SBNM-PURCHASE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-REDEEM-YTD
           END-IF
           ADD SBNM-REDEEM-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FACE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3260-FLAG-FACE.
           IF SBNM-FACE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBNM-FACE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FACE TIER TIER FOR THE TAXABLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-TIER-RATE(1) TO SBNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF SBNM-TAXABLE-YTD NOT < WS-FACE-TIER-LOW(WS-TX)
                   MOVE WS-FACE-TIER-RATE(WS-TX) TO SBNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3280-DESCRIBE-CHANNEL.
           EVALUATE SBNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3290-CHECK-EXPIRY.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO SBNM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DG -> UK WHEN LAST MAINT DATE IS PAST
      * UK -> VH WHEN LAST MAINT DATE IS PAST
      * VH -> XD WHEN LAST MAINT DATE IS PAST
      * XD -> DG WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3300-SET-REASON.
           EVALUATE SBNM-REASON-CD
               WHEN 'DG'
                   IF SBNM-LAST-MAINT-DATE > 0 AND SBNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UK' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF SBNM-LAST-MAINT-DATE > 0 AND SBNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VH' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VH'
                   IF SBNM-LAST-MAINT-DATE > 0 AND SBNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XD' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF SBNM-LAST-MAINT-DATE > 0 AND SBNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DG' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BY -> RD WHEN NEXT DUE DATE IS PAST
      * RD -> IR WHEN NEXT DUE DATE IS PAST
      * IR -> TX WHEN NEXT DUE DATE IS PAST
      * TX -> AD WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3310-SET-TRAN.
           EVALUATE SBNT-TRAN-CD
               WHEN 'BY'
                   IF SBNM-NEXT-DUE-DATE > 0 AND SBNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF SBNM-NEXT-DUE-DATE > 0 AND SBNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'IR' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'IR'
                   IF SBNM-NEXT-DUE-DATE > 0 AND SBNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TX'
                   IF SBNM-NEXT-DUE-DATE > 0 AND SBNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE REDEEM YTD FEE ON THE ISSUE PRICE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3320-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SBNM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SBNM-REDEEM-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE SBNM-REDEEM-YTD ROUNDED =
                       SBNM-ISSUE-PRICE-AMT * SBNM-PENALTY-PCT / 200
               WHEN SBNM-ISSUE-TYPE-CD = 'CW'
                   MOVE 20.00 TO SBNM-REDEEM-YTD
               WHEN SBNM-ISSUE-TYPE-CD = 'DY'
                   MOVE 5.00 TO SBNM-REDEEM-YTD
               WHEN OTHER
                   COMPUTE SBNM-REDEEM-YTD ROUNDED =
                       SBNM-ISSUE-PRICE-AMT * SBNM-PENALTY-PCT / 100
           END-EVALUATE
           IF SBNM-REDEEM-YTD > 0 AND SBNM-REDEEM-YTD < 1.00
               MOVE 1.00 TO SBNM-REDEEM-YTD
           END-IF
           IF SBNM-REDEEM-YTD > 50.00
               MOVE 50.00 TO SBNM-REDEEM-YTD
           END-IF
           ADD SBNM-REDEEM-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3330-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-SOURCE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUE PRICE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3340-SPLIT-ISSUE-PRICE.
           COMPUTE WS-WORK-AMT = SBNM-ISSUE-PRICE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SBNM-ISSUE-PRICE-AMT - (WS-WORK-AMT *
               3)
           COMPUTE SBNT-TAXABLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3350-COUNT-RETRY.
           IF SBNM-RETRY-CNT < 9999999
               ADD 1 TO SBNM-RETRY-CNT
           END-IF
           IF SBNM-RETRY-CNT NOT < 5
               MOVE 'Y' TO SBNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E746 ADDR REQUIRED
      * E658 TAXABLE AMT NEGATIVE
      * E334 TAXABLE AMT OVER 50000
      * E169 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-ADDR = SPACES
                   MOVE 'E746' TO WS-REJECT-CODE
               WHEN SBNM-TAXABLE-AMT < 0
                   MOVE 'E658' TO WS-REJECT-CODE
               WHEN SBNM-TAXABLE-AMT > 50000.00
                   MOVE 'E334' TO WS-REJECT-CODE
               WHEN SBNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E169' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * BD -> FC WHEN LAST PAY DATE IS PAST
      * FC -> KR WHEN LAST PAY DATE IS PAST
      * KR -> MP WHEN LAST PAY DATE IS PAST
      * MP -> VJ WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3370-SET-BRANCH.
           EVALUATE SBNM-BRANCH-CD
               WHEN 'BD'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FC' TO SBNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FC'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO SBNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MP' TO SBNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MP'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3380-DESCRIBE-STATUS.
           EVALUATE SBNM-STATUS-CD
               WHEN 'CS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3390-CHECK-LAST-PAY.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3400-DESCRIBE-TRAN.
           EVALUATE SBNT-TRAN-CD
               WHEN 'BY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'IR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'AD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUAL SCHED TIER FOR THE PURCHASE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3410-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUAL-SCHED-RATE(1) TO SBNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SBNM-PURCHASE-AMT NOT < WS-ACCRUAL-SCHED-LOW(WS-TX)
                   MOVE WS-ACCRUAL-SCHED-RATE(WS-TX) TO SBNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CS -> HF WHEN POST DATE IS PAST
      * HF -> JZ WHEN POST DATE IS PAST
      * JZ -> CS WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3420-SET-STATUS.
           EVALUATE SBNM-STATUS-CD
               WHEN 'CS'
                   IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HF' TO SBNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO SBNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CS' TO SBNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUE PRICE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3430-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SBNM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SBNM-ISSUE-PRICE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNT-REVERSAL-FLAG = 'Y'
                   COMPUTE SBNM-ISSUE-PRICE-YTD ROUNDED = WS-WORK-AMT *
                       SBNM-FEE-PCT / 200
               WHEN SBNT-TRAN-CD = 'BY'
                   MOVE 10.00 TO SBNM-ISSUE-PRICE-YTD
               WHEN SBNT-TRAN-CD = 'RD'
                   MOVE 5.00 TO SBNM-ISSUE-PRICE-YTD
               WHEN OTHER
                   COMPUTE SBNM-ISSUE-PRICE-YTD ROUNDED = WS-WORK-AMT *
                       SBNM-FEE-PCT / 100
           END-EVALUATE
           IF SBNM-ISSUE-PRICE-YTD > 0 AND SBNM-ISSUE-PRICE-YTD < 10.00
               MOVE 10.00 TO SBNM-ISSUE-PRICE-YTD
           END-IF
           IF SBNM-ISSUE-PRICE-YTD > 35.00
               MOVE 35.00 TO SBNM-ISSUE-PRICE-YTD
           END-IF
           ADD SBNM-ISSUE-PRICE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * REDEEM AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3440-LIMIT-REDEEM.
           IF SBNT-REDEEM-AMT < 100.00
               MOVE 100.00 TO SBNT-REDEEM-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNT-REDEEM-AMT > 10000.00
               MOVE 10000.00 TO SBNT-REDEEM-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3450-COUNT-EXCEPT.
           IF SBNM-EXCEPT-CNT < 9999999
               ADD 1 TO SBNM-EXCEPT-CNT
           END-IF
           IF SBNM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TAXABLE AMT.
      *----------------------------------------------------------------*
       3460-PRORATE-TAXABLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNT-TAXABLE-AMT ROUNDED =
                   SBNM-PURCHASE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNT-TAXABLE-AMT
           END-IF
           ADD SBNT-TAXABLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE REDEEM YTD FEE ON THE FACE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3470-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN SBNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO SBNM-REDEEM-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-REVIEW-FLAG = 'Y'
                   COMPUTE SBNM-REDEEM-YTD ROUNDED = SBNM-FACE-YTD *
                       SBNM-MARGIN / 200
               WHEN SBNM-STATUS-CD = 'CS'
                   MOVE 12.00 TO SBNM-REDEEM-YTD
               WHEN SBNM-STATUS-CD = 'HF'
                   MOVE 20.00 TO SBNM-REDEEM-YTD
               WHEN OTHER
                   COMPUTE SBNM-REDEEM-YTD ROUNDED = SBNM-FACE-YTD *
                       SBNM-MARGIN / 100
           END-EVALUATE
           IF SBNM-REDEEM-YTD > 0 AND SBNM-REDEEM-YTD < 5.00
               MOVE 5.00 TO SBNM-REDEEM-YTD
           END-IF
           IF SBNM-REDEEM-YTD > 75.00
               MOVE 75.00 TO SBNM-REDEEM-YTD
           END-IF
           ADD SBNM-REDEEM-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3480-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = SBNM-INTEREST-YTD / 2
           COMPUTE WS-WORK-AMT-2 = SBNM-INTEREST-YTD - (WS-WORK-AMT * 2)
           COMPUTE SBNM-ACCRUAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAXABLE FEE ON THE REDEEM YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3490-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SBNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SBNT-TAXABLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE SBNT-TAXABLE-AMT ROUNDED = SBNM-REDEEM-YTD *
                       SBNM-FEE-PCT / 200
               WHEN SBNM-REASON-CD = 'DG'
                   MOVE 12.00 TO SBNT-TAXABLE-AMT
               WHEN SBNM-REASON-CD = 'UK'
                   MOVE 20.00 TO SBNT-TAXABLE-AMT
               WHEN OTHER
                   COMPUTE SBNT-TAXABLE-AMT ROUNDED = SBNM-REDEEM-YTD *
                       SBNM-FEE-PCT / 100
           END-EVALUATE
           IF SBNT-TAXABLE-AMT > 0 AND SBNT-TAXABLE-AMT < 2.00
               MOVE 2.00 TO SBNT-TAXABLE-AMT
           END-IF
           IF SBNT-TAXABLE-AMT > 150.00
               MOVE 150.00 TO SBNT-TAXABLE-AMT
           END-IF
           ADD SBNT-TAXABLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REDEEM AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3500-ACCRUE-REDEEM.
           IF SBNM-REDEEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-REDEEM-AMT * SBNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBNM-ACCRUAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3510-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SBNM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO SBNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3520-AGE-EFFECTIVE.
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
                   ADD SBNM-ACCRUAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SBNM-ACCRUAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SBNM-ACCRUAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SBNM-ACCRUAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-ACCRUAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REDEEM YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ISSUE PRICE AMT.
      *----------------------------------------------------------------*
       3530-PRORATE-ISSUE-PRICE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-ISSUE-PRICE-AMT ROUNDED =
                   SBNM-REDEEM-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNM-ISSUE-PRICE-AMT
           END-IF
           ADD SBNM-ISSUE-PRICE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCRUAL AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3540-FLAG-ACCRUAL.
           IF SBNT-ACCRUAL-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBNT-ACCRUAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3550-COUNT-CYCLE.
           IF SBNM-CYCLE-CNT < 9999999
               ADD 1 TO SBNM-CYCLE-CNT
           END-IF
           IF SBNM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3560-LIMIT-FACE.
           IF SBNM-FACE-AMT < 100.00
               MOVE 100.00 TO SBNM-FACE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-FACE-AMT > 500.00
               MOVE 500.00 TO SBNM-FACE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3570-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SBNM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3580-FLAG-FACE.
           IF SBNM-FACE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBNM-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PURCHASE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3590-ACCRUE-PURCHASE.
           IF SBNM-PURCHASE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-PURCHASE-AMT * SBNM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBNM-ISSUE-PRICE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E259 NAME REQUIRED
      * E889 REDEEM AMT NEGATIVE
      * E318 REDEEM AMT OVER 50000
      * E885 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3600-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-NAME = SPACES
                   MOVE 'E259' TO WS-REJECT-CODE
               WHEN SBNT-REDEEM-AMT < 0
                   MOVE 'E889' TO WS-REJECT-CODE
               WHEN SBNT-REDEEM-AMT > 50000.00
                   MOVE 'E318' TO WS-REJECT-CODE
               WHEN SBNM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E885' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SERIES CD:
      * DD -> DF WHEN NOTICE DATE IS PAST
      * DF -> SS WHEN NOTICE DATE IS PAST
      * SS -> VJ WHEN NOTICE DATE IS PAST
      * VJ -> VX WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3610-SET-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   IF SBNM-NOTICE-DATE > 0 AND SBNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DF'
                   IF SBNM-NOTICE-DATE > 0 AND SBNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SS' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SS'
                   IF SBNM-NOTICE-DATE > 0 AND SBNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJ'
                   IF SBNM-NOTICE-DATE > 0 AND SBNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FACE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       3620-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-INTEREST-AMT ROUNDED =
                   SBNM-FACE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-INTEREST-AMT
           END-IF
           ADD SBNM-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE FACE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REDEEM AMT.
      *----------------------------------------------------------------*
       3630-PRORATE-REDEEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNT-REDEEM-AMT ROUNDED =
                   SBNM-FACE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNT-REDEEM-AMT
           END-IF
           ADD SBNT-REDEEM-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE ISSUE PRICE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PURCHASE AMT.
      *----------------------------------------------------------------*
       3640-PRORATE-PURCHASE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-PURCHASE-AMT ROUNDED =
                   SBNM-ISSUE-PRICE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-PURCHASE-AMT
           END-IF
           ADD SBNM-PURCHASE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-EXCEPT.
           IF SBNM-EXCEPT-CNT < 9999999
               ADD 1 TO SBNM-EXCEPT-CNT
           END-IF
           IF SBNM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO SBNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TAXABLE AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3660-LIMIT-TAXABLE.
           IF SBNM-TAXABLE-AMT < 25.00
               MOVE 25.00 TO SBNM-TAXABLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-TAXABLE-AMT > 5000.00
               MOVE 5000.00 TO SBNM-TAXABLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUE PRICE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUE PRICE AMT.
      *----------------------------------------------------------------*
       3670-PRORATE-ISSUE-PRICE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-ISSUE-PRICE-AMT ROUNDED =
                   SBNM-ISSUE-PRICE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-ISSUE-PRICE-AMT
           END-IF
           ADD SBNM-ISSUE-PRICE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE TAXABLE FEE ON THE INTEREST YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3680-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN SBNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO SBNT-TAXABLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-REVIEW-FLAG = 'Y'
                   COMPUTE SBNT-TAXABLE-AMT ROUNDED = SBNM-INTEREST-YTD
                       * SBNM-PENALTY-PCT / 200
               WHEN SBNT-TRAN-CD = 'BY'
                   MOVE 3.00 TO SBNT-TAXABLE-AMT
               WHEN SBNT-TRAN-CD = 'RD'
                   MOVE 7.00 TO SBNT-TAXABLE-AMT
               WHEN OTHER
                   COMPUTE SBNT-TAXABLE-AMT ROUNDED = SBNM-INTEREST-YTD
                       * SBNM-PENALTY-PCT / 100
           END-EVALUATE
           IF SBNT-TAXABLE-AMT > 0 AND SBNT-TAXABLE-AMT < 1.00
               MOVE 1.00 TO SBNT-TAXABLE-AMT
           END-IF
           IF SBNT-TAXABLE-AMT > 75.00
               MOVE 75.00 TO SBNT-TAXABLE-AMT
           END-IF
           ADD SBNT-TAXABLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3690-COUNT-CYCLE.
           IF SBNM-CYCLE-CNT < 9999999
               ADD 1 TO SBNM-CYCLE-CNT
           END-IF
           IF SBNM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E761 CUST ID REQUIRED
      * E055 ACCRUAL AMT NEGATIVE
      * E459 ACCRUAL AMT OVER 5000
      * E132 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3700-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-CUST-ID = SPACES
                   MOVE 'E761' TO WS-REJECT-CODE
               WHEN SBNM-ACCRUAL-AMT < 0
                   MOVE 'E055' TO WS-REJECT-CODE
               WHEN SBNM-ACCRUAL-AMT > 5000.00
                   MOVE 'E459' TO WS-REJECT-CODE
               WHEN SBNM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E132' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3710-CHECK-EFF.
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
               MOVE 'Y' TO SBNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3720-COUNT-RETRY.
           IF SBNM-RETRY-CNT < 9999999
               ADD 1 TO SBNM-RETRY-CNT
           END-IF
           IF SBNM-RETRY-CNT NOT < 6
               MOVE 'Y' TO SBNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FACE YTD FEE ON THE INTEREST YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3730-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SBNM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SBNM-FACE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-HOLD-FLAG = 'Y'
                   COMPUTE SBNM-FACE-YTD ROUNDED = SBNM-INTEREST-YTD *
                       SBNM-FEE-PCT / 200
               WHEN SBNM-SERIES-CD = 'DD'
                   MOVE 10.00 TO SBNM-FACE-YTD
               WHEN SBNM-SERIES-CD = 'DF'
                   MOVE 12.00 TO SBNM-FACE-YTD
               WHEN OTHER
                   COMPUTE SBNM-FACE-YTD ROUNDED = SBNM-INTEREST-YTD *
                       SBNM-FEE-PCT / 100
           END-EVALUATE
           IF SBNM-FACE-YTD > 0 AND SBNM-FACE-YTD < 10.00
               MOVE 10.00 TO SBNM-FACE-YTD
           END-IF
           IF SBNM-FACE-YTD > 150.00
               MOVE 150.00 TO SBNM-FACE-YTD
           END-IF
           ADD SBNM-FACE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3740-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF SBNM-NEXT-REVIEW-DATE > 0 AND SBNM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-NEXT-REVIEW-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 120
                   ADD SBNT-TAXABLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNT-TAXABLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE TAXABLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3750-PRORATE-TAXABLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-TAXABLE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3760-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SBNM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO SBNM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SERIES CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REDEEM YTD.
      *----------------------------------------------------------------*
       3780-PRORATE-REDEEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-REDEEM-YTD ROUNDED =
                   SBNT-INTEREST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-REDEEM-YTD
           END-IF
           ADD SBNM-REDEEM-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE ISSUE PRICE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3790-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SBNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO SBNT-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-HOLD-FLAG = 'Y'
                   COMPUTE SBNT-INTEREST-AMT ROUNDED =
                       SBNM-ISSUE-PRICE-AMT * SBNM-PENALTY-PCT / 200
               WHEN SBNT-TRAN-CD = 'BY'
                   MOVE 20.00 TO SBNT-INTEREST-AMT
               WHEN SBNT-TRAN-CD = 'RD'
                   MOVE 15.00 TO SBNT-INTEREST-AMT
               WHEN OTHER
                   COMPUTE SBNT-INTEREST-AMT ROUNDED =
                       SBNM-ISSUE-PRICE-AMT * SBNM-PENALTY-PCT / 100
           END-EVALUATE
           IF SBNT-INTEREST-AMT > 0 AND SBNT-INTEREST-AMT < 5.00
               MOVE 5.00 TO SBNT-INTEREST-AMT
           END-IF
           IF SBNT-INTEREST-AMT > 35.00
               MOVE 35.00 TO SBNT-INTEREST-AMT
           END-IF
           ADD SBNT-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * E -> J WHEN POST DATE IS PAST
      * J -> L WHEN POST DATE IS PAST
      * L -> U WHEN POST DATE IS PAST
      * U -> E WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3800-SET-SOURCE.
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
      * PURCHASE YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3810-LIMIT-PURCHASE.
           IF SBNM-PURCHASE-YTD < 0.00
               MOVE 0.00 TO SBNM-PURCHASE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-PURCHASE-YTD > 2500.00
               MOVE 2500.00 TO SBNM-PURCHASE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3820-CHECK-LAST-PAY.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO SBNM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REDEEM AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3830-SPLIT-REDEEM.
           COMPUTE WS-WORK-AMT = SBNM-REDEEM-AMT / 12
           COMPUTE WS-WORK-AMT-2 = SBNM-REDEEM-AMT - (WS-WORK-AMT * 12)
           COMPUTE SBNM-PURCHASE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAXABLE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3840-ACCRUE-TAXABLE.
           IF SBNM-TAXABLE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-TAXABLE-YTD * SBNM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBNM-FACE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E909 REJECT CD REQUIRED
      * E299 INTEREST AMT NEGATIVE
      * E202 INTEREST AMT OVER 250000
      * E945 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3850-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-REJECT-CD = SPACES
                   MOVE 'E909' TO WS-REJECT-CODE
               WHEN SBNT-INTEREST-AMT < 0
                   MOVE 'E299' TO WS-REJECT-CODE
               WHEN SBNT-INTEREST-AMT > 250000.00
                   MOVE 'E202' TO WS-REJECT-CODE
               WHEN SBNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E945' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REDEEM AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3860-ACCRUE-REDEEM.
           IF SBNM-REDEEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-REDEEM-AMT * SBNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO SBNT-ACCRUAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BY -> RD WHEN LAST PAY DATE IS PAST
      * RD -> IR WHEN LAST PAY DATE IS PAST
      * IR -> TX WHEN LAST PAY DATE IS PAST
      * TX -> AD WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3870-SET-TRAN.
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
      * DAILY INTEREST ON ACCRUAL AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3880-ACCRUE-ACCRUAL.
           IF SBNM-ACCRUAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-ACCRUAL-AMT * SBNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO SBNM-FACE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUE PRICE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3890-ACCRUE-ISSUE-PRICE.
           IF SBNM-ISSUE-PRICE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-ISSUE-PRICE-AMT * SBNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBNT-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3900-LIMIT-INTEREST.
           IF SBNM-INTEREST-YTD < 0.00
               MOVE 0.00 TO SBNM-INTEREST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-INTEREST-YTD > 2500.00
               MOVE 2500.00 TO SBNM-INTEREST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SERIES CD:
      * DD -> DF WHEN POST DATE IS PAST
      * DF -> SS WHEN POST DATE IS PAST
      * SS -> VJ WHEN POST DATE IS PAST
      * VJ -> VX WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3910-SET-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DF'
                   IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SS' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SS'
                   IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJ'
                   IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ACCRUAL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3920-SPLIT-ACCRUAL.
           COMPUTE WS-WORK-AMT = SBNT-ACCRUAL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SBNT-ACCRUAL-AMT - (WS-WORK-AMT * 2)
           COMPUTE SBNT-TAXABLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUAL SCHED TIER FOR THE ISSUE PRICE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUAL-SCHED-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SBNM-ISSUE-PRICE-YTD NOT < WS-ACCRUAL-SCHED-LOW(WS-TX)
                   MOVE WS-ACCRUAL-SCHED-RATE(WS-TX) TO SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3940-DESCRIBE-SOURCE.
           EVALUATE SBNM-SOURCE-CD
               WHEN 'E'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'J'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'U'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3950-COUNT-CYCLE.
           IF SBNM-CYCLE-CNT < 9999999
               ADD 1 TO SBNM-CYCLE-CNT
           END-IF
           IF SBNM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO SBNM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAXABLE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-TAXABLE.
           COMPUTE WS-WORK-AMT = SBNM-TAXABLE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = SBNM-TAXABLE-YTD - (WS-WORK-AMT * 3)
           COMPUTE SBNM-REDEEM-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUAL SCHED TIER FOR THE ISSUE PRICE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUAL-SCHED-RATE(1) TO SBNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SBNM-ISSUE-PRICE-AMT NOT < WS-ACCRUAL-SCHED-LOW(WS-TX)
                   MOVE WS-ACCRUAL-SCHED-RATE(WS-TX) TO SBNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E926 NAME REQUIRED
      * E244 PURCHASE YTD NEGATIVE
      * E666 PURCHASE YTD OVER 50000
      * E581 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3980-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-NAME = SPACES
                   MOVE 'E926' TO WS-REJECT-CODE
               WHEN SBNM-PURCHASE-YTD < 0
                   MOVE 'E244' TO WS-REJECT-CODE
               WHEN SBNM-PURCHASE-YTD > 50000.00
                   MOVE 'E666' TO WS-REJECT-CODE
               WHEN SBNM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E581' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3990-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ACCRUAL YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4000-SPLIT-ACCRUAL.
           COMPUTE WS-WORK-AMT = SBNM-ACCRUAL-YTD / 2
           COMPUTE WS-WORK-AMT-2 = SBNM-ACCRUAL-YTD - (WS-WORK-AMT * 2)
           COMPUTE SBNT-ACCRUAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4010-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = SBNM-INTEREST-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SBNM-INTEREST-AMT - (WS-WORK-AMT * 4)
           COMPUTE SBNM-PURCHASE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4020-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-BRANCH-CD
                   ADD SBNM-FACE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-FACE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E342 DESC REQUIRED
      * E080 WORK AMT 2 NEGATIVE
      * E612 WORK AMT 2 OVER 25000
      * E066 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4030-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-DESC = SPACES
                   MOVE 'E342' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E080' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E612' TO WS-REJECT-CODE
               WHEN SBNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E066' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SERIES CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4040-DESCRIBE-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4050-AGE-LAST-PAY.
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
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4060-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO SBNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ACCRUAL YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-SOURCE-CD
                   ADD SBNM-ACCRUAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-ACCRUAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUAL SCHED TIER FOR THE REDEEM AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4080-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUAL-SCHED-RATE(1) TO SBNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SBNT-REDEEM-AMT NOT < WS-ACCRUAL-SCHED-LOW(WS-TX)
                   MOVE WS-ACCRUAL-SCHED-RATE(WS-TX) TO SBNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4090-DESCRIBE-STATUS.
           EVALUATE SBNM-STATUS-CD
               WHEN 'CS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE INTEREST AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4100-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN SBNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO SBNM-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SBNM-INTEREST-AMT ROUNDED = SBNT-INTEREST-AMT
                       * SBNM-MARGIN / 200
               WHEN SBNM-REASON-CD = 'DG'
                   MOVE 3.00 TO SBNM-INTEREST-AMT
               WHEN SBNM-REASON-CD = 'UK'
                   MOVE 7.00 TO SBNM-INTEREST-AMT
               WHEN OTHER
                   COMPUTE SBNM-INTEREST-AMT ROUNDED = SBNT-INTEREST-AMT
                       * SBNM-MARGIN / 100
           END-EVALUATE
           IF SBNM-INTEREST-AMT > 0 AND SBNM-INTEREST-AMT < 5.00
               MOVE 5.00 TO SBNM-INTEREST-AMT
           END-IF
           IF SBNM-INTEREST-AMT > 100.00
               MOVE 100.00 TO SBNM-INTEREST-AMT
           END-IF
           ADD SBNM-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUE PRICE YTD BY STATUS CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4110-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-STATUS-CD
                   ADD SBNM-ISSUE-PRICE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-ISSUE-PRICE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4120-CHECK-NOTICE.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO SBNM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PURCHASE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4130-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = SBNM-PURCHASE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SBNM-PURCHASE-YTD - (WS-WORK-AMT * 4)
           COMPUTE SBNM-ACCRUAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BY -> RD WHEN NOTICE DATE IS PAST
      * RD -> IR WHEN NOTICE DATE IS PAST
      * IR -> TX WHEN NOTICE DATE IS PAST
      * TX -> AD WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4140-SET-TRAN.
           EVALUATE SBNT-TRAN-CD
               WHEN 'BY'
                   IF SBNM-NOTICE-DATE > 0 AND SBNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF SBNM-NOTICE-DATE > 0 AND SBNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'IR' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'IR'
                   IF SBNM-NOTICE-DATE > 0 AND SBNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TX'
                   IF SBNM-NOTICE-DATE > 0 AND SBNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ACCRUAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TAXABLE AMT.
      *----------------------------------------------------------------*
       4150-PRORATE-TAXABLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-TAXABLE-AMT ROUNDED =
                   SBNM-ACCRUAL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-TAXABLE-AMT
           END-IF
           ADD SBNM-TAXABLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE REDEEM AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4160-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-STATUS-CD
                   ADD SBNM-REDEEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-REDEEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUE PRICE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REDEEM YTD.
      *----------------------------------------------------------------*
       4170-PRORATE-REDEEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-REDEEM-YTD ROUNDED =
                   SBNM-ISSUE-PRICE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-REDEEM-YTD
           END-IF
           ADD SBNM-REDEEM-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE TIER TIER FOR THE ACCRUAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4180-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-TIER-RATE(1) TO SBNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF SBNT-ACCRUAL-AMT NOT < WS-PURCHASE-TIER-LOW(WS-TX)
                   MOVE WS-PURCHASE-TIER-RATE(WS-TX) TO SBNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SERIES CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4190-DESCRIBE-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4200-CHECK-MATURITY.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO SBNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE TIER TIER FOR THE PURCHASE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4210-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-TIER-RATE(1) TO SBNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF SBNM-PURCHASE-AMT NOT < WS-PURCHASE-TIER-LOW(WS-TX)
                   MOVE WS-PURCHASE-TIER-RATE(WS-TX) TO SBNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REDEEM YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4220-FLAG-REDEEM.
           IF SBNM-REDEEM-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBNM-REDEEM-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ACCRUAL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4230-SPLIT-ACCRUAL.
           COMPUTE WS-WORK-AMT = SBNM-ACCRUAL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SBNM-ACCRUAL-AMT - (WS-WORK-AMT * 2)
           COMPUTE SBNM-ACCRUAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4240-COUNT-PAYMENT.
           IF SBNM-PAYMENT-CNT < 9999999
               ADD 1 TO SBNM-PAYMENT-CNT
           END-IF
           IF SBNM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO SBNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAXABLE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4250-ACCRUE-TAXABLE.
           IF SBNT-TAXABLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNT-TAXABLE-AMT * SBNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO SBNM-PURCHASE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4260-LIMIT-PURCHASE.
           IF SBNM-PURCHASE-AMT < 100.00
               MOVE 100.00 TO SBNM-PURCHASE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-PURCHASE-AMT > 5000.00
               MOVE 5000.00 TO SBNM-PURCHASE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE TAXABLE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4270-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SBNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SBNT-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE SBNT-INTEREST-AMT ROUNDED = SBNM-TAXABLE-YTD
                       * SBNM-FEE-PCT / 200
               WHEN SBNM-SOURCE-CD = 'E'
                   MOVE 15.00 TO SBNT-INTEREST-AMT
               WHEN SBNM-SOURCE-CD = 'J'
                   MOVE 3.00 TO SBNT-INTEREST-AMT
               WHEN OTHER
                   COMPUTE SBNT-INTEREST-AMT ROUNDED = SBNM-TAXABLE-YTD
                       * SBNM-FEE-PCT / 100
           END-EVALUATE
           IF SBNT-INTEREST-AMT > 0 AND SBNT-INTEREST-AMT < 10.00
               MOVE 10.00 TO SBNT-INTEREST-AMT
           END-IF
           IF SBNT-INTEREST-AMT > 35.00
               MOVE 35.00 TO SBNT-INTEREST-AMT
           END-IF
           ADD SBNT-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCRUAL YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4280-LIMIT-ACCRUAL.
           IF SBNM-ACCRUAL-YTD < 10.00
               MOVE 10.00 TO SBNM-ACCRUAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-ACCRUAL-YTD > 10000.00
               MOVE 10000.00 TO SBNM-ACCRUAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUAL FEE ON THE REDEEM AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4290-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SBNM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SBNM-ACCRUAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-HOLD-FLAG = 'Y'
                   COMPUTE SBNM-ACCRUAL-AMT ROUNDED = SBNM-REDEEM-AMT *
                       SBNM-MARGIN / 200
               WHEN SBNT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO SBNM-ACCRUAL-AMT
               WHEN SBNT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO SBNM-ACCRUAL-AMT
               WHEN OTHER
                   COMPUTE SBNM-ACCRUAL-AMT ROUNDED = SBNM-REDEEM-AMT *
                       SBNM-MARGIN / 100
           END-EVALUATE
           IF SBNM-ACCRUAL-AMT > 0 AND SBNM-ACCRUAL-AMT < 2.00
               MOVE 2.00 TO SBNM-ACCRUAL-AMT
           END-IF
           IF SBNM-ACCRUAL-AMT > 250.00
               MOVE 250.00 TO SBNM-ACCRUAL-AMT
           END-IF
           ADD SBNM-ACCRUAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT TAXABLE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4300-SPLIT-TAXABLE.
           COMPUTE WS-WORK-AMT = SBNT-TAXABLE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SBNT-TAXABLE-AMT - (WS-WORK-AMT * 2)
           COMPUTE SBNM-ACCRUAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

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
                   MOVE SBLIM-ISSUE-PRICE-YTD
                     TO SBNM-ISSUE-PRICE-YTD
                   MOVE SBLIM-FACE-YTD
                     TO SBNM-FACE-YTD
                   MOVE SBLIM-PURCHASE-YTD
                     TO SBNM-PURCHASE-YTD
                   MOVE SBLIM-PURCHASE-AMT
                     TO SBNM-PURCHASE-AMT
                   MOVE SBLIM-PREFERRED-FLAG
                     TO SBNM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-SBLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE SBN FIELDS TO SBSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-SBSTAT.
           INITIALIZE SBSTAT-PARMS
           MOVE SBNM-ACCRUAL-AMT
             TO SBSTAT-ACCRUAL-AMT
           MOVE SBNM-TAXABLE-YTD
             TO SBSTAT-TAXABLE-YTD
           MOVE SBNM-INTEREST-YTD
             TO SBSTAT-INTEREST-YTD
           MOVE SBNM-FACE-AMT
             TO SBSTAT-FACE-AMT
           MOVE SBNM-REDEEM-AMT
             TO SBSTAT-REDEEM-AMT
           MOVE SBNM-FACE-YTD
             TO SBSTAT-FACE-YTD
           MOVE SBNM-MATURITY-DATE
             TO SBSTAT-MATURITY-DATE
           MOVE SBNM-EFFECTIVE-DATE
             TO SBSTAT-EFFECTIVE-DATE
           MOVE SBNM-NOTICE-DATE
             TO SBSTAT-NOTICE-DATE
           MOVE SBNM-NEXT-DUE-DATE
             TO SBSTAT-NEXT-DUE-DATE
           MOVE SBNM-REASON-CD
             TO SBSTAT-REASON-CD
           MOVE SBNM-SOURCE-CD
             TO SBSTAT-SOURCE-CD
           MOVE SBNM-STATUS-CD
             TO SBSTAT-STATUS-CD
           MOVE SBNM-ISSUE-TYPE-CD
             TO SBSTAT-ISSUE-TYPE-CD
           MOVE SBNM-REVIEW-FLAG
             TO SBSTAT-REVIEW-FLAG
           MOVE SBNM-PREFERRED-FLAG
             TO SBSTAT-PREFERRED-FLAG
           MOVE SBNM-AUTO-RENEW-FLAG
             TO SBSTAT-AUTO-RENEW-FLAG
           MOVE SBNM-EXCEPT-CNT
             TO SBSTAT-EXCEPT-CNT
           MOVE SBNM-RETRY-CNT
             TO SBSTAT-RETRY-CNT
           MOVE SBNM-MARGIN
             TO SBSTAT-MARGIN
           MOVE SBNM-PENALTY-PCT
             TO SBSTAT-PENALTY-PCT
           MOVE SBNM-DESC
             TO SBSTAT-DESC
           MOVE SBNM-NAME
             TO SBSTAT-NAME
           CALL 'SBSTAT' USING SBSTAT-PARMS
           EVALUATE SBSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SBSTAT-INTEREST-YTD
                     TO SBNM-INTEREST-YTD
                   MOVE SBSTAT-REDEEM-AMT
                     TO SBNM-REDEEM-AMT
                   MOVE SBSTAT-FACE-YTD
                     TO SBNM-FACE-YTD
                   MOVE SBSTAT-REVIEW-FLAG
                     TO SBNM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-SBSTAT' TO ABND-PARAGRAPH
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
           DISPLAY 'SBNCLSE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'SBNCLSE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
