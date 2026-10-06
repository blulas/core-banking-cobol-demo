      *================================================================*
      * PROGRAM  : SBNEXTR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : SAVINGS BOND SERVICES - NIGHTLY EXTRACT. READS THE  *
      *            SBN TRANSACTIONS, UPDATES THE SBN MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. US SAVINGS BOND SALES, *
      *            REDEMPTION AND TAX REPORTING FOR CUSTOMERS          *
      * INPUT    : SBNMST (KSDS), SBNTRN, BANKCTL                      *
      * OUTPUT   : SBNOUT, SBNEXRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, SBFEE                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-08-14 GAF  ORIGINAL                                       *
      * 1995-06-21 CRP  INTEREST STATUS CODES ADDED REQ4514            *
      * 1996-01-18 DPS  TAXABLE FEE SCHEDULE UPDATE CHG7695            *
      * 1997-09-08 SKN  AUDIT FINDING - ACCRUAL CONTROL TOTALS INC7293 *
      * 1999-12-27 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2007-06-19 TWH  FACE CUTOFF MOVED PRB3843                      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SBNEXTR.
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
           SELECT REPORT-FILE ASSIGN TO "SBNEXRPT"
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
                                           VALUE 24.000000.
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
      * WS-ISSUE-PR-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ISSUE-PR-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000848000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000383000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000409500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000361500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001150000000442000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000262500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002250000000283000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012250000000323000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012300000000245000'.
       01  WS-ISSUE-PR-BAND-TBL REDEFINES WS-ISSUE-PR-BAND-VALUES.
           05  WS-ISSUE-PR-BAND-ROW OCCURS 9 TIMES.
               10  WS-ISSUE-PR-BAND-LOW    PIC 9(09)V99.
               10  WS-ISSUE-PR-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INTEREST-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000531500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000451500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000541000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015050000000442000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000171000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017600000000488000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017850000000342000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018350000000487500'.
       01  WS-INTEREST-SCHED-TBL REDEFINES WS-INTEREST-SCHED-VALUES.
           05  WS-INTEREST-SCHED-ROW OCCURS 8 TIMES.
               10  WS-INTEREST-SCHED-LOW   PIC 9(09)V99.
               10  WS-INTEREST-SCHED-RATE  PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  SBNEXTR  SAVINGS BOND SERVICES'.
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
               PERFORM 3000-PRORATE-TAXABLE
               PERFORM 3010-ACCRUE-REDEEM
               PERFORM 3020-PRORATE-ISSUE-PRICE
               PERFORM 3030-ACCUM-SOURCE
               PERFORM 3040-ACCRUE-TAXABLE
               PERFORM 3050-AGE-LAST-MAINT
               PERFORM 3060-COUNT-CYCLE
               PERFORM 3070-EDIT-MEMO
               PERFORM 3080-CHECK-EFFECTIVE
               PERFORM 3090-FLAG-INTEREST
               PERFORM 3100-PRORATE-ISSUE-PRICE
               PERFORM 3110-SET-SERIES
               PERFORM 3120-SPLIT-PURCHASE
               PERFORM 3130-ACCRUE-PURCHASE
               PERFORM 3140-ASSESS-REVERSAL
               PERFORM 3150-AGE-MATURITY
               PERFORM 3160-ASSESS-REVERSAL
               PERFORM 3170-ACCUM-CHANNEL
               PERFORM 3180-ACCRUE-FACE
               PERFORM 3190-DESCRIBE-SOURCE
               PERFORM 3200-PRORATE-PURCHASE
               PERFORM 3210-ACCRUE-INTEREST
               PERFORM 3220-FLAG-INTEREST
               PERFORM 3230-LIMIT-REDEEM
               PERFORM 3240-AGE-MATURITY
               PERFORM 3250-SPLIT-TAXABLE
               PERFORM 3260-PRORATE-ACCRUAL
               PERFORM 3270-DESCRIBE-SOURCE
               PERFORM 3280-ACCRUE-INTEREST
               PERFORM 3290-COUNT-PAYMENT
               PERFORM 3300-FLAG-FACE
               PERFORM 3310-PRORATE-INTEREST
               PERFORM 3320-SET-REASON
               PERFORM 3330-ACCRUE-REDEEM
               PERFORM 3340-FLAG-PURCHASE
               PERFORM 3350-ACCRUE-ACCRUAL
               PERFORM 3360-CHECK-EXPIRY
               PERFORM 3370-DESCRIBE-REASON
               PERFORM 3380-FLAG-REDEEM
               PERFORM 3390-CHECK-MATURITY
               PERFORM 3400-SELECT-MARGIN
               PERFORM 3410-EDIT-MEMO
               PERFORM 3420-DESCRIBE-CHANNEL
               PERFORM 3430-DESCRIBE-REASON
               PERFORM 3440-COUNT-CYCLE
               PERFORM 3450-PRORATE-ACCRUAL
               PERFORM 3460-ACCRUE-REDEEM
               PERFORM 3470-EDIT-MEMO
               PERFORM 3480-DESCRIBE-ISSUE-TYPE
               PERFORM 3490-SET-BRANCH
               PERFORM 3500-SET-SOURCE
               PERFORM 3510-COUNT-PAYMENT
               PERFORM 3520-COUNT-SEQ
               PERFORM 3530-DESCRIBE-TRAN
               PERFORM 3540-CHECK-EXPIRY
               PERFORM 3550-ACCUM-REASON
               PERFORM 3560-CHECK-POST
               PERFORM 3570-LIMIT-INTEREST
               PERFORM 3580-SPLIT-TAXABLE
               PERFORM 3590-PRORATE-FACE
               PERFORM 3600-LIMIT-ITEM
               PERFORM 3610-SELECT-MARGIN
               PERFORM 3620-SPLIT-REDEEM
               PERFORM 3630-SELECT-PENALTY-PCT
               PERFORM 3640-SET-STATUS
               PERFORM 3650-COUNT-CYCLE
               PERFORM 3660-ACCRUE-PENALTY-PCT
               PERFORM 3670-LIMIT-ACCRUAL
               PERFORM 3680-FLAG-REDEEM
               PERFORM 3690-LIMIT-TAXABLE
               PERFORM 3700-CHECK-EFFECTIVE
               PERFORM 3710-SET-SERIES
               PERFORM 3720-SET-SERIES
               PERFORM 3730-CHECK-MATURITY
               PERFORM 3740-ACCUM-TRAN
               PERFORM 3750-SET-SOURCE
               PERFORM 3760-AGE-POST
               PERFORM 3770-DESCRIBE-REASON
               PERFORM 3780-FLAG-ACCRUAL
               PERFORM 3790-DESCRIBE-SOURCE
               PERFORM 3800-ASSESS-REVERSAL
               PERFORM 3810-SELECT-PENALTY-PCT
               PERFORM 3820-ACCRUE-ACCRUAL
               PERFORM 3830-ASSESS-HOLD
               PERFORM 3840-SELECT-FEE-PCT
               PERFORM 3850-FLAG-PURCHASE
               PERFORM 3860-SELECT-PENALTY-PCT
               PERFORM 3870-ACCUM-ISSUE-TYPE
               PERFORM 3880-DESCRIBE-CHANNEL
               PERFORM 3890-SPLIT-PURCHASE
               PERFORM 3900-ACCRUE-INTEREST
               PERFORM 3910-ACCUM-ISSUE-TYPE
               PERFORM 3920-AGE-NOTICE
               PERFORM 3930-FLAG-ISSUE-PRICE
               PERFORM 3940-COUNT-PAYMENT
               PERFORM 3950-PRORATE-TAXABLE
               PERFORM 3960-SET-TRAN
               PERFORM 3970-ACCRUE-ACCRUAL
               PERFORM 3980-AGE-EFF
               PERFORM 3990-ACCRUE-ISSUE-PRICE
               PERFORM 4000-ACCRUE-INTEREST
               PERFORM 4010-PRORATE-ISSUE-PRICE
               PERFORM 4020-EDIT-NAME
               PERFORM 4030-SPLIT-ACCRUAL
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
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TAXABLE AMT.
      *----------------------------------------------------------------*
       3000-PRORATE-TAXABLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-TAXABLE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-TAXABLE-AMT
           END-IF
           ADD SBNM-TAXABLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REDEEM AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-REDEEM.
           IF SBNT-REDEEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNT-REDEEM-AMT * SBNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO SBNM-FACE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUE PRICE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3020-PRORATE-ISSUE-PRICE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-ISSUE-PRICE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE TAXABLE AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3030-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-SOURCE-CD
                   ADD SBNT-TAXABLE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNT-TAXABLE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAXABLE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3040-ACCRUE-TAXABLE.
           IF SBNM-TAXABLE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-TAXABLE-YTD * SBNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBNM-PURCHASE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3050-AGE-LAST-MAINT.
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
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3060-COUNT-CYCLE.
           IF SBNM-CYCLE-CNT < 9999999
               ADD 1 TO SBNM-CYCLE-CNT
           END-IF
           IF SBNM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO SBNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E270 MEMO REQUIRED
      * E034 REDEEM AMT NEGATIVE
      * E343 REDEEM AMT OVER 25000
      * E094 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3070-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-MEMO = SPACES
                   MOVE 'E270' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-AMT < 0
                   MOVE 'E034' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-AMT > 25000.00
                   MOVE 'E343' TO WS-REJECT-CODE
               WHEN SBNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E094' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3080-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3090-FLAG-INTEREST.
           IF SBNM-INTEREST-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBNM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REDEEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUE PRICE YTD.
      *----------------------------------------------------------------*
       3100-PRORATE-ISSUE-PRICE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-ISSUE-PRICE-YTD ROUNDED =
                   SBNM-REDEEM-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-ISSUE-PRICE-YTD
           END-IF
           ADD SBNM-ISSUE-PRICE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SERIES CD:
      * DD -> DF WHEN NEXT DUE DATE IS PAST
      * DF -> SS WHEN NEXT DUE DATE IS PAST
      * SS -> VJ WHEN NEXT DUE DATE IS PAST
      * VJ -> VX WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   IF SBNM-NEXT-DUE-DATE > 0 AND SBNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DF'
                   IF SBNM-NEXT-DUE-DATE > 0 AND SBNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SS' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SS'
                   IF SBNM-NEXT-DUE-DATE > 0 AND SBNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJ'
                   IF SBNM-NEXT-DUE-DATE > 0 AND SBNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3120-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE SBNM-PURCHASE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PURCHASE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3130-ACCRUE-PURCHASE.
           IF SBNM-PURCHASE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-PURCHASE-AMT * SBNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBNM-ISSUE-PRICE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FACE YTD FEE ON THE ACCRUAL AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3140-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN SBNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO SBNM-FACE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SBNM-FACE-YTD ROUNDED = SBNM-ACCRUAL-AMT *
                       SBNM-FEE-PCT / 200
               WHEN SBNT-TRAN-CD = 'BY'
                   MOVE 15.00 TO SBNM-FACE-YTD
               WHEN SBNT-TRAN-CD = 'RD'
                   MOVE 15.00 TO SBNM-FACE-YTD
               WHEN OTHER
                   COMPUTE SBNM-FACE-YTD ROUNDED = SBNM-ACCRUAL-AMT *
                       SBNM-FEE-PCT / 100
           END-EVALUATE
           IF SBNM-FACE-YTD > 0 AND SBNM-FACE-YTD < 5.00
               MOVE 5.00 TO SBNM-FACE-YTD
           END-IF
           IF SBNM-FACE-YTD > 50.00
               MOVE 50.00 TO SBNM-FACE-YTD
           END-IF
           ADD SBNM-FACE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3150-AGE-MATURITY.
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
               WHEN WS-DAYS NOT > 30
                   ADD SBNM-ISSUE-PRICE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SBNM-ISSUE-PRICE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-ISSUE-PRICE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-ISSUE-PRICE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE REDEEM FEE ON THE REDEEM YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3160-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN SBNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO SBNM-REDEEM-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-HOLD-FLAG = 'Y'
                   COMPUTE SBNM-REDEEM-AMT ROUNDED = SBNM-REDEEM-YTD *
                       SBNM-FEE-PCT / 200
               WHEN SBNT-TRAN-CD = 'BY'
                   MOVE 20.00 TO SBNM-REDEEM-AMT
               WHEN SBNT-TRAN-CD = 'RD'
                   MOVE 3.00 TO SBNM-REDEEM-AMT
               WHEN OTHER
                   COMPUTE SBNM-REDEEM-AMT ROUNDED = SBNM-REDEEM-YTD *
                       SBNM-FEE-PCT / 100
           END-EVALUATE
           IF SBNM-REDEEM-AMT > 0 AND SBNM-REDEEM-AMT < 1.00
               MOVE 1.00 TO SBNM-REDEEM-AMT
           END-IF
           IF SBNM-REDEEM-AMT > 250.00
               MOVE 250.00 TO SBNM-REDEEM-AMT
           END-IF
           ADD SBNM-REDEEM-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE ACCRUAL AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNT-CHANNEL-CD
                   ADD SBNM-ACCRUAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-ACCRUAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3180-ACCRUE-FACE.
           IF SBNM-FACE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-FACE-YTD * SBNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBNM-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3190-DESCRIBE-SOURCE.
           EVALUATE SBNM-SOURCE-CD
               WHEN 'E'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'J'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'L'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'U'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FACE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PURCHASE YTD.
      *----------------------------------------------------------------*
       3200-PRORATE-PURCHASE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-PURCHASE-YTD ROUNDED =
                   SBNM-FACE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNM-PURCHASE-YTD
           END-IF
           ADD SBNM-PURCHASE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3210-ACCRUE-INTEREST.
           IF SBNM-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-INTEREST-AMT * SBNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBNT-REDEEM-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3220-FLAG-INTEREST.
           IF SBNM-INTEREST-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBNM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * REDEEM AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3230-LIMIT-REDEEM.
           IF SBNM-REDEEM-AMT < 10.00
               MOVE 10.00 TO SBNM-REDEEM-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-REDEEM-AMT > 2500.00
               MOVE 2500.00 TO SBNM-REDEEM-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3240-AGE-MATURITY.
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
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT TAXABLE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3250-SPLIT-TAXABLE.
           COMPUTE WS-WORK-AMT = SBNM-TAXABLE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = SBNM-TAXABLE-YTD - (WS-WORK-AMT * 12)
           COMPUTE SBNM-ISSUE-PRICE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAXABLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ACCRUAL YTD.
      *----------------------------------------------------------------*
       3260-PRORATE-ACCRUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-ACCRUAL-YTD ROUNDED =
                   SBNT-TAXABLE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNM-ACCRUAL-YTD
           END-IF
           ADD SBNM-ACCRUAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3270-DESCRIBE-SOURCE.
           EVALUATE SBNM-SOURCE-CD
               WHEN 'E'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'J'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'U'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-INTEREST.
           IF SBNM-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-INTEREST-AMT * SBNM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBNM-TAXABLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3290-COUNT-PAYMENT.
           IF SBNM-PAYMENT-CNT < 9999999
               ADD 1 TO SBNM-PAYMENT-CNT
           END-IF
           IF SBNM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO SBNM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3300-FLAG-FACE.
           IF SBNM-FACE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBNM-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ACCRUAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       3310-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-INTEREST-AMT ROUNDED =
                   SBNM-ACCRUAL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-INTEREST-AMT
           END-IF
           ADD SBNM-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DG -> UK WHEN OPEN DATE IS PAST
      * UK -> VH WHEN OPEN DATE IS PAST
      * VH -> XD WHEN OPEN DATE IS PAST
      * XD -> DG WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3320-SET-REASON.
           EVALUATE SBNM-REASON-CD
               WHEN 'DG'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UK' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VH' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VH'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DG' TO SBNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REDEEM AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3330-ACCRUE-REDEEM.
           IF SBNM-REDEEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-REDEEM-AMT * SBNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBNM-TAXABLE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3340-FLAG-PURCHASE.
           IF SBNM-PURCHASE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBNM-PURCHASE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ACCRUAL YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3350-ACCRUE-ACCRUAL.
           IF SBNM-ACCRUAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-ACCRUAL-YTD * SBNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBNM-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3360-CHECK-EXPIRY.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO SBNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3370-DESCRIBE-REASON.
           EVALUATE SBNM-REASON-CD
               WHEN 'DG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REDEEM AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3380-FLAG-REDEEM.
           IF SBNT-REDEEM-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBNT-REDEEM-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3390-CHECK-MATURITY.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO SBNM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE REDEEM YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3400-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO SBNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SBNM-REDEEM-YTD NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO SBNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E593 MEMO REQUIRED
      * E005 ISSUE PRICE YTD NEGATIVE
      * E383 ISSUE PRICE YTD OVER 1000000
      * E418 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3410-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-MEMO = SPACES
                   MOVE 'E593' TO WS-REJECT-CODE
               WHEN SBNM-ISSUE-PRICE-YTD < 0
                   MOVE 'E005' TO WS-REJECT-CODE
               WHEN SBNM-ISSUE-PRICE-YTD > 1000000.00
                   MOVE 'E383' TO WS-REJECT-CODE
               WHEN SBNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E418' TO WS-REJECT-CODE
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
       3420-DESCRIBE-CHANNEL.
           EVALUATE SBNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3430-DESCRIBE-REASON.
           EVALUATE SBNM-REASON-CD
               WHEN 'DG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3440-COUNT-CYCLE.
           IF SBNM-CYCLE-CNT < 9999999
               ADD 1 TO SBNM-CYCLE-CNT
           END-IF
           IF SBNM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO SBNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ACCRUAL AMT.
      *----------------------------------------------------------------*
       3450-PRORATE-ACCRUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNT-ACCRUAL-AMT ROUNDED =
                   SBNM-PURCHASE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNT-ACCRUAL-AMT
           END-IF
           ADD SBNT-ACCRUAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REDEEM AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3460-ACCRUE-REDEEM.
           IF SBNM-REDEEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-REDEEM-AMT * SBNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E621 MEMO REQUIRED
      * E312 REDEEM AMT NEGATIVE
      * E450 REDEEM AMT OVER 5000
      * E322 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3470-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-MEMO = SPACES
                   MOVE 'E621' TO WS-REJECT-CODE
               WHEN SBNT-REDEEM-AMT < 0
                   MOVE 'E312' TO WS-REJECT-CODE
               WHEN SBNT-REDEEM-AMT > 5000.00
                   MOVE 'E450' TO WS-REJECT-CODE
               WHEN SBNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E322' TO WS-REJECT-CODE
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
       3480-DESCRIBE-ISSUE-TYPE.
           EVALUATE SBNM-ISSUE-TYPE-CD
               WHEN 'CW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'DY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * BD -> FC WHEN MATURITY DATE IS PAST
      * FC -> KR WHEN MATURITY DATE IS PAST
      * KR -> MP WHEN MATURITY DATE IS PAST
      * MP -> VJ WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3490-SET-BRANCH.
           EVALUATE SBNM-BRANCH-CD
               WHEN 'BD'
                   IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FC' TO SBNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FC'
                   IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO SBNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MP' TO SBNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MP'
                   IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * E -> J WHEN POST DATE IS PAST
      * J -> L WHEN POST DATE IS PAST
      * L -> U WHEN POST DATE IS PAST
      * U -> E WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3500-SET-SOURCE.
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
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-PAYMENT.
           IF SBNM-PAYMENT-CNT < 9999999
               ADD 1 TO SBNM-PAYMENT-CNT
           END-IF
           IF SBNM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO SBNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3520-COUNT-SEQ.
           IF SBNT-SEQ < 9999999
               ADD 1 TO SBNT-SEQ
           END-IF
           IF SBNT-SEQ NOT < 5
               MOVE 'Y' TO SBNM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3530-DESCRIBE-TRAN.
           EVALUATE SBNT-TRAN-CD
               WHEN 'BY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'IR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'AD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3540-CHECK-EXPIRY.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO SBNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REDEEM AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3550-ACCUM-REASON.
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
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3560-CHECK-POST.
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
               MOVE 'Y' TO SBNM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3570-LIMIT-INTEREST.
           IF SBNT-INTEREST-AMT < 0.00
               MOVE 0.00 TO SBNT-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNT-INTEREST-AMT > 1000.00
               MOVE 1000.00 TO SBNT-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAXABLE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3580-SPLIT-TAXABLE.
           COMPUTE WS-WORK-AMT = SBNM-TAXABLE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = SBNM-TAXABLE-YTD - (WS-WORK-AMT * 2)
           COMPUTE SBNM-ISSUE-PRICE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAXABLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FACE YTD.
      *----------------------------------------------------------------*
       3590-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-FACE-YTD ROUNDED =
                   SBNM-TAXABLE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-FACE-YTD
           END-IF
           ADD SBNM-FACE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3600-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE TAXABLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3610-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO SBNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SBNM-TAXABLE-YTD NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO SBNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REDEEM YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3620-SPLIT-REDEEM.
           COMPUTE WS-WORK-AMT = SBNM-REDEEM-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SBNM-REDEEM-YTD - (WS-WORK-AMT * 4)
           COMPUTE SBNM-ACCRUAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUE PR BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3630-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUE-PR-BAND-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-ISSUE-PR-BAND-LOW(WS-TX)
                   MOVE WS-ISSUE-PR-BAND-RATE(WS-TX) TO SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CS -> HF WHEN NEXT REVIEW DATE IS PAST
      * HF -> JZ WHEN NEXT REVIEW DATE IS PAST
      * JZ -> CS WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3640-SET-STATUS.
           EVALUATE SBNM-STATUS-CD
               WHEN 'CS'
                   IF SBNM-NEXT-REVIEW-DATE > 0 AND
                       SBNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HF' TO SBNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF SBNM-NEXT-REVIEW-DATE > 0 AND
                       SBNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JZ' TO SBNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF SBNM-NEXT-REVIEW-DATE > 0 AND
                       SBNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'CS' TO SBNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-CYCLE.
           IF SBNM-CYCLE-CNT < 9999999
               ADD 1 TO SBNM-CYCLE-CNT
           END-IF
           IF SBNM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO SBNM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * SBNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBNM-REDEEM-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUAL AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3670-LIMIT-ACCRUAL.
           IF SBNT-ACCRUAL-AMT < 10.00
               MOVE 10.00 TO SBNT-ACCRUAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNT-ACCRUAL-AMT > 500.00
               MOVE 500.00 TO SBNT-ACCRUAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REDEEM YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3680-FLAG-REDEEM.
           IF SBNM-REDEEM-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBNM-REDEEM-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TAXABLE AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3690-LIMIT-TAXABLE.
           IF SBNT-TAXABLE-AMT < 0.00
               MOVE 0.00 TO SBNT-TAXABLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNT-TAXABLE-AMT > 2500.00
               MOVE 2500.00 TO SBNT-TAXABLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3700-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO SBNM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SERIES CD:
      * DD -> DF WHEN NOTICE DATE IS PAST
      * DF -> SS WHEN NOTICE DATE IS PAST
      * SS -> VJ WHEN NOTICE DATE IS PAST
      * VJ -> VX WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3710-SET-SERIES.
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
      * STATUS TRANSITIONS FOR SERIES CD:
      * DD -> DF WHEN EFFECTIVE DATE IS PAST
      * DF -> SS WHEN EFFECTIVE DATE IS PAST
      * SS -> VJ WHEN EFFECTIVE DATE IS PAST
      * VJ -> VX WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DF'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SS' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SS'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJ'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3730-CHECK-MATURITY.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO SBNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ACCRUAL AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3740-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNT-TRAN-CD
                   ADD SBNT-ACCRUAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNT-ACCRUAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * E -> J WHEN NEXT REVIEW DATE IS PAST
      * J -> L WHEN NEXT REVIEW DATE IS PAST
      * L -> U WHEN NEXT REVIEW DATE IS PAST
      * U -> E WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-SOURCE.
           EVALUATE SBNM-SOURCE-CD
               WHEN 'E'
                   IF SBNM-NEXT-REVIEW-DATE > 0 AND
                       SBNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'J' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF SBNM-NEXT-REVIEW-DATE > 0 AND
                       SBNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'L' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SBNM-NEXT-REVIEW-DATE > 0 AND
                       SBNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'U' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF SBNM-NEXT-REVIEW-DATE > 0 AND
                       SBNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'E' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3760-AGE-POST.
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
                   ADD SBNM-PURCHASE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SBNM-PURCHASE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SBNM-PURCHASE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBNM-PURCHASE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-PURCHASE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-REASON.
           EVALUATE SBNM-REASON-CD
               WHEN 'DG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCRUAL AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3780-FLAG-ACCRUAL.
           IF SBNT-ACCRUAL-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBNT-ACCRUAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3790-DESCRIBE-SOURCE.
           EVALUATE SBNM-SOURCE-CD
               WHEN 'E'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'J'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'L'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'U'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE ACCRUAL YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3800-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN SBNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SBNM-ACCRUAL-YTD *
                       SBNM-MARGIN / 200
               WHEN SBNT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN SBNT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SBNM-ACCRUAL-YTD *
                       SBNM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE FACE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3810-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SBNM-FACE-AMT NOT < WS-INTEREST-SCHED-LOW(WS-TX)
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
      * DAILY INTEREST ON ACCRUAL YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3820-ACCRUE-ACCRUAL.
           IF SBNM-ACCRUAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-ACCRUAL-YTD * SBNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBNM-TAXABLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REDEEM FEE ON THE WORK AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3830-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN SBNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO SBNT-REDEEM-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE SBNT-REDEEM-AMT ROUNDED = WS-WORK-AMT *
                       SBNM-PENALTY-PCT / 200
               WHEN SBNM-STATUS-CD = 'CS'
                   MOVE 10.00 TO SBNT-REDEEM-AMT
               WHEN SBNM-STATUS-CD = 'HF'
                   MOVE 3.00 TO SBNT-REDEEM-AMT
               WHEN OTHER
                   COMPUTE SBNT-REDEEM-AMT ROUNDED = WS-WORK-AMT *
                       SBNM-PENALTY-PCT / 100
           END-EVALUATE
           IF SBNT-REDEEM-AMT > 0 AND SBNT-REDEEM-AMT < 10.00
               MOVE 10.00 TO SBNT-REDEEM-AMT
           END-IF
           IF SBNT-REDEEM-AMT > 35.00
               MOVE 35.00 TO SBNT-REDEEM-AMT
           END-IF
           ADD SBNT-REDEEM-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE ACCRUAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3840-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO SBNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SBNM-ACCRUAL-AMT NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO SBNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3850-FLAG-PURCHASE.
           IF SBNM-PURCHASE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBNM-PURCHASE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUE PR BAND TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3860-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUE-PR-BAND-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SBNM-INTEREST-AMT NOT < WS-ISSUE-PR-BAND-LOW(WS-TX)
                   MOVE WS-ISSUE-PR-BAND-RATE(WS-TX) TO SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
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
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3880-DESCRIBE-CHANNEL.
           EVALUATE SBNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PURCHASE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3890-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = SBNM-PURCHASE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = SBNM-PURCHASE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE SBNM-ACCRUAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-INTEREST.
           IF SBNM-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-INTEREST-AMT * SBNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBNM-PURCHASE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY ISSUE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3910-ACCUM-ISSUE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-ISSUE-TYPE-CD
                   ADD SBNM-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-ISSUE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3920-AGE-NOTICE.
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
                   ADD SBNM-PURCHASE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNM-PURCHASE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SBNM-PURCHASE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-PURCHASE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-PURCHASE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ISSUE PRICE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3930-FLAG-ISSUE-PRICE.
           IF SBNM-ISSUE-PRICE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBNM-ISSUE-PRICE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3940-COUNT-PAYMENT.
           IF SBNM-PAYMENT-CNT < 9999999
               ADD 1 TO SBNM-PAYMENT-CNT
           END-IF
           IF SBNM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FACE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TAXABLE YTD.
      *----------------------------------------------------------------*
       3950-PRORATE-TAXABLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-TAXABLE-YTD ROUNDED =
                   SBNM-FACE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-TAXABLE-YTD
           END-IF
           ADD SBNM-TAXABLE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BY -> RD WHEN EXPIRY DATE IS PAST
      * RD -> IR WHEN EXPIRY DATE IS PAST
      * IR -> TX WHEN EXPIRY DATE IS PAST
      * TX -> AD WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3960-SET-TRAN.
           EVALUATE SBNT-TRAN-CD
               WHEN 'BY'
                   IF SBNM-EXPIRY-DATE > 0 AND SBNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF SBNM-EXPIRY-DATE > 0 AND SBNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'IR' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'IR'
                   IF SBNM-EXPIRY-DATE > 0 AND SBNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TX'
                   IF SBNM-EXPIRY-DATE > 0 AND SBNM-EXPIRY-DATE <
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
       3970-ACCRUE-ACCRUAL.
           IF SBNT-ACCRUAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNT-ACCRUAL-AMT * SBNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO SBNM-ISSUE-PRICE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3980-AGE-EFF.
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
                   ADD SBNM-ISSUE-PRICE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SBNM-ISSUE-PRICE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SBNM-ISSUE-PRICE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-ISSUE-PRICE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-ISSUE-PRICE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUE PRICE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3990-ACCRUE-ISSUE-PRICE.
           IF SBNM-ISSUE-PRICE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-ISSUE-PRICE-AMT * SBNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO SBNM-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4000-ACCRUE-INTEREST.
           IF SBNM-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-INTEREST-YTD * SBNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBNT-TAXABLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ISSUE PRICE YTD.
      *----------------------------------------------------------------*
       4010-PRORATE-ISSUE-PRICE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-ISSUE-PRICE-YTD ROUNDED =
                   SBNM-PURCHASE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNM-ISSUE-PRICE-YTD
           END-IF
           ADD SBNM-ISSUE-PRICE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E401 NAME REQUIRED
      * E303 REDEEM AMT NEGATIVE
      * E349 REDEEM AMT OVER 1000000
      * E347 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4020-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-NAME = SPACES
                   MOVE 'E401' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-AMT < 0
                   MOVE 'E303' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-AMT > 1000000.00
                   MOVE 'E349' TO WS-REJECT-CODE
               WHEN SBNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E347' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ACCRUAL YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4030-SPLIT-ACCRUAL.
           COMPUTE WS-WORK-AMT = SBNM-ACCRUAL-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SBNM-ACCRUAL-YTD - (WS-WORK-AMT * 4)
           COMPUTE SBNT-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
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
                   MOVE SBFEE-REDEEM-YTD
                     TO SBNM-REDEEM-YTD
                   MOVE SBFEE-AUTO-RENEW-FLAG
                     TO SBNM-AUTO-RENEW-FLAG
                   MOVE SBFEE-NOTICE-SENT-FLAG
                     TO SBNM-NOTICE-SENT-FLAG
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
           DISPLAY 'SBNEXTR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'SBNEXTR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
