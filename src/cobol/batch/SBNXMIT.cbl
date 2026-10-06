      *================================================================*
      * PROGRAM  : SBNXMIT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : SAVINGS BOND SERVICES - OUTBOUND TRANSMISSION. READS*
      *            THE SBN TRANSACTIONS, UPDATES THE SBN MASTER AND    *
      *            PRODUCES THE CONTROL REPORT. US SAVINGS BOND SALES, *
      *            REDEMPTION AND TAX REPORTING FOR CUSTOMERS          *
      * INPUT    : SBNMST (KSDS), SBNTRN, BANKCTL                      *
      * OUTPUT   : SBNOUT, SBNXMRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, SBPEN, SBLIM, SBFEE     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-08-07 SKN  ORIGINAL                                       *
      * 2001-05-22 JLM  REDEEM TABLE EXPANDED CHG2332                  *
      * 2001-02-07 LMB  ISSUE-PRICE NOTICE TEXT PER LEGAL INC0253      *
      * 2002-02-23 TWH  INTEREST ROUNDING - HALF UP INC4540            *
      * 2008-12-28 CRP  ACCRUAL STATUS CODES ADDED CHG9242             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SBNXMIT.
       AUTHOR. M K LOWE.

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
           SELECT REPORT-FILE ASSIGN TO "SBNXMRPT"
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
                                           VALUE 21.000000.
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
      * WS-INTEREST-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000252000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000889500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000489500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000852000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006050000000182000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000746000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006350000000414000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011350000000579500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016350000000126500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026350000000121000'.
       01  WS-INTEREST-TIER-TBL REDEFINES WS-INTEREST-TIER-VALUES.
           05  WS-INTEREST-TIER-ROW OCCURS 10 TIMES.
               10  WS-INTEREST-TIER-LOW    PIC 9(09)V99.
               10  WS-INTEREST-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ISSUE-PR-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ISSUE-PR-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000062500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000565000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000600000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005550000000838000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000256000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015600000000262000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020600000000648000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020850000000408000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021850000000153500'.
       01  WS-ISSUE-PR-SCHED-TBL REDEFINES WS-ISSUE-PR-SCHED-VALUES.
           05  WS-ISSUE-PR-SCHED-ROW OCCURS 9 TIMES.
               10  WS-ISSUE-PR-SCHED-LOW   PIC 9(09)V99.
               10  WS-ISSUE-PR-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INTEREST-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000094500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000826000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000194500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000439000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000400000000571500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005400000000697500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005650000000131000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008150000000678000'.
       01  WS-INTEREST-GRID-TBL REDEFINES WS-INTEREST-GRID-VALUES.
           05  WS-INTEREST-GRID-ROW OCCURS 8 TIMES.
               10  WS-INTEREST-GRID-LOW    PIC 9(09)V99.
               10  WS-INTEREST-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  SBNXMIT  SAVINGS BOND SERVICES'.
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
       COPY SBPENP.
       COPY SBLIMP.
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
               PERFORM 3000-CHECK-EFF
               PERFORM 3010-ACCRUE-INTEREST
               PERFORM 3020-COUNT-RETRY
               PERFORM 3030-EDIT-MEMO
               PERFORM 3040-SELECT-PENALTY-PCT
               PERFORM 3050-SET-BRANCH
               PERFORM 3060-SET-SERIES
               PERFORM 3070-COUNT-PAYMENT
               PERFORM 3080-EDIT-REF-NO
               PERFORM 3090-DESCRIBE-SOURCE
               PERFORM 3100-DESCRIBE-SERIES
               PERFORM 3110-SPLIT-REDEEM
               PERFORM 3120-COUNT-SEQ
               PERFORM 3130-LIMIT-REDEEM
               PERFORM 3140-SELECT-MARGIN
               PERFORM 3150-DESCRIBE-STATUS
               PERFORM 3160-FLAG-ACCRUAL
               PERFORM 3170-AGE-EFF
               PERFORM 3180-AGE-EFF
               PERFORM 3190-AGE-EFF
               PERFORM 3200-FLAG-REDEEM
               PERFORM 3210-EDIT-OPERATOR
               PERFORM 3220-ASSESS-OVERRIDE
               PERFORM 3230-ASSESS-REVIEW
               PERFORM 3240-AGE-NEXT-REVIEW
               PERFORM 3250-EDIT-CUST-ID
               PERFORM 3260-SELECT-FEE-PCT
               PERFORM 3270-PRORATE-ACCRUAL
               PERFORM 3280-PRORATE-ISSUE-PRICE
               PERFORM 3290-PRORATE-TAXABLE
               PERFORM 3300-DESCRIBE-SERIES
               PERFORM 3310-AGE-NEXT-DUE
               PERFORM 3320-SELECT-PENALTY-PCT
               PERFORM 3330-ACCRUE-ACCRUAL
               PERFORM 3340-FLAG-ISSUE-PRICE
               PERFORM 3350-EDIT-NAME
               PERFORM 3360-DESCRIBE-CHANNEL
               PERFORM 3370-SET-TRAN
               PERFORM 3380-SET-SOURCE
               PERFORM 3390-SPLIT-TAXABLE
               PERFORM 3400-SELECT-PENALTY-PCT
               PERFORM 3410-ACCRUE-REDEEM
               PERFORM 3420-ASSESS-NOTICE-SENT
               PERFORM 3430-DESCRIBE-REASON
               PERFORM 3440-DESCRIBE-SERIES
               PERFORM 3450-LIMIT-ISSUE-PRICE
               PERFORM 3460-SPLIT-ACCRUAL
               PERFORM 3470-SPLIT-REDEEM
               PERFORM 3480-SPLIT-REDEEM
               PERFORM 3490-SPLIT-TAXABLE
               PERFORM 3500-PRORATE-ISSUE-PRICE
               PERFORM 3510-SPLIT-TAXABLE
               PERFORM 3520-ASSESS-NOTICE-SENT
               PERFORM 3530-EDIT-ADDR
               PERFORM 3540-FLAG-FACE
               PERFORM 3550-SELECT-PENALTY-PCT
               PERFORM 3560-SELECT-FEE-PCT
               PERFORM 3570-ACCRUE-INTEREST
               PERFORM 3580-LIMIT-ITEM
               PERFORM 3590-AGE-NEXT-DUE
               PERFORM 3600-SET-ISSUE-TYPE
               PERFORM 3610-SPLIT-PURCHASE
               PERFORM 3620-EDIT-DESC
               PERFORM 3630-SET-SERIES
               PERFORM 3640-PRORATE-ACCRUAL
               PERFORM 3650-AGE-LAST-MAINT
               PERFORM 3660-EDIT-CONTACT
               PERFORM 3670-LIMIT-ACCRUAL
               PERFORM 3680-SPLIT-ISSUE-PRICE
               PERFORM 3690-CHECK-LAST-PAY
               PERFORM 3700-CHECK-LAST-MAINT
               PERFORM 3710-ACCUM-ISSUE-TYPE
               PERFORM 3720-ACCUM-STATUS
               PERFORM 3730-COUNT-SEQ
               PERFORM 3740-LIMIT-TAXABLE
               PERFORM 3750-LIMIT-ACCRUAL
               PERFORM 3760-AGE-EFF
               PERFORM 3770-SET-CHANNEL
               PERFORM 3780-ACCUM-STATUS
               PERFORM 3790-AGE-EFFECTIVE
               PERFORM 3800-SET-TRAN
               PERFORM 3810-ACCRUE-PURCHASE
               PERFORM 3820-CHECK-MATURITY
               PERFORM 3830-CHECK-EXPIRY
               PERFORM 3840-SET-BRANCH
               PERFORM 3850-SELECT-PENALTY-PCT
               PERFORM 3860-CHECK-EXPIRY
               PERFORM 3870-SET-SOURCE
               PERFORM 3880-ACCUM-REASON
               PERFORM 3890-SET-SERIES
               PERFORM 3900-SPLIT-TAXABLE
               PERFORM 3910-FLAG-PURCHASE
               PERFORM 3920-SPLIT-TAXABLE
               PERFORM 3930-FLAG-PURCHASE
               PERFORM 3940-SET-CHANNEL
               PERFORM 3950-ASSESS-REVERSAL
               PERFORM 3960-AGE-EXPIRY
               PERFORM 3970-SPLIT-REDEEM
               PERFORM 3980-COUNT-EXCEPT
               PERFORM 3990-EDIT-CUST-ID
               PERFORM 7000-CALL-SBPEN
               PERFORM 7020-CALL-SBLIM
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
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3000-CHECK-EFF.
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
      * DAILY INTEREST ON INTEREST AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-INTEREST.
           IF SBNM-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-INTEREST-AMT * SBNM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBNT-TAXABLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-RETRY.
           IF SBNM-RETRY-CNT < 9999999
               ADD 1 TO SBNM-RETRY-CNT
           END-IF
           IF SBNM-RETRY-CNT NOT < 3
               MOVE 'Y' TO SBNM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E642 MEMO REQUIRED
      * E802 PURCHASE AMT NEGATIVE
      * E630 PURCHASE AMT OVER 50000
      * E571 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3030-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-MEMO = SPACES
                   MOVE 'E642' TO WS-REJECT-CODE
               WHEN SBNM-PURCHASE-AMT < 0
                   MOVE 'E802' TO WS-REJECT-CODE
               WHEN SBNM-PURCHASE-AMT > 50000.00
                   MOVE 'E630' TO WS-REJECT-CODE
               WHEN SBNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E571' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUE PR SCHED TIER FOR THE TAXABLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3040-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUE-PR-SCHED-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SBNM-TAXABLE-YTD NOT < WS-ISSUE-PR-SCHED-LOW(WS-TX)
                   MOVE WS-ISSUE-PR-SCHED-RATE(WS-TX) TO
                       SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * BD -> FC WHEN MATURITY DATE IS PAST
      * FC -> KR WHEN MATURITY DATE IS PAST
      * KR -> MP WHEN MATURITY DATE IS PAST
      * MP -> VJ WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-BRANCH.
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
      * STATUS TRANSITIONS FOR SERIES CD:
      * DD -> DF WHEN MATURITY DATE IS PAST
      * DF -> SS WHEN MATURITY DATE IS PAST
      * SS -> VJ WHEN MATURITY DATE IS PAST
      * VJ -> VX WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3060-SET-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DF'
                   IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SS' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SS'
                   IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJ'
                   IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3070-COUNT-PAYMENT.
           IF SBNM-PAYMENT-CNT < 9999999
               ADD 1 TO SBNM-PAYMENT-CNT
           END-IF
           IF SBNM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E816 REF NO REQUIRED
      * E267 REDEEM AMT NEGATIVE
      * E571 REDEEM AMT OVER 1000000
      * E637 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-REF-NO = SPACES
                   MOVE 'E816' TO WS-REJECT-CODE
               WHEN SBNT-REDEEM-AMT < 0
                   MOVE 'E267' TO WS-REJECT-CODE
               WHEN SBNT-REDEEM-AMT > 1000000.00
                   MOVE 'E571' TO WS-REJECT-CODE
               WHEN SBNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E637' TO WS-REJECT-CODE
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
       3090-DESCRIBE-SOURCE.
           EVALUATE SBNM-SOURCE-CD
               WHEN 'E'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'J'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'L'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SERIES CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3100-DESCRIBE-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'DF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'YH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT REDEEM AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3110-SPLIT-REDEEM.
           COMPUTE WS-WORK-AMT = SBNT-REDEEM-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SBNT-REDEEM-AMT - (WS-WORK-AMT * 3)
           COMPUTE SBNM-ISSUE-PRICE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3120-COUNT-SEQ.
           IF SBNT-SEQ < 9999999
               ADD 1 TO SBNT-SEQ
           END-IF
           IF SBNT-SEQ NOT < 10
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REDEEM AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3130-LIMIT-REDEEM.
           IF SBNM-REDEEM-AMT < 10.00
               MOVE 10.00 TO SBNM-REDEEM-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-REDEEM-AMT > 5000.00
               MOVE 5000.00 TO SBNM-REDEEM-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUE PR SCHED TIER FOR THE ISSUE PRICE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3140-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUE-PR-SCHED-RATE(1) TO SBNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SBNM-ISSUE-PRICE-AMT NOT <
                   WS-ISSUE-PR-SCHED-LOW(WS-TX)
                   MOVE WS-ISSUE-PR-SCHED-RATE(WS-TX) TO SBNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3150-DESCRIBE-STATUS.
           EVALUATE SBNM-STATUS-CD
               WHEN 'CS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCRUAL AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3160-FLAG-ACCRUAL.
           IF SBNT-ACCRUAL-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBNT-ACCRUAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3170-AGE-EFF.
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
                   ADD SBNM-FACE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SBNM-FACE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SBNM-FACE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-FACE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-FACE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3180-AGE-EFF.
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
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3190-AGE-EFF.
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
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REDEEM YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3200-FLAG-REDEEM.
           IF SBNM-REDEEM-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBNM-REDEEM-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E924 OPERATOR REQUIRED
      * E707 FACE AMT NEGATIVE
      * E143 FACE AMT OVER 250000
      * E857 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3210-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-OPERATOR = SPACES
                   MOVE 'E924' TO WS-REJECT-CODE
               WHEN SBNM-FACE-AMT < 0
                   MOVE 'E707' TO WS-REJECT-CODE
               WHEN SBNM-FACE-AMT > 250000.00
                   MOVE 'E143' TO WS-REJECT-CODE
               WHEN SBNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E857' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3220-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SBNM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       SBNM-MARGIN / 200
               WHEN SBNM-SERIES-CD = 'DD'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN SBNM-SERIES-CD = 'DF'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       SBNM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE TAXABLE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3230-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SBNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SBNM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SBNM-INTEREST-YTD ROUNDED = SBNM-TAXABLE-AMT
                       * SBNM-PENALTY-PCT / 200
               WHEN SBNM-ISSUE-TYPE-CD = 'CW'
                   MOVE 7.00 TO SBNM-INTEREST-YTD
               WHEN SBNM-ISSUE-TYPE-CD = 'DY'
                   MOVE 20.00 TO SBNM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE SBNM-INTEREST-YTD ROUNDED = SBNM-TAXABLE-AMT
                       * SBNM-PENALTY-PCT / 100
           END-EVALUATE
           IF SBNM-INTEREST-YTD > 0 AND SBNM-INTEREST-YTD < 5.00
               MOVE 5.00 TO SBNM-INTEREST-YTD
           END-IF
           IF SBNM-INTEREST-YTD > 250.00
               MOVE 250.00 TO SBNM-INTEREST-YTD
           END-IF
           ADD SBNM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3240-AGE-NEXT-REVIEW.
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
                   ADD SBNM-ACCRUAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SBNM-ACCRUAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SBNM-ACCRUAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBNM-ACCRUAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-ACCRUAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E644 CUST ID REQUIRED
      * E124 ISSUE PRICE YTD NEGATIVE
      * E821 ISSUE PRICE YTD OVER 50000
      * E876 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3250-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-CUST-ID = SPACES
                   MOVE 'E644' TO WS-REJECT-CODE
               WHEN SBNM-ISSUE-PRICE-YTD < 0
                   MOVE 'E124' TO WS-REJECT-CODE
               WHEN SBNM-ISSUE-PRICE-YTD > 50000.00
                   MOVE 'E821' TO WS-REJECT-CODE
               WHEN SBNM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E876' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIER TIER FOR THE REDEEM YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3260-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO SBNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SBNM-REDEEM-YTD NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO SBNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ACCRUAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3270-PRORATE-ACCRUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   SBNM-ACCRUAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUE PRICE AMT.
      *----------------------------------------------------------------*
       3280-PRORATE-ISSUE-PRICE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-ISSUE-PRICE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-ISSUE-PRICE-AMT
           END-IF
           ADD SBNM-ISSUE-PRICE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE REDEEM YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TAXABLE YTD.
      *----------------------------------------------------------------*
       3290-PRORATE-TAXABLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-TAXABLE-YTD ROUNDED =
                   SBNM-REDEEM-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNM-TAXABLE-YTD
           END-IF
           ADD SBNM-TAXABLE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE SERIES CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3300-DESCRIBE-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3310-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SBNM-NEXT-DUE-DATE > 0 AND SBNM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIER TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3320-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SBNT-INTEREST-AMT NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ACCRUAL AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3330-ACCRUE-ACCRUAL.
           IF SBNM-ACCRUAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-ACCRUAL-AMT * SBNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ISSUE PRICE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3340-FLAG-ISSUE-PRICE.
           IF SBNM-ISSUE-PRICE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBNM-ISSUE-PRICE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E781 NAME REQUIRED
      * E223 REDEEM YTD NEGATIVE
      * E721 REDEEM YTD OVER 50000
      * E320 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3350-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-NAME = SPACES
                   MOVE 'E781' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-YTD < 0
                   MOVE 'E223' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-YTD > 50000.00
                   MOVE 'E721' TO WS-REJECT-CODE
               WHEN SBNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E320' TO WS-REJECT-CODE
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
       3360-DESCRIBE-CHANNEL.
           EVALUATE SBNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BY -> RD WHEN EXPIRY DATE IS PAST
      * RD -> IR WHEN EXPIRY DATE IS PAST
      * IR -> TX WHEN EXPIRY DATE IS PAST
      * TX -> AD WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3370-SET-TRAN.
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
      * STATUS TRANSITIONS FOR SOURCE CD:
      * E -> J WHEN EXPIRY DATE IS PAST
      * J -> L WHEN EXPIRY DATE IS PAST
      * L -> U WHEN EXPIRY DATE IS PAST
      * U -> E WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3380-SET-SOURCE.
           EVALUATE SBNM-SOURCE-CD
               WHEN 'E'
                   IF SBNM-EXPIRY-DATE > 0 AND SBNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF SBNM-EXPIRY-DATE > 0 AND SBNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SBNM-EXPIRY-DATE > 0 AND SBNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF SBNM-EXPIRY-DATE > 0 AND SBNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT TAXABLE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3390-SPLIT-TAXABLE.
           COMPUTE WS-WORK-AMT = SBNT-TAXABLE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SBNT-TAXABLE-AMT - (WS-WORK-AMT * 3)
           COMPUTE SBNM-PURCHASE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST GRID TIER FOR THE FACE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3400-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-GRID-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SBNM-FACE-AMT NOT < WS-INTEREST-GRID-LOW(WS-TX)
                   MOVE WS-INTEREST-GRID-RATE(WS-TX) TO SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REDEEM AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3410-ACCRUE-REDEEM.
           IF SBNT-REDEEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNT-REDEEM-AMT * SBNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBNM-PURCHASE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE ISSUE PRICE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3420-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SBNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO SBNM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-REVIEW-FLAG = 'Y'
                   COMPUTE SBNM-INTEREST-YTD ROUNDED =
                       SBNM-ISSUE-PRICE-YTD * SBNM-PENALTY-PCT / 200
               WHEN SBNM-SERIES-CD = 'DD'
                   MOVE 15.00 TO SBNM-INTEREST-YTD
               WHEN SBNM-SERIES-CD = 'DF'
                   MOVE 12.00 TO SBNM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE SBNM-INTEREST-YTD ROUNDED =
                       SBNM-ISSUE-PRICE-YTD * SBNM-PENALTY-PCT / 100
           END-EVALUATE
           IF SBNM-INTEREST-YTD > 0 AND SBNM-INTEREST-YTD < 5.00
               MOVE 5.00 TO SBNM-INTEREST-YTD
           END-IF
           IF SBNM-INTEREST-YTD > 25.00
               MOVE 25.00 TO SBNM-INTEREST-YTD
           END-IF
           ADD SBNM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3430-DESCRIBE-REASON.
           EVALUATE SBNM-REASON-CD
               WHEN 'DG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SERIES CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3440-DESCRIBE-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ISSUE PRICE AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3450-LIMIT-ISSUE-PRICE.
           IF SBNM-ISSUE-PRICE-AMT < 25.00
               MOVE 25.00 TO SBNM-ISSUE-PRICE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-ISSUE-PRICE-AMT > 500.00
               MOVE 500.00 TO SBNM-ISSUE-PRICE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ACCRUAL YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3460-SPLIT-ACCRUAL.
           COMPUTE WS-WORK-AMT = SBNM-ACCRUAL-YTD / 2
           COMPUTE WS-WORK-AMT-2 = SBNM-ACCRUAL-YTD - (WS-WORK-AMT * 2)
           COMPUTE SBNM-PURCHASE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REDEEM AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3470-SPLIT-REDEEM.
           COMPUTE WS-WORK-AMT = SBNM-REDEEM-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SBNM-REDEEM-AMT - (WS-WORK-AMT * 2)
           COMPUTE SBNM-PURCHASE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REDEEM YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3480-SPLIT-REDEEM.
           COMPUTE WS-WORK-AMT = SBNM-REDEEM-YTD / 3
           COMPUTE WS-WORK-AMT-2 = SBNM-REDEEM-YTD - (WS-WORK-AMT * 3)
           COMPUTE SBNM-REDEEM-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAXABLE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3490-SPLIT-TAXABLE.
           COMPUTE WS-WORK-AMT = SBNT-TAXABLE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SBNT-TAXABLE-AMT - (WS-WORK-AMT * 3)
           COMPUTE SBNM-TAXABLE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAXABLE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ISSUE PRICE YTD.
      *----------------------------------------------------------------*
       3500-PRORATE-ISSUE-PRICE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-ISSUE-PRICE-YTD ROUNDED =
                   SBNM-TAXABLE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNM-ISSUE-PRICE-YTD
           END-IF
           ADD SBNM-ISSUE-PRICE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT TAXABLE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3510-SPLIT-TAXABLE.
           COMPUTE WS-WORK-AMT = SBNM-TAXABLE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SBNM-TAXABLE-AMT - (WS-WORK-AMT * 4)
           COMPUTE SBNT-ACCRUAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REDEEM FEE ON THE ACCRUAL AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3520-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SBNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO SBNM-REDEEM-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNT-REVERSAL-FLAG = 'Y'
                   COMPUTE SBNM-REDEEM-AMT ROUNDED = SBNM-ACCRUAL-AMT *
                       SBNM-FEE-PCT / 200
               WHEN SBNM-ISSUE-TYPE-CD = 'CW'
                   MOVE 20.00 TO SBNM-REDEEM-AMT
               WHEN SBNM-ISSUE-TYPE-CD = 'DY'
                   MOVE 5.00 TO SBNM-REDEEM-AMT
               WHEN OTHER
                   COMPUTE SBNM-REDEEM-AMT ROUNDED = SBNM-ACCRUAL-AMT *
                       SBNM-FEE-PCT / 100
           END-EVALUATE
           IF SBNM-REDEEM-AMT > 0 AND SBNM-REDEEM-AMT < 1.00
               MOVE 1.00 TO SBNM-REDEEM-AMT
           END-IF
           IF SBNM-REDEEM-AMT > 35.00
               MOVE 35.00 TO SBNM-REDEEM-AMT
           END-IF
           ADD SBNM-REDEEM-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E024 ADDR REQUIRED
      * E897 WORK AMT NEGATIVE
      * E837 WORK AMT OVER 10000
      * E431 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3530-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-ADDR = SPACES
                   MOVE 'E024' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E897' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E837' TO WS-REJECT-CODE
               WHEN SBNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E431' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FACE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3540-FLAG-FACE.
           IF SBNM-FACE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBNM-FACE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIER TIER FOR THE ACCRUAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3550-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SBNT-ACCRUAL-AMT NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST GRID TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3560-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-GRID-RATE(1) TO SBNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SBNM-INTEREST-YTD NOT < WS-INTEREST-GRID-LOW(WS-TX)
                   MOVE WS-INTEREST-GRID-RATE(WS-TX) TO SBNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3570-ACCRUE-INTEREST.
           IF SBNM-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-INTEREST-YTD * SBNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO SBNT-ACCRUAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3580-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SBNM-NEXT-DUE-DATE > 0 AND SBNM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ISSUE TYPE CD:
      * CW -> DY WHEN EFF DATE IS PAST
      * DY -> JA WHEN EFF DATE IS PAST
      * JA -> KT WHEN EFF DATE IS PAST
      * KT -> MF WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3600-SET-ISSUE-TYPE.
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
      * SPLIT PURCHASE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3610-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = SBNM-PURCHASE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = SBNM-PURCHASE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE SBNM-FACE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E344 DESC REQUIRED
      * E690 WORK AMT NEGATIVE
      * E290 WORK AMT OVER 250000
      * E356 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3620-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-DESC = SPACES
                   MOVE 'E344' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E690' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E290' TO WS-REJECT-CODE
               WHEN SBNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E356' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SERIES CD:
      * DD -> DF WHEN MATURITY DATE IS PAST
      * DF -> SS WHEN MATURITY DATE IS PAST
      * SS -> VJ WHEN MATURITY DATE IS PAST
      * VJ -> VX WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DF'
                   IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SS' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SS'
                   IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJ'
                   IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REDEEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ACCRUAL YTD.
      *----------------------------------------------------------------*
       3640-PRORATE-ACCRUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-ACCRUAL-YTD ROUNDED =
                   SBNM-REDEEM-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-ACCRUAL-YTD
           END-IF
           ADD SBNM-ACCRUAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3650-AGE-LAST-MAINT.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E953 CONTACT REQUIRED
      * E027 PURCHASE AMT NEGATIVE
      * E107 PURCHASE AMT OVER 50000
      * E206 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3660-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-CONTACT = SPACES
                   MOVE 'E953' TO WS-REJECT-CODE
               WHEN SBNM-PURCHASE-AMT < 0
                   MOVE 'E027' TO WS-REJECT-CODE
               WHEN SBNM-PURCHASE-AMT > 50000.00
                   MOVE 'E107' TO WS-REJECT-CODE
               WHEN SBNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E206' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUAL AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3670-LIMIT-ACCRUAL.
           IF SBNM-ACCRUAL-AMT < 25.00
               MOVE 25.00 TO SBNM-ACCRUAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-ACCRUAL-AMT > 50000.00
               MOVE 50000.00 TO SBNM-ACCRUAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUE PRICE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3680-SPLIT-ISSUE-PRICE.
           COMPUTE WS-WORK-AMT = SBNM-ISSUE-PRICE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SBNM-ISSUE-PRICE-YTD - (WS-WORK-AMT *
               4)
           COMPUTE SBNM-FACE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3690-CHECK-LAST-PAY.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO SBNM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3700-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO SBNM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAXABLE YTD BY ISSUE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3710-ACCUM-ISSUE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-ISSUE-TYPE-CD
                   ADD SBNM-TAXABLE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-ISSUE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-TAXABLE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REDEEM YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3720-ACCUM-STATUS.
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
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3730-COUNT-SEQ.
           IF SBNT-SEQ < 9999999
               ADD 1 TO SBNT-SEQ
           END-IF
           IF SBNT-SEQ NOT < 6
               MOVE 'Y' TO SBNM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TAXABLE YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3740-LIMIT-TAXABLE.
           IF SBNM-TAXABLE-YTD < 50.00
               MOVE 50.00 TO SBNM-TAXABLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-TAXABLE-YTD > 500.00
               MOVE 500.00 TO SBNM-TAXABLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUAL YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3750-LIMIT-ACCRUAL.
           IF SBNM-ACCRUAL-YTD < 50.00
               MOVE 50.00 TO SBNM-ACCRUAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-ACCRUAL-YTD > 500.00
               MOVE 500.00 TO SBNM-ACCRUAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
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
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST MAINT DATE IS PAST
      * BAT -> MAL WHEN LAST MAINT DATE IS PAST
      * MAL -> TEL WHEN LAST MAINT DATE IS PAST
      * TEL -> ONL WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3770-SET-CHANNEL.
           EVALUATE SBNT-CHANNEL-CD
               WHEN 'BRN'
                   IF SBNM-LAST-MAINT-DATE > 0 AND SBNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF SBNM-LAST-MAINT-DATE > 0 AND SBNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF SBNM-LAST-MAINT-DATE > 0 AND SBNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF SBNM-LAST-MAINT-DATE > 0 AND SBNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE TAXABLE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3780-ACCUM-STATUS.
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
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3790-AGE-EFFECTIVE.
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
                   ADD SBNM-FACE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD SBNM-FACE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SBNM-FACE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-FACE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-FACE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BY -> RD WHEN EFF DATE IS PAST
      * RD -> IR WHEN EFF DATE IS PAST
      * IR -> TX WHEN EFF DATE IS PAST
      * TX -> AD WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3800-SET-TRAN.
           EVALUATE SBNT-TRAN-CD
               WHEN 'BY'
                   IF SBNT-EFF-DATE > 0 AND SBNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF SBNT-EFF-DATE > 0 AND SBNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'IR' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'IR'
                   IF SBNT-EFF-DATE > 0 AND SBNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TX'
                   IF SBNT-EFF-DATE > 0 AND SBNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PURCHASE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3810-ACCRUE-PURCHASE.
           IF SBNM-PURCHASE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-PURCHASE-AMT * SBNM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3820-CHECK-MATURITY.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO SBNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3830-CHECK-EXPIRY.
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
               MOVE 'Y' TO SBNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * BD -> FC WHEN MATURITY DATE IS PAST
      * FC -> KR WHEN MATURITY DATE IS PAST
      * KR -> MP WHEN MATURITY DATE IS PAST
      * MP -> VJ WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3840-SET-BRANCH.
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
      * SELECT THE INTEREST TIER TIER FOR THE PURCHASE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3850-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SBNM-PURCHASE-YTD NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3860-CHECK-EXPIRY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO SBNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * E -> J WHEN NOTICE DATE IS PAST
      * J -> L WHEN NOTICE DATE IS PAST
      * L -> U WHEN NOTICE DATE IS PAST
      * U -> E WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3870-SET-SOURCE.
           EVALUATE SBNM-SOURCE-CD
               WHEN 'E'
                   IF SBNM-NOTICE-DATE > 0 AND SBNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF SBNM-NOTICE-DATE > 0 AND SBNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SBNM-NOTICE-DATE > 0 AND SBNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF SBNM-NOTICE-DATE > 0 AND SBNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO SBNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3880-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-REASON-CD
                   ADD SBNM-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SERIES CD:
      * DD -> DF WHEN LAST PAY DATE IS PAST
      * DF -> SS WHEN LAST PAY DATE IS PAST
      * SS -> VJ WHEN LAST PAY DATE IS PAST
      * VJ -> VX WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3890-SET-SERIES.
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
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3900-SPLIT-TAXABLE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE SBNT-TAXABLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3910-FLAG-PURCHASE.
           IF SBNM-PURCHASE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBNM-PURCHASE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAXABLE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3920-SPLIT-TAXABLE.
           COMPUTE WS-WORK-AMT = SBNM-TAXABLE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = SBNM-TAXABLE-YTD - (WS-WORK-AMT * 12)
           COMPUTE SBNM-REDEEM-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3930-FLAG-PURCHASE.
           IF SBNM-PURCHASE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBNM-PURCHASE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3940-SET-CHANNEL.
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
      * ASSESS THE WORK FEE ON THE TAXABLE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3950-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN SBNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = SBNM-TAXABLE-AMT *
                       SBNM-MARGIN / 200
               WHEN SBNT-TRAN-CD = 'BY'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN SBNT-TRAN-CD = 'RD'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = SBNM-TAXABLE-AMT *
                       SBNM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3960-AGE-EXPIRY.
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
                   ADD SBNM-REDEEM-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD SBNM-REDEEM-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SBNM-REDEEM-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-REDEEM-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-REDEEM-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT REDEEM YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3970-SPLIT-REDEEM.
           COMPUTE WS-WORK-AMT = SBNM-REDEEM-YTD / 2
           COMPUTE WS-WORK-AMT-2 = SBNM-REDEEM-YTD - (WS-WORK-AMT * 2)
           COMPUTE SBNT-ACCRUAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-EXCEPT.
           IF SBNM-EXCEPT-CNT < 9999999
               ADD 1 TO SBNM-EXCEPT-CNT
           END-IF
           IF SBNM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO SBNM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E777 CUST ID REQUIRED
      * E422 PURCHASE AMT NEGATIVE
      * E958 PURCHASE AMT OVER 10000
      * E404 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3990-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-CUST-ID = SPACES
                   MOVE 'E777' TO WS-REJECT-CODE
               WHEN SBNM-PURCHASE-AMT < 0
                   MOVE 'E422' TO WS-REJECT-CODE
               WHEN SBNM-PURCHASE-AMT > 10000.00
                   MOVE 'E958' TO WS-REJECT-CODE
               WHEN SBNM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E404' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

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
                   MOVE SBPEN-PURCHASE-AMT
                     TO SBNM-PURCHASE-AMT
                   MOVE SBPEN-HOLD-FLAG
                     TO SBNM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-SBPEN' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
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
                   MOVE SBLIM-ISSUE-PRICE-YTD
                     TO SBNM-ISSUE-PRICE-YTD
                   MOVE SBLIM-FACE-YTD
                     TO SBNM-FACE-YTD
                   MOVE SBLIM-TAXABLE-AMT
                     TO SBNM-TAXABLE-AMT
                   MOVE SBLIM-PURCHASE-AMT
                     TO SBNM-PURCHASE-AMT
                   MOVE SBLIM-TAXABLE-YTD
                     TO SBNM-TAXABLE-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-SBLIM' TO ABND-PARAGRAPH
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
                   MOVE SBFEE-FACE-YTD
                     TO SBNM-FACE-YTD
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
           DISPLAY 'SBNXMIT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'SBNXMIT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
