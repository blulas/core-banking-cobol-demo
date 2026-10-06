      *================================================================*
      * PROGRAM  : SDBSTMT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : SAFE DEPOSIT BOXES - STATEMENT PREPARATION. READS   *
      *            THE SDB TRANSACTIONS, UPDATES THE SDB MASTER AND    *
      *            PRODUCES THE CONTROL REPORT. SAFE DEPOSIT BOX RENTAL*
      *            BILLING, ACCESS LOG AND DRILLING                    *
      * INPUT    : SDBMST (KSDS), SDBTRN, BANKCTL                      *
      * OUTPUT   : SDBOUT, SDBSTRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, SDFEE, SDRISK, SDWAIV   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-04-16 JLM  ORIGINAL                                       *
      * 1993-08-10 PQR  PERFORMANCE - PREPAID READ AHEAD REQ5863       *
      * 1996-12-14 GAF  KEY-FEE RERUN PROTECTION REQ0939               *
      * 1998-05-17 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-06-08 TWH  AUDIT FINDING - RENT CONTROL TOTALS INC2714    *
      * 2000-09-25 SKN  KEY-FEE TABLE EXPANDED CHG2945                 *
      * 2001-06-02 JLM  NEW DRILL REPORT COLUMNS INC0484               *
      * 2001-10-04 MKL  PREPAID STATUS CODES ADDED PRB0313             *
      * 2003-01-14 BHS  DRILL FEE SCHEDULE UPDATE CHG8624              *
      * 2003-08-02 BHS  LATE-FEE CALCULATION CORRECTED CHG4303         *
      * 2006-11-26 MKL  PREPAID ROUNDING - HALF UP CHG7705             *
      * 2008-06-26 CRP  LATE-FEE RERUN PROTECTION INC2679              *
      * 2009-01-05 JLM  KEY-FEE CUTOFF MOVED REQ1545                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SDBSTMT.
       AUTHOR. C R PATEL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "SDBMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS SDBM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "SDBTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "SDBOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "SDBSTRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY SDBMST.
       FD  TRAN-FILE.
       COPY SDBTRN.
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
                                           VALUE 14.000000.
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
      * WS-KEY-FEE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-KEY-FEE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000647000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000077000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000287500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000527000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021500000000806000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024000000000536000'.
       01  WS-KEY-FEE-SCHED-TBL REDEFINES WS-KEY-FEE-SCHED-VALUES.
           05  WS-KEY-FEE-SCHED-ROW OCCURS 6 TIMES.
               10  WS-KEY-FEE-SCHED-LOW    PIC 9(09)V99.
               10  WS-KEY-FEE-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-LATE-FEE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-LATE-FEE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000839500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000127500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000441000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000419500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000106000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000166500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020550000000312000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021050000000345500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021100000000341500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022100000000811500'.
       01  WS-LATE-FEE-TIER-TBL REDEFINES WS-LATE-FEE-TIER-VALUES.
           05  WS-LATE-FEE-TIER-ROW OCCURS 10 TIMES.
               10  WS-LATE-FEE-TIER-LOW    PIC 9(09)V99.
               10  WS-LATE-FEE-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-LATE-FEE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-LATE-FEE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000482000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000199000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000748500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006050000000523000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000376500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007100000000021000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008100000000878000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008350000000313500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008450000000582000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008500000000822000'.
       01  WS-LATE-FEE-GRID-TBL REDEFINES WS-LATE-FEE-GRID-VALUES.
           05  WS-LATE-FEE-GRID-ROW OCCURS 10 TIMES.
               10  WS-LATE-FEE-GRID-LOW    PIC 9(09)V99.
               10  WS-LATE-FEE-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  SDBSTMT  SAFE DEPOSIT BOXES'.
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
       COPY SDFEEP.
       COPY SDRISKP.
       COPY SDWAIVP.

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
               MOVE 'SDBMST' TO ABND-FILE-NAME
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
           MOVE SDBT-KEY TO SDBM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ACCUM-TRAN
               PERFORM 3010-ACCUM-VAULT
               PERFORM 3020-CHECK-NEXT-REVIEW
               PERFORM 3030-EDIT-CONTACT
               PERFORM 3040-SET-VAULT
               PERFORM 3050-SET-BRANCH
               PERFORM 3060-LIMIT-DRILL
               PERFORM 3070-CHECK-LAST-ACT
               PERFORM 3080-CHECK-NEXT-DUE
               PERFORM 3090-SET-CYCLE
               PERFORM 3100-DESCRIBE-REASON
               PERFORM 3110-FLAG-PREPAID
               PERFORM 3120-PRORATE-RENT
               PERFORM 3130-CHECK-OPEN
               PERFORM 3140-ASSESS-SUSPEND
               PERFORM 3150-LIMIT-ITEM
               PERFORM 3160-CHECK-NOTICE
               PERFORM 3170-AGE-NEXT-DUE
               PERFORM 3180-SPLIT-DRILL
               PERFORM 3190-AGE-POST
               PERFORM 3200-SPLIT-KEY-FEE
               PERFORM 3210-PRORATE-PREPAID
               PERFORM 3220-PRORATE-LATE-FEE
               PERFORM 3230-EDIT-REF-NO
               PERFORM 3240-EDIT-DESC
               PERFORM 3250-COUNT-SEQ
               PERFORM 3260-PRORATE-DEPOSIT
               PERFORM 3270-DESCRIBE-BRANCH
               PERFORM 3280-ACCUM-BRANCH
               PERFORM 3290-SELECT-FEE-PCT
               PERFORM 3300-FLAG-KEY-FEE
               PERFORM 3310-PRORATE-RENT
               PERFORM 3320-SELECT-DISCOUNT
               PERFORM 3330-SELECT-DISCOUNT
               PERFORM 3340-CHECK-NEXT-DUE
               PERFORM 3350-COUNT-SEQ
               PERFORM 3360-FLAG-DEPOSIT
               PERFORM 3370-ACCUM-VAULT
               PERFORM 3380-SELECT-FEE-PCT
               PERFORM 3390-SPLIT-RENT
               PERFORM 3400-EDIT-OPERATOR
               PERFORM 3410-EDIT-OPERATOR
               PERFORM 3420-CHECK-POST
               PERFORM 3430-COUNT-PAYMENT
               PERFORM 3440-ASSESS-OVERRIDE
               PERFORM 3450-AGE-LAST-ACT
               PERFORM 3460-SELECT-PENALTY-PCT
               PERFORM 3470-FLAG-KEY-FEE
               PERFORM 3480-COUNT-SEQ
               PERFORM 3490-ASSESS-SUSPEND
               PERFORM 3500-AGE-LAST-STMT
               PERFORM 3510-COUNT-PAYMENT
               PERFORM 3520-CHECK-LAST-ACT
               PERFORM 3530-SET-BRANCH
               PERFORM 3540-EDIT-CUST-ID
               PERFORM 3550-ASSESS-OVERRIDE
               PERFORM 3560-SELECT-PENALTY-PCT
               PERFORM 3570-CHECK-LAST-MAINT
               PERFORM 3580-SPLIT-DEPOSIT
               PERFORM 3590-ACCRUE-DRILL
               PERFORM 3600-DESCRIBE-TRAN
               PERFORM 3610-FLAG-RENT
               PERFORM 3620-PRORATE-KEY-FEE
               PERFORM 3630-EDIT-REJECT
               PERFORM 3640-ACCRUE-DRILL
               PERFORM 3650-ACCRUE-RENT
               PERFORM 3660-SPLIT-KEY-FEE
               PERFORM 3670-SET-BRANCH
               PERFORM 3680-SELECT-FEE-PCT
               PERFORM 3690-PRORATE-DEPOSIT
               PERFORM 3700-AGE-LAST-ACT
               PERFORM 3710-ASSESS-REVIEW
               PERFORM 3720-DESCRIBE-CHANNEL
               PERFORM 3730-SET-VAULT
               PERFORM 3740-LIMIT-RENT
               PERFORM 3750-SELECT-FEE-PCT
               PERFORM 3760-SELECT-FEE-PCT
               PERFORM 3770-AGE-POST
               PERFORM 3780-SET-CYCLE
               PERFORM 3790-LIMIT-DRILL
               PERFORM 3800-ASSESS-REVERSAL
               PERFORM 3810-FLAG-DRILL
               PERFORM 3820-DESCRIBE-CYCLE
               PERFORM 3830-DESCRIBE-SOURCE
               PERFORM 3840-SELECT-FEE-PCT
               PERFORM 3850-PRORATE-RENT
               PERFORM 3860-FLAG-DRILL
               PERFORM 3870-FLAG-ITEM
               PERFORM 3880-LIMIT-DEPOSIT
               PERFORM 3890-SPLIT-DEPOSIT
               PERFORM 3900-ACCRUE-DRILL
               PERFORM 3910-COUNT-ITEM
               PERFORM 3920-ASSESS-AUTO-RENEW
               PERFORM 3930-FLAG-DRILL
               PERFORM 3940-CHECK-LAST-STMT
               PERFORM 3950-EDIT-NAME
               PERFORM 3960-CHECK-EFF
               PERFORM 3970-PRORATE-LATE-FEE
               PERFORM 3980-SET-TRAN
               PERFORM 3990-ACCUM-CHANNEL
               PERFORM 4000-DESCRIBE-REASON
               PERFORM 4010-CHECK-NEXT-DUE
               PERFORM 7030-CALL-SDFEE
               PERFORM 7010-CALL-SDRISK
               PERFORM 7040-CALL-SDWAIV
               REWRITE SDB-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'SDBMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM SDB-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3000-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBT-TRAN-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY VAULT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3010-ACCUM-VAULT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-VAULT-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-VAULT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3020-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO SDBM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E854 CONTACT REQUIRED
      * E514 DRILL AMT NEGATIVE
      * E609 DRILL AMT OVER 5000
      * E865 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3030-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-CONTACT = SPACES
                   MOVE 'E854' TO WS-REJECT-CODE
               WHEN SDBM-DRILL-AMT < 0
                   MOVE 'E514' TO WS-REJECT-CODE
               WHEN SDBM-DRILL-AMT > 5000.00
                   MOVE 'E609' TO WS-REJECT-CODE
               WHEN SDBM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E865' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR VAULT CD:
      * BU -> ET WHEN NOTICE DATE IS PAST
      * ET -> FF WHEN NOTICE DATE IS PAST
      * FF -> NG WHEN NOTICE DATE IS PAST
      * NG -> NH WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3040-SET-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ET' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ET'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FF' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FF'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NG' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NG'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NH' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN MATURITY DATE IS PAST
      * FJW -> YFB WHEN MATURITY DATE IS PAST
      * YFB -> EXS WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   IF SDBM-MATURITY-DATE > 0 AND SDBM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJW' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJW'
                   IF SDBM-MATURITY-DATE > 0 AND SDBM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YFB' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YFB'
                   IF SDBM-MATURITY-DATE > 0 AND SDBM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EXS' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DRILL AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3060-LIMIT-DRILL.
           IF SDBM-DRILL-AMT < 25.00
               MOVE 25.00 TO SDBM-DRILL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-DRILL-AMT > 50000.00
               MOVE 50000.00 TO SDBM-DRILL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3070-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3080-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO SDBM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * AP -> DA WHEN NEXT REVIEW DATE IS PAST
      * DA -> HV WHEN NEXT REVIEW DATE IS PAST
      * HV -> SE WHEN NEXT REVIEW DATE IS PAST
      * SE -> AP WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3090-SET-CYCLE.
           EVALUATE SDBM-CYCLE-CD
               WHEN 'AP'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DA' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DA'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HV' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HV'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'SE' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SE'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'AP' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3100-DESCRIBE-REASON.
           EVALUATE SDBM-REASON-CD
               WHEN 'J'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'M'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PREPAID AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3110-FLAG-PREPAID.
           IF SDBM-PREPAID-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDBM-PREPAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RENT AMT.
      *----------------------------------------------------------------*
       3120-PRORATE-RENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBT-RENT-AMT ROUNDED =
                   SDBM-RENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBT-RENT-AMT
           END-IF
           ADD SDBT-RENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SDBM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO SDBM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DRILL FEE ON THE WORK AMT 2.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3140-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN SDBM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO SDBM-DRILL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   COMPUTE SDBM-DRILL-AMT ROUNDED = WS-WORK-AMT-2 *
                       SDBM-DISCOUNT / 200
               WHEN SDBM-SOURCE-CD = 'DD'
                   MOVE 5.00 TO SDBM-DRILL-AMT
               WHEN SDBM-SOURCE-CD = 'JZ'
                   MOVE 10.00 TO SDBM-DRILL-AMT
               WHEN OTHER
                   COMPUTE SDBM-DRILL-AMT ROUNDED = WS-WORK-AMT-2 *
                       SDBM-DISCOUNT / 100
           END-EVALUATE
           IF SDBM-DRILL-AMT > 0 AND SDBM-DRILL-AMT < 2.00
               MOVE 2.00 TO SDBM-DRILL-AMT
           END-IF
           IF SDBM-DRILL-AMT > 250.00
               MOVE 250.00 TO SDBM-DRILL-AMT
           END-IF
           ADD SDBM-DRILL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3150-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3160-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3170-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DRILL AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3180-SPLIT-DRILL.
           COMPUTE WS-WORK-AMT = SDBM-DRILL-AMT / 12
           COMPUTE WS-WORK-AMT-2 = SDBM-DRILL-AMT - (WS-WORK-AMT * 12)
           COMPUTE SDBT-RENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3190-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF SDBT-POST-DATE > 0 AND SDBT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT KEY FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3200-SPLIT-KEY-FEE.
           COMPUTE WS-WORK-AMT = SDBT-KEY-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SDBT-KEY-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE SDBM-RENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PREPAID AMT.
      *----------------------------------------------------------------*
       3210-PRORATE-PREPAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-PREPAID-AMT ROUNDED =
                   SDBM-LATE-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-PREPAID-AMT
           END-IF
           ADD SDBM-PREPAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE RENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LATE FEE YTD.
      *----------------------------------------------------------------*
       3220-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-LATE-FEE-YTD ROUNDED =
                   SDBM-RENT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-LATE-FEE-YTD
           END-IF
           ADD SDBM-LATE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E424 REF NO REQUIRED
      * E685 DRILL YTD NEGATIVE
      * E837 DRILL YTD OVER 250000
      * E338 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3230-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-REF-NO = SPACES
                   MOVE 'E424' TO WS-REJECT-CODE
               WHEN SDBM-DRILL-YTD < 0
                   MOVE 'E685' TO WS-REJECT-CODE
               WHEN SDBM-DRILL-YTD > 250000.00
                   MOVE 'E837' TO WS-REJECT-CODE
               WHEN SDBM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E338' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E706 DESC REQUIRED
      * E299 KEY FEE YTD NEGATIVE
      * E377 KEY FEE YTD OVER 99999
      * E130 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-DESC = SPACES
                   MOVE 'E706' TO WS-REJECT-CODE
               WHEN SDBM-KEY-FEE-YTD < 0
                   MOVE 'E299' TO WS-REJECT-CODE
               WHEN SDBM-KEY-FEE-YTD > 99999.00
                   MOVE 'E377' TO WS-REJECT-CODE
               WHEN SDBM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E130' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3250-COUNT-SEQ.
           IF SDBT-SEQ < 9999999
               ADD 1 TO SDBT-SEQ
           END-IF
           IF SDBT-SEQ NOT < 12
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEPOSIT AMT.
      *----------------------------------------------------------------*
       3260-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-DEPOSIT-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-DEPOSIT-AMT
           END-IF
           ADD SDBM-DEPOSIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3270-DESCRIBE-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FJW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YFB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE KEY FEE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3280-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BRANCH-CD
                   ADD SDBM-KEY-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-KEY-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LATE FEE TIER TIER FOR THE RENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LATE-FEE-TIER-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SDBM-RENT-YTD NOT < WS-LATE-FEE-TIER-LOW(WS-TX)
                   MOVE WS-LATE-FEE-TIER-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * KEY FEE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3300-FLAG-KEY-FEE.
           IF SDBM-KEY-FEE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-KEY-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRILL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RENT YTD.
      *----------------------------------------------------------------*
       3310-PRORATE-RENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-RENT-YTD ROUNDED =
                   SDBM-DRILL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-RENT-YTD
           END-IF
           ADD SDBM-RENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE KEY FEE SCHED TIER FOR THE PREPAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3320-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-KEY-FEE-SCHED-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF SDBM-PREPAID-YTD NOT < WS-KEY-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-KEY-FEE-SCHED-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE KEY FEE SCHED TIER FOR THE PREPAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-KEY-FEE-SCHED-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF SDBM-PREPAID-AMT NOT < WS-KEY-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-KEY-FEE-SCHED-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3350-COUNT-SEQ.
           IF SDBT-SEQ < 9999999
               ADD 1 TO SDBT-SEQ
           END-IF
           IF SDBT-SEQ NOT < 12
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3360-FLAG-DEPOSIT.
           IF SDBT-DEPOSIT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDBT-DEPOSIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE KEY FEE AMT BY VAULT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3370-ACCUM-VAULT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-VAULT-CD
                   ADD SDBT-KEY-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-VAULT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBT-KEY-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LATE FEE GRID TIER FOR THE PREPAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3380-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LATE-FEE-GRID-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SDBM-PREPAID-YTD NOT < WS-LATE-FEE-GRID-LOW(WS-TX)
                   MOVE WS-LATE-FEE-GRID-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3390-SPLIT-RENT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE SDBM-RENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E927 OPERATOR REQUIRED
      * E750 RENT AMT NEGATIVE
      * E384 RENT AMT OVER 99999
      * E134 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3400-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-OPERATOR = SPACES
                   MOVE 'E927' TO WS-REJECT-CODE
               WHEN SDBM-RENT-AMT < 0
                   MOVE 'E750' TO WS-REJECT-CODE
               WHEN SDBM-RENT-AMT > 99999.00
                   MOVE 'E384' TO WS-REJECT-CODE
               WHEN SDBM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E134' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E666 OPERATOR REQUIRED
      * E735 LATE FEE YTD NEGATIVE
      * E321 LATE FEE YTD OVER 50000
      * E145 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3410-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-OPERATOR = SPACES
                   MOVE 'E666' TO WS-REJECT-CODE
               WHEN SDBM-LATE-FEE-YTD < 0
                   MOVE 'E735' TO WS-REJECT-CODE
               WHEN SDBM-LATE-FEE-YTD > 50000.00
                   MOVE 'E321' TO WS-REJECT-CODE
               WHEN SDBM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E145' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3420-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF SDBT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3430-COUNT-PAYMENT.
           IF SDBM-PAYMENT-CNT < 9999999
               ADD 1 TO SDBM-PAYMENT-CNT
           END-IF
           IF SDBM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO SDBM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PREPAID YTD FEE ON THE KEY FEE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3440-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SDBM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SDBM-PREPAID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SDBM-PREPAID-YTD ROUNDED = SDBM-KEY-FEE-AMT *
                       SDBM-DISCOUNT / 200
               WHEN SDBM-REASON-CD = 'J'
                   MOVE 5.00 TO SDBM-PREPAID-YTD
               WHEN SDBM-REASON-CD = 'K'
                   MOVE 5.00 TO SDBM-PREPAID-YTD
               WHEN OTHER
                   COMPUTE SDBM-PREPAID-YTD ROUNDED = SDBM-KEY-FEE-AMT *
                       SDBM-DISCOUNT / 100
           END-EVALUATE
           IF SDBM-PREPAID-YTD > 0 AND SDBM-PREPAID-YTD < 5.00
               MOVE 5.00 TO SDBM-PREPAID-YTD
           END-IF
           IF SDBM-PREPAID-YTD > 25.00
               MOVE 25.00 TO SDBM-PREPAID-YTD
           END-IF
           ADD SDBM-PREPAID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3450-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE KEY FEE SCHED TIER FOR THE RENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3460-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-KEY-FEE-SCHED-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF SDBT-RENT-AMT NOT < WS-KEY-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-KEY-FEE-SCHED-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * KEY FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3470-FLAG-KEY-FEE.
           IF SDBM-KEY-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDBM-KEY-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3480-COUNT-SEQ.
           IF SDBT-SEQ < 9999999
               ADD 1 TO SDBT-SEQ
           END-IF
           IF SDBT-SEQ NOT < 10
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DRILL YTD FEE ON THE KEY FEE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3490-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN SDBM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO SDBM-DRILL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE SDBM-DRILL-YTD ROUNDED = SDBT-KEY-FEE-AMT *
                       SDBM-PENALTY-PCT / 200
               WHEN SDBM-SOURCE-CD = 'DD'
                   MOVE 12.00 TO SDBM-DRILL-YTD
               WHEN SDBM-SOURCE-CD = 'JZ'
                   MOVE 10.00 TO SDBM-DRILL-YTD
               WHEN OTHER
                   COMPUTE SDBM-DRILL-YTD ROUNDED = SDBT-KEY-FEE-AMT *
                       SDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF SDBM-DRILL-YTD > 0 AND SDBM-DRILL-YTD < 1.00
               MOVE 1.00 TO SDBM-DRILL-YTD
           END-IF
           IF SDBM-DRILL-YTD > 100.00
               MOVE 100.00 TO SDBM-DRILL-YTD
           END-IF
           ADD SDBM-DRILL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3500-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-LATE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDBM-LATE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD SDBM-LATE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-LATE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-LATE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-PAYMENT.
           IF SDBM-PAYMENT-CNT < 9999999
               ADD 1 TO SDBM-PAYMENT-CNT
           END-IF
           IF SDBM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3520-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN LAST ACT DATE IS PAST
      * FJW -> YFB WHEN LAST ACT DATE IS PAST
      * YFB -> EXS WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3530-SET-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJW' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJW'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YFB' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YFB'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EXS' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E073 CUST ID REQUIRED
      * E852 LATE FEE YTD NEGATIVE
      * E168 LATE FEE YTD OVER 5000
      * E091 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3540-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-CUST-ID = SPACES
                   MOVE 'E073' TO WS-REJECT-CODE
               WHEN SDBM-LATE-FEE-YTD < 0
                   MOVE 'E852' TO WS-REJECT-CODE
               WHEN SDBM-LATE-FEE-YTD > 5000.00
                   MOVE 'E168' TO WS-REJECT-CODE
               WHEN SDBM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E091' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DRILL FEE ON THE RENT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3550-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SDBM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SDBT-DRILL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBT-REVERSAL-FLAG = 'Y'
                   COMPUTE SDBT-DRILL-AMT ROUNDED = SDBM-RENT-AMT *
                       SDBM-DISCOUNT / 200
               WHEN SDBM-BOX-SIZE-CD = 'CU'
                   MOVE 15.00 TO SDBT-DRILL-AMT
               WHEN SDBM-BOX-SIZE-CD = 'MG'
                   MOVE 10.00 TO SDBT-DRILL-AMT
               WHEN OTHER
                   COMPUTE SDBT-DRILL-AMT ROUNDED = SDBM-RENT-AMT *
                       SDBM-DISCOUNT / 100
           END-EVALUATE
           IF SDBT-DRILL-AMT > 0 AND SDBT-DRILL-AMT < 2.00
               MOVE 2.00 TO SDBT-DRILL-AMT
           END-IF
           IF SDBT-DRILL-AMT > 75.00
               MOVE 75.00 TO SDBT-DRILL-AMT
           END-IF
           ADD SDBT-DRILL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE KEY FEE SCHED TIER FOR THE KEY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3560-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-KEY-FEE-SCHED-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF SDBM-KEY-FEE-YTD NOT < WS-KEY-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-KEY-FEE-SCHED-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3570-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3580-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = SDBM-DEPOSIT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = SDBM-DEPOSIT-YTD - (WS-WORK-AMT * 2)
           COMPUTE SDBM-DRILL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3590-ACCRUE-DRILL.
           IF SDBM-DRILL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-DRILL-AMT * SDBM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDBT-DRILL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3600-DESCRIBE-TRAN.
           EVALUATE SDBT-TRAN-CD
               WHEN 'RN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'AC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'CL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RENT YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3610-FLAG-RENT.
           IF SDBM-RENT-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-RENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO KEY FEE AMT.
      *----------------------------------------------------------------*
       3620-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-KEY-FEE-AMT ROUNDED =
                   SDBT-RENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-KEY-FEE-AMT
           END-IF
           ADD SDBM-KEY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E175 REJECT CD REQUIRED
      * E979 PREPAID YTD NEGATIVE
      * E002 PREPAID YTD OVER 10000
      * E070 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3630-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-REJECT-CD = SPACES
                   MOVE 'E175' TO WS-REJECT-CODE
               WHEN SDBM-PREPAID-YTD < 0
                   MOVE 'E979' TO WS-REJECT-CODE
               WHEN SDBM-PREPAID-YTD > 10000.00
                   MOVE 'E002' TO WS-REJECT-CODE
               WHEN SDBM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E070' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3640-ACCRUE-DRILL.
           IF SDBT-DRILL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBT-DRILL-AMT * SDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-PREPAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RENT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3650-ACCRUE-RENT.
           IF SDBM-RENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-RENT-AMT * SDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-PREPAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3660-SPLIT-KEY-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE SDBM-KEY-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN LAST ACT DATE IS PAST
      * FJW -> YFB WHEN LAST ACT DATE IS PAST
      * YFB -> EXS WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3670-SET-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJW' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJW'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YFB' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YFB'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EXS' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE LATE FEE GRID TIER FOR THE LATE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3680-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LATE-FEE-GRID-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SDBM-LATE-FEE-YTD NOT < WS-LATE-FEE-GRID-LOW(WS-TX)
                   MOVE WS-LATE-FEE-GRID-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRILL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEPOSIT AMT.
      *----------------------------------------------------------------*
       3690-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-DEPOSIT-AMT ROUNDED =
                   SDBT-DRILL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-DEPOSIT-AMT
           END-IF
           ADD SDBM-DEPOSIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT FEE ON THE PREPAID YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3710-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SDBM-DEPOSIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBT-REVERSAL-FLAG = 'Y'
                   COMPUTE SDBM-DEPOSIT-AMT ROUNDED = SDBM-PREPAID-YTD *
                       SDBM-PENALTY-PCT / 200
               WHEN SDBM-BOX-SIZE-CD = 'CU'
                   MOVE 5.00 TO SDBM-DEPOSIT-AMT
               WHEN SDBM-BOX-SIZE-CD = 'MG'
                   MOVE 10.00 TO SDBM-DEPOSIT-AMT
               WHEN OTHER
                   COMPUTE SDBM-DEPOSIT-AMT ROUNDED = SDBM-PREPAID-YTD *
                       SDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF SDBM-DEPOSIT-AMT > 0 AND SDBM-DEPOSIT-AMT < 10.00
               MOVE 10.00 TO SDBM-DEPOSIT-AMT
           END-IF
           IF SDBM-DEPOSIT-AMT > 35.00
               MOVE 35.00 TO SDBM-DEPOSIT-AMT
           END-IF
           ADD SDBM-DEPOSIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3720-DESCRIBE-CHANNEL.
           EVALUATE SDBT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR VAULT CD:
      * BU -> ET WHEN LAST ACT DATE IS PAST
      * ET -> FF WHEN LAST ACT DATE IS PAST
      * FF -> NG WHEN LAST ACT DATE IS PAST
      * NG -> NH WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3730-SET-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ET' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ET'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FF' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FF'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NG' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NG'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NH' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RENT AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3740-LIMIT-RENT.
           IF SDBM-RENT-AMT < 100.00
               MOVE 100.00 TO SDBM-RENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-RENT-AMT > 2500.00
               MOVE 2500.00 TO SDBM-RENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LATE FEE TIER TIER FOR THE KEY FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3750-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LATE-FEE-TIER-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SDBM-KEY-FEE-AMT NOT < WS-LATE-FEE-TIER-LOW(WS-TX)
                   MOVE WS-LATE-FEE-TIER-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LATE FEE TIER TIER FOR THE RENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3760-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LATE-FEE-TIER-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SDBM-RENT-AMT NOT < WS-LATE-FEE-TIER-LOW(WS-TX)
                   MOVE WS-LATE-FEE-TIER-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3770-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF SDBT-POST-DATE > 0 AND SDBT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * AP -> DA WHEN CLOSE DATE IS PAST
      * DA -> HV WHEN CLOSE DATE IS PAST
      * HV -> SE WHEN CLOSE DATE IS PAST
      * SE -> AP WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3780-SET-CYCLE.
           EVALUATE SDBM-CYCLE-CD
               WHEN 'AP'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DA' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DA'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HV' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HV'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SE' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SE'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AP' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DRILL AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3790-LIMIT-DRILL.
           IF SDBM-DRILL-AMT < 0.00
               MOVE 0.00 TO SDBM-DRILL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-DRILL-AMT > 1000.00
               MOVE 1000.00 TO SDBM-DRILL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DRILL FEE ON THE PREPAID AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3800-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN SDBT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO SDBM-DRILL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-OVERRIDE-FLAG = 'Y'
                   COMPUTE SDBM-DRILL-AMT ROUNDED = SDBM-PREPAID-AMT *
                       SDBM-PENALTY-PCT / 200
               WHEN SDBM-REASON-CD = 'J'
                   MOVE 7.00 TO SDBM-DRILL-AMT
               WHEN SDBM-REASON-CD = 'K'
                   MOVE 10.00 TO SDBM-DRILL-AMT
               WHEN OTHER
                   COMPUTE SDBM-DRILL-AMT ROUNDED = SDBM-PREPAID-AMT *
                       SDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF SDBM-DRILL-AMT > 0 AND SDBM-DRILL-AMT < 5.00
               MOVE 5.00 TO SDBM-DRILL-AMT
           END-IF
           IF SDBM-DRILL-AMT > 35.00
               MOVE 35.00 TO SDBM-DRILL-AMT
           END-IF
           ADD SDBM-DRILL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DRILL AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3810-FLAG-DRILL.
           IF SDBM-DRILL-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDBM-DRILL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3820-DESCRIBE-CYCLE.
           EVALUATE SDBM-CYCLE-CD
               WHEN 'AP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'SE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3830-DESCRIBE-SOURCE.
           EVALUATE SDBM-SOURCE-CD
               WHEN 'DD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE KEY FEE SCHED TIER FOR THE LATE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3840-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-KEY-FEE-SCHED-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF SDBM-LATE-FEE-AMT NOT < WS-KEY-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-KEY-FEE-SCHED-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PREPAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RENT YTD.
      *----------------------------------------------------------------*
       3850-PRORATE-RENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-RENT-YTD ROUNDED =
                   SDBM-PREPAID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-RENT-YTD
           END-IF
           ADD SDBM-RENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DRILL AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3860-FLAG-DRILL.
           IF SDBT-DRILL-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBT-DRILL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3870-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3880-LIMIT-DEPOSIT.
           IF SDBT-DEPOSIT-AMT < 0.00
               MOVE 0.00 TO SDBT-DEPOSIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBT-DEPOSIT-AMT > 2500.00
               MOVE 2500.00 TO SDBT-DEPOSIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3890-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = SDBM-DEPOSIT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SDBM-DEPOSIT-AMT - (WS-WORK-AMT * 4)
           COMPUTE SDBM-PREPAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-DRILL.
           IF SDBM-DRILL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-DRILL-AMT * SDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-RENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3910-COUNT-ITEM.
           IF SDBM-ITEM-CNT < 9999999
               ADD 1 TO SDBM-ITEM-CNT
           END-IF
           IF SDBM-ITEM-CNT NOT < 12
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DRILL YTD FEE ON THE DEPOSIT YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3920-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDBM-DRILL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   COMPUTE SDBM-DRILL-YTD ROUNDED = SDBM-DEPOSIT-YTD *
                       SDBM-FEE-PCT / 200
               WHEN SDBT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO SDBM-DRILL-YTD
               WHEN SDBT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO SDBM-DRILL-YTD
               WHEN OTHER
                   COMPUTE SDBM-DRILL-YTD ROUNDED = SDBM-DEPOSIT-YTD *
                       SDBM-FEE-PCT / 100
           END-EVALUATE
           IF SDBM-DRILL-YTD > 0 AND SDBM-DRILL-YTD < 2.00
               MOVE 2.00 TO SDBM-DRILL-YTD
           END-IF
           IF SDBM-DRILL-YTD > 50.00
               MOVE 50.00 TO SDBM-DRILL-YTD
           END-IF
           ADD SDBM-DRILL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DRILL YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3930-FLAG-DRILL.
           IF SDBM-DRILL-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-DRILL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3940-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E177 NAME REQUIRED
      * E631 DEPOSIT AMT NEGATIVE
      * E083 DEPOSIT AMT OVER 10000
      * E496 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3950-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-NAME = SPACES
                   MOVE 'E177' TO WS-REJECT-CODE
               WHEN SDBM-DEPOSIT-AMT < 0
                   MOVE 'E631' TO WS-REJECT-CODE
               WHEN SDBM-DEPOSIT-AMT > 10000.00
                   MOVE 'E083' TO WS-REJECT-CODE
               WHEN SDBM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E496' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3960-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF SDBT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRILL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LATE FEE YTD.
      *----------------------------------------------------------------*
       3970-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-LATE-FEE-YTD ROUNDED =
                   SDBM-DRILL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-LATE-FEE-YTD
           END-IF
           ADD SDBM-LATE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * RN -> AC WHEN EFF DATE IS PAST
      * AC -> DR WHEN EFF DATE IS PAST
      * DR -> KY WHEN EFF DATE IS PAST
      * KY -> CL WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3980-SET-TRAN.
           EVALUATE SDBT-TRAN-CD
               WHEN 'RN'
                   IF SDBT-EFF-DATE > 0 AND SDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AC' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AC'
                   IF SDBT-EFF-DATE > 0 AND SDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DR' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DR'
                   IF SDBT-EFF-DATE > 0 AND SDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KY' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KY'
                   IF SDBT-EFF-DATE > 0 AND SDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CL' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DRILL AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3990-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBT-CHANNEL-CD
                   ADD SDBT-DRILL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBT-DRILL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4000-DESCRIBE-REASON.
           EVALUATE SDBM-REASON-CD
               WHEN 'J'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'K'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'M'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'S'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'W'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4010-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE SDB FIELDS TO SDFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-SDFEE.
           INITIALIZE SDFEE-PARMS
           MOVE SDBM-PREPAID-AMT
             TO SDFEE-PREPAID-AMT
           MOVE SDBM-PREPAID-YTD
             TO SDFEE-PREPAID-YTD
           MOVE SDBM-KEY-FEE-AMT
             TO SDFEE-KEY-FEE-AMT
           MOVE SDBM-DEPOSIT-YTD
             TO SDFEE-DEPOSIT-YTD
           MOVE SDBM-RENT-YTD
             TO SDFEE-RENT-YTD
           MOVE SDBM-DRILL-YTD
             TO SDFEE-DRILL-YTD
           MOVE SDBM-LAST-MAINT-DATE
             TO SDFEE-LAST-MAINT-DATE
           MOVE SDBM-LAST-ACT-DATE
             TO SDFEE-LAST-ACT-DATE
           MOVE SDBM-MATURITY-DATE
             TO SDFEE-MATURITY-DATE
           MOVE SDBM-OPEN-DATE
             TO SDFEE-OPEN-DATE
           MOVE SDBM-SOURCE-CD
             TO SDFEE-SOURCE-CD
           MOVE SDBM-BRANCH-CD
             TO SDFEE-BRANCH-CD
           MOVE SDBM-BOX-SIZE-CD
             TO SDFEE-BOX-SIZE-CD
           MOVE SDBM-VAULT-CD
             TO SDFEE-VAULT-CD
           MOVE SDBM-SUSPEND-FLAG
             TO SDFEE-SUSPEND-FLAG
           MOVE SDBM-AUTO-RENEW-FLAG
             TO SDFEE-AUTO-RENEW-FLAG
           MOVE SDBM-EXEMPT-FLAG
             TO SDFEE-EXEMPT-FLAG
           MOVE SDBM-RETRY-CNT
             TO SDFEE-RETRY-CNT
           MOVE SDBM-ITEM-CNT
             TO SDFEE-ITEM-CNT
           MOVE SDBM-DISCOUNT
             TO SDFEE-DISCOUNT
           MOVE SDBM-PENALTY-PCT
             TO SDFEE-PENALTY-PCT
           MOVE SDBM-CONTACT
             TO SDFEE-CONTACT
           MOVE SDBM-CUST-ID
             TO SDFEE-CUST-ID
           CALL 'SDFEE' USING SDFEE-PARMS
           EVALUATE SDFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SDFEE-PREPAID-AMT
                     TO SDBM-PREPAID-AMT
                   MOVE SDFEE-KEY-FEE-AMT
                     TO SDBM-KEY-FEE-AMT
                   MOVE SDFEE-DEPOSIT-YTD
                     TO SDBM-DEPOSIT-YTD
                   MOVE SDFEE-RENT-YTD
                     TO SDBM-RENT-YTD
                   MOVE SDFEE-DRILL-YTD
                     TO SDBM-DRILL-YTD
                   MOVE SDFEE-AUTO-RENEW-FLAG
                     TO SDBM-AUTO-RENEW-FLAG
                   MOVE SDFEE-EXEMPT-FLAG
                     TO SDBM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-SDFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE SDB FIELDS TO SDRISK AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-SDRISK.
           INITIALIZE SDRISK-PARMS
           MOVE SDBM-LATE-FEE-YTD
             TO SDRISK-LATE-FEE-YTD
           MOVE SDBM-KEY-FEE-YTD
             TO SDRISK-KEY-FEE-YTD
           MOVE SDBM-DEPOSIT-AMT
             TO SDRISK-DEPOSIT-AMT
           MOVE SDBM-DEPOSIT-YTD
             TO SDRISK-DEPOSIT-YTD
           MOVE SDBM-LATE-FEE-AMT
             TO SDRISK-LATE-FEE-AMT
           MOVE SDBM-KEY-FEE-AMT
             TO SDRISK-KEY-FEE-AMT
           MOVE SDBM-NEXT-DUE-DATE
             TO SDRISK-NEXT-DUE-DATE
           MOVE SDBM-OPEN-DATE
             TO SDRISK-OPEN-DATE
           MOVE SDBM-NEXT-REVIEW-DATE
             TO SDRISK-NEXT-REVIEW-DATE
           MOVE SDBM-LAST-MAINT-DATE
             TO SDRISK-LAST-MAINT-DATE
           MOVE SDBM-REASON-CD
             TO SDRISK-REASON-CD
           MOVE SDBM-CYCLE-CD
             TO SDRISK-CYCLE-CD
           MOVE SDBM-BOX-SIZE-CD
             TO SDRISK-BOX-SIZE-CD
           MOVE SDBM-VAULT-CD
             TO SDRISK-VAULT-CD
           MOVE SDBM-AUTO-RENEW-FLAG
             TO SDRISK-AUTO-RENEW-FLAG
           MOVE SDBM-OVERRIDE-FLAG
             TO SDRISK-OVERRIDE-FLAG
           MOVE SDBM-PREFERRED-FLAG
             TO SDRISK-PREFERRED-FLAG
           MOVE SDBM-ITEM-CNT
             TO SDRISK-ITEM-CNT
           MOVE SDBM-PAYMENT-CNT
             TO SDRISK-PAYMENT-CNT
           MOVE SDBM-FEE-PCT
             TO SDRISK-FEE-PCT
           MOVE SDBM-PENALTY-PCT
             TO SDRISK-PENALTY-PCT
           MOVE SDBM-DESC
             TO SDRISK-DESC
           MOVE SDBM-CUST-ID
             TO SDRISK-CUST-ID
           CALL 'SDRISK' USING SDRISK-PARMS
           EVALUATE SDRISK-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SDRISK-KEY-FEE-YTD
                     TO SDBM-KEY-FEE-YTD
                   MOVE SDRISK-DEPOSIT-AMT
                     TO SDBM-DEPOSIT-AMT
                   MOVE SDRISK-DEPOSIT-YTD
                     TO SDBM-DEPOSIT-YTD
                   MOVE SDRISK-LATE-FEE-AMT
                     TO SDBM-LATE-FEE-AMT
                   MOVE SDRISK-KEY-FEE-AMT
                     TO SDBM-KEY-FEE-AMT
                   MOVE SDRISK-OVERRIDE-FLAG
                     TO SDBM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-SDRISK' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE SDB FIELDS TO SDWAIV AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-SDWAIV.
           INITIALIZE SDWAIV-PARMS
           MOVE SDBM-DEPOSIT-YTD
             TO SDWAIV-DEPOSIT-YTD
           MOVE SDBM-DRILL-AMT
             TO SDWAIV-DRILL-AMT
           MOVE SDBM-RENT-YTD
             TO SDWAIV-RENT-YTD
           MOVE SDBM-LATE-FEE-AMT
             TO SDWAIV-LATE-FEE-AMT
           MOVE SDBM-DRILL-YTD
             TO SDWAIV-DRILL-YTD
           MOVE SDBM-KEY-FEE-YTD
             TO SDWAIV-KEY-FEE-YTD
           MOVE SDBM-NEXT-DUE-DATE
             TO SDWAIV-NEXT-DUE-DATE
           MOVE SDBM-OPEN-DATE
             TO SDWAIV-OPEN-DATE
           MOVE SDBM-MATURITY-DATE
             TO SDWAIV-MATURITY-DATE
           MOVE SDBM-CLOSE-DATE
             TO SDWAIV-CLOSE-DATE
           MOVE SDBM-VAULT-CD
             TO SDWAIV-VAULT-CD
           MOVE SDBM-SOURCE-CD
             TO SDWAIV-SOURCE-CD
           MOVE SDBM-BOX-SIZE-CD
             TO SDWAIV-BOX-SIZE-CD
           MOVE SDBM-BRANCH-CD
             TO SDWAIV-BRANCH-CD
           MOVE SDBM-SUSPEND-FLAG
             TO SDWAIV-SUSPEND-FLAG
           MOVE SDBM-REVIEW-FLAG
             TO SDWAIV-REVIEW-FLAG
           MOVE SDBM-OVERRIDE-FLAG
             TO SDWAIV-OVERRIDE-FLAG
           MOVE SDBM-PAYMENT-CNT
             TO SDWAIV-PAYMENT-CNT
           MOVE SDBM-NOTICE-CNT
             TO SDWAIV-NOTICE-CNT
           MOVE SDBM-PENALTY-PCT
             TO SDWAIV-PENALTY-PCT
           MOVE SDBM-FEE-PCT
             TO SDWAIV-FEE-PCT
           MOVE SDBM-CUST-ID
             TO SDWAIV-CUST-ID
           MOVE SDBM-NAME
             TO SDWAIV-NAME
           CALL 'SDWAIV' USING SDWAIV-PARMS
           EVALUATE SDWAIV-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SDWAIV-KEY-FEE-YTD
                     TO SDBM-KEY-FEE-YTD
                   MOVE SDWAIV-SUSPEND-FLAG
                     TO SDBM-SUSPEND-FLAG
                   MOVE SDWAIV-OVERRIDE-FLAG
                     TO SDBM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RAIV' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-SDWAIV' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE SDBT-KEY TO RD-KEY
           MOVE SDBT-TRAN-CD TO RD-CODE
           MOVE SDBT-KEY-FEE-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE SDBT-KEY TO RD-KEY
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
           DISPLAY 'SDBSTMT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'SDBSTMT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
