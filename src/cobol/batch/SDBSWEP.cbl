      *================================================================*
      * PROGRAM  : SDBSWEP                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : SAFE DEPOSIT BOXES - SWEEP. READS THE SDB           *
      *            TRANSACTIONS, UPDATES THE SDB MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. SAFE DEPOSIT BOX RENTAL BILLING,*
      *            ACCESS LOG AND DRILLING                             *
      * INPUT    : SDBMST (KSDS), SDBTRN, BANKCTL                      *
      * OUTPUT   : SDBOUT, SDBSWRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, SDRISK, SDWAIV, SDLIM   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-10-25 DPS  ORIGINAL                                       *
      * 1994-08-04 PQR  LATE-FEE RERUN PROTECTION REQ0685              *
      * 1996-04-11 SKN  LATE-FEE RERUN PROTECTION PRB4381              *
      * 2001-01-28 EWS  RENT EXCEPTIONS TO OPS QUEUE CHG5254           *
      * 2001-10-14 RJT  PERFORMANCE - DRILL READ AHEAD INC6148         *
      * 2002-01-19 LMB  PREPAID LIMIT REVISED INC8231                  *
      * 2003-05-24 JLM  AUDIT FINDING - PREPAID CONTROL TOTALS REQ3083 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SDBSWEP.
       AUTHOR. T W HOLT.

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
           SELECT REPORT-FILE ASSIGN TO "SDBSWRPT"
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
                                           VALUE 25.000000.
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
      * WS-RENT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RENT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000894500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000792000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000199000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000764000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000682500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006050000000222000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006300000000705500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006800000000796500'.
       01  WS-RENT-BAND-TBL REDEFINES WS-RENT-BAND-VALUES.
           05  WS-RENT-BAND-ROW OCCURS 8 TIMES.
               10  WS-RENT-BAND-LOW        PIC 9(09)V99.
               10  WS-RENT-BAND-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DEPOSIT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEPOSIT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000017000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000551000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000063000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000249000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023000000000078000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023250000000872500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024250000000557500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024500000000449000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024550000000850000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025550000000091000'.
       01  WS-DEPOSIT-TIER-TBL REDEFINES WS-DEPOSIT-TIER-VALUES.
           05  WS-DEPOSIT-TIER-ROW OCCURS 10 TIMES.
               10  WS-DEPOSIT-TIER-LOW     PIC 9(09)V99.
               10  WS-DEPOSIT-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PREPAID-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PREPAID-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000851500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000137500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000543500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000282000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000812500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010850000000722000'.
       01  WS-PREPAID-TIER-TBL REDEFINES WS-PREPAID-TIER-VALUES.
           05  WS-PREPAID-TIER-ROW OCCURS 6 TIMES.
               10  WS-PREPAID-TIER-LOW     PIC 9(09)V99.
               10  WS-PREPAID-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RENT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RENT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000111000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000756000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000130000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000052000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000120000'.
       01  WS-RENT-GRID-TBL REDEFINES WS-RENT-GRID-VALUES.
           05  WS-RENT-GRID-ROW OCCURS 5 TIMES.
               10  WS-RENT-GRID-LOW        PIC 9(09)V99.
               10  WS-RENT-GRID-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  SDBSWEP  SAFE DEPOSIT BOXES'.
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
       COPY SDRISKP.
       COPY SDWAIVP.
       COPY SDLIMP.

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
               PERFORM 3000-ACCRUE-RENT
               PERFORM 3010-SET-BRANCH
               PERFORM 3020-ASSESS-OVERRIDE
               PERFORM 3030-COUNT-ITEM
               PERFORM 3040-CHECK-NOTICE
               PERFORM 3050-ACCUM-VAULT
               PERFORM 3060-SELECT-FEE-PCT
               PERFORM 3070-LIMIT-RENT
               PERFORM 3080-EDIT-CUST-ID
               PERFORM 3090-SET-BOX-SIZE
               PERFORM 3100-EDIT-CONTACT
               PERFORM 3110-AGE-NOTICE
               PERFORM 3120-ACCRUE-DEPOSIT
               PERFORM 3130-EDIT-REF-NO
               PERFORM 3140-COUNT-NOTICE
               PERFORM 3150-CHECK-POST
               PERFORM 3160-ASSESS-REVIEW
               PERFORM 3170-SELECT-PENALTY-PCT
               PERFORM 3180-SPLIT-LATE-FEE
               PERFORM 3190-SPLIT-KEY-FEE
               PERFORM 3200-ACCUM-BOX-SIZE
               PERFORM 3210-PRORATE-KEY-FEE
               PERFORM 3220-ASSESS-REVIEW
               PERFORM 3230-DESCRIBE-BRANCH
               PERFORM 3240-SET-BRANCH
               PERFORM 3250-FLAG-DEPOSIT
               PERFORM 3260-FLAG-LATE-FEE
               PERFORM 3270-PRORATE-KEY-FEE
               PERFORM 3280-COUNT-ITEM
               PERFORM 3290-AGE-NEXT-DUE
               PERFORM 3300-ACCUM-BRANCH
               PERFORM 3310-FLAG-DEPOSIT
               PERFORM 3320-EDIT-CUST-ID
               PERFORM 3330-COUNT-SEQ
               PERFORM 3340-LIMIT-PREPAID
               PERFORM 3350-ACCUM-REASON
               PERFORM 3360-ACCUM-TRAN
               PERFORM 3370-SELECT-DISCOUNT
               PERFORM 3380-SELECT-DISCOUNT
               PERFORM 3390-ASSESS-AUTO-RENEW
               PERFORM 3400-DESCRIBE-BOX-SIZE
               PERFORM 3410-SPLIT-DEPOSIT
               PERFORM 3420-CHECK-NOTICE
               PERFORM 3430-ACCRUE-LATE-FEE
               PERFORM 3440-CHECK-OPEN
               PERFORM 3450-FLAG-RENT
               PERFORM 3460-AGE-NEXT-DUE
               PERFORM 3470-ACCUM-TRAN
               PERFORM 3480-ACCRUE-RENT
               PERFORM 3490-ACCRUE-FEE-PCT
               PERFORM 3500-ACCRUE-KEY-FEE
               PERFORM 3510-EDIT-CUST-ID
               PERFORM 3520-FLAG-KEY-FEE
               PERFORM 3530-ACCRUE-KEY-FEE
               PERFORM 3540-ACCUM-BOX-SIZE
               PERFORM 3550-PRORATE-LATE-FEE
               PERFORM 3560-AGE-LAST-ACT
               PERFORM 3570-DESCRIBE-VAULT
               PERFORM 3580-SELECT-DISCOUNT
               PERFORM 3590-ACCRUE-RENT
               PERFORM 3600-CHECK-POST
               PERFORM 3610-PRORATE-PREPAID
               PERFORM 3620-CHECK-EFF
               PERFORM 3630-SELECT-FEE-PCT
               PERFORM 3640-ACCUM-BRANCH
               PERFORM 3650-SET-VAULT
               PERFORM 3660-SELECT-PENALTY-PCT
               PERFORM 3670-COUNT-NOTICE
               PERFORM 3680-CHECK-EFF
               PERFORM 3690-ACCRUE-PREPAID
               PERFORM 3700-FLAG-DRILL
               PERFORM 3710-SPLIT-DRILL
               PERFORM 3720-PRORATE-DEPOSIT
               PERFORM 3730-SPLIT-KEY-FEE
               PERFORM 3740-EDIT-ADDR
               PERFORM 3750-SELECT-PENALTY-PCT
               PERFORM 3760-ACCRUE-KEY-FEE
               PERFORM 3770-ASSESS-SUSPEND
               PERFORM 3780-PRORATE-PREPAID
               PERFORM 3790-CHECK-LAST-MAINT
               PERFORM 3800-FLAG-DEPOSIT
               PERFORM 3810-ACCRUE-KEY-FEE
               PERFORM 3820-LIMIT-RENT
               PERFORM 3830-ACCUM-BRANCH
               PERFORM 3840-CHECK-LAST-MAINT
               PERFORM 3850-COUNT-NOTICE
               PERFORM 3860-AGE-EFF
               PERFORM 3870-SELECT-PENALTY-PCT
               PERFORM 3880-ACCUM-BOX-SIZE
               PERFORM 3890-AGE-CLOSE
               PERFORM 3900-PRORATE-DEPOSIT
               PERFORM 3910-ACCRUE-DRILL
               PERFORM 3920-DESCRIBE-SOURCE
               PERFORM 3930-ACCRUE-LATE-FEE
               PERFORM 3940-FLAG-ITEM
               PERFORM 3950-CHECK-NOTICE
               PERFORM 3960-ACCRUE-KEY-FEE
               PERFORM 3970-ACCUM-VAULT
               PERFORM 3980-ACCRUE-RENT
               PERFORM 3990-ACCUM-CHANNEL
               PERFORM 4000-CHECK-LAST-STMT
               PERFORM 4010-CHECK-EFF
               PERFORM 4020-CHECK-NEXT-REVIEW
               PERFORM 4030-COUNT-NOTICE
               PERFORM 4040-CHECK-CLOSE
               PERFORM 4050-SPLIT-PREPAID
               PERFORM 4060-COUNT-ITEM
               PERFORM 4070-SPLIT-RENT
               PERFORM 4080-DESCRIBE-REASON
               PERFORM 4090-PRORATE-RENT
               PERFORM 4100-LIMIT-KEY-FEE
               PERFORM 4110-DESCRIBE-VAULT
               PERFORM 4120-AGE-NEXT-DUE
               PERFORM 4130-DESCRIBE-CYCLE
               PERFORM 4140-PRORATE-DEPOSIT
               PERFORM 4150-FLAG-PREPAID
               PERFORM 4160-FLAG-KEY-FEE
               PERFORM 4170-COUNT-PAYMENT
               PERFORM 4180-EDIT-DESC
               PERFORM 4190-SET-BRANCH
               PERFORM 4200-SET-TRAN
               PERFORM 4210-FLAG-LATE-FEE
               PERFORM 4220-SELECT-DISCOUNT
               PERFORM 4230-ACCUM-REASON
               PERFORM 4240-LIMIT-PREPAID
               PERFORM 4250-ACCRUE-DRILL
               PERFORM 4260-ACCRUE-RENT
               PERFORM 4270-SELECT-PENALTY-PCT
               PERFORM 4280-CHECK-NEXT-DUE
               PERFORM 4290-SPLIT-LATE-FEE
               PERFORM 4300-SET-TRAN
               PERFORM 4310-COUNT-SEQ
               PERFORM 4320-COUNT-SEQ
               PERFORM 4330-SELECT-PENALTY-PCT
               PERFORM 4340-ASSESS-SUSPEND
               PERFORM 4350-CHECK-EFF
               PERFORM 4360-AGE-MATURITY
               PERFORM 4370-ACCRUE-DEPOSIT
               PERFORM 4380-PRORATE-PREPAID
               PERFORM 4390-EDIT-CONTACT
               PERFORM 4400-PRORATE-PREPAID
               PERFORM 4410-AGE-EFF
               PERFORM 4420-ACCRUE-KEY-FEE
               PERFORM 7010-CALL-SDRISK
               PERFORM 7040-CALL-SDWAIV
               PERFORM 7020-CALL-SDLIM
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
      * DAILY INTEREST ON RENT AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3000-ACCRUE-RENT.
           IF SDBM-RENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-RENT-AMT * SDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBT-KEY-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN MATURITY DATE IS PAST
      * FJW -> YFB WHEN MATURITY DATE IS PAST
      * YFB -> EXS WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3010-SET-BRANCH.
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
      * ASSESS THE KEY FEE FEE ON THE LATE FEE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3020-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SDBM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SDBT-KEY-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   COMPUTE SDBT-KEY-FEE-AMT ROUNDED = SDBM-LATE-FEE-AMT
                       * SDBM-PENALTY-PCT / 200
               WHEN SDBM-VAULT-CD = 'BU'
                   MOVE 7.00 TO SDBT-KEY-FEE-AMT
               WHEN SDBM-VAULT-CD = 'ET'
                   MOVE 10.00 TO SDBT-KEY-FEE-AMT
               WHEN OTHER
                   COMPUTE SDBT-KEY-FEE-AMT ROUNDED = SDBM-LATE-FEE-AMT
                       * SDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF SDBT-KEY-FEE-AMT > 0 AND SDBT-KEY-FEE-AMT < 2.00
               MOVE 2.00 TO SDBT-KEY-FEE-AMT
           END-IF
           IF SDBT-KEY-FEE-AMT > 25.00
               MOVE 25.00 TO SDBT-KEY-FEE-AMT
           END-IF
           ADD SDBT-KEY-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3030-COUNT-ITEM.
           IF SDBM-ITEM-CNT < 9999999
               ADD 1 TO SDBM-ITEM-CNT
           END-IF
           IF SDBM-ITEM-CNT NOT < 10
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3040-CHECK-NOTICE.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY VAULT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3050-ACCUM-VAULT.
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
      * SELECT THE RENT BAND TIER FOR THE PREPAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3060-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RENT-BAND-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SDBM-PREPAID-AMT NOT < WS-RENT-BAND-LOW(WS-TX)
                   MOVE WS-RENT-BAND-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RENT AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3070-LIMIT-RENT.
           IF SDBM-RENT-AMT < 10.00
               MOVE 10.00 TO SDBM-RENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-RENT-AMT > 10000.00
               MOVE 10000.00 TO SDBM-RENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E058 CUST ID REQUIRED
      * E904 RENT YTD NEGATIVE
      * E483 RENT YTD OVER 25000
      * E597 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-CUST-ID = SPACES
                   MOVE 'E058' TO WS-REJECT-CODE
               WHEN SDBM-RENT-YTD < 0
                   MOVE 'E904' TO WS-REJECT-CODE
               WHEN SDBM-RENT-YTD > 25000.00
                   MOVE 'E483' TO WS-REJECT-CODE
               WHEN SDBM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E597' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOX SIZE CD:
      * CU -> MG WHEN NEXT DUE DATE IS PAST
      * MG -> VG WHEN NEXT DUE DATE IS PAST
      * VG -> VN WHEN NEXT DUE DATE IS PAST
      * VN -> XJ WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3090-SET-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MG'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VG'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VN' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VN'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E507 CONTACT REQUIRED
      * E571 KEY FEE YTD NEGATIVE
      * E799 KEY FEE YTD OVER 10000
      * E853 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3100-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-CONTACT = SPACES
                   MOVE 'E507' TO WS-REJECT-CODE
               WHEN SDBM-KEY-FEE-YTD < 0
                   MOVE 'E571' TO WS-REJECT-CODE
               WHEN SDBM-KEY-FEE-YTD > 10000.00
                   MOVE 'E799' TO WS-REJECT-CODE
               WHEN SDBM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E853' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3110-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEPOSIT YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3120-ACCRUE-DEPOSIT.
           IF SDBM-DEPOSIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-DEPOSIT-YTD * SDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-LATE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E224 REF NO REQUIRED
      * E571 DRILL YTD NEGATIVE
      * E749 DRILL YTD OVER 25000
      * E737 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3130-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-REF-NO = SPACES
                   MOVE 'E224' TO WS-REJECT-CODE
               WHEN SDBM-DRILL-YTD < 0
                   MOVE 'E571' TO WS-REJECT-CODE
               WHEN SDBM-DRILL-YTD > 25000.00
                   MOVE 'E749' TO WS-REJECT-CODE
               WHEN SDBM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E737' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3140-COUNT-NOTICE.
           IF SDBM-NOTICE-CNT < 9999999
               ADD 1 TO SDBM-NOTICE-CNT
           END-IF
           IF SDBM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO SDBM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-POST.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PREPAID YTD FEE ON THE DEPOSIT AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3160-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SDBM-PREPAID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-OVERRIDE-FLAG = 'Y'
                   COMPUTE SDBM-PREPAID-YTD ROUNDED = SDBM-DEPOSIT-AMT *
                       SDBM-DISCOUNT / 200
               WHEN SDBM-SOURCE-CD = 'DD'
                   MOVE 15.00 TO SDBM-PREPAID-YTD
               WHEN SDBM-SOURCE-CD = 'JZ'
                   MOVE 12.00 TO SDBM-PREPAID-YTD
               WHEN OTHER
                   COMPUTE SDBM-PREPAID-YTD ROUNDED = SDBM-DEPOSIT-AMT *
                       SDBM-DISCOUNT / 100
           END-EVALUATE
           IF SDBM-PREPAID-YTD > 0 AND SDBM-PREPAID-YTD < 2.00
               MOVE 2.00 TO SDBM-PREPAID-YTD
           END-IF
           IF SDBM-PREPAID-YTD > 75.00
               MOVE 75.00 TO SDBM-PREPAID-YTD
           END-IF
           ADD SDBM-PREPAID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE RENT GRID TIER FOR THE PREPAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3170-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RENT-GRID-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SDBM-PREPAID-AMT NOT < WS-RENT-GRID-LOW(WS-TX)
                   MOVE WS-RENT-GRID-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3180-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE SDBM-LATE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT KEY FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3190-SPLIT-KEY-FEE.
           COMPUTE WS-WORK-AMT = SDBT-KEY-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SDBT-KEY-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE SDBM-KEY-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DRILL YTD BY BOX SIZE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3200-ACCUM-BOX-SIZE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BOX-SIZE-CD
                   ADD SDBM-DRILL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BOX-SIZE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-DRILL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO KEY FEE YTD.
      *----------------------------------------------------------------*
       3210-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-KEY-FEE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-KEY-FEE-YTD
           END-IF
           ADD SDBM-KEY-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT FEE ON THE DRILL YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3220-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SDBM-DEPOSIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBT-REVERSAL-FLAG = 'Y'
                   COMPUTE SDBM-DEPOSIT-AMT ROUNDED = SDBM-DRILL-YTD *
                       SDBM-PENALTY-PCT / 200
               WHEN SDBM-BRANCH-CD = 'EXS'
                   MOVE 15.00 TO SDBM-DEPOSIT-AMT
               WHEN SDBM-BRANCH-CD = 'FJW'
                   MOVE 20.00 TO SDBM-DEPOSIT-AMT
               WHEN OTHER
                   COMPUTE SDBM-DEPOSIT-AMT ROUNDED = SDBM-DRILL-YTD *
                       SDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF SDBM-DEPOSIT-AMT > 0 AND SDBM-DEPOSIT-AMT < 5.00
               MOVE 5.00 TO SDBM-DEPOSIT-AMT
           END-IF
           IF SDBM-DEPOSIT-AMT > 75.00
               MOVE 75.00 TO SDBM-DEPOSIT-AMT
           END-IF
           ADD SDBM-DEPOSIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3230-DESCRIBE-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FJW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'YFB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN POST DATE IS PAST
      * FJW -> YFB WHEN POST DATE IS PAST
      * YFB -> EXS WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3240-SET-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   IF SDBT-POST-DATE > 0 AND SDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJW' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJW'
                   IF SDBT-POST-DATE > 0 AND SDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YFB' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YFB'
                   IF SDBT-POST-DATE > 0 AND SDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EXS' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DEPOSIT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3250-FLAG-DEPOSIT.
           IF SDBM-DEPOSIT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-DEPOSIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * LATE FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3260-FLAG-LATE-FEE.
           IF SDBM-LATE-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-LATE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO KEY FEE YTD.
      *----------------------------------------------------------------*
       3270-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-KEY-FEE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-KEY-FEE-YTD
           END-IF
           ADD SDBM-KEY-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3280-COUNT-ITEM.
           IF SDBM-ITEM-CNT < 9999999
               ADD 1 TO SDBM-ITEM-CNT
           END-IF
           IF SDBM-ITEM-CNT NOT < 12
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3290-AGE-NEXT-DUE.
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
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE RENT AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3300-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BRANCH-CD
                   ADD SDBT-RENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBT-RENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3310-FLAG-DEPOSIT.
           IF SDBM-DEPOSIT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDBM-DEPOSIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E317 CUST ID REQUIRED
      * E222 DEPOSIT AMT NEGATIVE
      * E452 DEPOSIT AMT OVER 50000
      * E259 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3320-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-CUST-ID = SPACES
                   MOVE 'E317' TO WS-REJECT-CODE
               WHEN SDBM-DEPOSIT-AMT < 0
                   MOVE 'E222' TO WS-REJECT-CODE
               WHEN SDBM-DEPOSIT-AMT > 50000.00
                   MOVE 'E452' TO WS-REJECT-CODE
               WHEN SDBM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E259' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3330-COUNT-SEQ.
           IF SDBT-SEQ < 9999999
               ADD 1 TO SDBT-SEQ
           END-IF
           IF SDBT-SEQ NOT < 10
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PREPAID YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3340-LIMIT-PREPAID.
           IF SDBM-PREPAID-YTD < 25.00
               MOVE 25.00 TO SDBM-PREPAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-PREPAID-YTD > 50000.00
               MOVE 50000.00 TO SDBM-PREPAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3350-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-REASON-CD
                   ADD SDBM-LATE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-LATE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RENT AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3360-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBT-TRAN-CD
                   ADD SDBT-RENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBT-RENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RENT GRID TIER FOR THE PREPAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3370-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-RENT-GRID-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SDBM-PREPAID-YTD NOT < WS-RENT-GRID-LOW(WS-TX)
                   MOVE WS-RENT-GRID-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT TIER TIER FOR THE LATE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3380-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-TIER-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SDBM-LATE-FEE-AMT NOT < WS-DEPOSIT-TIER-LOW(WS-TX)
                   MOVE WS-DEPOSIT-TIER-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE KEY FEE FEE ON THE WORK AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3390-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDBT-KEY-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-SUSPEND-FLAG = 'Y'
                   COMPUTE SDBT-KEY-FEE-AMT ROUNDED = WS-WORK-AMT *
                       SDBM-DISCOUNT / 200
               WHEN SDBM-BRANCH-CD = 'EXS'
                   MOVE 20.00 TO SDBT-KEY-FEE-AMT
               WHEN SDBM-BRANCH-CD = 'FJW'
                   MOVE 3.00 TO SDBT-KEY-FEE-AMT
               WHEN OTHER
                   COMPUTE SDBT-KEY-FEE-AMT ROUNDED = WS-WORK-AMT *
                       SDBM-DISCOUNT / 100
           END-EVALUATE
           IF SDBT-KEY-FEE-AMT > 0 AND SDBT-KEY-FEE-AMT < 5.00
               MOVE 5.00 TO SDBT-KEY-FEE-AMT
           END-IF
           IF SDBT-KEY-FEE-AMT > 35.00
               MOVE 35.00 TO SDBT-KEY-FEE-AMT
           END-IF
           ADD SDBT-KEY-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3400-DESCRIBE-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = SDBM-DEPOSIT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = SDBM-DEPOSIT-YTD - (WS-WORK-AMT * 3)
           COMPUTE SDBM-DRILL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3420-CHECK-NOTICE.
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
               MOVE 'Y' TO SDBM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3430-ACCRUE-LATE-FEE.
           IF SDBM-LATE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-LATE-FEE-AMT * SDBM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-DEPOSIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3440-CHECK-OPEN.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RENT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3450-FLAG-RENT.
           IF SDBM-RENT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-RENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3460-AGE-NEXT-DUE.
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
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE RENT AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3470-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBT-TRAN-CD
                   ADD SDBM-RENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-RENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RENT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3480-ACCRUE-RENT.
           IF SDBT-RENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBT-RENT-AMT * SDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SDBT-DRILL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3490-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * SDBM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-DEPOSIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON KEY FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3500-ACCRUE-KEY-FEE.
           IF SDBM-KEY-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-KEY-FEE-AMT * SDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-KEY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E385 CUST ID REQUIRED
      * E277 PREPAID YTD NEGATIVE
      * E276 PREPAID YTD OVER 99999
      * E847 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3510-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-CUST-ID = SPACES
                   MOVE 'E385' TO WS-REJECT-CODE
               WHEN SDBM-PREPAID-YTD < 0
                   MOVE 'E277' TO WS-REJECT-CODE
               WHEN SDBM-PREPAID-YTD > 99999.00
                   MOVE 'E276' TO WS-REJECT-CODE
               WHEN SDBT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E847' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * KEY FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3520-FLAG-KEY-FEE.
           IF SDBM-KEY-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDBM-KEY-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON KEY FEE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3530-ACCRUE-KEY-FEE.
           IF SDBM-KEY-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-KEY-FEE-YTD * SDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DRILL AMT BY BOX SIZE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3540-ACCUM-BOX-SIZE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BOX-SIZE-CD
                   ADD SDBM-DRILL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BOX-SIZE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-DRILL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRILL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LATE FEE YTD.
      *----------------------------------------------------------------*
       3550-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-LATE-FEE-YTD ROUNDED =
                   SDBM-DRILL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-LATE-FEE-YTD
           END-IF
           ADD SDBM-LATE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3560-AGE-LAST-ACT.
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
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE VAULT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3570-DESCRIBE-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ET'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RENT BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3580-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-RENT-BAND-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-RENT-BAND-LOW(WS-TX)
                   MOVE WS-RENT-BAND-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RENT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3590-ACCRUE-RENT.
           IF SDBT-RENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBT-RENT-AMT * SDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO SDBT-DEPOSIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3600-CHECK-POST.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PREPAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3610-PRORATE-PREPAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   SDBM-PREPAID-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3620-CHECK-EFF.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RENT BAND TIER FOR THE RENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3630-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RENT-BAND-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SDBT-RENT-AMT NOT < WS-RENT-BAND-LOW(WS-TX)
                   MOVE WS-RENT-BAND-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3640-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BRANCH-CD
                   ADD SDBM-LATE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-LATE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR VAULT CD:
      * BU -> ET WHEN NEXT REVIEW DATE IS PAST
      * ET -> FF WHEN NEXT REVIEW DATE IS PAST
      * FF -> NG WHEN NEXT REVIEW DATE IS PAST
      * NG -> NH WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3650-SET-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ET' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ET'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'FF' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FF'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'NG' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NG'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'NH' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PREPAID TIER TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3660-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PREPAID-TIER-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF SDBT-DEPOSIT-AMT NOT < WS-PREPAID-TIER-LOW(WS-TX)
                   MOVE WS-PREPAID-TIER-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-NOTICE.
           IF SDBM-NOTICE-CNT < 9999999
               ADD 1 TO SDBM-NOTICE-CNT
           END-IF
           IF SDBM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3680-CHECK-EFF.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PREPAID AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3690-ACCRUE-PREPAID.
           IF SDBM-PREPAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-PREPAID-AMT * SDBM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-DRILL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DRILL YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3700-FLAG-DRILL.
           IF SDBM-DRILL-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-DRILL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3710-SPLIT-DRILL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE SDBT-DRILL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PREPAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEPOSIT AMT.
      *----------------------------------------------------------------*
       3720-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBT-DEPOSIT-AMT ROUNDED =
                   SDBM-PREPAID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBT-DEPOSIT-AMT
           END-IF
           ADD SDBT-DEPOSIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT KEY FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3730-SPLIT-KEY-FEE.
           COMPUTE WS-WORK-AMT = SDBT-KEY-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SDBT-KEY-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE SDBT-RENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E509 ADDR REQUIRED
      * E362 DRILL AMT NEGATIVE
      * E247 DRILL AMT OVER 10000
      * E739 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3740-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-ADDR = SPACES
                   MOVE 'E509' TO WS-REJECT-CODE
               WHEN SDBT-DRILL-AMT < 0
                   MOVE 'E362' TO WS-REJECT-CODE
               WHEN SDBT-DRILL-AMT > 10000.00
                   MOVE 'E247' TO WS-REJECT-CODE
               WHEN SDBM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PREPAID TIER TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3750-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PREPAID-TIER-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF SDBM-DEPOSIT-AMT NOT < WS-PREPAID-TIER-LOW(WS-TX)
                   MOVE WS-PREPAID-TIER-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON KEY FEE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3760-ACCRUE-KEY-FEE.
           IF SDBT-KEY-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBT-KEY-FEE-AMT * SDBM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-LATE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RENT YTD FEE ON THE PREPAID AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3770-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN SDBM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO SDBM-RENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE SDBM-RENT-YTD ROUNDED = SDBM-PREPAID-AMT *
                       SDBM-PENALTY-PCT / 200
               WHEN SDBT-TRAN-CD = 'RN'
                   MOVE 3.00 TO SDBM-RENT-YTD
               WHEN SDBT-TRAN-CD = 'AC'
                   MOVE 10.00 TO SDBM-RENT-YTD
               WHEN OTHER
                   COMPUTE SDBM-RENT-YTD ROUNDED = SDBM-PREPAID-AMT *
                       SDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF SDBM-RENT-YTD > 0 AND SDBM-RENT-YTD < 1.00
               MOVE 1.00 TO SDBM-RENT-YTD
           END-IF
           IF SDBM-RENT-YTD > 250.00
               MOVE 250.00 TO SDBM-RENT-YTD
           END-IF
           ADD SDBM-RENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PREPAID YTD.
      *----------------------------------------------------------------*
       3780-PRORATE-PREPAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-PREPAID-YTD ROUNDED =
                   SDBM-LATE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-PREPAID-YTD
           END-IF
           ADD SDBM-PREPAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3790-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3800-FLAG-DEPOSIT.
           IF SDBM-DEPOSIT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-DEPOSIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON KEY FEE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3810-ACCRUE-KEY-FEE.
           IF SDBM-KEY-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-KEY-FEE-YTD * SDBM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RENT AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3820-LIMIT-RENT.
           IF SDBM-RENT-AMT < 100.00
               MOVE 100.00 TO SDBM-RENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-RENT-AMT > 500.00
               MOVE 500.00 TO SDBM-RENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DRILL AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3830-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BRANCH-CD
                   ADD SDBT-DRILL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBT-DRILL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3840-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3850-COUNT-NOTICE.
           IF SDBM-NOTICE-CNT < 9999999
               ADD 1 TO SDBM-NOTICE-CNT
           END-IF
           IF SDBM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3860-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF SDBT-EFF-DATE > 0 AND SDBT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RENT BAND TIER FOR THE KEY FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3870-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RENT-BAND-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SDBT-KEY-FEE-AMT NOT < WS-RENT-BAND-LOW(WS-TX)
                   MOVE WS-RENT-BAND-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE KEY FEE AMT BY BOX SIZE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3880-ACCUM-BOX-SIZE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BOX-SIZE-CD
                   ADD SDBM-KEY-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BOX-SIZE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-KEY-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3890-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEPOSIT AMT.
      *----------------------------------------------------------------*
       3900-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-DEPOSIT-AMT ROUNDED =
                   SDBM-LATE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-DEPOSIT-AMT
           END-IF
           ADD SDBM-DEPOSIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3910-ACCRUE-DRILL.
           IF SDBM-DRILL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-DRILL-YTD * SDBM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-LATE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3920-DESCRIBE-SOURCE.
           EVALUATE SDBM-SOURCE-CD
               WHEN 'DD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3930-ACCRUE-LATE-FEE.
           IF SDBM-LATE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-LATE-FEE-AMT * SDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3940-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-NOTICE.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON KEY FEE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3960-ACCRUE-KEY-FEE.
           IF SDBT-KEY-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBT-KEY-FEE-AMT * SDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-RENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RENT YTD BY VAULT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3970-ACCUM-VAULT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-VAULT-CD
                   ADD SDBM-RENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-VAULT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-RENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RENT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3980-ACCRUE-RENT.
           IF SDBT-RENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBT-RENT-AMT * SDBM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-DRILL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE KEY FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3990-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBT-CHANNEL-CD
                   ADD SDBT-KEY-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBT-KEY-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4000-CHECK-LAST-STMT.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4010-CHECK-EFF.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4020-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO SDBM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4030-COUNT-NOTICE.
           IF SDBM-NOTICE-CNT < 9999999
               ADD 1 TO SDBM-NOTICE-CNT
           END-IF
           IF SDBM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4040-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4050-SPLIT-PREPAID.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE SDBM-PREPAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4060-COUNT-ITEM.
           IF SDBM-ITEM-CNT < 9999999
               ADD 1 TO SDBM-ITEM-CNT
           END-IF
           IF SDBM-ITEM-CNT NOT < 3
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RENT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4070-SPLIT-RENT.
           COMPUTE WS-WORK-AMT = SDBT-RENT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SDBT-RENT-AMT - (WS-WORK-AMT * 2)
           COMPUTE SDBM-RENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4080-DESCRIBE-REASON.
           EVALUATE SDBM-REASON-CD
               WHEN 'J'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'L'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'S'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'W'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DRILL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RENT AMT.
      *----------------------------------------------------------------*
       4090-PRORATE-RENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-RENT-AMT ROUNDED =
                   SDBM-DRILL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-RENT-AMT
           END-IF
           ADD SDBM-RENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * KEY FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4100-LIMIT-KEY-FEE.
           IF SDBM-KEY-FEE-AMT < 50.00
               MOVE 50.00 TO SDBM-KEY-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-KEY-FEE-AMT > 50000.00
               MOVE 50000.00 TO SDBM-KEY-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE VAULT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4110-DESCRIBE-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ET'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4120-AGE-NEXT-DUE.
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
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4130-DESCRIBE-CYCLE.
           EVALUATE SDBM-CYCLE-CD
               WHEN 'AP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEPOSIT YTD.
      *----------------------------------------------------------------*
       4140-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-DEPOSIT-YTD ROUNDED =
                   SDBT-RENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-DEPOSIT-YTD
           END-IF
           ADD SDBM-DEPOSIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PREPAID AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4150-FLAG-PREPAID.
           IF SDBM-PREPAID-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDBM-PREPAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * KEY FEE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4160-FLAG-KEY-FEE.
           IF SDBM-KEY-FEE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDBM-KEY-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4170-COUNT-PAYMENT.
           IF SDBM-PAYMENT-CNT < 9999999
               ADD 1 TO SDBM-PAYMENT-CNT
           END-IF
           IF SDBM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO SDBM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E509 DESC REQUIRED
      * E213 WORK AMT NEGATIVE
      * E288 WORK AMT OVER 50000
      * E154 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4180-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-DESC = SPACES
                   MOVE 'E509' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E213' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E288' TO WS-REJECT-CODE
               WHEN SDBM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E154' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN CLOSE DATE IS PAST
      * FJW -> YFB WHEN CLOSE DATE IS PAST
      * YFB -> EXS WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4190-SET-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJW' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJW'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YFB' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YFB'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EXS' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * RN -> AC WHEN NEXT REVIEW DATE IS PAST
      * AC -> DR WHEN NEXT REVIEW DATE IS PAST
      * DR -> KY WHEN NEXT REVIEW DATE IS PAST
      * KY -> CL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4200-SET-TRAN.
           EVALUATE SDBT-TRAN-CD
               WHEN 'RN'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'AC' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AC'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DR' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DR'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KY' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KY'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'CL' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LATE FEE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4210-FLAG-LATE-FEE.
           IF SDBM-LATE-FEE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-LATE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RENT BAND TIER FOR THE PREPAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4220-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-RENT-BAND-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SDBM-PREPAID-AMT NOT < WS-RENT-BAND-LOW(WS-TX)
                   MOVE WS-RENT-BAND-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RENT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4230-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-REASON-CD
                   ADD SDBM-RENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-RENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PREPAID AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4240-LIMIT-PREPAID.
           IF SDBM-PREPAID-AMT < 10.00
               MOVE 10.00 TO SDBM-PREPAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-PREPAID-AMT > 500.00
               MOVE 500.00 TO SDBM-PREPAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4250-ACCRUE-DRILL.
           IF SDBM-DRILL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-DRILL-AMT * SDBM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDBT-RENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RENT YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4260-ACCRUE-RENT.
           IF SDBM-RENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-RENT-YTD * SDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-PREPAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RENT BAND TIER FOR THE KEY FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4270-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RENT-BAND-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SDBM-KEY-FEE-AMT NOT < WS-RENT-BAND-LOW(WS-TX)
                   MOVE WS-RENT-BAND-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4280-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO SDBM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4290-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = SDBM-LATE-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SDBM-LATE-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE SDBT-DRILL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * RN -> AC WHEN NOTICE DATE IS PAST
      * AC -> DR WHEN NOTICE DATE IS PAST
      * DR -> KY WHEN NOTICE DATE IS PAST
      * KY -> CL WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4300-SET-TRAN.
           EVALUATE SDBT-TRAN-CD
               WHEN 'RN'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AC' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AC'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DR' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DR'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KY' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KY'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CL' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4310-COUNT-SEQ.
           IF SDBT-SEQ < 9999999
               ADD 1 TO SDBT-SEQ
           END-IF
           IF SDBT-SEQ NOT < 3
               MOVE 'Y' TO SDBM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4320-COUNT-SEQ.
           IF SDBT-SEQ < 9999999
               ADD 1 TO SDBT-SEQ
           END-IF
           IF SDBT-SEQ NOT < 3
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT TIER TIER FOR THE KEY FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4330-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-TIER-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SDBM-KEY-FEE-AMT NOT < WS-DEPOSIT-TIER-LOW(WS-TX)
                   MOVE WS-DEPOSIT-TIER-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE LATE FEE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4340-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN SDBM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SDBM-LATE-FEE-AMT *
                       SDBM-FEE-PCT / 200
               WHEN SDBM-BOX-SIZE-CD = 'CU'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN SDBM-BOX-SIZE-CD = 'MG'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SDBM-LATE-FEE-AMT *
                       SDBM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4350-CHECK-EFF.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO SDBM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4360-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SDBM-MATURITY-DATE > 0 AND SDBM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEPOSIT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4370-ACCRUE-DEPOSIT.
           IF SDBM-DEPOSIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-DEPOSIT-AMT * SDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-KEY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PREPAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PREPAID AMT.
      *----------------------------------------------------------------*
       4380-PRORATE-PREPAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-PREPAID-AMT ROUNDED =
                   SDBM-PREPAID-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-PREPAID-AMT
           END-IF
           ADD SDBM-PREPAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E997 CONTACT REQUIRED
      * E946 RENT AMT NEGATIVE
      * E936 RENT AMT OVER 1000000
      * E020 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4390-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-CONTACT = SPACES
                   MOVE 'E997' TO WS-REJECT-CODE
               WHEN SDBM-RENT-AMT < 0
                   MOVE 'E946' TO WS-REJECT-CODE
               WHEN SDBM-RENT-AMT > 1000000.00
                   MOVE 'E936' TO WS-REJECT-CODE
               WHEN SDBM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E020' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PREPAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4400-PRORATE-PREPAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-PREPAID-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4410-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF SDBT-EFF-DATE > 0 AND SDBT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-LATE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDBM-LATE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-LATE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBM-LATE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-LATE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON KEY FEE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4420-ACCRUE-KEY-FEE.
           IF SDBM-KEY-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-KEY-FEE-YTD * SDBM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBT-RENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

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
                   MOVE SDRISK-LATE-FEE-YTD
                     TO SDBM-LATE-FEE-YTD
                   MOVE SDRISK-LATE-FEE-AMT
                     TO SDBM-LATE-FEE-AMT
                   MOVE SDRISK-AUTO-RENEW-FLAG
                     TO SDBM-AUTO-RENEW-FLAG
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
                   MOVE SDWAIV-DEPOSIT-YTD
                     TO SDBM-DEPOSIT-YTD
                   MOVE SDWAIV-LATE-FEE-AMT
                     TO SDBM-LATE-FEE-AMT
                   MOVE SDWAIV-KEY-FEE-YTD
                     TO SDBM-KEY-FEE-YTD
                   MOVE SDWAIV-SUSPEND-FLAG
                     TO SDBM-SUSPEND-FLAG
                   MOVE SDWAIV-REVIEW-FLAG
                     TO SDBM-REVIEW-FLAG
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

      *----------------------------------------------------------------*
      * PASS THE SDB FIELDS TO SDLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-SDLIM.
           INITIALIZE SDLIM-PARMS
           MOVE SDBM-RENT-YTD
             TO SDLIM-RENT-YTD
           MOVE SDBM-DEPOSIT-YTD
             TO SDLIM-DEPOSIT-YTD
           MOVE SDBM-LATE-FEE-YTD
             TO SDLIM-LATE-FEE-YTD
           MOVE SDBM-DEPOSIT-AMT
             TO SDLIM-DEPOSIT-AMT
           MOVE SDBM-LATE-FEE-AMT
             TO SDLIM-LATE-FEE-AMT
           MOVE SDBM-DRILL-YTD
             TO SDLIM-DRILL-YTD
           MOVE SDBM-LAST-ACT-DATE
             TO SDLIM-LAST-ACT-DATE
           MOVE SDBM-OPEN-DATE
             TO SDLIM-OPEN-DATE
           MOVE SDBM-NEXT-DUE-DATE
             TO SDLIM-NEXT-DUE-DATE
           MOVE SDBM-NOTICE-DATE
             TO SDLIM-NOTICE-DATE
           MOVE SDBM-CYCLE-CD
             TO SDLIM-CYCLE-CD
           MOVE SDBM-BRANCH-CD
             TO SDLIM-BRANCH-CD
           MOVE SDBM-REASON-CD
             TO SDLIM-REASON-CD
           MOVE SDBM-BOX-SIZE-CD
             TO SDLIM-BOX-SIZE-CD
           MOVE SDBM-OVERRIDE-FLAG
             TO SDLIM-OVERRIDE-FLAG
           MOVE SDBM-EXEMPT-FLAG
             TO SDLIM-EXEMPT-FLAG
           MOVE SDBM-REVIEW-FLAG
             TO SDLIM-REVIEW-FLAG
           MOVE SDBM-RETRY-CNT
             TO SDLIM-RETRY-CNT
           MOVE SDBM-NOTICE-CNT
             TO SDLIM-NOTICE-CNT
           MOVE SDBM-DISCOUNT
             TO SDLIM-DISCOUNT
           MOVE SDBM-FEE-PCT
             TO SDLIM-FEE-PCT
           MOVE SDBM-CONTACT
             TO SDLIM-CONTACT
           MOVE SDBM-ADDR
             TO SDLIM-ADDR
           CALL 'SDLIM' USING SDLIM-PARMS
           EVALUATE SDLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SDLIM-RENT-YTD
                     TO SDBM-RENT-YTD
                   MOVE SDLIM-LATE-FEE-YTD
                     TO SDBM-LATE-FEE-YTD
                   MOVE SDLIM-OVERRIDE-FLAG
                     TO SDBM-OVERRIDE-FLAG
                   MOVE SDLIM-EXEMPT-FLAG
                     TO SDBM-EXEMPT-FLAG
                   MOVE SDLIM-REVIEW-FLAG
                     TO SDBM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-SDLIM' TO ABND-PARAGRAPH
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
           DISPLAY 'SDBSWEP READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'SDBSWEP' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
