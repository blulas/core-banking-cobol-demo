      *================================================================*
      * PROGRAM  : IRACLSE                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : IRA CUSTODY - CLOSURE PROCESSING. READS THE IRA     *
      *            TRANSACTIONS, UPDATES THE IRA MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. INDIVIDUAL RETIREMENT ACCOUNT   *
      *            CONTRIBUTIONS, DISTRIBUTIONS AND 5498               *
      * INPUT    : IRAMST (KSDS), IRATRN, BANKCTL                      *
      * OUTPUT   : IRAOUT, IRACLRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, IRVAL                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-08-17 BHS  ORIGINAL                                       *
      * 1999-07-11 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-05-10 JLM  NEW FMV REPORT COLUMNS CHG2536                 *
      * 2001-01-28 RJT  WITHHOLD LIMIT REVISED REQ2239                 *
      * 2002-06-21 PQR  ROLLOVER LIMIT REVISED REQ5000                 *
      * 2004-07-24 BHS  WITHHOLD STATUS CODES ADDED INC6210            *
      * 2010-09-15 JLM  AUDIT FINDING - RMD CONTROL TOTALS INC1634     *
      * 2011-07-04 PQR  RMD NOTICE TEXT PER LEGAL INC4421              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IRACLSE.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "IRAMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS IRAM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "IRATRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "IRAOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "IRACLRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY IRAMST.
       FD  TRAN-FILE.
       COPY IRATRN.
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
      * WS-RMD-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RMD-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000850500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000635500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000796000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000824500'.
       01  WS-RMD-TIER-TBL REDEFINES WS-RMD-TIER-VALUES.
           05  WS-RMD-TIER-ROW OCCURS 4 TIMES.
               10  WS-RMD-TIER-LOW         PIC 9(09)V99.
               10  WS-RMD-TIER-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-EXCESS-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-EXCESS-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000413500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000012500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000200000000846500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002700000000769500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012700000000139500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012800000000073000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013300000000445500'.
       01  WS-EXCESS-SCHED-TBL REDEFINES WS-EXCESS-SCHED-VALUES.
           05  WS-EXCESS-SCHED-ROW OCCURS 7 TIMES.
               10  WS-EXCESS-SCHED-LOW     PIC 9(09)V99.
               10  WS-EXCESS-SCHED-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  IRACLSE  IRA CUSTODY'.
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
       COPY IRVALP.

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
               MOVE 'IRAMST' TO ABND-FILE-NAME
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
           MOVE IRAT-KEY TO IRAM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-COUNT-CYCLE
               PERFORM 3010-SET-TRAN
               PERFORM 3020-CHECK-LAST-ACT
               PERFORM 3030-LIMIT-DISTRIB
               PERFORM 3040-SPLIT-CONTRIB
               PERFORM 3050-SET-IRA-TYPE
               PERFORM 3060-PRORATE-EXCESS
               PERFORM 3070-SPLIT-RMD
               PERFORM 3080-ASSESS-STOP
               PERFORM 3090-COUNT-ITEM
               PERFORM 3100-EDIT-MEMO
               PERFORM 3110-FLAG-FMV
               PERFORM 3120-LIMIT-RMD
               PERFORM 3130-SELECT-FEE-PCT
               PERFORM 3140-SET-TYPE
               PERFORM 3150-PRORATE-ROLLOVER
               PERFORM 3160-PRORATE-ROLLOVER
               PERFORM 3170-FLAG-ROLLOVER
               PERFORM 3180-COUNT-SEQ
               PERFORM 3190-SPLIT-CONTRIB
               PERFORM 3200-SELECT-RATE
               PERFORM 3210-LIMIT-CONTRIB
               PERFORM 3220-DESCRIBE-DIST-CODE
               PERFORM 3230-PRORATE-EXCESS
               PERFORM 3240-EDIT-MEMO
               PERFORM 3250-FLAG-FMV
               PERFORM 3260-ASSESS-STOP
               PERFORM 3270-COUNT-SEQ
               PERFORM 3280-CHECK-NOTICE
               PERFORM 3290-DESCRIBE-TRAN
               PERFORM 3300-ASSESS-HOLD
               PERFORM 3310-COUNT-SEQ
               PERFORM 3320-AGE-POST
               PERFORM 3330-PRORATE-EXCESS
               PERFORM 3340-SELECT-RATE
               PERFORM 3350-ASSESS-SUSPEND
               PERFORM 3360-SPLIT-CONTRIB
               PERFORM 3370-ACCUM-IRA-TYPE
               PERFORM 3380-ACCRUE-CONTRIB
               PERFORM 3390-PRORATE-ROLLOVER
               PERFORM 3400-LIMIT-DISTRIB
               PERFORM 3410-AGE-NOTICE
               PERFORM 3420-CHECK-POST
               PERFORM 3430-ASSESS-HOLD
               PERFORM 3440-ACCRUE-RMD
               PERFORM 3450-ACCUM-TRAN
               PERFORM 3460-CHECK-POST
               PERFORM 3470-AGE-NEXT-REVIEW
               PERFORM 3480-SPLIT-DISTRIB
               PERFORM 3490-ASSESS-REVIEW
               PERFORM 3500-ACCUM-TYPE
               PERFORM 3510-SPLIT-DISTRIB
               PERFORM 3520-EDIT-REJECT
               PERFORM 3530-DESCRIBE-CHANNEL
               PERFORM 3540-SELECT-RATE
               PERFORM 3550-PRORATE-DISTRIB
               PERFORM 3560-FLAG-RMD
               PERFORM 3570-ACCUM-IRA-TYPE
               PERFORM 3580-AGE-LAST-ACT
               PERFORM 3590-SPLIT-EXCESS
               PERFORM 3600-AGE-EXPIRY
               PERFORM 3610-ASSESS-REVIEW
               PERFORM 3620-SPLIT-EXCESS
               PERFORM 3630-EDIT-NAME
               PERFORM 3640-COUNT-CYCLE
               PERFORM 3650-ACCRUE-ROLLOVER
               PERFORM 3660-AGE-NOTICE
               PERFORM 3670-DESCRIBE-TRAN
               PERFORM 3680-EDIT-CONTACT
               PERFORM 3690-FLAG-ITEM
               PERFORM 3700-SELECT-FEE-PCT
               PERFORM 3710-ASSESS-HOLD
               PERFORM 3720-LIMIT-EXCESS
               PERFORM 3730-COUNT-PAYMENT
               PERFORM 3740-SELECT-FEE-PCT
               PERFORM 3750-COUNT-RETRY
               PERFORM 3760-FLAG-RMD
               PERFORM 3770-LIMIT-ROLLOVER
               PERFORM 3780-AGE-NOTICE
               PERFORM 3790-AGE-EFFECTIVE
               PERFORM 3800-EDIT-OPERATOR
               PERFORM 3810-ASSESS-REVERSAL
               PERFORM 3820-COUNT-ITEM
               PERFORM 3830-ACCRUE-EXCESS
               PERFORM 3840-SPLIT-ROLLOVER
               PERFORM 3850-SELECT-MARGIN
               PERFORM 3860-AGE-POST
               PERFORM 3870-DESCRIBE-IRA-TYPE
               PERFORM 3880-EDIT-NAME
               PERFORM 3890-AGE-LAST-STMT
               PERFORM 3900-SET-CHANNEL
               PERFORM 3910-EDIT-REJECT
               PERFORM 3920-COUNT-PAYMENT
               PERFORM 3930-PRORATE-RMD
               PERFORM 3940-COUNT-PAYMENT
               PERFORM 3950-PRORATE-DISTRIB
               PERFORM 3960-ACCUM-REGION
               PERFORM 3970-SET-IRA-TYPE
               PERFORM 3980-PRORATE-ROLLOVER
               PERFORM 3990-SPLIT-CONTRIB
               PERFORM 4000-LIMIT-ROLLOVER
               PERFORM 4010-PRORATE-RMD
               PERFORM 4020-SPLIT-ROLLOVER
               PERFORM 4030-LIMIT-DISTRIB
               PERFORM 4040-SET-CHANNEL
               PERFORM 4050-DESCRIBE-REASON
               PERFORM 4060-ACCUM-TYPE
               PERFORM 4070-DESCRIBE-TYPE
               PERFORM 4080-DESCRIBE-IRA-TYPE
               PERFORM 4090-ACCRUE-DISTRIB
               PERFORM 4100-LIMIT-DISTRIB
               PERFORM 4110-SELECT-RATE
               PERFORM 4120-LIMIT-RMD
               PERFORM 4130-FLAG-ROLLOVER
               PERFORM 4140-SPLIT-RMD
               PERFORM 4150-SPLIT-EXCESS
               PERFORM 4160-SPLIT-FMV
               PERFORM 4170-ACCUM-DIST-CODE
               PERFORM 7010-CALL-IRVAL
               REWRITE IRA-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'IRAMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM IRA-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3000-COUNT-CYCLE.
           IF IRAM-CYCLE-CNT < 9999999
               ADD 1 TO IRAM-CYCLE-CNT
           END-IF
           IF IRAM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO IRAM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CN -> DS WHEN NEXT DUE DATE IS PAST
      * DS -> RO WHEN NEXT DUE DATE IS PAST
      * RO -> RC WHEN NEXT DUE DATE IS PAST
      * RC -> WH WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3010-SET-TRAN.
           EVALUATE IRAT-TRAN-CD
               WHEN 'CN'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DS' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DS'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RO' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RO'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WH' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3020-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF IRAM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO IRAM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3030-LIMIT-DISTRIB.
           IF IRAT-DISTRIB-AMT < 10.00
               MOVE 10.00 TO IRAT-DISTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAT-DISTRIB-AMT > 10000.00
               MOVE 10000.00 TO IRAT-DISTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3040-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = IRAM-CONTRIB-YTD / 2
           COMPUTE WS-WORK-AMT-2 = IRAM-CONTRIB-YTD - (WS-WORK-AMT * 2)
           COMPUTE IRAM-RMD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR IRA TYPE CD:
      * F -> V WHEN LAST STMT DATE IS PAST
      * V -> W WHEN LAST STMT DATE IS PAST
      * W -> Y WHEN LAST STMT DATE IS PAST
      * Y -> F WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-IRA-TYPE.
           EVALUATE IRAM-IRA-TYPE-CD
               WHEN 'F'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FMV YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO EXCESS YTD.
      *----------------------------------------------------------------*
       3060-PRORATE-EXCESS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-EXCESS-YTD ROUNDED =
                   IRAM-FMV-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAM-EXCESS-YTD
           END-IF
           ADD IRAM-EXCESS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT RMD AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3070-SPLIT-RMD.
           COMPUTE WS-WORK-AMT = IRAT-RMD-AMT / 2
           COMPUTE WS-WORK-AMT-2 = IRAT-RMD-AMT - (WS-WORK-AMT * 2)
           COMPUTE IRAM-EXCESS-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RMD FEE ON THE DISTRIB YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3080-ASSESS-STOP.
           EVALUATE TRUE
               WHEN IRAM-STOP-FLAG = 'Y'
                   MOVE ZERO TO IRAT-RMD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE IRAT-RMD-AMT ROUNDED = IRAM-DISTRIB-YTD *
                       IRAM-MARGIN / 200
               WHEN IRAT-TRAN-CD = 'CN'
                   MOVE 15.00 TO IRAT-RMD-AMT
               WHEN IRAT-TRAN-CD = 'DS'
                   MOVE 20.00 TO IRAT-RMD-AMT
               WHEN OTHER
                   COMPUTE IRAT-RMD-AMT ROUNDED = IRAM-DISTRIB-YTD *
                       IRAM-MARGIN / 100
           END-EVALUATE
           IF IRAT-RMD-AMT > 0 AND IRAT-RMD-AMT < 2.00
               MOVE 2.00 TO IRAT-RMD-AMT
           END-IF
           IF IRAT-RMD-AMT > 75.00
               MOVE 75.00 TO IRAT-RMD-AMT
           END-IF
           ADD IRAT-RMD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3090-COUNT-ITEM.
           IF IRAM-ITEM-CNT < 9999999
               ADD 1 TO IRAM-ITEM-CNT
           END-IF
           IF IRAM-ITEM-CNT NOT < 3
               MOVE 'Y' TO IRAM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E543 MEMO REQUIRED
      * E409 CONTRIB AMT NEGATIVE
      * E776 CONTRIB AMT OVER 5000
      * E418 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3100-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAT-MEMO = SPACES
                   MOVE 'E543' TO WS-REJECT-CODE
               WHEN IRAM-CONTRIB-AMT < 0
                   MOVE 'E409' TO WS-REJECT-CODE
               WHEN IRAM-CONTRIB-AMT > 5000.00
                   MOVE 'E776' TO WS-REJECT-CODE
               WHEN IRAM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E418' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FMV YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3110-FLAG-FMV.
           IF IRAM-FMV-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE IRAM-FMV-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RMD AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3120-LIMIT-RMD.
           IF IRAT-RMD-AMT < 25.00
               MOVE 25.00 TO IRAT-RMD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAT-RMD-AMT > 500.00
               MOVE 500.00 TO IRAT-RMD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCESS SCHED TIER FOR THE CONTRIB YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3130-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EXCESS-SCHED-RATE(1) TO IRAM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF IRAM-CONTRIB-YTD NOT < WS-EXCESS-SCHED-LOW(WS-TX)
                   MOVE WS-EXCESS-SCHED-RATE(WS-TX) TO IRAM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * EA -> HR WHEN LAST ACT DATE IS PAST
      * HR -> XZ WHEN LAST ACT DATE IS PAST
      * XZ -> EA WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3140-SET-TYPE.
           EVALUATE IRAM-TYPE-CD
               WHEN 'EA'
                   IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XZ' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XZ'
                   IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EA' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WITHHOLD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ROLLOVER AMT.
      *----------------------------------------------------------------*
       3150-PRORATE-ROLLOVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAT-ROLLOVER-AMT ROUNDED =
                   IRAM-WITHHOLD-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAT-ROLLOVER-AMT
           END-IF
           ADD IRAT-ROLLOVER-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE ROLLOVER AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ROLLOVER AMT.
      *----------------------------------------------------------------*
       3160-PRORATE-ROLLOVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-ROLLOVER-AMT ROUNDED =
                   IRAT-ROLLOVER-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAM-ROLLOVER-AMT
           END-IF
           ADD IRAM-ROLLOVER-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ROLLOVER AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3170-FLAG-ROLLOVER.
           IF IRAT-ROLLOVER-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRAT-ROLLOVER-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3180-COUNT-SEQ.
           IF IRAT-SEQ < 9999999
               ADD 1 TO IRAT-SEQ
           END-IF
           IF IRAT-SEQ NOT < 3
               MOVE 'Y' TO IRAM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3190-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE IRAM-CONTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCESS SCHED TIER FOR THE RMD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3200-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-EXCESS-SCHED-RATE(1) TO IRAM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF IRAM-RMD-YTD NOT < WS-EXCESS-SCHED-LOW(WS-TX)
                   MOVE WS-EXCESS-SCHED-RATE(WS-TX) TO IRAM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTRIB YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3210-LIMIT-CONTRIB.
           IF IRAM-CONTRIB-YTD < 50.00
               MOVE 50.00 TO IRAM-CONTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-CONTRIB-YTD > 10000.00
               MOVE 10000.00 TO IRAM-CONTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DIST CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3220-DESCRIBE-DIST-CODE.
           EVALUATE IRAM-DIST-CODE-CD
               WHEN 'C'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'J'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'U'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'X'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RMD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO EXCESS AMT.
      *----------------------------------------------------------------*
       3230-PRORATE-EXCESS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAT-EXCESS-AMT ROUNDED =
                   IRAM-RMD-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAT-EXCESS-AMT
           END-IF
           ADD IRAT-EXCESS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E839 MEMO REQUIRED
      * E027 WORK AMT 2 NEGATIVE
      * E789 WORK AMT 2 OVER 1000000
      * E236 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAT-MEMO = SPACES
                   MOVE 'E839' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E027' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E789' TO WS-REJECT-CODE
               WHEN IRAM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E236' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FMV YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3250-FLAG-FMV.
           IF IRAM-FMV-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRAM-FMV-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE DISTRIB YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3260-ASSESS-STOP.
           EVALUATE TRUE
               WHEN IRAM-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = IRAM-DISTRIB-YTD *
                       IRAM-MARGIN / 200
               WHEN IRAT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN IRAT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = IRAM-DISTRIB-YTD *
                       IRAM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3270-COUNT-SEQ.
           IF IRAT-SEQ < 9999999
               ADD 1 TO IRAT-SEQ
           END-IF
           IF IRAT-SEQ NOT < 12
               MOVE 'Y' TO IRAM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3280-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO IRAM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3290-DESCRIBE-TRAN.
           EVALUATE IRAT-TRAN-CD
               WHEN 'CN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RO'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ROLLOVER FEE ON THE FMV YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3300-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN IRAM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO IRAT-ROLLOVER-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE IRAT-ROLLOVER-AMT ROUNDED = IRAM-FMV-YTD *
                       IRAM-RATE / 200
               WHEN IRAT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO IRAT-ROLLOVER-AMT
               WHEN IRAT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO IRAT-ROLLOVER-AMT
               WHEN OTHER
                   COMPUTE IRAT-ROLLOVER-AMT ROUNDED = IRAM-FMV-YTD *
                       IRAM-RATE / 100
           END-EVALUATE
           IF IRAT-ROLLOVER-AMT > 0 AND IRAT-ROLLOVER-AMT < 1.00
               MOVE 1.00 TO IRAT-ROLLOVER-AMT
           END-IF
           IF IRAT-ROLLOVER-AMT > 100.00
               MOVE 100.00 TO IRAT-ROLLOVER-AMT
           END-IF
           ADD IRAT-ROLLOVER-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3310-COUNT-SEQ.
           IF IRAT-SEQ < 9999999
               ADD 1 TO IRAT-SEQ
           END-IF
           IF IRAT-SEQ NOT < 10
               MOVE 'Y' TO IRAM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3320-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FMV AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO EXCESS AMT.
      *----------------------------------------------------------------*
       3330-PRORATE-EXCESS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-EXCESS-AMT ROUNDED =
                   IRAM-FMV-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-EXCESS-AMT
           END-IF
           ADD IRAM-EXCESS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE RMD TIER TIER FOR THE DISTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3340-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RMD-TIER-RATE(1) TO IRAM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF IRAM-DISTRIB-AMT NOT < WS-RMD-TIER-LOW(WS-TX)
                   MOVE WS-RMD-TIER-RATE(WS-TX) TO IRAM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EXCESS FEE ON THE ROLLOVER YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3350-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN IRAM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO IRAT-EXCESS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-REVIEW-FLAG = 'Y'
                   COMPUTE IRAT-EXCESS-AMT ROUNDED = IRAM-ROLLOVER-YTD *
                       IRAM-MARGIN / 200
               WHEN IRAM-CHANNEL-CD = 'AS'
                   MOVE 12.00 TO IRAT-EXCESS-AMT
               WHEN IRAM-CHANNEL-CD = 'AY'
                   MOVE 10.00 TO IRAT-EXCESS-AMT
               WHEN OTHER
                   COMPUTE IRAT-EXCESS-AMT ROUNDED = IRAM-ROLLOVER-YTD *
                       IRAM-MARGIN / 100
           END-EVALUATE
           IF IRAT-EXCESS-AMT > 0 AND IRAT-EXCESS-AMT < 10.00
               MOVE 10.00 TO IRAT-EXCESS-AMT
           END-IF
           IF IRAT-EXCESS-AMT > 25.00
               MOVE 25.00 TO IRAT-EXCESS-AMT
           END-IF
           ADD IRAT-EXCESS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3360-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = IRAM-CONTRIB-AMT / 3
           COMPUTE WS-WORK-AMT-2 = IRAM-CONTRIB-AMT - (WS-WORK-AMT * 3)
           COMPUTE IRAM-CONTRIB-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB AMT BY IRA TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3370-ACCUM-IRA-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-IRA-TYPE-CD
                   ADD IRAM-DISTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-IRA-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-DISTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3380-ACCRUE-CONTRIB.
           IF IRAM-CONTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-CONTRIB-AMT * IRAM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO IRAT-EXCESS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ROLLOVER YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ROLLOVER AMT.
      *----------------------------------------------------------------*
       3390-PRORATE-ROLLOVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-ROLLOVER-AMT ROUNDED =
                   IRAM-ROLLOVER-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-ROLLOVER-AMT
           END-IF
           ADD IRAM-ROLLOVER-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DISTRIB YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3400-LIMIT-DISTRIB.
           IF IRAM-DISTRIB-YTD < 50.00
               MOVE 50.00 TO IRAM-DISTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-DISTRIB-YTD > 1000.00
               MOVE 1000.00 TO IRAM-DISTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3420-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF IRAT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO IRAM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EXCESS YTD FEE ON THE WITHHOLD YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3430-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN IRAM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO IRAM-EXCESS-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-REVIEW-FLAG = 'Y'
                   COMPUTE IRAM-EXCESS-YTD ROUNDED = IRAM-WITHHOLD-YTD *
                       IRAM-RATE / 200
               WHEN IRAM-REASON-CD = 'CMR'
                   MOVE 12.00 TO IRAM-EXCESS-YTD
               WHEN IRAM-REASON-CD = 'LJS'
                   MOVE 7.00 TO IRAM-EXCESS-YTD
               WHEN OTHER
                   COMPUTE IRAM-EXCESS-YTD ROUNDED = IRAM-WITHHOLD-YTD *
                       IRAM-RATE / 100
           END-EVALUATE
           IF IRAM-EXCESS-YTD > 0 AND IRAM-EXCESS-YTD < 10.00
               MOVE 10.00 TO IRAM-EXCESS-YTD
           END-IF
           IF IRAM-EXCESS-YTD > 100.00
               MOVE 100.00 TO IRAM-EXCESS-YTD
           END-IF
           ADD IRAM-EXCESS-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RMD AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3440-ACCRUE-RMD.
           IF IRAT-RMD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAT-RMD-AMT * IRAM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FMV AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3450-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAT-TRAN-CD
                   ADD IRAM-FMV-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-FMV-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3460-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF IRAT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO IRAM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3470-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NEXT-REVIEW-DATE > 0 AND IRAM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-RMD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRAM-RMD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD IRAM-RMD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRAM-RMD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-RMD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3480-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = IRAT-DISTRIB-AMT / 2
           COMPUTE WS-WORK-AMT-2 = IRAT-DISTRIB-AMT - (WS-WORK-AMT * 2)
           COMPUTE IRAM-ROLLOVER-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RMD FEE ON THE ROLLOVER AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3490-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN IRAM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO IRAT-RMD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-OVERRIDE-FLAG = 'Y'
                   COMPUTE IRAT-RMD-AMT ROUNDED = IRAT-ROLLOVER-AMT *
                       IRAM-FEE-PCT / 200
               WHEN IRAM-TYPE-CD = 'EA'
                   MOVE 15.00 TO IRAT-RMD-AMT
               WHEN IRAM-TYPE-CD = 'HR'
                   MOVE 15.00 TO IRAT-RMD-AMT
               WHEN OTHER
                   COMPUTE IRAT-RMD-AMT ROUNDED = IRAT-ROLLOVER-AMT *
                       IRAM-FEE-PCT / 100
           END-EVALUATE
           IF IRAT-RMD-AMT > 0 AND IRAT-RMD-AMT < 5.00
               MOVE 5.00 TO IRAT-RMD-AMT
           END-IF
           IF IRAT-RMD-AMT > 50.00
               MOVE 50.00 TO IRAT-RMD-AMT
           END-IF
           ADD IRAT-RMD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTRIB AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3500-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-TYPE-CD
                   ADD IRAM-CONTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-CONTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3510-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = IRAM-DISTRIB-YTD / 12
           COMPUTE WS-WORK-AMT-2 = IRAM-DISTRIB-YTD - (WS-WORK-AMT * 12)
           COMPUTE IRAM-CONTRIB-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E956 REJECT CD REQUIRED
      * E378 DISTRIB AMT NEGATIVE
      * E708 DISTRIB AMT OVER 250000
      * E235 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3520-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAT-REJECT-CD = SPACES
                   MOVE 'E956' TO WS-REJECT-CODE
               WHEN IRAM-DISTRIB-AMT < 0
                   MOVE 'E378' TO WS-REJECT-CODE
               WHEN IRAM-DISTRIB-AMT > 250000.00
                   MOVE 'E708' TO WS-REJECT-CODE
               WHEN IRAM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E235' TO WS-REJECT-CODE
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
       3530-DESCRIBE-CHANNEL.
           EVALUATE IRAT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE EXCESS SCHED TIER FOR THE EXCESS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3540-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-EXCESS-SCHED-RATE(1) TO IRAM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF IRAM-EXCESS-YTD NOT < WS-EXCESS-SCHED-LOW(WS-TX)
                   MOVE WS-EXCESS-SCHED-RATE(WS-TX) TO IRAM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISTRIB AMT.
      *----------------------------------------------------------------*
       3550-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-DISTRIB-AMT ROUNDED =
                   IRAT-DISTRIB-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-DISTRIB-AMT
           END-IF
           ADD IRAM-DISTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RMD AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3560-FLAG-RMD.
           IF IRAM-RMD-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE IRAM-RMD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB AMT BY IRA TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3570-ACCUM-IRA-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-IRA-TYPE-CD
                   ADD IRAM-DISTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-IRA-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-DISTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3580-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAT-DISTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD IRAT-DISTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD IRAT-DISTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRAT-DISTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAT-DISTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT EXCESS YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3590-SPLIT-EXCESS.
           COMPUTE WS-WORK-AMT = IRAM-EXCESS-YTD / 12
           COMPUTE WS-WORK-AMT-2 = IRAM-EXCESS-YTD - (WS-WORK-AMT * 12)
           COMPUTE IRAM-FMV-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3600-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE CONTRIB AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3610-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN IRAM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = IRAM-CONTRIB-AMT *
                       IRAM-MARGIN / 200
               WHEN IRAM-DIST-CODE-CD = 'C'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN IRAM-DIST-CODE-CD = 'J'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = IRAM-CONTRIB-AMT *
                       IRAM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT EXCESS AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3620-SPLIT-EXCESS.
           COMPUTE WS-WORK-AMT = IRAM-EXCESS-AMT / 12
           COMPUTE WS-WORK-AMT-2 = IRAM-EXCESS-AMT - (WS-WORK-AMT * 12)
           COMPUTE IRAM-CONTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E067 NAME REQUIRED
      * E430 WITHHOLD YTD NEGATIVE
      * E939 WITHHOLD YTD OVER 10000
      * E296 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3630-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-NAME = SPACES
                   MOVE 'E067' TO WS-REJECT-CODE
               WHEN IRAM-WITHHOLD-YTD < 0
                   MOVE 'E430' TO WS-REJECT-CODE
               WHEN IRAM-WITHHOLD-YTD > 10000.00
                   MOVE 'E939' TO WS-REJECT-CODE
               WHEN IRAT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E296' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3640-COUNT-CYCLE.
           IF IRAM-CYCLE-CNT < 9999999
               ADD 1 TO IRAM-CYCLE-CNT
           END-IF
           IF IRAM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO IRAM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROLLOVER AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3650-ACCRUE-ROLLOVER.
           IF IRAM-ROLLOVER-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-ROLLOVER-AMT * IRAM-RATE / 100 / 365
               ADD WS-WORK-AMT TO IRAT-RMD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3660-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-EXCESS-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRAM-EXCESS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD IRAM-EXCESS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD IRAM-EXCESS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-EXCESS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3670-DESCRIBE-TRAN.
           EVALUATE IRAT-TRAN-CD
               WHEN 'CN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RO'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E314 CONTACT REQUIRED
      * E338 WORK AMT 2 NEGATIVE
      * E524 WORK AMT 2 OVER 99999
      * E583 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3680-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-CONTACT = SPACES
                   MOVE 'E314' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E338' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E524' TO WS-REJECT-CODE
               WHEN IRAT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E583' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3690-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RMD TIER TIER FOR THE RMD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3700-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RMD-TIER-RATE(1) TO IRAM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF IRAT-RMD-AMT NOT < WS-RMD-TIER-LOW(WS-TX)
                   MOVE WS-RMD-TIER-RATE(WS-TX) TO IRAM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ROLLOVER FEE ON THE ROLLOVER AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3710-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN IRAM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO IRAM-ROLLOVER-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-STOP-FLAG = 'Y'
                   COMPUTE IRAM-ROLLOVER-AMT ROUNDED = IRAT-ROLLOVER-AMT
                       * IRAM-RATE / 200
               WHEN IRAT-TRAN-CD = 'CN'
                   MOVE 5.00 TO IRAM-ROLLOVER-AMT
               WHEN IRAT-TRAN-CD = 'DS'
                   MOVE 5.00 TO IRAM-ROLLOVER-AMT
               WHEN OTHER
                   COMPUTE IRAM-ROLLOVER-AMT ROUNDED = IRAT-ROLLOVER-AMT
                       * IRAM-RATE / 100
           END-EVALUATE
           IF IRAM-ROLLOVER-AMT > 0 AND IRAM-ROLLOVER-AMT < 5.00
               MOVE 5.00 TO IRAM-ROLLOVER-AMT
           END-IF
           IF IRAM-ROLLOVER-AMT > 100.00
               MOVE 100.00 TO IRAM-ROLLOVER-AMT
           END-IF
           ADD IRAM-ROLLOVER-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EXCESS AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3720-LIMIT-EXCESS.
           IF IRAM-EXCESS-AMT < 25.00
               MOVE 25.00 TO IRAM-EXCESS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-EXCESS-AMT > 50000.00
               MOVE 50000.00 TO IRAM-EXCESS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3730-COUNT-PAYMENT.
           IF IRAM-PAYMENT-CNT < 9999999
               ADD 1 TO IRAM-PAYMENT-CNT
           END-IF
           IF IRAM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO IRAM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RMD TIER TIER FOR THE ROLLOVER YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3740-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RMD-TIER-RATE(1) TO IRAM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF IRAM-ROLLOVER-YTD NOT < WS-RMD-TIER-LOW(WS-TX)
                   MOVE WS-RMD-TIER-RATE(WS-TX) TO IRAM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3750-COUNT-RETRY.
           IF IRAM-RETRY-CNT < 9999999
               ADD 1 TO IRAM-RETRY-CNT
           END-IF
           IF IRAM-RETRY-CNT NOT < 3
               MOVE 'Y' TO IRAM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RMD AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3760-FLAG-RMD.
           IF IRAT-RMD-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE IRAT-RMD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ROLLOVER YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3770-LIMIT-ROLLOVER.
           IF IRAM-ROLLOVER-YTD < 0.00
               MOVE 0.00 TO IRAM-ROLLOVER-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-ROLLOVER-YTD > 2500.00
               MOVE 2500.00 TO IRAM-ROLLOVER-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3780-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-DISTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD IRAM-DISTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD IRAM-DISTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRAM-DISTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-DISTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3790-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E945 OPERATOR REQUIRED
      * E742 RMD AMT NEGATIVE
      * E537 RMD AMT OVER 5000
      * E171 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3800-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAT-OPERATOR = SPACES
                   MOVE 'E945' TO WS-REJECT-CODE
               WHEN IRAT-RMD-AMT < 0
                   MOVE 'E742' TO WS-REJECT-CODE
               WHEN IRAT-RMD-AMT > 5000.00
                   MOVE 'E537' TO WS-REJECT-CODE
               WHEN IRAM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E171' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ROLLOVER FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3810-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN IRAT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO IRAT-ROLLOVER-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE IRAT-ROLLOVER-AMT ROUNDED = WS-WORK-AMT-2 *
                       IRAM-FEE-PCT / 200
               WHEN IRAM-REASON-CD = 'CMR'
                   MOVE 7.00 TO IRAT-ROLLOVER-AMT
               WHEN IRAM-REASON-CD = 'LJS'
                   MOVE 5.00 TO IRAT-ROLLOVER-AMT
               WHEN OTHER
                   COMPUTE IRAT-ROLLOVER-AMT ROUNDED = WS-WORK-AMT-2 *
                       IRAM-FEE-PCT / 100
           END-EVALUATE
           IF IRAT-ROLLOVER-AMT > 0 AND IRAT-ROLLOVER-AMT < 2.00
               MOVE 2.00 TO IRAT-ROLLOVER-AMT
           END-IF
           IF IRAT-ROLLOVER-AMT > 50.00
               MOVE 50.00 TO IRAT-ROLLOVER-AMT
           END-IF
           ADD IRAT-ROLLOVER-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3820-COUNT-ITEM.
           IF IRAM-ITEM-CNT < 9999999
               ADD 1 TO IRAM-ITEM-CNT
           END-IF
           IF IRAM-ITEM-CNT NOT < 6
               MOVE 'Y' TO IRAT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCESS YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3830-ACCRUE-EXCESS.
           IF IRAM-EXCESS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-EXCESS-YTD * IRAM-RATE / 100 / 360
               ADD WS-WORK-AMT TO IRAM-ROLLOVER-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3840-SPLIT-ROLLOVER.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE IRAT-ROLLOVER-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCESS SCHED TIER FOR THE ROLLOVER YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3850-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-EXCESS-SCHED-RATE(1) TO IRAM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF IRAM-ROLLOVER-YTD NOT < WS-EXCESS-SCHED-LOW(WS-TX)
                   MOVE WS-EXCESS-SCHED-RATE(WS-TX) TO IRAM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3860-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-CONTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRAM-CONTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD IRAM-CONTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRAM-CONTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-CONTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE IRA TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3870-DESCRIBE-IRA-TYPE.
           EVALUATE IRAM-IRA-TYPE-CD
               WHEN 'F'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'V'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'W'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E623 NAME REQUIRED
      * E209 FMV AMT NEGATIVE
      * E665 FMV AMT OVER 99999
      * E559 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3880-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-NAME = SPACES
                   MOVE 'E623' TO WS-REJECT-CODE
               WHEN IRAM-FMV-AMT < 0
                   MOVE 'E209' TO WS-REJECT-CODE
               WHEN IRAM-FMV-AMT > 99999.00
                   MOVE 'E665' TO WS-REJECT-CODE
               WHEN IRAM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E559' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3890-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAT-DISTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD IRAT-DISTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD IRAT-DISTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRAT-DISTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAT-DISTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AS -> AY WHEN EFFECTIVE DATE IS PAST
      * AY -> JR WHEN EFFECTIVE DATE IS PAST
      * JR -> TA WHEN EFFECTIVE DATE IS PAST
      * TA -> UM WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3900-SET-CHANNEL.
           EVALUATE IRAM-CHANNEL-CD
               WHEN 'AS'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AY' TO IRAM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AY'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JR' TO IRAM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JR'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TA' TO IRAM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UM' TO IRAM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E898 REJECT CD REQUIRED
      * E786 ROLLOVER AMT NEGATIVE
      * E772 ROLLOVER AMT OVER 250000
      * E543 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3910-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAT-REJECT-CD = SPACES
                   MOVE 'E898' TO WS-REJECT-CODE
               WHEN IRAT-ROLLOVER-AMT < 0
                   MOVE 'E786' TO WS-REJECT-CODE
               WHEN IRAT-ROLLOVER-AMT > 250000.00
                   MOVE 'E772' TO WS-REJECT-CODE
               WHEN IRAT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E543' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3920-COUNT-PAYMENT.
           IF IRAM-PAYMENT-CNT < 9999999
               ADD 1 TO IRAM-PAYMENT-CNT
           END-IF
           IF IRAM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO IRAM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ROLLOVER AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RMD YTD.
      *----------------------------------------------------------------*
       3930-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-RMD-YTD ROUNDED =
                   IRAM-ROLLOVER-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-RMD-YTD
           END-IF
           ADD IRAM-RMD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3940-COUNT-PAYMENT.
           IF IRAM-PAYMENT-CNT < 9999999
               ADD 1 TO IRAM-PAYMENT-CNT
           END-IF
           IF IRAM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO IRAM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3950-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   IRAM-DISTRIB-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE FMV YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3960-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-REGION-CD
                   ADD IRAM-FMV-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-FMV-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR IRA TYPE CD:
      * F -> V WHEN EFFECTIVE DATE IS PAST
      * V -> W WHEN EFFECTIVE DATE IS PAST
      * W -> Y WHEN EFFECTIVE DATE IS PAST
      * Y -> F WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3970-SET-IRA-TYPE.
           EVALUATE IRAM-IRA-TYPE-CD
               WHEN 'F'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ROLLOVER AMT.
      *----------------------------------------------------------------*
       3980-PRORATE-ROLLOVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAT-ROLLOVER-AMT ROUNDED =
                   IRAM-CONTRIB-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAT-ROLLOVER-AMT
           END-IF
           ADD IRAT-ROLLOVER-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CONTRIB YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3990-SPLIT-CONTRIB.
           COMPUTE WS-WORK-AMT = IRAM-CONTRIB-YTD / 12
           COMPUTE WS-WORK-AMT-2 = IRAM-CONTRIB-YTD - (WS-WORK-AMT * 12)
           COMPUTE IRAM-DISTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ROLLOVER AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4000-LIMIT-ROLLOVER.
           IF IRAT-ROLLOVER-AMT < 50.00
               MOVE 50.00 TO IRAT-ROLLOVER-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAT-ROLLOVER-AMT > 50000.00
               MOVE 50000.00 TO IRAT-ROLLOVER-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RMD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RMD AMT.
      *----------------------------------------------------------------*
       4010-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-RMD-AMT ROUNDED =
                   IRAM-RMD-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-RMD-AMT
           END-IF
           ADD IRAM-RMD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT ROLLOVER AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4020-SPLIT-ROLLOVER.
           COMPUTE WS-WORK-AMT = IRAM-ROLLOVER-AMT / 12
           COMPUTE WS-WORK-AMT-2 = IRAM-ROLLOVER-AMT - (WS-WORK-AMT *
               12)
           COMPUTE IRAM-DISTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4030-LIMIT-DISTRIB.
           IF IRAM-DISTRIB-YTD < 25.00
               MOVE 25.00 TO IRAM-DISTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-DISTRIB-YTD > 1000.00
               MOVE 1000.00 TO IRAM-DISTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFFECTIVE DATE IS PAST
      * BAT -> MAL WHEN EFFECTIVE DATE IS PAST
      * MAL -> TEL WHEN EFFECTIVE DATE IS PAST
      * TEL -> ONL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4040-SET-CHANNEL.
           EVALUATE IRAT-CHANNEL-CD
               WHEN 'BRN'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO IRAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO IRAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO IRAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO IRAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4050-DESCRIBE-REASON.
           EVALUATE IRAM-REASON-CD
               WHEN 'CMR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LJS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NRT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UGA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WBS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZYU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4060-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-TYPE-CD
                   ADD IRAM-WITHHOLD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-WITHHOLD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4070-DESCRIBE-TYPE.
           EVALUATE IRAM-TYPE-CD
               WHEN 'EA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE IRA TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4080-DESCRIBE-IRA-TYPE.
           EVALUATE IRAM-IRA-TYPE-CD
               WHEN 'F'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'V'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Y'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4090-ACCRUE-DISTRIB.
           IF IRAM-DISTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-DISTRIB-YTD * IRAM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4100-LIMIT-DISTRIB.
           IF IRAT-DISTRIB-AMT < 50.00
               MOVE 50.00 TO IRAT-DISTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAT-DISTRIB-AMT > 50000.00
               MOVE 50000.00 TO IRAT-DISTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCESS SCHED TIER FOR THE CONTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4110-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-EXCESS-SCHED-RATE(1) TO IRAM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF IRAM-CONTRIB-AMT NOT < WS-EXCESS-SCHED-LOW(WS-TX)
                   MOVE WS-EXCESS-SCHED-RATE(WS-TX) TO IRAM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RMD AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4120-LIMIT-RMD.
           IF IRAT-RMD-AMT < 50.00
               MOVE 50.00 TO IRAT-RMD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAT-RMD-AMT > 10000.00
               MOVE 10000.00 TO IRAT-RMD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ROLLOVER YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4130-FLAG-ROLLOVER.
           IF IRAM-ROLLOVER-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRAM-ROLLOVER-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RMD AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4140-SPLIT-RMD.
           COMPUTE WS-WORK-AMT = IRAT-RMD-AMT / 3
           COMPUTE WS-WORK-AMT-2 = IRAT-RMD-AMT - (WS-WORK-AMT * 3)
           COMPUTE IRAM-EXCESS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCESS YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4150-SPLIT-EXCESS.
           COMPUTE WS-WORK-AMT = IRAM-EXCESS-YTD / 12
           COMPUTE WS-WORK-AMT-2 = IRAM-EXCESS-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FMV YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4160-SPLIT-FMV.
           COMPUTE WS-WORK-AMT = IRAM-FMV-YTD / 2
           COMPUTE WS-WORK-AMT-2 = IRAM-FMV-YTD - (WS-WORK-AMT * 2)
           COMPUTE IRAM-ROLLOVER-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB YTD BY DIST CODE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4170-ACCUM-DIST-CODE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-DIST-CODE-CD
                   ADD IRAM-DISTRIB-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-DIST-CODE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-DISTRIB-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE IRA FIELDS TO IRVAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-IRVAL.
           INITIALIZE IRVAL-PARMS
           MOVE IRAM-WITHHOLD-YTD
             TO IRVAL-WITHHOLD-YTD
           MOVE IRAM-EXCESS-AMT
             TO IRVAL-EXCESS-AMT
           MOVE IRAM-ROLLOVER-AMT
             TO IRVAL-ROLLOVER-AMT
           MOVE IRAM-RMD-YTD
             TO IRVAL-RMD-YTD
           MOVE IRAM-FMV-YTD
             TO IRVAL-FMV-YTD
           MOVE IRAM-FMV-AMT
             TO IRVAL-FMV-AMT
           MOVE IRAM-NEXT-DUE-DATE
             TO IRVAL-NEXT-DUE-DATE
           MOVE IRAM-EFFECTIVE-DATE
             TO IRVAL-EFFECTIVE-DATE
           MOVE IRAM-MATURITY-DATE
             TO IRVAL-MATURITY-DATE
           MOVE IRAM-LAST-STMT-DATE
             TO IRVAL-LAST-STMT-DATE
           MOVE IRAM-TYPE-CD
             TO IRVAL-TYPE-CD
           MOVE IRAM-DIST-CODE-CD
             TO IRVAL-DIST-CODE-CD
           MOVE IRAM-CHANNEL-CD
             TO IRVAL-CHANNEL-CD
           MOVE IRAM-REGION-CD
             TO IRVAL-REGION-CD
           MOVE IRAM-HOLD-FLAG
             TO IRVAL-HOLD-FLAG
           MOVE IRAM-OVERRIDE-FLAG
             TO IRVAL-OVERRIDE-FLAG
           MOVE IRAM-STOP-FLAG
             TO IRVAL-STOP-FLAG
           MOVE IRAM-ITEM-CNT
             TO IRVAL-ITEM-CNT
           MOVE IRAM-CYCLE-CNT
             TO IRVAL-CYCLE-CNT
           MOVE IRAM-RATE
             TO IRVAL-RATE
           MOVE IRAM-MARGIN
             TO IRVAL-MARGIN
           MOVE IRAM-CONTACT
             TO IRVAL-CONTACT
           MOVE IRAM-ADDR
             TO IRVAL-ADDR
           CALL 'IRVAL' USING IRVAL-PARMS
           EVALUATE IRVAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE IRVAL-FMV-AMT
                     TO IRAM-FMV-AMT
                   MOVE IRVAL-HOLD-FLAG
                     TO IRAM-HOLD-FLAG
                   MOVE IRVAL-OVERRIDE-FLAG
                     TO IRAM-OVERRIDE-FLAG
                   MOVE IRVAL-STOP-FLAG
                     TO IRAM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-IRVAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE IRAT-KEY TO RD-KEY
           MOVE IRAT-TRAN-CD TO RD-CODE
           MOVE IRAT-DISTRIB-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE IRAT-KEY TO RD-KEY
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
           DISPLAY 'IRACLSE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'IRACLSE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
